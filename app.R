library(dplyr)
library(stringr)
library(rhandsontable)
#library(shinyalert)
library(plater)
library(tibble)
library(reactable)
library(rmarkdown)
library(shiny)
library(shinydashboard)
library(curl)
library(waiter)
#library(xlsx)

# creates base empty dataframe to view and fill later
make_dest <- function() {
  dest <- tibble(src_type = '', 
                 src_well = "", 
                 sample_name = "", 
                 customer_primer = "", 
                 bcl_primer = as.character(NA), 
                 dest_well = wells_colwise)
                 #mycolor = NA)
  dest
}

tab1 <- fluidRow(
  box(width = 12, height = 2800, status = "info", solidHeader = FALSE,
      title = "", collapsible = F,
      fluidRow(
        column(5, tags$p("Source plate/strip - enter sample information here"), 
               rHandsontableOutput('hot')
               ),
        column(7, 
               tags$p("Reaction plate preview"),
               reactableOutput('plate'),
               downloadButton('download', 'Download Opentrons script', style = 'margin-top:25px'),
               downloadButton('download_samples', 'Download sample sheet', style = 'margin-top:25px')
               #tags$p('hot preview'),
               #verbatimTextOutput('hot_preview')
               )
      ))
)

tab2 <- fluidRow(
  box(width = 12, status = "info", solidHeader = FALSE, title = "Opentrons protocol preview", collapsible = F,
      verbatimTextOutput('protocol_preview')
  ))

tab3 <- fluidRow(
  box(width = 12, status = 'info', solidHeader = FALSE, title = "Deck view", collapsible = F,
      htmlOutput('deck'))
)

ui <- dashboardPage(
  useWaiter(),
  #waiterOnBusy(),
  
  header = dashboardHeader(title = 'Generate Sanger Opentrons protocol', titleWidth = 800),
  sidebar = dashboardSidebar(disable = T),
  body = dashboardBody(
    tabsetPanel(
      tabPanel(title = "Enter samples", icon = icon("vials"),
               tab1),
      tabPanel(title = "Opentrons script preview", icon = icon('list'),
               tab2),
      tabPanel(title = "Deck view", icon = icon('table-cells'), 
      tab3)
    )
  )
)

server = function(input, output, session) {
  ### read template
  protocol_url <- "https://raw.githubusercontent.com/angelovangel/opentrons/main/protocols/01-Sanger-setup-BCL.py"
  
  if (curl::has_internet()) {
    waiter_show(html = spin_wave())
    con <- url(protocol_url)
    protocol_template <- readLines(con, warn = F)
    close(con)
    waiter_hide()
  } else {
    protocol_template <- readLines("sanger-otp-template.py", warn = F)
  }
  
  
  # REACTIVES
  hot <- reactive({
    if(!is.null(input$hot)) {
      as_tibble(hot_to_r(input$hot))
    } else{
    make_dest()
    }
  })
  
  plate <- reactive({
    if(!is.null(input$hot)) {
      df <- hot() %>% mutate(label = str_c(sample_name, "<br>", bcl_primer))
      plater::view_plate(
        #hot_to_r(input$hot) , 
        df,
        well_ids_column = 'dest_well', columns_to_display = c('label')
      )
    } else {
      plater::view_plate(make_dest() %>% mutate(label = dest_well), well_ids_column = 'dest_well', columns_to_display = c('label'))
    }
  })
  
  # CORE 
  myvalues <- reactive({
    # use only ones where there is sample name and source type selected
    # this avoids a bug where deleting a well did not leave an empty entry in the vector
    i <-  which(hot()$src_type %in% 'plate')
    j <-  which(hot()$src_type %in% 'strip')
    sourcewells1 <- rep('', 96)
    sourcewells2 <- rep('', 96)
    sourcewells1[i] <- hot()$src_well[i]
    sourcewells2[j] <- hot()$src_well[j]
    
    # this took  while to figure out, the length of the result is the same as the first arg of match
    sourcewells3 <- bcl_primers$primer_well[match(hot()$bcl_primer, bcl_primers$primer_name)] %>% str_replace_na(replacement = '')
    
    # when something is deleted in hot() it becomes ''
    volume1 <- rep(0, 96)
    k <- which(hot()$bcl_primer %in% bcl_primers$primer_name & hot()$src_type == 'plate')
    l <- which(hot()$customer_primer != '' & hot()$src_type == 'plate')
    volume1[k] <- 10
    volume1[l] <- 15
    
    volume2 <- rep(0, 96)
    m <- which(hot()$bcl_primer %in% bcl_primers$primer_name & hot()$src_type == 'strip')
    n <- which(hot()$customer_primer != '' & hot()$src_type == 'strip')
    volume2[m] <- 10
    volume2[n] <- 15
    
    volume3 <- rep(0, 96)
    o <- which(hot()$bcl_primer != '')
    volume3[o] <- 5
    
    
    c(
      str_flatten(sourcewells1, collapse = "','"),  
      # ! this collapses NA vector to 1 NA !!! See:
      # rep(' ', 10) %>% str_flatten(collapse = ', ')
      # rep(NA, 10) %>% str_flatten(collapse = ', ')
      
      str_flatten(volume1, collapse = ", "),
      str_flatten(sourcewells2, collapse = "','"),
      str_flatten(volume2, collapse = ", "),
      str_flatten(sourcewells3, collapse = "','"),
      str_flatten(volume3, collapse = ", ")
    ) 
  })
  
  myprotocol <- reactive({
    str_replace(protocol_template, 'sourcewells1=.*', paste0("sourcewells1=['", myvalues()[1], "']")) %>%
    str_replace('volume1=.*', paste0('volume1=[', myvalues()[2], ']')) %>%
    str_replace('sourcewells2=.*', paste0("sourcewells2=['", myvalues()[3], "']")) %>%
    str_replace('volume2=.*', paste0('volume2=[', myvalues()[4], ']')) %>%
    str_replace('sourcewells3=.*', paste0("sourcewells3=['", myvalues()[5], "']")) %>%
    str_replace('volume3=.*', paste0('volume3=[', myvalues()[6], ']'))
  })
  
  # RENDERS
  
  output$hot <- renderRHandsontable({
    rhandsontable(hot(),
                  rowHeaders = NULL, height = 2800, stretchH = "all") %>%
      hot_col(col = 'src_type', type = 'dropdown', source = c('strip', 'plate'), strict = T) %>%
      #hot_validate_character('src_type', choices = c('strip', 'plate'), allowInvalid = F) %>%
      hot_col(col = 'src_well', type = 'dropdown', source = wells_colwise, strict = T) %>%
      hot_col(col = 'bcl_primer', type = 'autocomplete', source = bcl_primers$primer_name) %>%
      hot_col(col = 'dest_well', readOnly = T) %>%
      hot_cols(colWidths = c(60, 50, 90, 90, 90, 50))
      
  })
  
  output$plate <- renderReactable({
    reactable(plate()$label, 
              highlight = T, wrap = F, 
              bordered = T, compact = T, 
              fullWidth = T, sortable = F,
              defaultColDef = colDef(
                minWidth = 50, html = TRUE, style = list(fontSize = '80%'),
                headerStyle = list(background = "#f7f7f8"))
              )
  })
  
  # output$hot_preview <- renderText({
  #   #hot()$src_well
  #     myvalues()[2]
  # })
  
  output$protocol_preview <- renderPrint({
    write(myprotocol(), file = "")
  })
  
  output$deck <- renderUI({
    HTML('<img src="deck.png" height="600">')
  })
  
  ### Downloads
  output$download <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), '-sanger-protocol.py')
    },
    content = function(con) {
      write(myprotocol(), con)
    }
  )
  
  output$download_samples <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), '-sanger-samples.csv')
    },
    content = function(con) {
      write.csv(hot(), con, row.names = F)
    }
  )
  
}

shinyApp(ui, server)