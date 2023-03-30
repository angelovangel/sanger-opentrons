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

# creates base empty dataframe to view and fill later
make_dest <- function() {
  dest <- tibble(src_type = 'plate', 
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
        column(4, tags$p("Source plate/strip - enter sample information here"), 
               rHandsontableOutput('hot')
               ),
        column(8, 
               tags$p("Reaction plate preview"),
               reactableOutput('plate'),
               tags$p('hot preview'),
               verbatimTextOutput('hot_preview'))
      ))
)

tab2 <- fluidRow(
  box(width = 12, status = "info", solidHeader = FALSE, title = "Opentrons protocol preview", collapsible = F,
      verbatimTextOutput('protocol_preview')
  ))

ui <- dashboardPage(
  #useShinyalert(),
  
  header = dashboardHeader(title = 'Generate Sanger Opentrons protocol', titleWidth = 800),
  sidebar = dashboardSidebar(disable = T),
  body = dashboardBody(
    tabsetPanel(
      tabPanel(title = "Enter samples", icon = icon("vials"),
               tab1),
      tabPanel(title = "Opentrons script preview", icon = icon('list'),
               tab2)
    )
  )
)

server = function(input, output, session) {
  ### read template
  protocol_template <- readLines('sanger-otp-template.py', warn = F)
  
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
    sourcewells1 <- rep('', 96)
    sourcewells1[hot()$src_type == 'plate'] <- hot()$src_well
    
    sourcewells2 <- rep('', 96)
    sourcewells2[hot()$src_type == 'strip'] <- hot()$src_well
    
    # this took  while to figure out, the length of the result is the same as the first arg of match
    sourcewells3 <- bcl_primers$primer_well[match(hot()$bcl_primer, bcl_primers$primer_name)] %>% str_replace_na(replacement = ' ')
    
    # when something is deleted in hot() it becomes ''
    volume1 <- rep(0, 96)
    volume1[hot()$bcl_primer != ''] <- 10
    volume1[hot()$customer_primer != ''] <- 15
    
    
    volume2 <- rep(0, 96)
    volume2[hot()$bcl_primer != ''] <- 10
    volume2[hot()$customer_primer != ''] <- 15
    
    volume3 <- rep(5, 96)[hot()$sample_name != '' & hot()$bcl_primer != ''] %>% str_replace_na(replacement = '0') 
    
    # premix_pos1 <- which(hot()$customer_primer != '' & hot()$sample_name != '' & hot()$src_type == 'plate')
    # volume1[premix_pos1] <- 15
    # 
    # premix_pos2 <- which(hot()$customer_primer != '' & hot()$sample_name != '' & hot()$src_type == 'strip')
    # volume2[premix_pos2] <- 15
    
    # set these to 15 when customer primer is used
    #volume1[!is.na(hot()$customer_primer)] <- 15
    #volume2[hot()$customer_primer != ''] <- 15
    
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
      hot_validate_character('src_type', choices = c('strip', 'plate'), allowInvalid = F) %>%
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
  
  output$hot_preview <- renderText({
    #hot()$src_well
    myvalues()[2]
  })
  
  output$protocol_preview <- renderPrint({
    write(myprotocol(), file = "")
  })
}

shinyApp(ui, server)