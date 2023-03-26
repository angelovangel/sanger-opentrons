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
  dest <- tibble(src_type = as.character(NA), 
                 src_well = wells_colwise, 
                 sample_name = as.character(NA), 
                 customer_primer = as.character(NA), 
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
               reactableOutput('plate'))
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
    sourcewells1 <- wells_colwise[hot()$sample_name != '' & hot()$src_type == 'plate'] %>% str_replace_na(replacement = ' ')
    sourcewells2 <- wells_colwise[hot()$sample_name != '' & hot()$src_type == 'strip'] %>% str_replace_na(replacement = ' ')
    sourcewells3 <- tuberack_wells[hot()$bcl_primer != ''] %>% str_replace_na(replacement = ' ')
      
    volume1 <- rep(10, 96)[hot()$sample_name != '' & hot()$src_type == 'plate'] %>% str_replace_na(replacement = '0')
    volume2 <- rep(10, 96)[hot()$sample_name != '' & hot()$src_type == 'strip'] %>% str_replace_na(replacement = '0')
    volume3 <- rep(5, 96)[hot()$bcl_primer != ''] %>% str_replace_na(replacement = '0') 
    # see this how it works
    # rep(1, 96)[match(c('barcode03', '', '', 'barcode01'), barcodes)]
    
    # set these to 15 when customer primer is used
    #volume1[!is.na(hot()$customer_primer)] <- 15
    #volume2[hot()$customer_primer != ''] <- 15
    
    c(
      str_flatten(sourcewells1, collapse = "','"),  
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
      hot_col(col = 'src_type', type = 'autocomplete', source = c('strip', 'plate'), strict = T) %>%
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
  
  output$protocol_preview <- renderPrint({
    write(myprotocol(), file = "")
  })
}

shinyApp(ui, server)