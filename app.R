library(rhandsontable)
library(shiny)
library(shinydashboard)
#library(shinyalert)
library(plater)
library(tibble)
library(stringr)
library(reactable)
library(dplyr)
library(rmarkdown)

# creates base empty dataframe to view and fill later
make_dest <- function() {
  dest <- tibble(src_type = NA, 
                 src_well = wells_colwise, 
                 sample_name = as.character(NA), 
                 customer_primer = as.character(NA), 
                 bcl_primer = NA, 
                 dest_well = wells_colwise) 
                 #mycolor = as.character(NA))
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

ui <- dashboardPage(
  #useShinyalert(),
  
  header = dashboardHeader(title = 'Generate Sanger Opentrons protocol', titleWidth = 800),
  sidebar = dashboardSidebar(disable = T),
  body = dashboardBody(
    tabsetPanel(
      tabPanel(title = "Enter samples", icon = icon("vials"),
               tab1
      )
    )
  )
)

server = function(input, output, session) {
  
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
      plater::view_plate(make_dest(), well_ids_column = 'dest_well', columns_to_display = c('sample_name'))
    }
  })
  
  # RENDERS
  
  output$hot <- renderRHandsontable({
    rhandsontable(hot(), 
                  rowHeaders = NULL, height = 2800, stretchH = "all") %>%
      hot_col(col = 'src_type', type = 'autocomplete', source = c('strip', 'plate'), strict = T) %>%
      hot_col(col = 'src_well', type = 'dropdown', source = wells_colwise, strict = T) %>%
      hot_col(col = 'bcl_primer', type = 'autocomplete', source = bcl_primers$primer_name) %>%
      hot_col(col = 'dest_well', readOnly = T) %>%
      hot_cols(colWidths = c(50, 50, 90, 90, 90, 50))
      
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
}

shinyApp(ui, server)