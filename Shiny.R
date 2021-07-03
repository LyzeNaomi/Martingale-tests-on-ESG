ui <- fluidPage(
  fileInput('file1', 'Insert File', accept = c(".xlsx", ".csv")),
  textInput('file1sheet','Name of Sheet (Case-Sensitive)'),
  tableOutput("value")
)

server <- function(input, output) {
  
  sheets_name <- reactive({
    if (!is.null(input$file1)) {
      return(excel_sheets(path = input$file1$datapath))  
    } else {
      return(NULL)
    }
  })
  
  output$value <- renderTable({
    if (!is.null(input$file1) && #not null, we read the file
        (input$file1sheet %in% sheets_name())) {
      return(openxlsx::read.xlsx(input$file1$datapath, 
                        sheet = input$file1sheet, colNames = input$header, 
                        cols = c(input$columns)))
    } else {
      return(NULL)
    }
  })
}

shinyApp(ui, server)