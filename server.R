#setwd("~/WORK/ESG/R_Codes")
source('Re_calculation.R')

shinyServer(function(session, input, output){
  #input$file is a data frame and contains the details around the name and temp location of the files uploaded
  # reactive output display the content of the input$file dataframe
    data <- reactive({
      req(input$file)
      openxlsx::read.xlsx(input$file$datapath, colNames = input$header)
    })

    sheet_names <- reactive({
      req(input$file_short)
      sheet_names <- readxl::excel_sheets(path = input$file_short$datapath)
    })

##    
    filtereddata <- eventReactive({
      input$update
      data()
    }, {
      req(data())
      if(is.null(input$select)|| input$select==''){return()}
      else
        data()[,colnames(data()) %in% input$select]
    })
    
    observeEvent(data(), {
      updateSelectInput(session, 'select', choices = colnames(data()))
    })
    
##    
    filtereddata_short <- eventReactive({
     input$Import ; req(input$file_short)
      sheet_names()
    },{
      req(sheet_names())
      if(is.null(input$sheets)|| input$sheets==''){return()}
      else{
        SheetList <- lapply(input$sheets, openxlsx::read.xlsx, xlsxFile = input$file_short$datapath)
        SheetList <- lapply(SheetList, function(x){x[-1,]}) ; SheetList <- lapply(SheetList, function(x){x[,-1]}) ; names(SheetList) <- input$sheets
        SheetList <- lapply(SheetList, Cum_Returns)
      }
      SheetList
    })

    observeEvent(sheet_names(), {
      updateSelectInput(session, 'sheets', choices = sheet_names())
    })

##
    returns_data <- reactive({
      disc <- c() ; req(filtereddata()) ; ncols = as.numeric(ncol(filtereddata()))
      for (i in 3: ncols){
        disc[[i]]=  subset(filtereddata(), select=c(names(filtereddata())[1], names(filtereddata())[2], names(filtereddata())[i]))
        disc[[i]] = Data_Cleaning(tidyr::spread(disc[[i]], names(filtereddata())[2], names(filtereddata())[i]))
        disc[[i]] = Gross_Returns(disc[[i]])
        disc[[i]] = Cum_Returns(disc[[i]])
      }
      disc <- list.clean(disc) ; names(disc) <- names(filtereddata())[3:ncols]
      disc
    })
  
 ##   
    returns_data1 <-eventReactive({ #just for viewing
      input$view 
      returns_data()
    },{
      req(returns_data())
      returns_data()[names(returns_data()) %in% input$assets]
    })
    
    observeEvent(returns_data(), {
      updateSelectInput(session, 'assets', choices = names(returns_data()))
    })
  
 ##   
    returns_dataSHORT <-eventReactive({ #just for viewing
      input$VIEW
      filtereddata_short()
    },{
      req(filtereddata_short())
      filtereddata_short()[names(filtereddata_short()) %in% input$assets_short]
    })
    
    observeEvent(filtereddata_short(), {
      updateSelectInput(session, 'assets_short', choices = names(filtereddata_short()))
    })
  
    
  ##      
    discount_data <- eventReactive({
      input$validate 
      data()
    }, {
      req(data())
      if(is.null(input$selectt)|| input$selectt==''){return()}
      else
        data()[,colnames(data()) %in% input$selectt]
    })
    
    observeEvent(data(), {
      updateSelectInput(session, 'selectt', choices = colnames(data()))
    })
 
       
  ##
    discount_short <- eventReactive({
      input$vali_disc ; req(input$file_short)
      sheet_names()
    },{
      req(sheet_names())
      if(is.null(input$sel_disc)|| input$sel_disc==''){return()}    
      else{
        disc_short <- openxlsx::read.xlsx(xlsxFile = input$file_short$datapath, sheet = input$sel_disc, colNames = TRUE)
        disc_short <- disc_short[,-1] ; disc_short <- disc_short[-1,] ; names(disc_short) <- input$sel_disc
      }
      disc_short = 1/disc_short
      disc_short
    })
    
    observeEvent(sheet_names(), {
      updateSelectInput(session, 'sel_disc', choices = sheet_names())
    })
    
    
    output$filedf <- renderTable({
      if(is.null(input$file)) {return()}
      input$file#file input data frame object that contains the file attributes
    })
    
    #extract the file path from the file
    output$filedf2 <- renderTable({
      if(is.null(input$file)){return()}
      input$file$datapath
    })
    
    # Code to display the xture of the input file object
    output$fileob <- renderPrint({
      if(is.null(input$file)){return()}
      str(input$file)
    })
    
    deflators <- reactive({
      req(discount_data())
      data2 = tidyr::spread(discount_data(), names(discount_data())[2], names(discount_data())[3])
      deflators = Data_Cleaning(data2)
      deflators
    })
  
    init <- reactive({ # enter the initiation time
      req(deflators()) ; validate(need(is.numeric(input$num), 'Please input a number'))
      init <- as.numeric(input$num)
      init <- ifelse(init >=ncol(deflators()), ncol(deflators())-1, init)
      init
    })
    
    
  ##for short format
    init_s <- reactive({ # enter the initiation time
      req(discount_short()) ; validate(need(is.numeric(input$numm), 'Please input a number'))
      init_s <- as.numeric(input$numm)
      init_s <- ifelse(init_s >=ncol(discount_short()), ncol(discount_short())-1, init_s)
      init_s
    })
    
    
    ##perform the martingale test--long format
    Martingale_test <- reactive ({
      req(returns_data()) ; req(deflators()) ; req(init()) ; req(filtereddata()) ; ncols = as.numeric(ncol(filtereddata())) ; Martingale_test <- c()
      Period = (init()+1):ncol(deflators())
      if(init()==0){
        for (i in 1:(ncols-2)){
          Martingale_test[[i]] = as.data.frame(test0(deflators(), returns_data()[[i]]), col.names = c("Mean", "quant_based_stdev","empe_quant"))
          Martingale_test[[i]]  <- cbind(Period, Martingale_test[[i]])
        }
      }
      
      else
        for (i in 1:(ncols-2)){
          Martingale_test[[i]] = as.data.frame(testx(deflators(), returns_data()[[i]], init()), col.names = c("Mean", "quant_based_stdev","empe_quant"))
          Martingale_test[[i]]  <- cbind(Period, Martingale_test[[i]])    
        }
      names(Martingale_test) <- names(returns_data())
      Martingale_test
    })
  
   
  ##perform the martingale test--short
    Martingale_test_short <- reactive ({
      req(filtereddata_short()) ; req(discount_short()) ; req(init_s()) ; ncols = as.numeric(length(filtereddata_short())) ; Martingale_test <- c()
      Period = (init_s()+1):ncol(discount_short())

      if(init_s()==0){
        for (i in 1:(ncols)){
          Martingale_test[[i]] = as.data.frame(test0(discount_short(), filtereddata_short()[[i]]), col.names = c("Mean", "quant_based_stdev","empe_quant"))
          Martingale_test[[i]]  <- cbind(Period, Martingale_test[[i]])
        }
      }

      else
        for (i in 1:(ncols)){
          Martingale_test[[i]] = as.data.frame(testx(discount_short(), filtereddata_short()[[i]], init_s()), col.names = c("Mean", "quant_based_stdev","empe_quant"))
          Martingale_test[[i]]  <- cbind(Period, Martingale_test[[i]])
        }
      names(Martingale_test) <- names(filtereddata_short())
      Martingale_test
    })
 
       
   ##    plots for long data format
     plot_results <- reactive({
       req(Martingale_test()) ; plot_results <-c() ; req(filtereddata()) ; ncols = as.numeric(ncol(filtereddata()))
       for (i in 1:(ncols-2)){
         plot_results[[i]] = Martingale_test()[[i]][,c('Period', input$vars), drop=FALSE]
         plot_results[[i]] <- melt(plot_results[[i]] , id="Period")  # convert to long format    
       }
       plot_results
     })
  ##   
     output$plotInput = renderPlotly({
       req(plot_results()) ; req(filtereddata()) ; ncols = as.numeric(ncol(filtereddata())) ; plots <- c() ; req(Martingale_test())
       name <- names(Martingale_test())
       for (i in 1:(ncols-2)){
         plots[[i]] = ggplotly(ggplot(data = plot_results()[[i]],
                                      aes(x=Period, y=value, colour=variable, group = 1))+geom_point() + theme(legend.title = element_blank())
                               + facet_wrap(~name[i]) 
                               
                               , tooltip = c("y", "text"))%>% layout(height = 1000, width = 1000) 
       }
       p <- subplot(plots[1:(input$nplots)],nrows = 2, shareX = TRUE, heights = c(0.5, 0.5), 
                    margin = 0.05) 
       dev.off()
       p
     })
     
     
     
     ##   plots for short data format 
     plot_results_s <- reactive({
       req(Martingale_test_short()) ; plot_results <-c() ; req(filtereddata_short()) ; ncols = as.numeric(length(filtereddata_short()))
       for (i in 1:(ncols)){
         plot_results[[i]] = Martingale_test_short()[[i]][,c('Period', input$vars_s), drop=FALSE]
         plot_results[[i]] <- melt(plot_results[[i]] , id="Period")  # convert to long format    
       }
       plot_results 
     })
     
     ##   
     output$plotInput_s = renderPlotly({
       req(plot_results_s()) ; req(filtereddata_short()) ;  ncols = as.numeric(length(filtereddata_short())) ; plots <- c() ; req(Martingale_test_short())
       name <- names(Martingale_test_short())
       for (i in 1:(ncols)){
         plots[[i]] = ggplotly(ggplot(data = plot_results_s()[[i]],
                                      aes(x=Period, y=value, colour=variable, group = 1))+geom_point() + theme(legend.title = element_blank())
                               + facet_wrap(~name[i]) +  theme(legend.position = "none")
                               
                               , tooltip = c("y", "text"))%>% layout(height = 1000, width = 1000) 
       }
       p <- subplot(plots[1:(input$nplots_s)], nrows = 2, shareX = TRUE, heights = c(0.5, 0.5), 
                    margin = 0.05) 
       dev.off()
       p
     })
    ##output$short <- renderTable(names(filtereddata_short()))
    output$ret <- renderTable(returns_data1()) 
    output$def <- renderTable(discount_data())
    output$R <- renderTable(Martingale_test()[[2]])
    output$dat <- renderTable(names(returns_data()))

    output$ret_s <- renderTable(filtereddata_short()) 
    output$defla_s <- renderTable(discount_short())
    output$R_s <- renderTable(Martingale_test_short()[[2]])
    output$dat_s <- renderTable(names(returns_dataSHORT()))
    
    output$downloadData <- downloadHandler(
      filename = function(){
        paste('Results','csv',sep = '.')
      },
      content = function(file){
        write.csv(Martingale_test(), file, row.names = FALSE)
      }
    )
    
    output$downloadPlot <- downloadHandler(
      filename = function(){
        paste('plot','png',sep = '.')
      },
      content = function(file){
        ggsave(file, plotInput)
      }
    )
  
    output$tb <-renderUI({
      if(is.null(input$file) & is.null(input$file_short)) {return()}
      else
        if (input$format=='Long'){
        tabsetPanel(
          #tabPanel('Input File Object DF', tableOutput('filedf'), tableOutput('filedf2')),
          #tabPanel('test Object Structure', verbatimTextOutput('testob')),
          tabPanel('Dataset', tableOutput('dat')),
          tabPanel('Returns', tableOutput('ret')),
          tabPanel('Deflators',tableOutput('def')),
          #tabPanel('Results', tableOutput('R')),, downloadButton('downloadPlot', 'Download Plot')
          tabPanel('Results', tableOutput('R'), downloadButton('downloadData', 'Download Data')),
          tabPanel('Plot', plotlyOutput('plotInput')))}
        
        else if (input$format=='Short'){
        tabsetPanel(
          tabPanel('Dataset', tableOutput('dat_s')),
          tabPanel('Returns', tableOutput('ret_s')),
          tabPanel('Deflators',tableOutput('def_s')),
          tabPanel('Results', tableOutput('R_s'), downloadButton('downloadData', 'Download Data')),
          tabPanel('Plot', plotlyOutput('plotInput_s')))}
    })
  })