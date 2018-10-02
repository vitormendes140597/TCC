server <- function(input, output) {

  # Defining some initial values
  #rv <- reactiveValues(histLibrary = "r")
  
  output$dadosSP <- DT::renderDataTable({
    datatable(sp,class="cell-border",options = list(
      scrollX = TRUE,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
      ))
    })
  
  output$dadosMogi <- DT::renderDataTable({
    datatable(mogi,class="cell-border",options = list(
      scrollX = TRUE,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    ))
  })
  
  ##############################################
  # Histogramas São Paulo
  ##############################################
  # Escolhe apenas as variáveis numéricas para plotar os histogramas
  output$escolherHistVarsSP <- renderUI({
    columns <- colnames(select_if(sp,is.numeric))
    selectInput(
      "spHistVars",
      label = "Coluna",
      choices = columns,
      selected = columns[1]
    )
  })
  
  
  spHistData <- reactive({
    variableName = input$spHistVars
    if(variableName == "mediaHorariaPpm") {
      description <- "Concentração de Monóxido de Carbono"
    } else if(variableName == "minHorariaPpm") {
      description <- "Média das menores concentrações de CO por hora"
    } else if(variableName == "mediaMovel") {
      description <- "Concentração da Média Móvel"
    } else if (variableName == "maxHorariaPpm") {
      description <- "Média das Maiores Concentrações de CO por hora"
    } else {
      description <- "Indice numérico de Qualidade"
    }
    
    result <- list(
      "variableName" = variableName,
      "description" = description,
      "data" = sp[,variableName]
    )
    return(result)
  })

  output$rBaseHistSP <- renderPlot({
    dataList <- spHistData()
    data <- dataList$data
    description <- dataList$description
    
    hist(data,main= "São Paulo", xlab = description, col = "lightblue",ylab = "Frequência")
  }) 
  
  output$rPlotlyHistSP <- renderPlotly({
    dataList <- spHistData()
    data <- dataList$data
    description <- dataList$description
    
    plot_ly(x = data, type = "histogram") %>% layout(
      title = "São Paulo",
      xaxis = list(title = description)
    )
  })
  
  output$rHChartHistSP <- renderHighchart({
    dataList <- spHistData()
    data <- dataList$data
    description <- dataList$description
    
    hchart(data,color = "#6666ff") %>% hc_title(
      text = "<h3>São Paulo</h3>",color = "blue"
    ) %>%  hc_xAxis(title = list(description)) %>%
    hc_add_theme(hc_theme_flat()) 
  })
  
  output$rGgplotHistSP <- renderPlot({
    dataList <- spHistData()
    data <- dataList$data
    description <- dataList$description
    
    ggplot(sp, aes(x = eval(parse(text = dataList$variableName)))) +
      geom_histogram(binwidth = 1,fill = "blue") + scale_x_continuous(name = description) + 
      scale_y_continuous(name = "Qtde de Observações") + ggtitle("São Paulo") + 
      theme(plot.title = element_text(face="bold", size = 14))
  })
  
  
  ##############################################
  # Histogramas Mogi das Cruzes
  ##############################################
  
  output$escolherHistVarsMogi <- renderUI({
    columns <- colnames(select_if(mogi,is.numeric))
    selectInput(
      "mogiHistVars",
      label = "Coluna",
      choices = columns,
      selected = columns[1]
    )
  })
  
  
  mogiHistData <- reactive({
    variableName = input$mogiHistVars
    if(variableName == "mediaHorariaPpm") {
      description <- "Concentração de Monóxido de Carbono"
    } else if(variableName == "minHorariaPpm") {
      description <- "Média das menores concentrações de CO por hora"
    } else if(variableName == "mediaMovel") {
      description <- "Concentração da Média Móvel"
    } else if (variableName == "maxHorariaPpm") {
      description <- "Média das Maiores Concentrações de CO por hora"
    } else {
      description <- "Indice numérico de Qualidade"
    }

    result <- list(
      "variableName" = variableName,
      "description" = description,
      "data" = mogi[,variableName]
    )
    return(result)
  })

  output$rBaseHistMogi <- renderPlot({
    dataList <- mogiHistData()
    data <- dataList$data
    description <- dataList$description

    hist(data,main= "Mogi das Cruzes", xlab = description, col = "lightblue")
  })

  output$rPlotlyHistMogi <- renderPlotly({
    dataList <- mogiHistData()
    data <- dataList$data
    description <- dataList$description

    plot_ly(x = data, type = "histogram") %>% layout(
      title = "Mogi das Cruzes",
      xaxis = list(title = description)
    )
  })

  output$rHChartHistMogi <- renderHighchart({
    dataList <- mogiHistData()
    data <- dataList$data
    description <- dataList$description

    hchart(data,color = "#6666ff") %>% hc_title(
      text = "<h3>Mogi das Cruzes</h3>",color = "blue"
    ) %>%  hc_xAxis(title = list(description)) %>%
      hc_add_theme(hc_theme_flat())
  })

  output$rGgplotHistMogi <- renderPlot({
    dataList <- mogiHistData()
    data <- dataList$data
    description <- dataList$description

    ggplot(mogi, aes(x = eval(parse(text = dataList$variableName)))) +
      geom_histogram(binwidth = 1,fill = "blue") + scale_x_continuous(name = description) +
      scale_y_continuous(name = "Qtde de Observações") + ggtitle("Mogi das Cruzes") +
      theme(plot.title = element_text(face="bold", size = 14))

  })
  
  ##############################################
  # Histogramas Mogi das Cruzes e SP
  ##############################################
  output$histSPMogi <- renderPlotly({
    mogiData <- mogiHistData()
    spData <- spHistData()
    
    plot_ly(alpha = 0.6) %>%
      add_histogram(x = sp[[spData$variableName]], name = "São Paulo") %>%
      add_histogram(x = mogi[[mogiData$variableName]], name = "Mogi das Cruzes") %>%
      layout(barmode = "overlay")
  })
  
  
  ##############################################
  # Box Plot São Paulo
  ##############################################

  output$rGgplotBoxPlotSP <- renderPlot({
    spByDay <- sp %>% group_by(date,month) %>% 
      summarise(indicador = max(eval(parse(text = input$spBoxPlotVars ))))
    
    ggplot(spByDay,aes(x = month, y = indicador, fill = month)) + 
      geom_boxplot() + scale_x_discrete(name = "Mês") + ggtitle("São Paulo")+
      scale_y_continuous(name = input$spBoxPlotVars)
  })
  
  observeEvent(input$spBoxPlotVars, {
    spByDay <- sp %>% group_by(date,month) %>% 
      summarise(indicador = max(eval(parse(text = input$spBoxPlotVars ))))
    
    output$rPlotlyBoxPlotSP <- renderPlotly({
      plot_ly(spByDay, y = ~indicador, color = ~month, type = "box") %>% 
        layout(title = "São Paulo", yaxis = list(title = input$spBoxPlotVars))
    })
  })
  
  ##############################################
  # Box Plot Mogi
  ##############################################
  
  output$rGgplotBoxPlotMogi <- renderPlot({
    mogiByDay <- mogi %>% group_by(date,month) %>% 
                          summarise(indicador = max(eval(parse(text = input$mogiBoxPlotVars ))))
    
    ggplot(mogiByDay,aes(x = month, y = indicador, fill = month)) + 
      geom_boxplot() + scale_x_discrete(name = "Mês") + ggtitle("Mogi das Cruzes")+
      scale_y_continuous(name = input$mogiBoxPlotVars)
  })
  
  observeEvent(input$mogiBoxPlotVars ,{
    output$rPlotlyBoxPlotMogi <- renderPlotly({
      mogiByDay <- mogi %>% group_by(date,month) %>%
      summarise(indicador = max(eval(parse(text = input$mogiBoxPlotVars ))))
      
      print(mogiByDay)
      
      plot_ly(mogiByDay, y = ~indicador, color = ~month, type = "box") %>% 
        layout(title = "Mogi das Cruzes", yaxis = list(title = input$mogiBoxPlotVars))
    })
  })
  
  ##################################################
  # Box Plot Dias da Semana SP
  ##################################################
  output$rPlotlyBoxPlotDiarySP <- renderPlotly({
      plot_ly(sp, x = ~month, y = ~mediaMovel, color = ~weekday, type = "box") %>%
      layout(boxmode = "group", xaxis = list(title="Dia da Semana"), yaxis = list(title="Distribuição da Média Móvel"))
  }) 
  
  output$rGgplotBoxPlotDiarySP <- renderPlot({
    ggplot(sp, aes(x = weekday, y = mediaMovel, fill = weekday)) + geom_boxplot() +
      facet_wrap(~ month) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$rHCBoxPlotDiarySP <- renderHighchart({
    hcboxplot(x = sp$mediaMovel, var = sp$weekday,var2 = sp$month,outliers = TRUE) %>%
      hc_chart(type = "column")
  })
  
  ##################################################
  # Box Plot Dias da Semana Mogi
  ##################################################
  output$rPlotlyBoxPlotDiaryMogi <- renderPlotly({
    plot_ly(mogi, x = ~month, y = ~mediaMovel, color = ~weekday, type = "box") %>%
      layout(boxmode = "group", xaxis = list(title="Dia da Semana"), yaxis = list(title="Média Móvel"))
  }) 
  
  output$rGgplotBoxPlotDiaryMogi <- renderPlot({
    ggplot(mogi, aes(x = weekday, y = mediaMovel, fill = weekday)) + geom_boxplot() +
      facet_wrap(~ month) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$rHCBoxPlotDiaryMogi <- renderHighchart({
    hcboxplot(x = mogi$mediaMovel, var = mogi$weekday,var2 = mogi$month,outliers = TRUE) %>%
      hc_chart(type = "column")
  })
 
  ################################
  # Análise Mensal
  ################################
  
  output$monoxido_por_dia_mes_sp <- renderHighchart({
    
    by_month <- sp %>% filter(month %in% c("Junho","Julho","Agosto","Setembro")) %>%
      group_by(month,date) %>% 
      summarise(
        mediaMovel = max(mediaMovel)
      ) %>% mutate(indiceDia = row_number())
    
    l <- split(by_month, f = by_month$month)
    
    highchart() %>% 
      hc_xAxis(categories = unique(by_month$indiceDia), title = list(text = "Dias")) %>%
      hc_yAxis(title = list("Concentração CO(ppm) / 8 horas")) %>%
      hc_add_series(name = "Junho", data = l$Junho$mediaMovel) %>%
      hc_add_series(name = "Julho", data = l$Julho$mediaMovel) %>%
      hc_add_series(name = "Agosto", data = l$Agosto$mediaMovel) %>%
      hc_add_series(name = "Setembro", data = l$Setembro$mediaMovel) %>%
      hc_title(text = "Concentração de CO por dias em cada Mês")
    
  })
  
  output$monoxido_por_dia_mes_mogi <- renderHighchart({
    
    by_month <- mogi %>% filter(month %in% c("Junho","Julho","Agosto","Setembro")) %>%
      group_by(month,date) %>% 
      summarise(
        mediaMovel = max(mediaMovel)
      ) %>% mutate(indiceDia = row_number())
    
    l <- split(by_month, f = by_month$month)
    
    highchart() %>% 
      hc_xAxis(categories = unique(by_month$indiceDia)) %>% 
      hc_add_series(name = "Junho", data = l$Junho$mediaMovel) %>%
      hc_add_series(name = "Julho", data = l$Julho$mediaMovel) %>%
      hc_add_series(name = "Agosto", data = l$Agosto$mediaMovel) %>%
      hc_add_series(name = "Setembro", data = l$Setembro$mediaMovel)
    
  })
  
  output$monoxido_mediacum_por_dia_mes_sp <- renderHighchart({
    
    by_month <- sp %>% filter(month %in% c("Junho","Julho","Agosto","Setembro")) %>%
      group_by(month,date) %>% 
      summarise(
        mediaMovel = max(mediaMovel)
      ) %>% mutate(indiceDia = row_number())
    
    l <- split(by_month, f = by_month$month)
    
    highchart() %>% 
      hc_xAxis(categories = unique(by_month$indiceDia)) %>% 
      hc_add_series(name = "Junho", data = cummean(l$Junho$mediaMovel)) %>%
      hc_add_series(name = "Julho", data = cummean(l$Julho$mediaMovel)) %>%
      hc_add_series(name = "Agosto", data = cummean(l$Agosto$mediaMovel)) %>%
      hc_add_series(name = "Setembro", data = cummean(l$Setembro$mediaMovel))
    
  })

  output$monoxido_mediacum_por_dia_mes_mogi <- renderHighchart({
    
    by_month <- mogi %>% filter(month %in% c("Junho","Julho","Agosto","Setembro")) %>%
      group_by(month,date) %>% 
      summarise(
        mediaMovel = max(mediaMovel)
      ) %>% mutate(indiceDia = row_number())
    
    l <- split(by_month, f = by_month$month)
    
    highchart() %>% 
      hc_xAxis(categories = unique(by_month$indiceDia),title = list(text = "Dias")) %>%
      hc_yAxis(title = list(text = "Concentração CO(ppm) / 8 horas")) %>%
      hc_add_series(name = "Junho", data = cummean(l$Junho$mediaMovel)) %>%
      hc_add_series(name = "Julho", data = cummean(l$Julho$mediaMovel)) %>%
      hc_add_series(name = "Agosto", data = cummean(l$Agosto$mediaMovel)) %>%
      hc_add_series(name = "Setembro", data = cummean(l$Setembro$mediaMovel)) %>%
      hc_title(text = "Média Acumulada")
    
  })
  
  
  ########################################
  # Tabela Dinâmica
  ######################################
  output$pivotTableCO <- renderRpivotTable({
    allData <- rbind(sp,mogi)
    rpivotTable(
      allData
    )
  })
   
}

shinyApp(ui,server)
