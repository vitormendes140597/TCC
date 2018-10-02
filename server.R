# Inicializating Hadoop HDFS
hdfs.init()
# Inicializating Spark
# sqlContext <- SparkR::sparkR.session(master = "local[*]",
#                                      sparkEnvir = list(spark.driver.memory="6g"),
#                                      enableHiveSupport = FALSE)
# 
# ###############################
# # 1) Data Import
# ###############################
# sensores <- fromJSON("http://201.6.243.44:3000/dump")
# iotMogi  <- sensores[[3]]
# iotSp    <- sensores[[5]]
# 
# iotMogi$city <- "Mogi das Cruzes"
# iotSp$city   <- "São Paulo"
# ###############################
# # 2) Creating Spark DataFrame
# ###############################
# df <- SparkR::rbind(
#   SparkR::as.DataFrame(iotMogi),
#   SparkR::as.DataFrame(iotSp)
# )
# 
# # Caching Spark DataFrame on Memory
# SparkR::cache(df)
# 
# ###############################
# # 3) Data Cleaning
# ###############################
# df$dateCast <- SparkR::from_unixtime(df$date/1000)
# df$data <- SparkR::cast(df$data,"double")
# df2 <- SparkR::selectExpr(df, "dateCast as datetimeOriginal","split(dateCast,' ')[0] as date", "split(dateCast,' ')[1] as time", "data", "city")
# df3 <- SparkR::as.DataFrame(SparkR::dapplyCollect(df2,adc2ppm))
# df4 <- SparkR::as.DataFrame(SparkR::dapplyCollect(df3,function(x) { 
#   xcopy <- x
#   xcopy2 <- apply(xcopy[,],1, function(x){
#     #as.data.frame(time_round(x))
#     x = unname(x)
#     y = list(
#       "originalDate" = x[1],
#       "date" = as.Date(x[2]),
#       "time" = x[3],
#       "data" = x[4],
#       "city" = x[5],
#       "ppm"  = x[6]
#     )
#     result = as.data.frame(time_round(y))
#     return(result)
#   })
#   xcopy3 <- plyr::rbind.fill(xcopy2)
#   return(xcopy3)
# }))
# df4$ppm <- SparkR::cast(df4$ppm,"double")
# df4$weekday <- SparkR::dayofweek(df4$date)
# df4$month   <- SparkR::month(df4$date)
# 
# 
# # Transform Spark Dataframe into SQL Table
# SparkR::createOrReplaceTempView(df4,"df")
# 
# df5 <- SparkR::sql("Select 
#                    date,
#                    time,
#                    cast(data as double),
#                    ppm,
#                    city,
#                    case 
#                    when weekday = 1 then 'Domingo'
#                    when weekday = 2 then 'Segunda'
#                    when weekday = 3 then 'Terça'
#                    when weekday = 4 then 'Quarta'
#                    when weekday = 5 then 'Quinta'
#                    when weekday = 6 then 'Sexta'
#                    when weekday = 7 then 'Sábado'
#                    end as weekday,
#                    case 
#                    when month = 1 then 'Janeiro'
#                    when month = 2 then 'Fevereiro'
#                    when month = 3 then 'Março'
#                    when month = 4 then 'Abril'
#                    when month = 5 then 'Maio'
#                    when month = 6 then 'Junho'
#                    when month = 7 then 'Julho'
#                    when month = 8 then 'Agosto'
#                    when month = 9 then 'Setembro'
#                    when month = 10 then 'Outubro'
#                    when month = 11 then 'Novembro'
#                    when month = 12 then 'Dezembro'
#                    end as month
#                    From df
#                    ")



dfCopy <- mtcars
rv <- reactiveValues(df = mtcars)
server <- function(input, output) {
  
  ###############################
  # 1) SHINY
  ###############################

  output$table <- DT::renderDataTable({
    #x <- SparkR::collect(df5)
    datatable(rv$df,class = "cell-border")
  })
  
  ##########################
  # Clean Tab
  #########################
  output$clean.choose_NumericVars <- renderUI({
    columns <- colnames(select_if(rv$df,is.numeric))
    
    selectInput(
      "clean.numericVariables",
      label = "Coluna",
      choices = columns,
      selected = columns[1]
    )
  })
  
  # Apply Capping
  observeEvent(input$clean.applyCapping, {
    column <- input$clean.numericVariables # Column which will be aplied the replacement
    condition <- with(rv$df, eval(parse(text = input$replaceCondition)))
    
    if(class(condition) != "logical") { # If user does not pass condition string, apply replacement in all column
      condition <- rep(TRUE,nrow(rv$df))
    }
    
    if(input$cappingMethod == 1 & any(condition)) { # & has at least one row that conditions match
      rv$df[condition,column] <- mean(rv$df[[column]], na.rm = TRUE)
    } else if(input$cappingMethod == 2 & any(condition)) {
      rv$df[condition,column] <- median(rv$df[[column]], na.rm = TRUE)
    }
    else if(input$cappingMethod == 3 & any(condition)) {
      probs <- as.numeric(input$cleanPercentile)
      rv$df[condition,column] <- quantile(rv$df[,column],probs = probs)
    }
    else {
      # Do Nothing
    }
  })
  
  # Filter Table
  observeEvent(input$clean.filterds, {
    condition <- with(rv$df, eval(parse(text = input$filterCondition)))
  
    if(any(condition)) {
      rv$df <- rv$df[condition,]
    }
  })
  
  observeEvent(input$restoreds,{
    rv$df <- dfCopy
  })
  
}

shinyApp(ui, server)
