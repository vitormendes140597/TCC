# Setting environment variables
Sys.setenv(HADOOP_CMD="/usr/bin/hadoop")
Sys.setenv(HADOOP_HOME="/usr/lib/hadoop-0.20-mapreduce/")
Sys.setenv(HADOOP_STREAMING="/usr/lib/hadoop-mapreduce/hadoop-streaming-2.6.0-cdh5.10.0.jar")
Sys.setenv(LD_LIBRARY_PATH="/usr/lib/hadoop/lib/native")
Sys.setenv(JAVA_HOME="/home/cloudera/Documents/java8/jdk1.8.0_171")
Sys.setenv(SPARK_HOME = "/home/cloudera/Downloads/spark2.3")
Sys.setenv("HADOOP_CONF_DIR" = "/etc/hadoop/conf")
Sys.setenv(HADOOP_OPTS="-Djava.library.path=HADOOP_HOME/lib")


# Loading Libraries  install:: devtools::install_github("MangoTheCat/radarchart")
suppressMessages({
  library(rhdfs)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(jsonlite)
  library(ggplot2)
  library(corrplot)
  library(plotly)
  library(gridExtra)
  library(radarchart)
})


# Inicializating Hadoop HDFS
hdfs.init()
# Creating Spark Session
config <- list(
  spark.driver.memory="5g",
  spark.driver.cores = 4
)
sqlContext <- SparkR::sparkR.session(master = "local[*]", sparkConfig = config,enableHiveSupport = FALSE)

# Retrieving Data
sensores <- fromJSON("http://201.6.243.44:3000/dump")
iotMogi  <- sensores[[3]]
iotSp    <- sensores[[5]]

iotMogi$city <- "Mogi das Cruzes"
iotSp$city   <- "São Paulo"

# Moving to HDFS
hdfs.mogi = hdfs.file(paste0("/tcc/sensorMogi",Sys.Date(),".txt"),"w",blocksize = 1048576*20,replication = 3,buffersize = 1048576*20,overwrite = TRUE)
hdfs.write(charToRaw(jsonlite::toJSON(iotMogi)), hdfs.mogi)
hdfs.close(hdfs.mogi)

# Moving to HDFS
hdfs.sp = hdfs.file(paste0("/tcc/sensorSP",Sys.Date(),".txt"),"w",blocksize = 1048576*20,replication = 3,buffersize = 1048576*20,overwrite = TRUE)
hdfs.write(charToRaw(jsonlite::toJSON(iotSp)), hdfs.sp)
hdfs.close(hdfs.sp)

# Creating Spark Distributed Data Frame based on Hadoop files storage

# Creating Spark Distributed Data Frame
df <- SparkR::rbind(
  SparkR::read.json(paste0("hdfs://localhost:8020/tcc/sensorSP",Sys.Date(),".txt")),
  SparkR::read.json(paste0("hdfs://localhost:8020/tcc/sensorMogi",Sys.Date(),".txt"))
)

# Caching Spark DataFrame on Memory
SparkR::cache(df)

# Data Cleaning 
df$dateCast <- SparkR::from_unixtime(df$date/1000)
df$data <- SparkR::cast(df$data,"double")
df2 <- SparkR::selectExpr(df, "dateCast as datetimeOriginal","split(dateCast,' ')[0] as date", "split(dateCast,' ')[1] as time", "data", "city")
df3 <- SparkR::as.DataFrame(SparkR::dapplyCollect(df2,adc2ppm))
df4 <- SparkR::as.DataFrame(SparkR::dapplyCollect(df3,function(x) { 
  xcopy <- x
  xcopy2 <- apply(xcopy[,],1, function(x){
    #as.data.frame(time_round(x))
    x = unname(x)
    y = list(
      "originalDate" = x[1],
      "date" = as.Date(x[2]),
      "time" = x[3],
      "data" = x[4],
      "city" = x[5],
      "ppm"  = x[6]
    )
    result = as.data.frame(time_round(y))
    return(result)
  })
  xcopy3 <- plyr::rbind.fill(xcopy2)
  return(xcopy3)
}))
df4$ppm <- SparkR::cast(df4$ppm,"double")
df4$weekday <- SparkR::dayofweek(df4$date)
df4$month   <- SparkR::month(df4$date)


# Transform Spark Dataframe into SQL Table
SparkR::createOrReplaceTempView(df4,"df")

df5 <- SparkR::sql("Select 
                   date,
                   time,
                   cast(data as double),
                   ppm,
                   city,
                   case 
                   when weekday = 1 then 'Domingo'
                   when weekday = 2 then 'Segunda'
                   when weekday = 3 then 'Terça'
                   when weekday = 4 then 'Quarta'
                   when weekday = 5 then 'Quinta'
                   when weekday = 6 then 'Sexta'
                   when weekday = 7 then 'Sábado'
                   end as weekday,
                   case 
                   when month = 1 then 'Janeiro'
                   when month = 2 then 'Fevereiro'
                   when month = 3 then 'Março'
                   when month = 4 then 'Abril'
                   when month = 5 then 'Maio'
                   when month = 6 then 'Junho'
                   when month = 7 then 'Julho'
                   when month = 8 then 'Agosto'
                   when month = 9 then 'Setembro'
                   when month = 10 then 'Outubro'
                   when month = 11 then 'Novembro'
                   when month = 12 then 'Dezembro'
                   end as month
                   From df
                   ")

# Excluindo outliers (Tudo maior que 99%)
# Os arrays do Spark são equivalentes as listas do R.
df5 <- SparkR::filter(df5, df5$ppm <= unlist(SparkR::approxQuantile(df5,"ppm",0.99,0.0)) ) 
SparkR::createOrReplaceTempView(df5,"df")

df6 <- SparkR::sql("Select
                   date,
                   time,
                   city,
                   weekday,
                   month,
                   mean(data) as   mediaHorariaRaw,
                   mean(ppm)  as   mediaHorariaPpm,
                   max(data)  as   maxHorariaRaw,
                   max(ppm)   as   maxHorariaPpm,
                   min(data)  as   minHorariaRaw,
                   min(ppm)   as   minHorariaPpm
                   From df
                   Group By date,time,city,weekday,month")


dfCollected <- SparkR::collect(df6)
dfCollected <- select(dfCollected,-mediaHorariaRaw,-maxHorariaRaw,-minHorariaRaw)

# Filtrando por mogi e por SP
sp <- dfCollected %>% filter(city == "São Paulo")
mogi <- dfCollected %>% filter(city != "São Paulo")

# Alguns meses são tem volume o suficiente para a análise
# 24 registros diários * 31 dias = 744 registros mensais
sp %>% group_by(month) %>% summarise(total = n(), percentage = total / 744) %>% arrange(desc(total))
mogi %>% group_by(month) %>% summarise(total = n(), percentage = total / 744) %>% arrange(desc(total))


# Definindo constantes
meses <- c("Janeiro","Fevereiro","Março","Abril","Maio","Junho","Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")
mesesComVolume <- c("Junho","Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")
diasSemanas <- c("Domingo","Segunda","Terça","Quarta","Quinta","Sexta","Sábado")



####################################################################
# Filtrando meses com número de observações equivalentes a 30 dias
####################################################################
sp <- filter(sp, month %in% mesesComVolume) %>% arrange(date,time) %>% 
  mutate(month = factor(month,meses),weekday = factor(weekday,diasSemanas))

mogi <- filter(mogi, month %in% mesesComVolume) %>% arrange(date,time) %>% 
  mutate(month = factor(month,meses),weekday = factor(weekday,diasSemanas))


########################################
# Calculando a Média Móvel
########################################
sp$mediaMovel <- mediaMovel(sp$mediaHorariaPpm)
mogi$mediaMovel <- mediaMovel(mogi$mediaHorariaPpm)

sp[is.na(sp$mediaMovel),"mediaMovel"] <- mean(sp[is.na(sp$mediaMovel),"mediaHorariaPpm"])
mogi[is.na(mogi$mediaMovel),"mediaMovel"] <- mean(mogi[is.na(mogi$mediaMovel),"mediaHorariaPpm"])

########################################
# Calculando Indice de Qualidade do Ar
########################################
spIndice <- bind_rows(lapply(sp$mediaMovel,indiceQualidade))
sp <- cbind(sp,spIndice)

mogiIndice <- bind_rows(lapply(mogi$mediaMovel,indiceQualidade))
mogi <- cbind(mogi,mogiIndice)

