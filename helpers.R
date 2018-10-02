# Defining Useful Functions
adc2ppm <- function(x) {
  x$ppm <- (x[['data']] / 1024) * 5
  x$ppm <- (5-x[['ppm']]) / x[['ppm']]
  x$ppm <- x[['ppm']] / 10
  x$ppm <- round(37143 * (x[['ppm']] ^ -3.178)/1000, digits = 4)
  return(x)
}
# Round time - Example 12:45 to 13:00
time_round = function(x) {
  
  date = x[["date"]]
  time = x[["time"]]
  
  hour = as.numeric(stringr::str_split(time,":")[[1]][1])
  minute = as.numeric(stringr::str_split(time,":")[[1]][2])
  
  if(minute >= 30) {
    minute = 0
    if(hour + 1 == 24) {
      hour = 0
      date = date + 1
    }
    else {
      hour = hour + 1
    }
  }
  else {
    minute = 0
  }
  
  # Building the new string time
  strHour   = as.character(hour)
  strMinute = as.character(minute)
  
  if(stringr::str_length(strHour) == 1) {
    strHour = paste0("0",strHour)
  }
  if(stringr::str_length(strMinute) == 1) {
    strMinute = paste0("0",strMinute)
  }
  
  strTime = paste0(strHour,":",strMinute)
  
  return(
    list(
      "date" = date,
      "time" = strTime,
      "data" = x[["data"]],
      "ppm"  = x[["ppm"]],
      "city" = x["city"]
    )
  )
}

printSparkDF <- function(df,rows = NULL) {
  if(class(df) != "SparkDataFrame") {
    stop("Not a Spark DataFrame")
  }
  rows <- ifelse(is.null(rows),5,rows)
  SparkR::head(
    SparkR::showDF(df)
    ,rows
  )
}

printSparkDF <- function(df,rows = NULL) {
  if(class(df) != "SparkDataFrame") {
    stop("Not a Spark DataFrame")
  }
  rows <- ifelse(is.null(rows),5,rows)
  SparkR::head(
    SparkR::showDF(df)
    ,rows
  )
}


mediaMovel <- function(mediaPPM) {
  mediaMovel <- (lag(mediaPPM,1)+lag(mediaPPM,2)+lag(mediaPPM,3)+lag(mediaPPM,4)+lag(mediaPPM,5)+lag(mediaPPM,6)+lag(mediaPPM,7)+lag(mediaPPM,8))/8
  return(mediaMovel)
}

indiceQualidade <- function(mediaMovel) {
  if(mediaMovel <=9) {
    return(data.frame("indiceNumber" = 0,"indiceLabel" = "Boa",stringsAsFactors = FALSE))
  } else if(mediaMovel > 9 & mediaMovel <= 11) {
    return(data.frame("indiceNumber" = 1,"indiceLabel" = "Moderado",stringsAsFactors = FALSE))
  } else if(mediaMovel > 11 & mediaMovel <= 13) {
    return(data.frame("indiceNumber" = 2,"indiceLabel" = "Ruim",stringsAsFactors = FALSE))
  } else if(mediaMovel >13 & mediaMovel <= 15) {
    return(data.frame("indiceNumber" = 3,"indiceLabel" = "Muito Ruim",stringsAsFactors = FALSE))
  } else {
    return(data.frame("indiceNumber" = 4,"indiceLabel" = "Péssimo",stringsAsFactors = FALSE))
  }
}
