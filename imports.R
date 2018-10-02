Sys.setenv(HADOOP_CMD="/usr/bin/hadoop")
Sys.setenv(HADOOP_HOME="/usr/lib/hadoop-0.20-mapreduce/")
Sys.setenv(HADOOP_STREAMING="/usr/lib/hadoop-mapreduce/hadoop-streaming-2.6.0-cdh5.10.0.jar")
Sys.setenv(LD_LIBRARY_PATH="/usr/lib/hadoop/lib/native")
Sys.setenv(JAVA_HOME="/home/cloudera/Documents/java8/jdk1.8.0_171")
Sys.setenv(SPARK_HOME = "/home/cloudera/Downloads/spark2.3")
Sys.setenv("HADOOP_CONF_DIR" = "/etc/hadoop/conf")
Sys.setenv(HADOOP_OPTS="-Djava.library.path=HADOOP_HOME/lib")

# Statistics like kurtosis and skewness
library(e1071)

# Data Import
library(jsonlite)

# Data Manipulation
library(dplyr)
library(stringr)

# Hadoop
library(rhdfs)

# Dashboard
library(shiny)
library(shinydashboard)
library(shinycssloaders)

# Data Visualization
library(ggplot2)
library(corrplot)
library(plotly)
library(highcharter)
library(echarts4r)
library(rpivotTable)
library(patchwork)
library(DT)
library(gridExtra)
library(radarchart)
library(rpivotTable)
