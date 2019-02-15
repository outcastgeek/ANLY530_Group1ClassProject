## @knitr installLibraries

install.packages("knitr")
install.packages("kableExtra")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("cluster")
install.packages("rpart")
install.packages("rpart.plot") 
install.packages("RColorBrewer") 
install.packages("rattle")
install.packages("NbClust")

## @knitr loadLibraries

library(dplyr)
library(ggplot2)
library(cluster)
library(rpart)
library(rpart.plot) 
library(RColorBrewer) 
library(rattle)
library(NbClust)

## @knitr helperFunctions

# Obtains the full File Path
fullFilePath <- function(fileName)
{
  fileFolder <- "./data/"
  fileNamePath <- paste(fileFolder, fileName, sep = "")
  fileNamePath
}

# Converts column of Timestamps to Date
ttColToDate <- function(dFrame, colName) {
  dFrame[colName] <- as.POSIXct(dFrame[colName], origin="1970-01-01")
  dFrame
}

# Converts column to utf-8
toUtf8 <- function(column) {
  columnUtf8 <- iconv(enc2utf8(column), sub = "byte")
  columnUtf8
}

# Formats Data
fmt <- function(dt, caption = "") {
  fmt_dt <- dt %>%
    kable("latex", longtable = T, booktabs = T)
  fmt_dt
}

# Style Data
style <- function(dt, full_width = F, angle = 0) {
  style_dt <- dt %>%
    kable_styling(latex_options = "striped", full_width = full_width) %>%
    row_spec(0, angle = angle)
  style_dt
}

#Plot the within (cluster) sum of squares to determine the initial value for "k" 
wssplot <- function(data, nc=15, seed=1234){ 
  wss <- (nrow(data)-1)*sum(apply(data,2,var)) 
  for (i in 2:nc){ 
    set.seed(seed) 
    wss[i] <- sum(kmeans(data, centers=i)$withinss)} 
  plot(1:nc, wss, type="b", xlab="Number of Clusters", 
       ylab="Within groups sum of squares")}

## @knitr loadSheets

#Set Data File Name:
Absenteeism_at_work_file <- "Absenteeism_at_work.csv"

# Absenteeism
Absenteeism_data <- Absenteeism_at_work_file %>%
  fullFilePath %>%
  read.csv(encoding = "UTF-8", header=TRUE, stringsAsFactors=FALSE)

## @knitr crunchingNumbers

# Crunch some numbers....
