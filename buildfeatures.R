library(data.table)
library(Rtsne)
library(ggplot2)
library(caret)
library(Metrics)
library(ClusterR)

set.seed(3000)

#raw data
data <- fread("./project/volume/data/raw/data.csv")

id<-data$id
data$id<-NULL

data<-cbind(id=id, data)

PCA <- prcomp(data[,-1], center = TRUE, scale. = TRUE)

dt <- data.table(unclass(PCA)$x)

