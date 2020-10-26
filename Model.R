library(arm)
library(pROC)
library(e1071)
library(caret)
library(nnet)
library(knitr)
library(MASS)
library(ggplot2)

mdata<-read.csv('cleaned_data.csv')
mdata<-subset(mdata, select=-c(Type,ID,Uri,Ref_Track,URL_features))

mdata$Overall_Genre<-as.factor(mdata$Overall_Genre)

mdata$Duration_ms<-mdata$Duration_ms/1000
names(mdata)[names(mdata) == "Duration(seconds)"] <- "Duration_seconds"

# Model Building
model1 <- multinom(Overall_Genre ~ Danceability,data=mdata)
                   
                   
                   +Energy+Loudness+Speechness+
Acousticness+Instrumentalness+Liveness+Valence+Tempo,
data=mdata)
summary(model1)
