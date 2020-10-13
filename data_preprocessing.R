
library(dplyr)
library(ggplot2)

data1<-read.csv('data-2.csv')
data2 <- read.delim("~/Desktop/Duke/702/Final Project/Music_Genre_Prediction/songDb.tsv", comment.char="#")

table(data2$Genre)

Metal<-data2 %>%
  filter(Genre=='celticmetal' | Genre=='post-metal')

ggplot(aes(x=ID,y=Danceability,color=Genre),data=Metal)+geom_point()
ggplot(aes(x=ID,y=Energy,color=Genre),data=Metal)+geom_point()
ggplot(aes(x=ID,y=Loudness,color=Genre),data=Metal)+geom_point()
ggplot(aes(x=ID,y=Speechness,color=Genre),data=Metal)+geom_point()
ggplot(aes(x=ID,y=Instrumentalness,color=Genre),data=Metal)+geom_point()
ggplot(aes(x=ID,y=Tempo,color=Genre),data=Metal)+geom_point()
ggplot(aes(x=ID,y=Duration_ms,color=Genre),data=Metal)+geom_point()


b<-table(data2$Genre)[4]


new_data<-data2 %>%
  
  filter(table(data2$Genre))>100
