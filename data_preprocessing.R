
library(dplyr)
library(ggplot2)

data1<-read.csv('data-2.csv')
data2 <- read.delim("~/Desktop/Duke/702/Final Project/Music_Genre_Prediction/songDb.tsv", comment.char="#")

table(data2$Genre)

Metal<-data2 %>%
  filter(Genre=='celticmetal' | Genre=='post-metal')



#NEW DATA has genres that have atleast 100 songs
new_data<-data2[data2$Genre %in% names(table(data2$Genre))[table(data2$Genre)>=100],]


#DIFFERENT TYPES OF METAL SONGS
metalist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('metal', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    metalist<-append(metalist,new_data[i,'Genre'])
    }
}
metalist<-unique(metalist)

Metal<-data2[data2$Genre %in% metalist,]

Metal$index<-c(1:dim(Metal)[1])
Metal$Genre<-factor(Metal$Genre)

ggplot(aes(x=Genre,y=Danceability,color=Genre),data=Metal)+geom_boxplot()
ggplot(aes(x=Genre,y=Energy,color=Genre),data=Metal)+geom_boxplot()
ggplot(aes(x=Genre,y=Loudness,color=Genre),data=Metal)+geom_boxplot()
ggplot(aes(x=Genre,y=Speechness,color=Genre),data=Metal)+geom_boxplot()
ggplot(aes(x=Genre,y=Instrumentalness,color=Genre),data=Metal)+geom_boxplot()
ggplot(aes(x=Genre,y=Tempo,color=Genre),data=Metal)+geom_boxplot()
ggplot(aes(x=Genre,y=Duration_ms,color=Genre),data=Metal)+geom_boxplot()
ggplot(aes(x=Genre,y=Valence,color=Genre),data=Metal)+geom_boxplot()

#Groove - Trash



### CLASSICAL


classicalist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('classical', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    classicalist<-append(classicalist,new_data[i,'Genre'])
  }
}
classicalist<-unique(classicalist)

Classical<-data2[data2$Genre %in% classicalist,]
Classical$index<-c(1:dim(Classical)[1])
Classical$Genre<-factor(Classical$Genre)

ggplot(aes(x=Genre,y=Danceability,color=Genre),data=Classical)+geom_boxplot()
ggplot(aes(x=Genre,y=Energy,color=Genre),data=Classical)+geom_boxplot()
ggplot(aes(x=Genre,y=Loudness,color=Genre),data=Classical)+geom_boxplot()
ggplot(aes(x=Genre,y=Speechness,color=Genre),data=Classical)+geom_boxplot()
ggplot(aes(x=Genre,y=Instrumentalness,color=Genre),data=Classical)+geom_boxplot()
ggplot(aes(x=Genre,y=Tempo,color=Genre),data=Classical)+geom_boxplot()
ggplot(aes(x=Genre,y=Duration_ms,color=Genre),data=Classical)+geom_boxplot()
ggplot(aes(x=Genre,y=Valence,color=Genre),data=Classical)+geom_boxplot()


#CLASSICAL AND METAL
cm<-data2[data2$Genre %in% classicalist | data2$Genre %in% metalist,]
cm$Genre<-factor(cm$Genre)

ggplot(aes(x=Genre,y=Danceability,color=Genre),data=cm)+geom_boxplot()
ggplot(aes(x=Genre,y=Energy,color=Genre),data=cm)+geom_boxplot()
ggplot(aes(x=Genre,y=Loudness,color=Genre),data=cm)+geom_boxplot()
ggplot(aes(x=Genre,y=Speechness,color=Genre),data=cm)+geom_boxplot()
ggplot(aes(x=Genre,y=Instrumentalness,color=Genre),data=cm)+geom_boxplot()
#ggplot(aes(x=Genre,y=Tempo,color=Genre),data=cm)+geom_boxplot()
ggplot(aes(x=Genre,y=Duration_ms,color=Genre),data=cm)+geom_boxplot()


### Pop


poplist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('pop', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    poplist<-append(poplist,new_data[i,'Genre'])
  }
}
poplist<-unique(poplist)

Pop<-data2[data2$Genre %in% poplist,]
#Classical$index<-c(1:dim(Classical)[1])
Pop$Genre<-factor(Pop$Genre)

ggplot(aes(x=Genre,y=Danceability,color=Genre),data=Pop)+geom_boxplot()
ggplot(aes(x=Genre,y=Energy,color=Genre),data=Pop)+geom_boxplot()
ggplot(aes(x=Genre,y=Loudness,color=Genre),data=Pop)+geom_boxplot()
ggplot(aes(x=Genre,y=Speechness,color=Genre),data=Pop)+geom_boxplot()
ggplot(aes(x=Genre,y=Instrumentalness,color=Genre),data=Pop)+geom_boxplot()
ggplot(aes(x=Genre,y=Tempo,color=Genre),data=Pop)+geom_boxplot()
ggplot(aes(x=Genre,y=Duration_ms,color=Genre),data=Pop)+geom_boxplot()
ggplot(aes(x=Genre,y=Valence,color=Genre),data=Pop)+geom_boxplot()




### Folk


folklist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('folk', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    folklist<-append(folklist,new_data[i,'Genre'])
  }
}
folklist<-unique(folklist)

Folk<-data2[data2$Genre %in% folklist,]
#Classical$index<-c(1:dim(Classical)[1])
Folk$Genre<-factor(Folk$Genre)

ggplot(aes(x=Genre,y=Danceability,color=Genre),data=Folk)+geom_boxplot()
ggplot(aes(x=Genre,y=Energy,color=Genre),data=Folk)+geom_boxplot()
ggplot(aes(x=Genre,y=Loudness,color=Genre),data=Folk)+geom_boxplot()
ggplot(aes(x=Genre,y=Speechness,color=Genre),data=Folk)+geom_boxplot()
ggplot(aes(x=Genre,y=Instrumentalness,color=Genre),data=Folk)+geom_boxplot()
ggplot(aes(x=Genre,y=Tempo,color=Genre),data=Folk)+geom_boxplot()
ggplot(aes(x=Genre,y=Duration_ms,color=Genre),data=Folk)+geom_boxplot()


