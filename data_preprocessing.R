
library(dplyr)
library(ggplot2)

data1<-read.csv('data-2.csv')
data2 <- read.delim("~/Desktop/Duke/702/Final Project/Music_Genre_Prediction/songDb.tsv", comment.char="#")

data2$Tempo<-as.numeric(data2$Tempo)
properties<-data.frame("Variable"=c("Name","Danceability"  ,   "Energy"    ,       "Key"       ,       "Loudness",
                             "Mode"     ,        "Speechness"      , "Acousticness"  ,   "Instrumentalness" ,
                             "Liveness"    ,     "Valence"      ,    "Tempo"       ,  "Duration_ms"   ,   "time_signature" ,  "Genre"),
                "Description"=c("Song Name",
                                "Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. ",
                                "Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity.",
                                "The estimated overall key of the track.",
                                "The overall loudness of a track in decibels (dB).",
                                "Mode indicates the modality (major or minor) of a track.",
                                "Speechiness detects the presence of spoken words in a track.",
                                "A confidence measure from 0.0 to 1.0 of whether the track is acoustic.",
                                "Predicts whether a track contains no vocals.",
                                "Detects the presence of an audience in the recording.",
                                "A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track.",
                                "The overall estimated tempo of a track in beats per minute (BPM).",
                                "The duration of the track in milliseconds.",
                                "An estimated overall time signature of a track.",
                                "Genre of the Song"))


####### Implementing Merging Year on ID

merged<-merge(x=data2,y=data1,by.x="ID",by.y="id",all.x=TRUE)



library(tidyr)
merged<-merged %>% drop_na()

#####







table(data2$Genre)


Metal<-data2 %>%
  filter(Genre=='celticmetal' | Genre=='post-metal')



#NEW DATA has genres that have atleast 100 songs
#new_data<-data2[data2$Genre %in% names(table(data2$Genre))[table(data2$Genre)>=100],]
new_data<-data2

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
#ggplot(aes(x=Genre,y=Tempo,color=Genre),data=Classical)+geom_boxplot()
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
#ggplot(aes(x=Genre,y=Tempo,color=Genre),data=Pop)+geom_boxplot()
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
#ggplot(aes(x=Genre,y=Tempo,color=Genre),data=Folk)+geom_boxplot()
ggplot(aes(x=Genre,y=Duration_ms,color=Genre),data=Folk)+geom_boxplot()


### Rock

rocklist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('rock', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    rocklist<-append(rocklist,new_data[i,'Genre'])
  }
}
rocklist<-unique(rocklist)

Rock<-data2[data2$Genre %in% rocklist,]
#Classical$index<-c(1:dim(Classical)[1])
Rock$Genre<-factor(Rock$Genre)

ggplot(aes(x=Genre,y=Danceability,color=Genre),data=Rock)+geom_boxplot()
ggplot(aes(x=Genre,y=Energy,color=Genre),data=Rock)+geom_boxplot()
ggplot(aes(x=Genre,y=Loudness,color=Genre),data=Rock)+geom_boxplot()
ggplot(aes(x=Genre,y=Speechness,color=Genre),data=Rock)+geom_boxplot()
ggplot(aes(x=Genre,y=Instrumentalness,color=Genre),data=Folk)+geom_boxplot()
ggplot(aes(x=Genre,y=Tempo,color=Genre),data=Rock)+geom_boxplot()
ggplot(aes(x=Genre,y=Duration_ms,color=Genre),data=Rock)+geom_boxplot()



### Indie

indielist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('indie', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    indielist<-append(indielist,new_data[i,'Genre'])
  }
}
indielist<-unique(indielist)

Indie<-data2[data2$Genre %in% indielist,]
#Classical$index<-c(1:dim(Classical)[1])
Indie$Genre<-factor(Indie$Genre)

ggplot(aes(x=Genre,y=Danceability,color=Genre),data=Indie)+geom_boxplot()
ggplot(aes(x=Genre,y=Energy,color=Genre),data=Indie)+geom_boxplot()
ggplot(aes(x=Genre,y=Loudness,color=Genre),data=Indie)+geom_boxplot()
ggplot(aes(x=Genre,y=Speechness,color=Genre),data=Indie)+geom_boxplot()
ggplot(aes(x=Genre,y=Instrumentalness,color=Genre),data=Indie)+geom_boxplot()
ggplot(aes(x=Genre,y=Tempo,color=Genre),data=Indie)+geom_boxplot()
ggplot(aes(x=Genre,y=Duration_ms,color=Genre),data=Indie)+geom_boxplot()



### Hiphop


hiphoplist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('hiphop', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    hiphoplist<-append(hiphoplist,new_data[i,'Genre'])
  }
}
hiphoplist<-unique(hiphoplist)
HipHop<-data2[data2$Genre %in% hiphoplist,]
#Classical$index<-c(1:dim(Classical)[1])
HipHop$Genre<-factor(HipHop$Genre)

ggplot(aes(x=Genre,y=Danceability,color=Genre),data=HipHop)+geom_boxplot()
ggplot(aes(x=Genre,y=Energy,color=Genre),data=HipHop)+geom_boxplot()
ggplot(aes(x=Genre,y=Loudness,color=Genre),data=HipHop)+geom_boxplot()
ggplot(aes(x=Genre,y=Speechness,color=Genre),data=HipHop)+geom_boxplot()
ggplot(aes(x=Genre,y=Instrumentalness,color=Genre),data=HipHop)+geom_boxplot()
ggplot(aes(x=Genre,y=Tempo,color=Genre),data=HipHop)+geom_boxplot()
ggplot(aes(x=Genre,y=Duration_ms,color=Genre),data=HipHop)+geom_boxplot()



### R&B


rblist=c()


for (i in (1:dim(new_data)[1])){
  flag<-regexpr('r&b', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    rblist<-append(rblist,new_data[i,'Genre'])
  }
}

rblist<-unique(rblist)
RB<-data2[data2$Genre %in% rblist,]
#Classical$index<-c(1:dim(Classical)[1])
RB$Genre<-factor(RB$Genre)

ggplot(aes(x=Genre,y=Danceability,color=Genre),data=RB)+geom_boxplot()
ggplot(aes(x=Genre,y=Energy,color=Genre),data=RB)+geom_boxplot()
ggplot(aes(x=Genre,y=Loudness,color=Genre),data=RB)+geom_boxplot()
ggplot(aes(x=Genre,y=Speechness,color=Genre),data=RB)+geom_boxplot()
ggplot(aes(x=Genre,y=Instrumentalness,color=Genre),data=RB)+geom_boxplot()
ggplot(aes(x=Genre,y=Tempo,color=Genre),data=RB)+geom_boxplot()
ggplot(aes(x=Genre,y=Duration_ms,color=Genre),data=RB)+geom_boxplot()




### Jazz
jazzlist=c()
for (i in (1:dim(new_data)[1])){
  flag<-regexpr('jazz', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    jazzlist<-append(jazzlist,new_data[i,'Genre'])
  }
}
jazzlist<-unique(jazzlist)


Jazz<-data2[data2$Genre %in% jazzlist,]
#Classical$index<-c(1:dim(Classical)[1])
Jazz$Genre<-factor(Jazz$Genre)

ggplot(aes(x=Genre,y=Danceability,color=Genre),data=Jazz)+geom_boxplot()
ggplot(aes(x=Genre,y=Energy,color=Genre),data=Jazz)+geom_boxplot()
ggplot(aes(x=Genre,y=Loudness,color=Genre),data=Jazz)+geom_boxplot()
ggplot(aes(x=Genre,y=Speechness,color=Genre),data=Jazz)+geom_boxplot()
ggplot(aes(x=Genre,y=Instrumentalness,color=Genre),data=Jazz)+geom_boxplot()
ggplot(aes(x=Genre,y=Tempo,color=Genre),data=Jazz)+geom_boxplot()
ggplot(aes(x=Genre,y=Duration_ms,color=Genre),data=Jazz)+geom_boxplot()

### Blues
blueslist=c()
for (i in (1:dim(new_data)[1])){
  flag<-regexpr('blues', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    blueslist<-append(blueslist,new_data[i,'Genre'])
  }
}
blueslist<-unique(blueslist)


Blues<-data2[data2$Genre %in% blueslist,]
#Classical$index<-c(1:dim(Classical)[1])
Blues$Genre<-factor(Blues$Genre)



### House

houselist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('house', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    houselist<-append(houselist,new_data[i,'Genre'])
  }
}
houselist<-unique(houselist)


House<-data2[data2$Genre %in% houselist,]
#Classical$index<-c(1:dim(Classical)[1])
House$Genre<-factor(House$Genre)

ggplot(aes(x=Genre,y=Danceability,color=Genre),data=House)+geom_boxplot()
ggplot(aes(x=Genre,y=Energy,color=Genre),data=House)+geom_boxplot()
ggplot(aes(x=Genre,y=Loudness,color=Genre),data=House)+geom_boxplot()
ggplot(aes(x=Genre,y=Speechness,color=Genre),data=House)+geom_boxplot()
ggplot(aes(x=Genre,y=Instrumentalness,color=Genre),data=House)+geom_boxplot()
ggplot(aes(x=Genre,y=Tempo,color=Genre),data=House)+geom_boxplot()
ggplot(aes(x=Genre,y=Duration_ms,color=Genre),data=House)+geom_boxplot()



######Dubstep 

dubsteplist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('dubstep', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    dubsteplist<-append(dubsteplist,new_data[i,'Genre'])
  }
}
dubsteplist<-unique(dubsteplist)


Dubstep<-data2[data2$Genre %in% dubsteplist,]
#Classical$index<-c(1:dim(Classical)[1])
Dubstep$Genre<-factor(Dubstep$Genre)


###### Punk

punklist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('punk', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    punklist<-append(punklist,new_data[i,'Genre'])
  }
}
punklist<-unique(punklist)


Punk<-data2[data2$Genre %in% punklist,]
#Classical$index<-c(1:dim(Classical)[1])
Punk$Genre<-factor(Punk$Genre)



#### soundtrack


soundtracklist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('soundtrack', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    soundtracklist<-append(soundtracklist,new_data[i,'Genre'])
  }
}
soundtracklist<-unique(soundtracklist)


Soundtrack<-data2[data2$Genre %in% soundtracklist,]
#Classical$index<-c(1:dim(Classical)[1])
Soundtrack$Genre<-factor(Soundtrack$Genre)

ggplot(aes(x=Genre,y=Danceability,color=Genre),data=Soundtrack)+geom_boxplot()
ggplot(aes(x=Genre,y=Energy,color=Genre),data=Soundtrack)+geom_boxplot()
ggplot(aes(x=Genre,y=Loudness,color=Genre),data=Soundtrack)+geom_boxplot()

ggplot(aes(x=Genre,y=Speechness,color=Genre),data=Soundtrack)+geom_boxplot()
ggplot(aes(x=Genre,y=Instrumentalness,color=Genre),data=Soundtrack)+geom_boxplot()
ggplot(aes(x=Genre,y=Tempo,color=Genre),data=Soundtrack)+geom_boxplot()
ggplot(aes(x=Genre,y=Duration_ms,color=Genre),data=Soundtrack)+geom_boxplot()





########### Common Genres

Genres_list=c('blueslist','classicalist','folklist','hiphoplist','houselist',
            'indielist','jazzlist','metalist','poplist','rblist','rocklist')


Genres_list=c(blueslist,classicalist,folklist,hiphoplist,houselist,
              indielist,jazzlist,metalist,poplist,rblist,rocklist,
              punklist,dubsteplist)

print(intersect(classicalist,folklist))

gl
for (i in Genres_list){
  gl<-append(gl,i)
}

 

test<-data2[!data2$Genre %in% gl,]





##Combined
for (i in (1:dim(data2)[1])){
  folk<-regexpr('folk', data2[i,'Genre'], ignore.case ="True")
  if (folk!=-1){
    data2[i,'Overall_Genre']<-"Folk"
  }
  classical<-regexpr('classical', data2[i,'Genre'], ignore.case ="True")
  if (classical!=-1){
    data2[i,'Overall_Genre']<-"Classical"
  }
  metal<-regexpr('metal', data2[i,'Genre'], ignore.case ="True")
  if (metal!=-1){
    data2[i,'Overall_Genre']<-"Metal"
  }
  pop<-regexpr('pop', data2[i,'Genre'], ignore.case ="True")
  if (pop!=-1){
    data2[i,'Overall_Genre']<-"Pop"
  }
  house<-regexpr('house', data2[i,'Genre'], ignore.case ="True")
  if (house!=-1){
    data2[i,'Overall_Genre']<-"House"
  }
}
all<-data2[data2$Overall_Genre=="Classical" | data2$Overall_Genre == "Metal" |
             data2$Overall_Genre == "Pop"| data2$Overall_Genre=="Folk" | data2$Overall_Genre=='House',]
library(dplyr)
library(tidyr)
all<-all %>%
  drop_na()
ggplot(aes(x=Overall_Genre,y=Danceability,color=Overall_Genre),data=all)+geom_boxplot()
ggplot(aes(x=Overall_Genre,y=Energy,color=Overall_Genre),data=all)+geom_boxplot()
ggplot(aes(x=Overall_Genre,y=Loudness,color=Overall_Genre),data=all)+geom_boxplot()

ggplot(aes(x=Overall_Genre,y=Speechness,color=Overall_Genre),data=all)+geom_boxplot()
ggplot(aes(x=Overall_Genre,y=Instrumentalness,color=Overall_Genre),data=all)+geom_boxplot()
ggplot(aes(x=Overall_Genre,y=Tempo,color=Overall_Genre),data=all)+geom_boxplot()
ggplot(aes(x=Overall_Genre,y=Duration_ms,color=Overall_Genre),data=all)+geom_boxplot()

