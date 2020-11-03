music<-read.csv('cleaned_data.csv')
music<-subset(music, select=-c(Type,ID,Uri,Ref_Track,URL_features))


music$Duration_ms<-music$Duration_ms/1000
names(music)[names(music) == "Duration_ms"] <- "Duration(seconds)"


ggplot(data=music,aes(x=Danceability,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Energy,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Key,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Loudness,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Mode,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=time_signature,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Acousticness,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Instrumentalness,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Liveness,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Valence,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Tempo,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Duration(seconds),y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()


0.5,.0.6,0.3,0.7,0.8