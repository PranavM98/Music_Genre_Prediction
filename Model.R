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

#mdata$Overall_Genre<-as.factor(mdata$Overall_Genre)

mdata$Duration_ms<-mdata$Duration_ms/1000
names(mdata)[names(mdata) == "Duration(seconds)"] <- "Duration_seconds"


lista<-names(which(table(mdata$Overall_Genre)>2000))

subset_data<-mdata[mdata$Overall_Genre %in% lista,]

lista[1]
#library(dplyr)
#subset_data<- mdata %>%
#  filter(Overall_Genre=="Blues" | Overall_Genre=="Classical"|
#           Overall_Genre=="Folk" | Overall_Genre=="Hiphop"|
#           Overall_Genre=="House" | Overall_Genre=="Indie"|
#           Overall_Genre=="Jazz" | Overall_Genre=="Metal"|
#           Overall_Genre=="Pop" | Overall_Genre=="R&B" |
#            Overall_Genre=="Rock"| Overall_Genre=="Punk" |
#            Overall_Genre=="Soundtrack"| Overall_Genre=="EDM"
          
#           )

library(tidyr)  
subset_data<-subset_data %>% drop_na()

subset_data$Overall_Genre<-as.factor(subset_data$Overall_Genre)


# Model Building

model1 <- multinom(Overall_Genre ~ Danceability +Energy+Loudness+Speechness+ Key+ Mode+ time_signature+
Acousticness+Instrumentalness+Liveness+Valence+Tempo,data=subset_data)
summary(model1)


predprobs <- fitted(model1) 

predprobs[1,]
clist=c()
for (i in 1:dim(predprobs)[1]){
  
  clist<-append(clist,match(max(predprobs[i,]),predprobs[i,]))
  
  }
  
subset_data$genre_class[subset_data$Overall_Genre=='Ambient']<-1
subset_data$genre_class[subset_data$Overall_Genre=='Country']<-2
subset_data$genre_class[subset_data$Overall_Genre=='Electronic']<-3
subset_data$genre_class[subset_data$Overall_Genre=='Folk']<-4
subset_data$genre_class[subset_data$Overall_Genre=='Funk']<-5
subset_data$genre_class[subset_data$Overall_Genre=='Hiphop']<-6
subset_data$genre_class[subset_data$Overall_Genre=='House']<-7
subset_data$genre_class[subset_data$Overall_Genre=='Indie']<-8
subset_data$genre_class[subset_data$Overall_Genre=='Jazz']<-9
subset_data$genre_class[subset_data$Overall_Genre=='Metal']<-10
subset_data$genre_class[subset_data$Overall_Genre=='Pop']<-11
subset_data$genre_class[subset_data$Overall_Genre=='Punk']<-12
subset_data$genre_class[subset_data$Overall_Genre=='Rap']<-13
subset_data$genre_class[subset_data$Overall_Genre=='Rock']<-14
subset_data$genre_class[subset_data$Overall_Genre=='Techno']<-15

confusionMatrix(factor(clist), factor(subset_data$genre_class))


 ###### Diagnostics
####diagnostics comparing average raw residuals across bins based on predictor values
#for viewcat = 1:  create a raw residual using only the first column of the predicted probabilities
rawresid1 <- (subset_data$Overall_Genre == 'Ambient') -  predprobs[,1]
#for viewcat = 2:  create a raw residual using only the second column of the predicted probabilities
rawresid2 <- (subset_data$Overall_Genre == 'Country') -  predprobs[,2]
#for viewcat = 3:  create a raw residual using only the third column of the predicted probabilities
rawresid3 <- (subset_data$Overall_Genre == 'Electronic') -  predprobs[,3]
#for viewcat = 4:  create a raw residual using only the fourth column of the predicted probabilities
rawresid4 <- (subset_data$Overall_Genre == 'Folk') -  predprobs[,4]
rawresid5 <- (subset_data$Overall_Genre == 'Funk') -  predprobs[,5]
rawresid6 <- (subset_data$Overall_Genre == 'Hiphop') -  predprobs[,6]
rawresid7 <- (subset_data$Overall_Genre == 'House') -  predprobs[,7]
rawresid8 <- (subset_data$Overall_Genre == 'Indie') -  predprobs[,8]
rawresid9 <- (subset_data$Overall_Genre == 'Jazz') -  predprobs[,9]
rawresid10 <- (subset_data$Overall_Genre == 'Metal') -  predprobs[,10]
rawresid11 <- (subset_data$Overall_Genre == 'Pop') -  predprobs[,11]
rawresid12 <- (subset_data$Overall_Genre == 'Punk') -  predprobs[,12]
rawresid13 <- (subset_data$Overall_Genre == 'Rap') -  predprobs[,13]
rawresid14 <- (subset_data$Overall_Genre == 'Rock') -  predprobs[,14]
rawresid15 <- (subset_data$Overall_Genre == 'Techno') -  predprobs[,15]


##can do binned plots for continuous variables
#make a 2 by 2 graphical display

#Danceability
par(mfcol = c(2,2))
binnedplot(subset_data$Danceability, rawresid1, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Danceability, rawresid2, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Danceability, rawresid3, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Danceability, rawresid4, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(subset_data$Danceability, rawresid5, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Danceability, rawresid6, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Danceability, rawresid7, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Danceability, rawresid8, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(subset_data$Danceability, rawresid9, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Danceability, rawresid10, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Danceability, rawresid11, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Danceability, rawresid12, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(subset_data$Danceability, rawresid13, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Danceability, rawresid14, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Danceability, rawresid15, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")



#Energy
par(mfcol = c(2,2))
binnedplot(subset_data$Energy, rawresid1, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Energy, rawresid2, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Energy, rawresid3, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Energy, rawresid4, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(subset_data$Energy, rawresid5, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Energy, rawresid6, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Energy, rawresid7, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Energy, rawresid8, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
#U Shaped Curve
binnedplot(subset_data$Energy, rawresid9, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Energy, rawresid10, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Energy, rawresid11, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Energy, rawresid12, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,1))
binnedplot(subset_data$Energy, rawresid13, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Energy, rawresid14, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")


#Loudness
par(mfcol = c(2,2))
binnedplot(subset_data$Loudness, rawresid1, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Loudness, rawresid2, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Loudness, rawresid3, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Loudness, rawresid4, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(subset_data$Loudness, rawresid5, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Loudness, rawresid6, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Loudness, rawresid7, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Loudness, rawresid8, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
#U Shaped Curve
binnedplot(subset_data$Loudness, rawresid9, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Loudness, rawresid10, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Loudness, rawresid11, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Loudness, rawresid12, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(subset_data$Loudness, rawresid13, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Loudness, rawresid14, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Loudness, rawresid15, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")





#  Overall_Genre=="Country"| Overall_Genre=="Rap" |
#  Overall_Genre=="Techno"| Overall_Genre=="IndieRock" |
#  Overall_Genre=="IndiePop"| Overall_Genre=="Dubstep" 
