############ Data File ######################
library(pitchRx)
library(dplyr)
library(XML2R)
data1<-scrape(start="2016-06-01", end="2016-06-01")
dim(data1)
View(data1[["pitch"]])
View(data1[["batter"]])
View(data1[["atbat"]])
data2<-data1[["atbat"]]
data2<-as.data.frame(data2)
data3<-data2[,c("pitcher","pitcher_name","batter","batter_name", "inning","inning_side","num", "b", "s","o", "event","p_throws","stand","event_num")]

pitch<-data1[["pitch"]]
pitch<-as.data.frame(pitch)
pitch<-pitch[,c("des","num","event_num","pitch_type","start_speed","end_speed")]

data.baseball<-inner_join(pitch,data3, by="event_num")
save(data.baseball, file="data")
load(file="~/Desktop/Documents/Statistics/Thesis/Thesis Codes/mcmcdat.Rdata")
View(head(BaseballData))
View(head(data.baseball))
################# End Data File ###################

######### Toy Data ####################
BaseballDataT <- data.frame(rep(NA,20))
BaseballDataT[,"Name"] <- c( rep("a",5), rep("b",5), rep("c",5), rep("d",5) )
BaseballDataT[,"Position"] <- as.factor(c( rep("1B",5), rep("1B",5), rep("2B",5), rep("2B",5) ))
BaseballDataT[,"Year"] <- rep( 2012:2016, 4)
BaseballDataT[,"Age"] <- rep(1:10,2)
BaseballDataT[,"Park"] <- as.factor(c( rep("P1",5), rep("P2",5), rep("P1",5), rep("P2",5) ))
BaseballDataT[,"AB"] <- rep(100,20)
BaseballDataT[,"Homeruns"] <- c(5,5,5,10,10,7,7,7,7,12, 6,6,6,15,15, 3,3,10,3,3)
BaseballDataT[,"Hand"]<-as.factor(c(rep("R",10),rep("L",5),rep("R",5)))
View(BaseballDataT[,2:9])

## Set Parameters
P<-length(levels(BaseballData[,"Position"]))
Positions<-levels(BaseballData[,"Position"])
w<-1
Draws<-100
nu<-list(NULL)
nu[[1]]<-rep(NA,P)
nu[[2]]<-rep(NA,P)
v00<-matrix(NA,Draws,P) # Each v is a draws x # positions matrix
v01<-matrix(NA,Draws,P)
v10<-matrix(NA,Draws,P)
v11<-matrix(NA,Draws,P)
colnames(v00)<-Positions
colnames(v01)<-Positions
colnames(v10)<-Positions
colnames(v11)<-Positions
#First Base Transition Numbers
n001<-5
n011<-2
n111<-1
n101<-2
#Second Base transition Numbers
n002<-4
n012<-2
n112<-2
n102<-2
#Generate Latent Variable
nu[[1]][1]<-rbeta(1,n001+w,n011+w)
nu[[1]][2]<-rbeta(1,n002+w,n012+w)
nu[[2]][1]<-rbeta(1,n111+w,n101+w)
nu[[2]][2]<-rbeta(1,n112+w,n102+w)
nu
#Generate Initial Transition probabilities
v00[1,]<-nu[[1]]
v01[1,]<-1-nu[[1]]
v11[1,]<-nu[[2]]
v10[1,]<-1-nu[[2]]
head(v01)
# Initialize Elite Matrix
Elite<-rep(NA,dim(YMatrix)[1])
c<-rep(NA,P)
names(c)<-Positions
for(j in 1:P){
  tmp <- BaseballData[, "Homeruns"] / BaseballData[, "AB"]
  TempSelection <- BaseballData[, "Position"]==PositionList[j]
  c[j] <- quantile(tmp[TempSelection], .9)
  Elite[TempSelection & tmp>c[j]] <- 1 * 
    (TempSelection & tmp>c[j])[TempSelection & tmp>c[j]]
}
Elite[is.na(Elite)]<-0
Elite

########## Bad Example ###########
library(HMM)
hmm<-initHMM(c("Elite","Non-Elite"), c("1B","2B"), c(.20,.80), matrix(c(.65,.15,.35,.85)), matrix(c(.1,.9,.15,.85)))
obs<-BaseballDataT[BaseballDataT$Year==2016,]
post<-posterior(hmm,obs$Position)
post
