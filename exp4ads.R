data()
my_data<-airquality
head(my_data,8)
install.packages("modeest")
require(modeest)
install.packages("ggpubr")
library(ggpubr)

ggboxplot(my_data,y="Wind",width=0.6)
data<-airquality[,1:4]
dim(data)
quartiles<-quantile(data$Wind,probs=c(.25,.75),na.rm=FALSE)
quartiles
IQR<-IQR(data$Wind)
IQR
Lower<-quartiles[1]-1.5*IQR
Upper<-quartiles[2]+1.5*IQR
Lower
Upper
data_no_outlier<-subset(data,data$Wind>Lower & data$Wind<Upper)
dim(data_no_outlier)
ggboxplot(data_no_outlier,y="Wind",width=0.6)
na.omit
na.exclude(5)
dataframe<-data.frame(Name=c("Tiger","Lion","Leopard","Cheetah"),
                      India=c(3000,700,10000,30),
                      SouthAfrica=c(NA,2500,3000,2600),
                      Nepal=c(900,NA,1250,NA))
print(dataframe)
listMissingColumns<-colnames(dataframe)[apply(dataframe,2,anyNA)]
print(listMissingColumns)
meanMissing <- apply(dataframe[,colnames(dataframe)%in%listMissingColumns],
                     2,mean,na.rm=TRUE)
print(meanMissing)
medianMissing<-apply(dataframe[,colnames(dataframe)%in% listMissingColumns],
                     2,median,na.rm=TRUE)
print(medianMissing)
newDataFrameMean<-dataframe%>%mutate(
  SouthAfrica=ifelse(is.na(SouthAfrica),meanMissing[1],SouthAfrica),
  Nepal=ifelse(is.na(Nepal),meanMissing[2],Nepal)
)
print(newDataFrameMean)
newDataFrameMedian<-dataframe%>%mutate(
  SouthAfrica=ifelse(is.na(SouthAfrica),medianMissing[1],SouthAfrica),
  Nepal=ifelse(is.na(Nepal),medianMissing[2],Nepal)
)
print(newDataFrameMedian)
install.packages("moments")
library(moments)
skewness(airquality$Wind)
kurtosis(airquality$Wind)
hist(airquality$Wind)
stem(airquality$Wind)
boxplot(airquality$Wind)
qqnorm(airquality$Wind)
qqline(airquality$Wind)
a <- c(7,6,9,12,14)
b <- c(1,4,19,23,24)
print(cor(a,b))
print(cor(a,b,method="spearman"))
plot(airquality$Wind,airquality$Temp,main="Edgar Anderson's Iris Data")
pairs(~Wind+Temp+Ozone+Month+Day,data=airquality)