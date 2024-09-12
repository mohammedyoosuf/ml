# Load Required Packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("NbClust")
install.packages("cluster")
install.packages("readxl")
install.packages("tidyverse")
install.packages("factoextra")
install.packages("stats")
install.packages("fpc")
install.packages("neuralnet")
install.packages("grid")
install.packages("MASS")
install.packages("Metrics")
install.packages("MLmetrics")
install.packages("zoo")
install.packages("forecast")

library(dplyr)
library(ggplot2)
library(ggfortify)
library(NbClust)
library(cluster)
library(readxl)
library(tidyverse)
library(factoextra)
library(stats)
library(fpc)
library(neuralnet)
library(grid)
library(MASS)
library(Metrics)
library(MLmetrics)
library(zoo)
library(forecast)
# Read the data
data<-read_excel("C:/Users/User.DESKTOP-7P834E1/Desktop/MLCW/Whitewine_v6 (1).xlsx")
head(data)

# Data Cleaning
sum(is.na(data))  # Check for missing values
data_without_last<-data[,-ncol(data)]
print(data_without_last)
head(data_without_last)



detect_out=function(fun){
  q1<-quantile(fun,probs=.25,na.rm=TRUE)
  q2<-quantile(fun,probs=.75,na.rm=TRUE)
  q3<-q2-q1
  fun>q2+(q3*1.5)|fun<q1-(q3*1.5)
}
rem_out=function(datas,colu=names(datas)){
  for(coll in colu){
    datas<-datas[!detect_out(datas[[coll]]),]
    
  }
  return(datas)
}
cleaned<-rem_out(data_without_last,names(data_without_last))
head(cleaned)

#nrow(data_without_last)-nrow(cleaned)
###############
scaleddata_withoutlast=as.data.frame(scale(cleaned))
head(scaleddata_withoutlast)




sdata=scale(scaled_data)
print(sdata)


# NbClust
set.seed(66)
clusterNo <-NbClust(sdata,distance="euclidean",min.nc=2,max.nc=10,method="kmeans")
clusterNo 
summary(clusterNo)


#elbow
elbow <-fviz_nbclust(sdata,kmeans,method="wss")
plot(elbow)

#silhouette
silhouette<-fviz_nbclust(sdata,kmeans,method="silhouette")
plot(silhouette)

#gap
gap<-fviz_nbclust(sdata,kmeans,method="gap_stat")
plot(gap)


k=2
kmeanir=kmeans(sdata,centers=k,nstart=10)
fviz_cluster(kmeanir,data=scaled_data)


optimal_k <- 2 

set.seed(26)
kmeans_result<-kmeans(sdata,centers=optimal_k)

# Calculating BSS and WSS
total_ss<-sum((sdata-colMeans(sdata))^2)
wss<-sum(kmeans_result$withinss)
bss<-total_ss-wss


# Print WSS and BSS
print(paste("WSS:",wss))
print(paste("BSS:",bss))

# Ratio of BSS to TSS
ratio_bss_tss<-bss/total_ss
print(paste("Ratio of BSS/TSS:",ratio_bss_tss))


silhouette_widths<-silhouette(kmeanir$cluster,dist(sdata))
plottsilhouette_widths=fviz_silhouette(silhouette_widths)
plot(plottsilhouette_widths)

###pca#####

pcafor2ndtask=prcomp(data,center=TRUE,scale=TRUE)
summary(pcafor2ndtask)

afterpca=as.data.frame(-pcafor2ndtask$x[,1:7])
head(afterpca)
print(afterpca)

# NbClust
set.seed(26)
clusterNo1=NbClust(afterpca,distance="euclidean",min.nc=2,max.nc=10,method="kmeans")
clusterNo1

#elbow
pcaelbow<-fviz_nbclust(afterpca,kmeans,method="wss")
plot(pcaelbow)

#silhouette
pcasil<-fviz_nbclust(afterpca,kmeans,method="silhouette")
plot(pcasil)
#gap
pcagap<-fviz_nbclust(afterpca,kmeans,method="gap_stat")
plot(pcagap)

# 1.Calculate Total Sum of Squares (TSS)
total_mean<-colMeans(afterpca)
total_ss<-sum((afterpca - total_mean)^2)
print(total_ss)
# 2.Calculate Within Cluster Sum of Squares(WSS)
wss<-sum(pcafor2ndtask$withinss)
print(wss)

# 3.Calculate Between Cluster Sum of Squares(BSS)
bss<-total_ss-wss


# Print WSS and BSS
print(paste("WSS:",wss))
print(paste("BSS:",bss))

# 4. Calculate and print the ratio of BSS over TSS
ratio_bss_tss<-bss/total_ss
print(paste("Ratio of BSS/TSS:",ratio_bss_tss))

k=2
kmeaninpca=kmeans(afterpca,centers=k,nstart=10)
fviz_cluster(kmeaninpca,data=afterpca)

silhouette_widthsinpca<-silhouette(kmeaninpca$cluster, dist(afterpca))
fviz_silhouette(silhouette_widthsinpca)

hii=calinhara(afterpca,2,cn=max(2))
print(hii)


## Neural network  part###   ## 
#choosing 3rd column as a input

# 
ExchangeUSD_2_1_<-read_excel("C:/Users/User.DESKTOP-7P834E1/Desktop/MLCW/ExchangeUSD (2) (1).xlsx")

choosed_column<-ExchangeUSD_2_1_[["USD/EUR"]]
print(choosed_column)
# 
# 
choosed_column_standardized<-(choosed_column-mean(choosed_column))/sd(choosed_column)
# 
print(choosed_column_standardized)

traingdata<-choosed_column_standardized[1:181]
print(traingdata)
head(traingdata)
testingdata<-choosed_column_standardized[182:300]
print(testingdata)
dataoftestint<-c(testingdata)
dataofcolumn<-data.frame(Columnoftestingdata = dataoftestint)
print(dataofcolumn)

time_lagged_data<-bind_cols(G_previous2=lag(traingdata,3),G_previous=lag(traingdata,2),G_current=lag(traingdata,1),G_pred=traingdata)
time_lagged_data<-time_lagged_data[complete.cases(time_lagged_data),]

head(time_lagged_data)

str(time_lagged_data)

##NN1###

NN1_lagged_datatraing<-choosed_column_standardized[1:60]
print(NN1_lagged_datatraing)
NN1_lagged_datatesting<-choosed_column_standardized[182:192]
print(NN1_lagged_datatesting)
NN1datatrain<-bind_cols(G_previous2=lag(traingdata, 4),G_current=lag(traingdata, 1),G_pred1=traingdata)
NN1datatest<-bind_cols(G_previous2=lag(testingdata, 4),G_current=lag(testingdata, 1),G_pred1=testingdata)
nrow(NN1datatest)

NN1cleaneddatatrain<-NN1datatrain[complete.cases(NN1datatrain),]
NN1cleaneddatatest<-NN1datatest[complete.cases(NN1datatest),]
nrow(NN1cleaneddatatest)

autoreg_model1<-neuralnet(G_pred1~G_previous2+G_current,data=NN1cleaneddatatrain,hidden=c(1),linear.output=TRUE)
plot(autoreg_model1)

NN1Res<-compute(autoreg_model1,NN1cleaneddatatest[1:3])
normalNN1=NN1Res$net.result
print(normalNN1)
#############
unnormalize<-function(NN1_lagged_datatraing, min, max) {
  return((max-min)*NN1_lagged_datatraing + min)
}
max_NN1<-max(NN1_lagged_datatraing)
min_NN1<-min(NN1_lagged_datatraing)
DenormtrainNN1=unnormalize(normalNN1,min_NN1,max_NN1)
DenormtestNN1=unnormalize(NN1cleaneddatatest,min_NN1,max_NN1)

SummaryofNN1=cbind(DenormtrainNN1,DenormtestNN1)

############
max_timelagged1<-max(NN1_lagged_datatesting)
min_timelagged1<-min(NN1_lagged_datatesting)

unnormalize<-function(data,min,max){return((max-min)*data+min)}
print(unnormalize)
type_pred1<-unnormalize(predicted,min_timelagged1,max_timelagged1)
print(type_pred1)
NNtypepred<-type_pred1[complete.cases(type_pred1)]
print(NNtypepred)

val1ofNNtypepred=normalNN1[1]
val1ofNN1_lagged_data=NN1_lagged_datatesting[1]
errorNN1<-(val1ofNN1_lagged_data-val1ofNNtypepred)
print(errorNN1)

cleanrmseNN1<-errorNN1[complete.cases(errorNN1)]
rmse1<-function(cleanrmse) {
  sqrt(mean(cleanrmse^2))
}
print(rmse1)
rmse_valueNN1<-rmse1(cleanrmseNN1)
print(rmse_valueNN1)
rmse(NNtypepred,cleanrmseNN1)


###MAE####
MAEDATANN1<-data.frame(NN1cleaneddatatest[,c("G_previous2","G_current","G_pred1")])
print(MAEDATANN1)
model<-lm(G_current~G_previous2+G_pred1,data=MAEDATANN1)
print(model)
MAENN1<-mae(MAEDATANN1$G_current,predict(model))
print(MAENN1)
####


#MAPE

n_rows_cleanrmseNN1<-length(cleanrmseNN1)
print(n_rows_cleanrmseNN1)
n_rows_NN1cleaneddata<-length(NN1_lagged_datatesting)
print(n_rows_NN1cleaneddata)
min_rowsNN1<-min(n_rows_cleanrmseNN1,n_rows_NN1cleaneddata)
cleanrmseNN1_trimmed<-cleanrmseNN1[min_rowsNN1]
NN1cleaneddata_trimmed<-NN1_lagged_datatesting[min_rowsNN1]
#minvalueonNN1cleaneddata_trimmed=min(NN1cleaneddata_trimmed)
#print(minvalueonNN1cleaneddata_trimmed)
dtaframeformapeNN1=data.frame(At=c(cleanrmseNN1_trimmed),
                              Ft=c(NN1cleaneddata_trimmed))
print(dtaframeformapeNN1)
mape=function(dtaframeformapeNN1){
  resultNN1 = mean(abs((dtaframeformapeNN1$At-dtaframeformapeNN1$Ft)
                       /dtaframeformapeNN1$At)) * 100
  return(resultNN1)
}
resultNN1=mape(dtaframeformapeNN1)
print(resultNN1)
####################

#SMAPE
NN1S<-function(cleanrmseNN1,NN1_lagged_datatesting){
  return (1 /length(cleanrmseNN1)*sum(2*abs(NN1_lagged_datatesting-cleanrmseNN1)/
                                        (abs(cleanrmseNN1)+abs(NN1_lagged_datatesting))*100))
}
SAMPENN1=NN1S(cleanrmseNN1,NN1_lagged_datatesting)
print(SAMPENN1)

########################################
NN2_lagged_datatraing<-choosed_column_standardized[61:70]
print(NN2_lagged_datatraing)
NN2_lagged_datatesting<-choosed_column_standardized[101:120]
print(NN2_lagged_datatesting)
NN2datatrain<-bind_cols(G_previous2=lag(traingdata,4),G_current=lag(traingdata,1),G_pred1=traingdata)
NN2datatest<-bind_cols(G_previous2=lag(testingdata,4),G_current=lag(testingdata,1),G_pred1=testingdata)
nrow(NN2datatest)

NN2cleaneddatatrain<-NN2datatrain[complete.cases(NN2datatrain),]
NN2cleaneddatatest<-NN2datatest[complete.cases(NN2datatest),]
nrow(NN2cleaneddatatest)

autoreg_model2<-neuralnet(G_pred1~G_previous2+G_current,data=NN2cleaneddatatrain,hidden=c(2),linear.output=TRUE)
plot(autoreg_model2)

NN2Res<-compute(autoreg_model2,NN2cleaneddatatest[1:3])
normalNN2=NN2Res$net.result
print(normalNN2)
#############
unnormalize<-function(NN2_lagged_datatraing,min,max) {
  return((max-min) * NN2_lagged_datatraing+min)
}
max_NN2<-max(NN2_lagged_datatraing)
min_NN2<-min(NN2_lagged_datatraing)
DenormtrainNN2=unnormalize(normalNN2,min_NN2,max_NN2)
DenormtestNN2 = unnormalize(NN2cleaneddatatest, min_NN2, max_NN2)

SummaryofNN2=cbind(DenormtrainNN2, DenormtestNN2)
view(SummaryofNN2)

max_timelagged2<-max(NN2_lagged_datatesting)
min_timelagged2<-min(NN2_lagged_datatesting)

unnormalize<-function(data,min,max){return((max-min)*data+min) }
print(unnormalize)
type_pred2<-unnormalize(predicted,min_timelagged2,max_timelagged2)
print(type_pred2)
NNtypepred2<-type_pred2[complete.cases(type_pred2)]
print(NNtypepred2)

val2ofNNtypepred=normalNN2[1]
val2ofNN2_lagged_data=NN2_lagged_datatesting[1]
errorNN2<-(val2ofNN2_lagged_data-val2ofNNtypepred)
print(errorNN2)

cleanrmseNN2<-errorNN2[complete.cases(errorNN2)]
rmse2<-function(cleanrmse) {
  sqrt(mean(cleanrmse^2))
}
print(rmse2)
rmse_valueNN2<-rmse2(cleanrmseNN2)
print(rmse_valueNN2)
rmse(NNtypepred2,cleanrmseNN2)


###MAE####
MAEDATANN2<-data.frame(NN2cleaneddatatest[,c("G_previous2","G_current","G_pred1")])
print(MAEDATANN2)
model<-lm(G_current~G_previous2+G_pred1,data=MAEDATANN2)
print(model)
MAENN2<-mae(MAEDATANN2$G_current,predict(model))
print(MAENN2)
####


#MAPE

n_rows_cleanrmseNN2 <- length(cleanrmseNN2)
print(n_rows_cleanrmseNN2)
n_rows_NN2cleaneddata <- length(NN2_lagged_datatesting)
print(n_rows_NN2cleaneddata)
min_rowsNN2 <- min(n_rows_cleanrmseNN2, n_rows_NN2cleaneddata)
cleanrmseNN2_trimmed <- cleanrmseNN2[min_rowsNN2]
NN2cleaneddata_trimmed <- NN2_lagged_datatesting[min_rowsNN2]
dtaframeformapeNN2 = data.frame(At = c(cleanrmseNN2_trimmed),
                                Ft = c(NN2cleaneddata_trimmed))
print(dtaframeformapeNN2)
mape = function(dtaframeformapeNN2) {
  resultNN2 = mean(abs((dtaframeformapeNN2$At - dtaframeformapeNN2$Ft)
                       / dtaframeformapeNN2$At)) * 100
  return(resultNN2)
}
resultNN2 = mape(dtaframeformapeNN2)
print(resultNN2)
####################

#SMAPE
NN2S <- function(cleanrmseNN2, NN2_lagged_datatesting) {
  return (1 / length(cleanrmseNN2) * sum(2 * abs(NN2_lagged_datatesting - cleanrmseNN2) /
                                           (abs(cleanrmseNN2) + abs(NN2_lagged_datatesting)) * 100))
}
SAMPENN2 = NN2S(cleanrmseNN2, NN2_lagged_datatesting)
print(SAMPENN2)


#####################################################################################
NN3_lagged_datatraing<-choosed_column_standardized[71:80]
print(NN3_lagged_datatraing)
NN3_lagged_datatesting<-choosed_column_standardized[193:203]
print(NN3_lagged_datatesting)
NN3datatrain<-bind_cols(G_previous2=lag(traingdata,4),G_current=lag(traingdata,1),G_pred1=traingdata)
NN3datatest<-bind_cols(G_previous2=lag(testingdata,4),G_current=lag(testingdata,1),G_pred1=testingdata)
nrow(NN3datatest)

NN3cleaneddatatrain<-NN3datatrain[complete.cases(NN3datatrain),]
NN3cleaneddatatest<-NN3datatest[complete.cases(NN3datatest),]
nrow(NN3cleaneddatatest)

autoreg_model3 <- neuralnet(G_pred1~G_previous2+ G_current,data=NN3cleaneddatatrain,hidden =c(2),linear.output=TRUE)
plot(autoreg_model3)

NN3Res <- compute(autoreg_model3, NN3cleaneddatatest[1:3])
normalNN3 = NN3Res$net.result
print(normalNN3)
#############
unnormalize <- function(NN3_lagged_datatraing, min, max) {
  return((max - min) * NN3_lagged_datatraing + min)
}
max_NN3 <- max(NN3_lagged_datatraing)
min_NN3 <- min(NN3_lagged_datatraing)
DenormtrainNN3 = unnormalize(normalNN3, min_NN3, max_NN3)
DenormtestNN3 = unnormalize(NN3cleaneddatatest, min_NN3, max_NN3)

SummaryofNN3 = cbind(DenormtrainNN3, DenormtestNN3)
#view(SummaryofNN3)

max_timelagged3 <- max(NN3_lagged_datatesting)
min_timelagged3 <- min(NN3_lagged_datatesting)

unnormalize <- function(data, min, max) { return((max - min) * data + min) }
print(unnormalize)
type_pred3 <- unnormalize(predicted, min_timelagged3, max_timelagged3)
print(type_pred3)
NNtypepred3 <- type_pred3[complete.cases(type_pred3)]
print(NNtypepred3)

val3ofNNtypepred = normalNN3[1]
val3ofNN3_lagged_data = NN3_lagged_datatesting[1]
errorNN3 <- (val3ofNN3_lagged_data - val3ofNNtypepred)
print(errorNN3)

cleanrmseNN3 <- errorNN3[complete.cases(errorNN3)]
rmse3 <- function(cleanrmse) {
  sqrt(mean(cleanrmse^2))
}
print(rmse3)
rmse_valueNN3 <- rmse3(cleanrmseNN3)
print(rmse_valueNN3)
rmse(NNtypepred3, cleanrmseNN3)


###MAE####
MAEDATANN3 <- data.frame(NN3cleaneddatatest[, c("G_previous2", "G_current", "G_pred1")])
print(MAEDATANN3)
model <- lm(G_current ~ G_previous2 + G_pred1, data = MAEDATANN3)
print(model)
MAENN3 <- mae(MAEDATANN3$G_current, predict(model))
print(MAENN3)
####


#MAPE

n_rows_cleanrmseNN3 <- length(cleanrmseNN3)
print(n_rows_cleanrmseNN3)
n_rows_NN3cleaneddata <- length(NN3_lagged_datatesting)
print(n_rows_NN3cleaneddata)
min_rowsNN3 <- min(n_rows_cleanrmseNN3, n_rows_NN3cleaneddata)
cleanrmseNN3_trimmed <- cleanrmseNN3[min_rowsNN3]
NN3cleaneddata_trimmed <- NN3_lagged_datatesting[min_rowsNN3]
dtaframeformapeNN3 = data.frame(At = c(cleanrmseNN3_trimmed),
                                Ft = c(NN3cleaneddata_trimmed))
print(dtaframeformapeNN3)
mape = function(dtaframeformapeNN3) {
  resultNN3 = mean(abs((dtaframeformapeNN3$At - dtaframeformapeNN3$Ft)
                       / dtaframeformapeNN3$At)) * 100
  return(resultNN3)
}
resultNN3 = mape(dtaframeformapeNN3)
print(resultNN3)
####################

#SMAPE
NN3S <- function(cleanrmseNN3, NN3_lagged_datatesting) {
  return (1 / length(cleanrmseNN3) * sum(2 * abs(NN3_lagged_datatesting - cleanrmseNN3) /
                                           (abs(cleanrmseNN3) + abs(NN3_lagged_datatesting)) * 100))
}
SAMPENN3 = NN3S(cleanrmseNN3, NN3_lagged_datatesting)
print(SAMPENN3)



##################################################################################
NN4_lagged_datatraing <- choosed_column_standardized[81:90]
print(NN4_lagged_datatraing)
NN4_lagged_datatesting <- choosed_column_standardized[204:214]
print(NN4_lagged_datatesting)
NN4datatrain <- bind_cols(G_previous2 = lag(traingdata, 4), G_current = lag(traingdata, 1), G_pred1 = traingdata)
NN4datatest <- bind_cols(G_previous2 = lag(testingdata, 4), G_current = lag(testingdata, 1), G_pred1 = testingdata)
nrow(NN4datatest)

NN4cleaneddatatrain <- NN4datatrain[complete.cases(NN4datatrain), ]
NN4cleaneddatatest <- NN4datatest[complete.cases(NN4datatest), ]
nrow(NN4cleaneddatatest)

autoreg_model4 <- neuralnet(G_pred1 ~ G_previous2 + G_current, data = NN4cleaneddatatrain, hidden = c(2), linear.output = TRUE)
plot(autoreg_model4)

NN4Res <- compute(autoreg_model4, NN4cleaneddatatest[1:3])
normalNN4 = NN4Res$net.result
print(normalNN4)
#############
unnormalize <- function(NN4_lagged_datatraing, min, max) {
  return((max - min) * NN4_lagged_datatraing + min)
}
max_NN4 <- max(NN4_lagged_datatraing)
min_NN4 <- min(NN4_lagged_datatraing)
DenormtrainNN4 = unnormalize(normalNN4, min_NN4, max_NN4)
DenormtestNN4 = unnormalize(NN4cleaneddatatest, min_NN4, max_NN4)

SummaryofNN4 = cbind(DenormtrainNN4, DenormtestNN4)
# view(SummaryofNN4)

max_timelagged4 <- max(NN4_lagged_datatesting)
min_timelagged4 <- min(NN4_lagged_datatesting)

unnormalize <- function(data, min, max) { return((max - min) * data + min) }
print(unnormalize)
type_pred4 <- unnormalize(predicted, min_timelagged4, max_timelagged4)
print(type_pred4)
NNtypepred4 <- type_pred4[complete.cases(type_pred4)]
print(NNtypepred4)

val4ofNNtypepred = normalNN4[1]
val4ofNN4_lagged_data = NN4_lagged_datatesting[1]
errorNN4 <- (val4ofNN4_lagged_data - val4ofNNtypepred)
print(errorNN4)

cleanrmseNN4 <- errorNN4[complete.cases(errorNN4)]
rmse4 <- function(cleanrmse) {
  sqrt(mean(cleanrmse^2))
}
print(rmse4)
rmse_valueNN4 <- rmse4(cleanrmseNN4)
print(rmse_valueNN4)
rmse(NNtypepred4, cleanrmseNN4)


###MAE####
MAEDATANN4 <- data.frame(NN4cleaneddatatest[, c("G_previous2", "G_current", "G_pred1")])
print(MAEDATANN4)
model <- lm(G_current ~ G_previous2 + G_pred1, data = MAEDATANN4)
print(model)
MAENN4 <- mae(MAEDATANN4$G_current, predict(model))
print(MAENN4)
####


#MAPE

n_rows_cleanrmseNN4 <- length(cleanrmseNN4)
print(n_rows_cleanrmseNN4)
n_rows_NN4cleaneddata <- length(NN4_lagged_datatesting)
print(n_rows_NN4cleaneddata)
min_rowsNN4 <- min(n_rows_cleanrmseNN4, n_rows_NN4cleaneddata)
cleanrmseNN4_trimmed <- cleanrmseNN4[min_rowsNN4]
NN4cleaneddata_trimmed <- NN4_lagged_datatesting[min_rowsNN4]
dtaframeformapeNN4 = data.frame(At = c(cleanrmseNN4_trimmed),
                                Ft = c(NN4cleaneddata_trimmed))
print(dtaframeformapeNN4)
mape = function(dtaframeformapeNN4) {
  resultNN4 = mean(abs((dtaframeformapeNN4$At - dtaframeformapeNN4$Ft)
                       / dtaframeformapeNN4$At)) * 100
  return(resultNN4)
}
resultNN4 = mape(dtaframeformapeNN4)
print(resultNN4)
####################

#SMAPE
NN4S <- function(cleanrmseNN4, NN4_lagged_datatesting) {
  return (1 / length(cleanrmseNN4) * sum(2 * abs(NN4_lagged_datatesting - cleanrmseNN4) /
                                           (abs(cleanrmseNN4) + abs(NN4_lagged_datatesting)) * 100))
}
SAMPENN4 = NN4S(cleanrmseNN4, NN4_lagged_datatesting)
print(SAMPENN4)


#######################################################################################################################


NN5_lagged_datatraing <- choosed_column_standardized[91:100]
print(NN5_lagged_datatraing)
NN5_lagged_datatesting <- choosed_column_standardized[215:225]
print(NN5_lagged_datatesting)
NN5datatrain <- bind_cols(G_previous2 = lag(traingdata, 4), G_current = lag(traingdata, 1), G_pred1 = traingdata)
NN5datatest <- bind_cols(G_previous2 = lag(testingdata, 4), G_current = lag(testingdata, 1), G_pred1 = testingdata)
nrow(NN5datatest)

NN5cleaneddatatrain <- NN5datatrain[complete.cases(NN5datatrain), ]
NN5cleaneddatatest <- NN5datatest[complete.cases(NN5datatest), ]
nrow(NN5cleaneddatatest)

autoreg_model5 <- neuralnet(G_pred1 ~ G_previous2 + G_current, data = NN5cleaneddatatrain, hidden = c(2), linear.output = TRUE)
plot(autoreg_model5)

NN5Res <- compute(autoreg_model5, NN5cleaneddatatest[1:3])
normalNN5 = NN5Res$net.result
print(normalNN5)
#############
unnormalize <- function(NN5_lagged_datatraing, min, max) {
  return((max - min) * NN5_lagged_datatraing + min)
}
max_NN5 <- max(NN5_lagged_datatraing)
min_NN5 <- min(NN5_lagged_datatraing)
DenormtrainNN5 = unnormalize(normalNN5, min_NN5, max_NN5)
DenormtestNN5 = unnormalize(NN5cleaneddatatest, min_NN5, max_NN5)

SummaryofNN5 = cbind(DenormtrainNN5, DenormtestNN5)
#view(SummaryofNN5)

max_timelagged5 <- max(NN5_lagged_datatesting)
min_timelagged5 <- min(NN5_lagged_datatesting)

unnormalize <- function(data, min, max) { return((max - min) * data + min) }
print(unnormalize)
type_pred5 <- unnormalize(predicted, min_timelagged5, max_timelagged5)
print(type_pred5)
NNtypepred5 <- type_pred5[complete.cases(type_pred5)]
print(NNtypepred5)

val5ofNNtypepred = normalNN5[1]
val5ofNN5_lagged_data = NN5_lagged_datatesting[1]
errorNN5 <- (val5ofNN5_lagged_data - val5ofNNtypepred)
print(errorNN5)

cleanrmseNN5 <- errorNN5[complete.cases(errorNN5)]
rmse5 <- function(cleanrmse) {
  sqrt(mean(cleanrmse^2))
}
print(rmse5)
rmse_valueNN5 <- rmse5(cleanrmseNN5)
print(rmse_valueNN5)
rmse(NNtypepred5, cleanrmseNN5)


###MAE####
MAEDATANN5 <- data.frame(NN5cleaneddatatest[, c("G_previous2", "G_current", "G_pred1")])
print(MAEDATANN5)
model <- lm(G_current ~ G_previous2 + G_pred1, data = MAEDATANN5)
print(model)
MAENN5 <- mae(MAEDATANN5$G_current, predict(model))
print(MAENN5)
####


#MAPE

n_rows_cleanrmseNN5 <- length(cleanrmseNN5)
print(n_rows_cleanrmseNN5)
n_rows_NN5cleaneddata <- length(NN5_lagged_datatesting)
print(n_rows_NN5cleaneddata)
min_rowsNN5 <- min(n_rows_cleanrmseNN5, n_rows_NN5cleaneddata)
cleanrmseNN5_trimmed <- cleanrmseNN5[min_rowsNN5]
NN5cleaneddata_trimmed <- NN5_lagged_datatesting[min_rowsNN5]
dtaframeformapeNN5 = data.frame(At = c(cleanrmseNN5_trimmed),
                                Ft = c(NN5cleaneddata_trimmed))
print(dtaframeformapeNN5)
mape = function(dtaframeformapeNN5) {
  resultNN5 = mean(abs((dtaframeformapeNN5$At - dtaframeformapeNN5$Ft)
                       / dtaframeformapeNN5$At)) * 100
  return(resultNN5)
}
resultNN5 = mape(dtaframeformapeNN5)
print(resultNN5)
####################

#SMAPE
NN5S <- function(cleanrmseNN5, NN5_lagged_datatesting) {
  return (1 / length(cleanrmseNN5) * sum(2 * abs(NN5_lagged_datatesting - cleanrmseNN5) /
                                           (abs(cleanrmseNN5) + abs(NN5_lagged_datatesting)) * 100))
}
SAMPENN5 = NN5S(cleanrmseNN5, NN5_lagged_datatesting)
print(SAMPENN5)

##################################################################################################################
NN6_lagged_datatraing <- choosed_column_standardized[101:110]
print(NN6_lagged_datatraing)
NN6_lagged_datatesting <- choosed_column_standardized[226:236]
print(NN6_lagged_datatesting)
NN6datatrain <- bind_cols(G_previous2 = lag(traingdata, 4), G_current = lag(traingdata, 1), G_pred1 = traingdata)
NN6datatest <- bind_cols(G_previous2 = lag(testingdata, 4), G_current = lag(testingdata, 1), G_pred1 = testingdata)
nrow(NN6datatest)

NN6cleaneddatatrain <- NN6datatrain[complete.cases(NN6datatrain), ]
NN6cleaneddatatest <- NN6datatest[complete.cases(NN6datatest), ]
nrow(NN6cleaneddatatest)

autoreg_model6 <- neuralnet(G_pred1 ~ G_previous2 + G_current, data = NN6cleaneddatatrain, hidden =c(2), linear.output = TRUE)
plot(autoreg_model6)

NN6Res <- compute(autoreg_model6, NN6cleaneddatatest[1:3])
normalNN6 = NN6Res$net.result
print(normalNN6)
#############
unnormalize <- function(NN6_lagged_datatraing, min, max) {
  return((max - min) * NN6_lagged_datatraing + min)
}
max_NN6 <- max(NN6_lagged_datatraing)
min_NN6 <- min(NN6_lagged_datatraing)
DenormtrainNN6 = unnormalize(normalNN6, min_NN6, max_NN6)
DenormtestNN6 = unnormalize(NN6cleaneddatatest, min_NN6, max_NN6)

SummaryofNN6 = cbind(DenormtrainNN6, DenormtestNN6)
#view(SummaryofNN6)

max_timelagged6 <- max(NN6_lagged_datatesting)
min_timelagged6 <- min(NN6_lagged_datatesting)

unnormalize <- function(data, min, max) { return((max - min) * data + min) }
print(unnormalize)
type_pred6 <- unnormalize(predicted, min_timelagged6, max_timelagged6)
print(type_pred6)
NNtypepred6 <- type_pred6[complete.cases(type_pred6)]
print(NNtypepred6)

val6ofNNtypepred = normalNN6[1]
val6ofNN6_lagged_data = NN6_lagged_datatesting[1]
errorNN6 <- (val6ofNN6_lagged_data - val6ofNNtypepred)
print(errorNN6)

cleanrmseNN6 <- errorNN6[complete.cases(errorNN6)]
rmse6 <- function(cleanrmse) {
  sqrt(mean(cleanrmse^2))
}
print(rmse6)
rmse_valueNN6 <- rmse6(cleanrmseNN6)
print(rmse_valueNN6)
rmse(NNtypepred6, cleanrmseNN6)


###MAE####
MAEDATANN6 <- data.frame(NN6cleaneddatatest[, c("G_previous2", "G_current", "G_pred1")])
print(MAEDATANN6)
model <- lm(G_current ~ G_previous2 + G_pred1, data = MAEDATANN6)
print(model)
MAENN6 <- mae(MAEDATANN6$G_current, predict(model))
print(MAENN6)
####


#MAPE

n_rows_cleanrmseNN6 <- length(cleanrmseNN6)
print(n_rows_cleanrmseNN6)
n_rows_NN6cleaneddata <- length(NN6_lagged_datatesting)
print(n_rows_NN6cleaneddata)
min_rowsNN6 <- min(n_rows_cleanrmseNN6, n_rows_NN6cleaneddata)
cleanrmseNN6_trimmed <- cleanrmseNN6[min_rowsNN6]
NN6cleaneddata_trimmed <- NN6_lagged_datatesting[min_rowsNN6]
dtaframeformapeNN6 = data.frame(At = c(cleanrmseNN6_trimmed),
                                Ft = c(NN6cleaneddata_trimmed))
print(dtaframeformapeNN6)
mape = function(dtaframeformapeNN6) {
  resultNN6 = mean(abs((dtaframeformapeNN6$At - dtaframeformapeNN6$Ft)
                       / dtaframeformapeNN6$At)) * 100
  return(resultNN6)
}
resultNN6 = mape(dtaframeformapeNN6)
print(resultNN6)
####################

#SMAPE
NN6S <- function(cleanrmseNN6, NN6_lagged_datatesting) {
  return (1 / length(cleanrmseNN6) * sum(2 * abs(NN6_lagged_datatesting - cleanrmseNN6) /
                                           (abs(cleanrmseNN6) + abs(NN6_lagged_datatesting)) * 100))
}
SAMPENN6 = NN6S(cleanrmseNN6, NN6_lagged_datatesting)
print(SAMPENN6)




##################################################################################################################
NN7_lagged_datatraing <- choosed_column_standardized[111:120]
print(NN7_lagged_datatraing)
NN7_lagged_datatesting <- choosed_column_standardized[237:247]
print(NN7_lagged_datatesting)
NN7datatrain <- bind_cols(G_previous2 = lag(traingdata, 4), G_current = lag(traingdata, 1), G_pred1 = traingdata)
NN7datatest <- bind_cols(G_previous2 = lag(testingdata, 4), G_current = lag(testingdata, 1), G_pred1 = testingdata)
nrow(NN7datatest)

NN7cleaneddatatrain <- NN7datatrain[complete.cases(NN7datatrain), ]
NN7cleaneddatatest <- NN7datatest[complete.cases(NN7datatest), ]
nrow(NN7cleaneddatatest)

autoreg_model7 <- neuralnet(G_pred1 ~ G_previous2 + G_current, data = NN7cleaneddatatrain, hidden = c(2), linear.output = TRUE)
plot(autoreg_model7)

NN7Res <- compute(autoreg_model7, NN7cleaneddatatest[1:3])
normalNN7 = NN7Res$net.result
print(normalNN7)
#############
unnormalize <- function(NN7_lagged_datatraing, min, max) {
  return((max - min) * NN7_lagged_datatraing + min)
}
max_NN7 <- max(NN7_lagged_datatraing)
min_NN7 <- min(NN7_lagged_datatraing)
DenormtrainNN7 = unnormalize(normalNN7, min_NN7, max_NN7)
DenormtestNN7 = unnormalize(NN7cleaneddatatest, min_NN7, max_NN7)

SummaryofNN7 = cbind(DenormtrainNN7, DenormtestNN7)
view(SummaryofNN7)

max_timelagged7 <- max(NN7_lagged_datatesting)
min_timelagged7 <- min(NN7_lagged_datatesting)

unnormalize <- function(data, min, max) { return((max - min) * data + min) }
print(unnormalize)
type_pred7 <- unnormalize(predicted, min_timelagged7, max_timelagged7)
print(type_pred7)
NNtypepred7 <- type_pred7[complete.cases(type_pred7)]
print(NNtypepred7)

val7ofNNtypepred = normalNN7[1]
val7ofNN7_lagged_data = NN7_lagged_datatesting[1]
errorNN7 <- (val7ofNN7_lagged_data - val7ofNNtypepred)
print(errorNN7)

cleanrmseNN7 <- errorNN7[complete.cases(errorNN7)]
rmse7 <- function(cleanrmse) {
  sqrt(mean(cleanrmse^2))
}
print(rmse7)
rmse_valueNN7 <- rmse7(cleanrmseNN7)
print(rmse_valueNN7)
rmse(NNtypepred7, cleanrmseNN7)


###MAE####
MAEDATANN7 <- data.frame(NN7cleaneddatatest[, c("G_previous2", "G_current", "G_pred1")])
print(MAEDATANN7)
model <- lm(G_current ~ G_previous2 + G_pred1, data = MAEDATANN7)
print(model)
MAENN7 <- mae(MAEDATANN7$G_current, predict(model))
print(MAENN7)
####


#MAPE

n_rows_cleanrmseNN7 <- length(cleanrmseNN7)
print(n_rows_cleanrmseNN7)
n_rows_NN7cleaneddata <- length(NN7_lagged_datatesting)
print(n_rows_NN7cleaneddata)
min_rowsNN7 <- min(n_rows_cleanrmseNN7, n_rows_NN7cleaneddata)
cleanrmseNN7_trimmed <- cleanrmseNN7[min_rowsNN7]
NN7cleaneddata_trimmed <- NN7_lagged_datatesting[min_rowsNN7]
dtaframeformapeNN7 = data.frame(At = c(cleanrmseNN7_trimmed),
                                Ft = c(NN7cleaneddata_trimmed))
print(dtaframeformapeNN7)
mape = function(dtaframeformapeNN7) {
  resultNN7 = mean(abs((dtaframeformapeNN7$At - dtaframeformapeNN7$Ft)
                       / dtaframeformapeNN7$At)) * 100
  return(resultNN7)
}
resultNN7 = mape(dtaframeformapeNN7)
print(resultNN7)
####################

#SMAPE
NN7S <- function(cleanrmseNN7, NN7_lagged_datatesting) {
  return (1 / length(cleanrmseNN7) * sum(2 * abs(NN7_lagged_datatesting - cleanrmseNN7) /
                                           (abs(cleanrmseNN7) + abs(NN7_lagged_datatesting)) * 100))
}
SAMPENN7 = NN7S(cleanrmseNN7, NN7_lagged_datatesting)
print(SAMPENN7)

#####################################################################################################

NN8_lagged_datatraing <- choosed_column_standardized[121:130]
print(NN8_lagged_datatraing)
NN8_lagged_datatesting <- choosed_column_standardized[248:258]
print(NN8_lagged_datatesting)
NN8datatrain <- bind_cols(G_previous2 = lag(traingdata, 4), G_current = lag(traingdata, 1), G_pred1 = traingdata)
NN8datatest <- bind_cols(G_previous2 = lag(testingdata, 4), G_current = lag(testingdata, 1), G_pred1 = testingdata)
nrow(NN8datatest)

NN8cleaneddatatrain <- NN8datatrain[complete.cases(NN8datatrain), ]
NN8cleaneddatatest <- NN8datatest[complete.cases(NN8datatest), ]
nrow(NN8cleaneddatatest)

autoreg_model8 <- neuralnet(G_pred1 ~ G_previous2 + G_current, data = NN8cleaneddatatrain, hidden = c(1,2), linear.output = TRUE)
plot(autoreg_model8)

NN8Res <- compute(autoreg_model8, NN8cleaneddatatest[1:3])
normalNN8 = NN8Res$net.result
print(normalNN8)
#############
unnormalize <- function(NN8_lagged_datatraing, min, max) {
  return((max - min) * NN8_lagged_datatraing + min)
}
max_NN8 <- max(NN8_lagged_datatraing)
min_NN8 <- min(NN8_lagged_datatraing)
DenormtrainNN8 = unnormalize(normalNN8, min_NN8, max_NN8)
DenormtestNN8 = unnormalize(NN8cleaneddatatest, min_NN8, max_NN8)

SummaryofNN8 = cbind(DenormtrainNN8, DenormtestNN8)
#view(SummaryofNN8)

max_timelagged8 <- max(NN8_lagged_datatesting)
min_timelagged8 <- min(NN8_lagged_datatesting)

unnormalize <- function(data, min, max) { return((max - min) * data + min) }
print(unnormalize)
type_pred8 <- unnormalize(predicted, min_timelagged8, max_timelagged8)
print(type_pred8)
NNtypepred8 <- type_pred8[complete.cases(type_pred8)]
print(NNtypepred8)

val8ofNNtypepred = normalNN8[1]
val8ofNN8_lagged_data = NN8_lagged_datatesting[1]
errorNN8 <- (val8ofNN8_lagged_data - val8ofNNtypepred)
print(errorNN8)

cleanrmseNN8 <- errorNN8[complete.cases(errorNN8)]
rmse8 <- function(cleanrmse) {
  sqrt(mean(cleanrmse^2))
}
print(rmse8)
rmse_valueNN8 <- rmse8(cleanrmseNN8)
print(rmse_valueNN8)
rmse(NNtypepred8, cleanrmseNN8)


###MAE####
MAEDATANN8 <- data.frame(NN8cleaneddatatest[, c("G_previous2", "G_current", "G_pred1")])
print(MAEDATANN8)
model <- lm(G_current ~ G_previous2 + G_pred1, data = MAEDATANN8)
print(model)
MAENN8 <- mae(MAEDATANN8$G_current, predict(model))
print(MAENN8)
####


#MAPE

n_rows_cleanrmseNN8 <- length(cleanrmseNN8)
print(n_rows_cleanrmseNN8)
n_rows_NN8cleaneddata <- length(NN8_lagged_datatesting)
print(n_rows_NN8cleaneddata)
min_rowsNN8 <- min(n_rows_cleanrmseNN8, n_rows_NN8cleaneddata)
cleanrmseNN8_trimmed <- cleanrmseNN8[min_rowsNN8]
NN8cleaneddata_trimmed <- NN8_lagged_datatesting[min_rowsNN8]
dtaframeformapeNN8 = data.frame(At = c(cleanrmseNN8_trimmed),
                                Ft = c(NN8cleaneddata_trimmed))
print(dtaframeformapeNN8)
mape = function(dtaframeformapeNN8) {
  resultNN8 = mean(abs((dtaframeformapeNN8$At - dtaframeformapeNN8$Ft)
                       / dtaframeformapeNN8$At)) * 100
  return(resultNN8)
}
resultNN8 = mape(dtaframeformapeNN8)
print(resultNN8)
####################

#SMAPE
NN8S <- function(cleanrmseNN8, NN8_lagged_datatesting) {
  return (1 / length(cleanrmseNN8) * sum(2 * abs(NN8_lagged_datatesting - cleanrmseNN8) /
                                           (abs(cleanrmseNN8) + abs(NN8_lagged_datatesting)) * 100))
}
SAMPENN8 = NN8S(cleanrmseNN8, NN8_lagged_datatesting)
print(SAMPENN8)




##############################################################################################################


NN9_lagged_datatraing <- choosed_column_standardized[131:140]
print(NN9_lagged_datatraing)
NN9_lagged_datatesting <- choosed_column_standardized[259:269]
print(NN9_lagged_datatesting)
NN9datatrain <- bind_cols(G_previous2 = lag(traingdata, 4), G_current = lag(traingdata, 1), G_pred1 = traingdata)
NN9datatest <- bind_cols(G_previous2 = lag(testingdata, 4), G_current = lag(testingdata, 1), G_pred1 = testingdata)
nrow(NN9datatest)

NN9cleaneddatatrain <- NN9datatrain[complete.cases(NN9datatrain), ]
NN9cleaneddatatest <- NN9datatest[complete.cases(NN9datatest), ]
nrow(NN9cleaneddatatest)

autoreg_model9 <- neuralnet(G_pred1 ~ G_previous2 + G_current, data = NN9cleaneddatatrain, hidden = c(2), linear.output = TRUE)
plot(autoreg_model9)

NN9Res <- compute(autoreg_model9, NN9cleaneddatatest[1:3])
normalNN9 = NN9Res$net.result
print(normalNN9)
#############
unnormalize <- function(NN9_lagged_datatraing, min, max) {
  return((max - min) * NN9_lagged_datatraing + min)
}
max_NN9 <- max(NN9_lagged_datatraing)
min_NN9 <- min(NN9_lagged_datatraing)
DenormtrainNN9 = unnormalize(normalNN9, min_NN9, max_NN9)
DenormtestNN9 = unnormalize(NN9cleaneddatatest, min_NN9, max_NN9)

SummaryofNN9 = cbind(DenormtrainNN9, DenormtestNN9)
view(SummaryofNN9)

max_timelagged9 <- max(NN9_lagged_datatesting)
min_timelagged9 <- min(NN9_lagged_datatesting)

unnormalize <- function(data, min, max) { return((max - min) * data + min) }
print(unnormalize)
type_pred9 <- unnormalize(predicted, min_timelagged9, max_timelagged9)
print(type_pred9)
NNtypepred9 <- type_pred9[complete.cases(type_pred9)]
print(NNtypepred9)

val9ofNNtypepred = normalNN9[1]
val9ofNN9_lagged_data = NN9_lagged_datatesting[1]
errorNN9 <- (val9ofNN9_lagged_data - val9ofNNtypepred)
print(errorNN9)

cleanrmseNN9 <- errorNN9[complete.cases(errorNN9)]
rmse9 <- function(cleanrmse) {
  sqrt(mean(cleanrmse^2))
}
print(rmse9)
rmse_valueNN9 <- rmse9(cleanrmseNN9)
print(rmse_valueNN9)
rmse(NNtypepred9, cleanrmseNN9)


###MAE####
MAEDATANN9 <- data.frame(NN9cleaneddatatest[, c("G_previous2", "G_current", "G_pred1")])
print(MAEDATANN9)
model <- lm(G_current ~ G_previous2 + G_pred1, data = MAEDATANN9)
print(model)
MAENN9 <- mae(MAEDATANN9$G_current, predict(model))
print(MAENN9)
####


#MAPE

n_rows_cleanrmseNN9 <- length(cleanrmseNN9)
print(n_rows_cleanrmseNN9)
n_rows_NN9cleaneddata <- length(NN9_lagged_datatesting)
print(n_rows_NN9cleaneddata)
min_rowsNN9 <- min(n_rows_cleanrmseNN9, n_rows_NN9cleaneddata)
cleanrmseNN9_trimmed <- cleanrmseNN9[min_rowsNN9]
NN9cleaneddata_trimmed <- NN9_lagged_datatesting[min_rowsNN9]
dtaframeformapeNN9 = data.frame(At = c(cleanrmseNN9_trimmed),
                                Ft = c(NN9cleaneddata_trimmed))
print(dtaframeformapeNN9)
mape = function(dtaframeformapeNN9) {
  resultNN9 = mean(abs((dtaframeformapeNN9$At - dtaframeformapeNN9$Ft)
                       / dtaframeformapeNN9$At)) * 100
  return(resultNN9)
}
resultNN9 = mape(dtaframeformapeNN9)
print(resultNN9)
####################

#SMAPE
NN9S <- function(cleanrmseNN9, NN9_lagged_datatesting) {
  return (1 / length(cleanrmseNN9) * sum(2 * abs(NN9_lagged_datatesting - cleanrmseNN9) /
                                           (abs(cleanrmseNN9) + abs(NN9_lagged_datatesting)) * 100))
}
SAMPENN9 = NN9S(cleanrmseNN9, NN9_lagged_datatesting)
print(SAMPENN9)

################################################################################################

NN10_lagged_datatraing <- choosed_column_standardized[141:150]
print(NN10_lagged_datatraing)
NN10_lagged_datatesting <- choosed_column_standardized[270:280]
print(NN10_lagged_datatesting)
NN10datatrain<-bind_cols(G_previous2 = lag(traingdata, 4), G_current = lag(traingdata, 1), G_pred1 = traingdata)
NN10datatest<-bind_cols(G_previous2 = lag(testingdata, 4), G_current = lag(testingdata, 1), G_pred1 = testingdata)
nrow(NN10datatest)

NN10cleaneddatatrain<-NN10datatrain[complete.cases(NN10datatrain), ]
NN10cleaneddatatest<-NN10datatest[complete.cases(NN10datatest), ]
nrow(NN10cleaneddatatest)

autoreg_model10<-neuralnet(G_pred1~G_previous2+G_current,data=NN10cleaneddatatrain,hidden = c(2), linear.output = TRUE)
plot(autoreg_model10)

NN10Res <- compute(autoreg_model10, NN10cleaneddatatest[1:3])
normalNN10 = NN10Res$net.result
print(normalNN10)
#############
unnormalize <- function(NN10_lagged_datatraing, min, max) {
  return((max - min) * NN10_lagged_datatraing + min)
}
max_NN10 <- max(NN10_lagged_datatraing)
min_NN10 <- min(NN10_lagged_datatraing)
DenormtrainNN10 = unnormalize(normalNN10, min_NN10, max_NN10)
DenormtestNN10 = unnormalize(NN10cleaneddatatest, min_NN10, max_NN10)

SummaryofNN10 = cbind(DenormtrainNN10, DenormtestNN10)
view(SummaryofNN10)

max_timelagged10 <- max(NN10_lagged_datatesting)
min_timelagged10 <- min(NN10_lagged_datatesting)

unnormalize <- function(data, min, max) { return((max - min) * data + min) }
print(unnormalize)
type_pred10 <- unnormalize(predicted, min_timelagged10, max_timelagged10)
print(type_pred10)
NNtypepred10 <- type_pred10[complete.cases(type_pred10)]
print(NNtypepred10)

val10ofNNtypepred = normalNN10[1]
val10ofNN10_lagged_data = NN10_lagged_datatesting[1]
errorNN10 <- (val10ofNN10_lagged_data - val10ofNNtypepred)
print(errorNN10)

cleanrmseNN10 <- errorNN10[complete.cases(errorNN10)]
rmse10 <- function(cleanrmse) {
  sqrt(mean(cleanrmse^2))
}
print(rmse10)
rmse_valueNN10 <- rmse10(cleanrmseNN10)
print(rmse_valueNN10)
rmse(NNtypepred10, cleanrmseNN10)


###MAE####
MAEDATANN10 <- data.frame(NN10cleaneddatatest[, c("G_previous2", "G_current", "G_pred1")])
print(MAEDATANN10)
model <- lm(G_current ~ G_previous2 + G_pred1, data = MAEDATANN10)
print(model)
MAENN10 <- mae(MAEDATANN10$G_current, predict(model))
print(MAENN10)
####


#MAPE

n_rows_cleanrmseNN10 <- length(cleanrmseNN10)
print(n_rows_cleanrmseNN10)
n_rows_NN10cleaneddata <- length(NN10_lagged_datatesting)
print(n_rows_NN10cleaneddata)
min_rowsNN10 <- min(n_rows_cleanrmseNN10, n_rows_NN10cleaneddata)
cleanrmseNN10_trimmed <- cleanrmseNN10[min_rowsNN10]
NN10cleaneddata_trimmed <- NN10_lagged_datatesting[min_rowsNN10]
dtaframeformapeNN10 = data.frame(At = c(cleanrmseNN10_trimmed),
                                 Ft = c(NN10cleaneddata_trimmed))
print(dtaframeformapeNN10)
mape = function(dtaframeformapeNN10) {
  resultNN10 = mean(abs((dtaframeformapeNN10$At - dtaframeformapeNN10$Ft)
                        / dtaframeformapeNN10$At)) * 100
  return(resultNN10)
}
resultNN10 = mape(dtaframeformapeNN10)
print(resultNN10)
####################

#SMAPE
NN10S <- function(cleanrmseNN10, NN10_lagged_datatesting) {
  return (1 / length(cleanrmseNN10) * sum(2 * abs(NN10_lagged_datatesting - cleanrmseNN10) /
                                            (abs(cleanrmseNN10) + abs(NN10_lagged_datatesting)) * 100))
}
SAMPENN10 = NN10S(cleanrmseNN10, NN10_lagged_datatesting)
print(SAMPENN10)

#########################################################################################################
NN11_lagged_datatraing <- choosed_column_standardized[151:160]
print(NN11_lagged_datatraing)
NN11_lagged_datatesting <- choosed_column_standardized[281:291]
print(NN11_lagged_datatesting)
NN11datatrain <- bind_cols(G_previous2 = lag(traingdata, 4), G_current = lag(traingdata, 1), G_pred1 = traingdata)
NN11datatest <- bind_cols(G_previous2 = lag(testingdata, 4), G_current = lag(testingdata, 1), G_pred1 = testingdata)
nrow(NN11datatest)

NN11cleaneddatatrain <- NN11datatrain[complete.cases(NN11datatrain), ]
NN11cleaneddatatest <- NN11datatest[complete.cases(NN11datatest), ]
nrow(NN11cleaneddatatest)

autoreg_model11 <- neuralnet(G_pred1 ~ G_previous2 + G_current, data = NN11cleaneddatatrain, hidden = c(1,2), linear.output = TRUE)
plot(autoreg_model11)

NN11Res <- compute(autoreg_model11, NN11cleaneddatatest[1:3])
normalNN11 = NN11Res$net.result
print(normalNN11)
#############
unnormalize <- function(NN11_lagged_datatraing, min, max) {
  return((max - min) * NN11_lagged_datatraing + min)
}
max_NN11 <- max(NN11_lagged_datatraing)
min_NN11 <- min(NN11_lagged_datatraing)
DenormtrainNN11 = unnormalize(normalNN11, min_NN11, max_NN11)
DenormtestNN11 = unnormalize(NN11cleaneddatatest, min_NN11, max_NN11)

SummaryofNN11 = cbind(DenormtrainNN11, DenormtestNN11)
view(SummaryofNN11)

max_timelagged11 <- max(NN11_lagged_datatesting)
min_timelagged11 <- min(NN11_lagged_datatesting)

unnormalize <- function(data, min, max) { return((max - min) * data + min) }
print(unnormalize)
type_pred11 <- unnormalize(predicted, min_timelagged11, max_timelagged11)
print(type_pred11)
NNtypepred11 <- type_pred11[complete.cases(type_pred11)]
print(NNtypepred11)

val11ofNNtypepred = normalNN11[1]
val11ofNN11_lagged_data = NN11_lagged_datatesting[1]
errorNN11 <- (val11ofNN11_lagged_data - val11ofNNtypepred)
print(errorNN11)

cleanrmseNN11 <- errorNN11[complete.cases(errorNN11)]
rmse11 <- function(cleanrmse) {
  sqrt(mean(cleanrmse^2))
}
print(rmse11)
rmse_valueNN11 <- rmse11(cleanrmseNN11)
print(rmse_valueNN11)
rmse(NNtypepred11, cleanrmseNN11)


###MAE####
MAEDATANN11 <- data.frame(NN11cleaneddatatest[, c("G_previous2", "G_current", "G_pred1")])
print(MAEDATANN11)
model <- lm(G_current ~ G_previous2 + G_pred1, data = MAEDATANN11)
print(model)
MAENN11 <- mae(MAEDATANN11$G_current, predict(model))
print(MAENN11)
####


#MAPE

n_rows_cleanrmseNN11 <- length(cleanrmseNN11)
print(n_rows_cleanrmseNN11)
n_rows_NN11cleaneddata <- length(NN11_lagged_datatesting)
print(n_rows_NN11cleaneddata)
min_rowsNN11 <- min(n_rows_cleanrmseNN11, n_rows_NN11cleaneddata)
cleanrmseNN11_trimmed <- cleanrmseNN11[min_rowsNN11]
NN11cleaneddata_trimmed <- NN11_lagged_datatesting[min_rowsNN11]
dtaframeformapeNN11 = data.frame(At = c(cleanrmseNN11_trimmed),
                                 Ft = c(NN11cleaneddata_trimmed))
print(dtaframeformapeNN11)
mape = function(dtaframeformapeNN11) {
  resultNN11 = mean(abs((dtaframeformapeNN11$At - dtaframeformapeNN11$Ft)
                        / dtaframeformapeNN11$At)) * 100
  return(resultNN11)
}
resultNN11 = mape(dtaframeformapeNN11)
print(resultNN11)
####################

#SMAPE
NN11S <- function(cleanrmseNN11, NN11_lagged_datatesting) {
  return (1 / length(cleanrmseNN11) * sum(2 * abs(NN11_lagged_datatesting - cleanrmseNN11) /
                                            (abs(cleanrmseNN11) + abs(NN11_lagged_datatesting)) * 100))
}
SAMPENN11 = NN11S(cleanrmseNN11, NN11_lagged_datatesting)
print(SAMPENN11)

##################################################################################################
NN12_lagged_datatraing <- choosed_column_standardized[171:180]
print(NN12_lagged_datatraing)
NN12_lagged_datatesting <- choosed_column_standardized[292:302]
print(NN12_lagged_datatesting)
NN12datatrain <- bind_cols(G_previous2 = lag(traingdata, 4), G_current = lag(traingdata, 1), G_pred1 = traingdata)
NN12datatest <- bind_cols(G_previous2 = lag(testingdata, 4), G_current = lag(testingdata, 1), G_pred1 = testingdata)
nrow(NN12datatest)

NN12cleaneddatatrain <- NN12datatrain[complete.cases(NN12datatrain), ]
NN12cleaneddatatest <- NN12datatest[complete.cases(NN12datatest), ]
nrow(NN12cleaneddatatest)

autoreg_model12 <- neuralnet(G_pred1 ~ G_previous2 + G_current, data = NN12cleaneddatatrain, hidden = c(2), linear.output = TRUE)
plot(autoreg_model12)

NN12Res <- compute(autoreg_model12, NN12cleaneddatatest[1:3])
normalNN12 = NN12Res$net.result
print(normalNN12)
#############
unnormalize<-function(NN12_lagged_datatraing,min,max) {
  return((max-min)*NN12_lagged_datatraing+min)
}
max_NN12<-max(NN12_lagged_datatraing)
min_NN12 <- min(NN12_lagged_datatraing)
DenormtrainNN12 = unnormalize(normalNN12, min_NN12, max_NN12)
DenormtestNN12 = unnormalize(NN12cleaneddatatest, min_NN12, max_NN12)

SummaryofNN12 = cbind(DenormtrainNN12, DenormtestNN12)
view(SummaryofNN12)

max_timelagged12 <- max(NN12_lagged_datatesting)
min_timelagged12 <- min(NN12_lagged_datatesting)

unnormalize <- function(data, min, max) { return((max - min) * data + min) }
print(unnormalize)
type_pred12 <- unnormalize(predicted, min_timelagged12, max_timelagged12)
print(type_pred12)
NNtypepred12 <- type_pred12[complete.cases(type_pred12)]
print(NNtypepred12)

val12ofNNtypepred = normalNN12[1]
val12ofNN12_lagged_data = NN12_lagged_datatesting[1]
errorNN12 <- (val12ofNN12_lagged_data - val12ofNNtypepred)
print(errorNN12)

cleanrmseNN12 <- errorNN12[complete.cases(errorNN12)]
rmse12 <- function(cleanrmse) {
  sqrt(mean(cleanrmse^2))
}
print(rmse12)
rmse_valueNN12 <- rmse12(cleanrmseNN12)
print(rmse_valueNN12)
rmse(NNtypepred12, cleanrmseNN12)

###MAE####
MAEDATANN12 <- data.frame(NN12cleaneddatatest[, c("G_previous2", "G_current", "G_pred1")])
print(MAEDATANN12)
model <- lm(G_current ~ G_previous2 + G_pred1, data = MAEDATANN12)
print(model)
MAENN12 <- mae(MAEDATANN12$G_current, predict(model))
print(MAENN12)
####

#MAPE
n_rows_cleanrmseNN12 <- length(cleanrmseNN12)
print(n_rows_cleanrmseNN12)
n_rows_NN12cleaneddata <- length(NN12_lagged_datatesting)
print(n_rows_NN12cleaneddata)
min_rowsNN12 <- min(n_rows_cleanrmseNN12, n_rows_NN12cleaneddata)
cleanrmseNN12_trimmed <- cleanrmseNN12[min_rowsNN12]
NN12cleaneddata_trimmed <- NN12_lagged_datatesting[min_rowsNN12]
dtaframeformapeNN12 = data.frame(At = c(cleanrmseNN12_trimmed),
                                 Ft = c(NN12cleaneddata_trimmed))
print(dtaframeformapeNN12)
mape = function(dtaframeformapeNN12) {
  resultNN12 = mean(abs((dtaframeformapeNN12$At - dtaframeformapeNN12$Ft)
                        / dtaframeformapeNN12$At)) * 100
  return(resultNN12)
}
resultNN12 = mape(dtaframeformapeNN12)
print(resultNN12)
####################

#SMAPE
NN12S<-function(cleanrmseNN12,NN12_lagged_datatesting) {
  return (1 / length(cleanrmseNN12)*sum(2 * abs(NN12_lagged_datatesting - cleanrmseNN12) /
                                          (abs(cleanrmseNN12) + abs(NN12_lagged_datatesting)) * 100))
}
SAMPENN12 = NN12S(cleanrmseNN12, NN12_lagged_datatesting)
print(SAMPENN12)

############################################################################################################
###plotting_corect_NN##
length_pred<-length(normalNN12)
print(length_pred)
length_actual<-length(NN12_lagged_datatesting)
print(length_actual)

min_length<-min(length_pred, length_actual)
print(min_length)
NNtypepred12_trimmed<-normalNN12[1:min_length]
print(NNtypepred12_trimmed)
NN12_lagged_datatesting_trimmed<-NN12_lagged_datatesting[1:min_length]
print(NN12_lagged_datatesting_trimmed)

plot(NNtypepred12_trimmed,NN12_lagged_datatesting_trimmed,col='red',main='data vs neuralnetwork',pch=18,cex=0.7)
abline(a=0.5,b=2,col="blue" )

n =1:length(NN12_lagged_datatesting_trimmed)
plot(n,NN12_lagged_datatesting_trimmed,col="red",type="l",lwd=2,main="predictionofdatas")
lines(n,NNtypepred12_trimmed,col="blue",lwd=2)
legend("topright",legend=c("originalvalue","predictedvalue"),fill=c("red","blue"),col=2:2,adj=c(0, 0.6))
grid()
