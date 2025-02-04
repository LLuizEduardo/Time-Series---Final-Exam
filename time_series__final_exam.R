
# Final Exam - Time Series - Econometric I --------------------------------

# set directory
setwd('G:/Meu Drive/3� Sem/Econometria II/Final Exam')

library(Quandl)
library(xts)

# import key
key <- read.table(file = 'api_key.txt', header = FALSE)
Quandl.api_key(unlist(key))

# import data
gbp=Quandl( 'FRED/DEXUSUK' , type= 'ts' , collapse = 'monthly' , order= 'asc' )
aud=Quandl( 'FRED/DEXUSAL' , type= 'ts' , collapse = 'monthly' , order= 'asc' )

#####Creating a New Matrix with both series#############
data=cbind(gbp, aud)
#########Computing the compounded return###############
data_er_ret=diff(log(data))
############Excluding the first observation in the prices######
data_er=ts(data[2:nrow(data),],start=c(1971,2),frequency=12)
##########Time series names#####################
names_data_er=colnames(data_er)
#########Time series date#######################
date=seq(as.Date('1971-02-01'), by= 'month', along=data_er_ret[,1])
######Plot Exchangerate
plot(data_er)
#######Plot returns###########
plot(data_er_ret)


# unit root test
library(tseries)

testList <- cbind(data_er, data_er_ret)
name_testList <- colnames(testList)
Results.adf<- matrix(ncol = 1, nrow = length(name_testList))

for (i in 1:length(name_testList)) {
  aTem <- suppressWarnings(tseries::adf.test(testList[,i]))
  if (aTem$p.value < 0.05) {
    Results.adf[i,1] <- paste0( name_testList[i],
    ' is a Stationary process because it has P-valor ',
    round(aTem$p.value,2),'%')
  }else{
    Results.adf[i,1] <- paste0( name_testList[i],
    ' is a Non-stationary process because it has P-valor ',
    round(aTem$p.value,2), '%')
  }
}
View(Results.adf)
rm(aTem, aud, gbp, testList)

# lags select - var criteria
library(vars)
varModel<-VARselect(data_er_ret, type = 'const', lag.max = 12)
nlag<- min(varModel$selection[1],varModel$selection[3])

# testing cointegration via regression
# reg<-lm(gbp ~ aud, data_er_ret)
# plot.ts(reg$residuals)
# adf.test(reg$residuals)
# if p-valor < 0.01 residuals are stationary. so it means the series are cointegrated

# estimate VECM model and testing whether the series are cointegrated
if (nlag < 2) {K <- 2 # adjusting the lags for the VECM model
}else{                # K cannot be equal to 1
  K<- nlag
} 

library(tsDyn)
vecmModel<-VECM(data_er_ret, lag = K-1, estim = 'ML')
summary(vecmModel)

cointTest<-tsDyn::rank.test(vecmModel, cval = 0.05)
summary(cointTest)

# other test
#coint<-ca.jo(data_er_ret)
#summary(coint)


# forecast - one spep forward
h=1
t<-120
T<-length(data_er_ret[,1])-t

forecastRet<-matrix(ncol=4, nrow = length(data_er_ret[,1]))
forecastRet[,1:2]<-data_er_ret
#forecastRet[1:length(data_er_ret[,1])+h,1:2]<-data_er_ret[1:length(data_er_ret[,1]),1:2]
# use the previous line if adding an out-of-sample forecast

for (i in 1:T) {
  forecastRet[t+i,3:4]<-predict(VECM(data_er_ret[1:t+i,],
                                     lag = K-1, estim = 'ML'), n.ahead = h)
}

colnames(forecastRet)<- c('gbpRet', 'audRet', 'gbpRet h+1','audRet h+1')
forecastRet<-ts(forecastRet)
forecastRet<-as.xts(forecastRet)
forecastRetTemp<-forecastRet
forecastRetTemp[,3]<-forecastRet[,2]
forecastRetTemp[,2]<-forecastRet[,3]

forecastRet<-forecastRetTemp
colnames(forecastRet)<- c('gbpRet', 'gbpRet h+1', 'audRet', 'audRet h+1')
rm(forecastRetTemp)
index(forecastRet)<-date

par(mfrow=c(1,2))
plot.xts(forecastRet[t:T,1:2], grid.col = FALSE, main ='', legend.loc = 3)
plot.xts(forecastRet[t:T,3:4], grid.col = FALSE, main ='', legend.loc = 3)



#   decision

decision<-matrix(ncol = 8, nrow = T)
decision[1,1:4]<-1 # recovery initial prices and set projection prices
forecastRetTemp<-na.omit(forecastRet)

for (i in 2:T) {
  decision[i,1:4]<-decision[i-1,1:4]*(1+forecastRetTemp[i,])
}

colnames(decision)<- c('gbp', 'gbp h+1', 'aud', 'aud h+1',
                       'gbp decision 1','aud decision 1',
                       'gbp decision 2','aud decision 2')


for (i in 2:T) {
  if (decision[i-1,1]<decision[i,2]) {
    decision[i,5]<-1
  }else if(decision[i-1,1]>decision[i,2]){
    decision[i,5]<--1
  }else{
    decision[i,5]<-0 
  }
  
  if (decision[i-1,3]<decision[i,4]) {
    decision[i,6]<-1
  }else if(decision[i-1,3]>decision[i,4]){
    decision[i,6]<--1
  }else{
    decision[i,6]<-0 
  }
}


# determining ratio of assets
library(TTR)
ratio<-data_er[,1]/data_er[,2]
ratio<-cbind(ratio, SMA(ratio, n=5))
ratio<-as.xts(ts(ratio))
colnames(ratio)<-c('ratio','MAratio')
index(ratio)<-date
plot.xts(ratio, legend.loc = 1, grid.col = FALSE)


for (i in 2:T) {
  if (decision[i-1,1]<decision[i,2] & ratio[t+i,1]>ratio[t+i,2]){#
    decision[i,7]<-1
  }else if(decision[i-1,1]>decision[i,2] & ratio[t+i,1]<ratio[t+i,2]){#
    decision[i,7]<--1
  }else{
    decision[i,7]<-0 
  }
  
  if (decision[i-1,3]<decision[i,4] & ratio[t+i,1]<ratio[t+i,2]) {#
    decision[i,8]<-1
  }else if(decision[i-1,3]>decision[i,4] & ratio[t+i,1]>ratio[t+i,2]){#
    decision[i,8]<--1
  }else{
    decision[i,8]<-0 
  }
}

plot.ts(decision[,5:8])



# calculated return

rStar<- cbind(decision[,5]*forecastRetTemp[,1],
              decision[,6]*forecastRetTemp[,3],
              decision[,7]*forecastRetTemp[,1],
              decision[,8]*forecastRetTemp[,3])

colnames(rStar)<- c('rStar_gbp_decision1','rStar_aud_decision1',
                    'rStar_gbp_decision2','rStar_aud_decision2')

par(mfrow=c(1,2))
plot(rStar[,1:2], main = 'decision 1')
plot(rStar[,3:4], main = 'decision 2')



# accumulated return

initialInvestment<-1

accumulatedReturn<-matrix(nrow = T, ncol = 6)
accumulatedReturn[1,1:4]<-initialInvestment

for (i in 2:T) {
 accumulatedReturn[i,1:2]<-accumulatedReturn[i-1,1:2]*(1+rStar[i,1:2])
 accumulatedReturn[i,3:4]<-accumulatedReturn[i-1,3:4]*(1+rStar[i,3:4])
 }
accumulatedReturn[,5]<-accumulatedReturn[,1]+accumulatedReturn[,2]
accumulatedReturn[,6]<-accumulatedReturn[,3]+accumulatedReturn[,4]

colnames(accumulatedReturn)<-c('AcRet gbp d1','AcRet aud d1',
                               'AcRet gbp d2','AcumRet aud d2',
                               'Total Return d1', 'Total Return d2')

accumulatedReturnTemp<-as.xts(ts(accumulatedReturn))
index(accumulatedReturnTemp)<-date[1:T]

accumulatedReturnTemp[,2]<-accumulatedReturn[,3]
accumulatedReturnTemp[,3]<-accumulatedReturn[,2]

colnames(accumulatedReturnTemp)<-c('AcRet gbp d1','AcRet gbp d2',
                                   'AcRet aud d1','AcRet aud d2',
                                   'Total Return d1', 'Total Return d2')

par(mfrow=c(1,3))
plot.xts(accumulatedReturnTemp[,1:2], grid.col = FALSE, main ='', legend.loc = 1)
plot.xts(accumulatedReturnTemp[,3:4], grid.col = FALSE, main ='', legend.loc = 1)
plot.xts(accumulatedReturnTemp[,5:6], grid.col = FALSE, main ='', legend.loc = 1)


#-----------------------------end code------------------------------------------
