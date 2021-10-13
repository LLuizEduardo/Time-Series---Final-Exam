
# set directory
setwd('G:/Meu Drive/3º Sem/Econometria II/Final Exam')

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
    ' is a Stationary process with probability ', round(1-aTem$p.value,2)*100,'%')
  }else{
    Results.adf[i,1] <- paste0( name_testList[i],
    ' is a Non-stationary process because it has P-valor ', round(aTem$p.value,2), ' >5%')
  }
}

rm(aTem)

# lags select - var criteria
library(vars)
varModel<-VARselect(data_er_ret, type = 'const', lag.max = 12)
nlag<- min(varModel$selection[1],varModel$selection[3])

# testing cointegration via regression
reg<-lm(gbp ~ aud, data_er_ret)
plot.ts(reg$residuals)
adf.test(reg$residuals) #p-valor < 0.01 os residuos sao estacionarios. logo as series sao cointegradas


# estimate VECM model and testing whether the series are cointegrated
if (nlag < 2) {K <- 2
}else{
  K<- nlag
}

library(tsDyn)
o.VECM<-VECM(data_er_ret, lag = K-1, estim = 'ML')
summary(o.VECM)

a<-tsDyn::rank.test(o.VECM, cval = 0.05)
summary(a)

# other test
#coint<-ca.jo(data_er_ret)
#summary(coint)

# forecast - one spep forward

h=1
t<-120
T<-length(data_er_ret[,1])-t


for (i in 1:T) {
  previsao[t+i,3:4]<-predict(VECM(data_er_ret[1:t+i,], lag = K-1, estim = 'ML'), n.ahead = h)
}


previsao1<-ts(previsao)

plot.ts(previsao1)
previsao1<-as.xts(previsao1)
previsao2[,3]<-previsao1[,2]
plot.xts(previsao2[t:T,3:4])


acum<-matrix(ncol = 2, nrow = t+T)
acum[1,]<-1
for (i in 2:609) {
  acum[i,]<-acum[i-1,]*(1+data_er_ret[i,])
}

View(acum)

acum1<-as.xts(ts(acum))
plot.xts(acum1)


for (i in 1:T) {
  acum2[i,]<-acum[i-1,]*(1+data_er_ret[i,])
}


#   -----------------------------------------------------------------------

previsao1

decision<-matrix(ncol = 6, nrow = t+T)
decision[1,1:2]<-1
for (i in 2:609) {
  decision[i,1:2]<-acum[i-1,]*(1+data_er_ret[i,])
  
}


#   -----------------------------------------------------------------------


if (p1>p0) {
  
}else if(p1<p0){
  
}else{
  
}




#   -----------------------------------------------------------------------




p1<-predict(VECM(data_er_ret[1:t+i,], lag = K-1, estim = 'ML'), n.ahead = h)

if (p1>p0) {
  
}else if(p1<p0){
  
}else{
  
}





previsao<-matrix(ncol=4, nrow = length(data_er_ret[,1])+h)

previsao[1:length(data_er_ret[,1]),1:2]<-data_er_ret[1:length(data_er_ret[,1]),1:2]

