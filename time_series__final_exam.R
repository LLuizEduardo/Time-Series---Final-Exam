
setwd('G:/Meu Drive/3º Sem/Econometria II/atividade2')

key <- read.table(file = 'api_key.txt', header = FALSE)

library(Quandl)
library(xts)

Quandl.api_key(unlist(key))

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
date=seq(as.Date('1971−02−01'), by= 'month', along=data_er_ret[,1])
######Plot Exchangerate
plot(data_er)
#######Plot returns###########
plot(data_er_ret)

#
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

library(vars)
varModel<-VARselect(data_er_ret, type = 'const', lag.max = 12)
nlag<- min(varModel$selection[1],varModel$selection[3])

if (nlag < 2) {K <- 2
}else{
  K<- nlag
}

library(tsDyn)
o.VECM<-VECM(data_er_ret, lag = K-1, estim = 'ML')
summary(o.VECM)

a<-tsDyn::rank.test(o.VECM, cval = 0.05)
summary(a)

