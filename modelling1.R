if (!require("pacman")) install.packages("pacman")
pacman::p_load("readxl","stringr","lubridate","data.table","zoo","xts","dplyr","magrittr","forecast","doParallel","glmnet","Matrix")
# timezone <- "EST"
# timezone <- 
  dayFormat <- "%Y%m%d"

# read holidays---
dummies <- read_excel("Dummy_variables_v2.xlsx")
dummies <- as.data.table(dummies)
xts(dummies[,-"time_id",with=F],order.by = dummies[,strptime(time_id,dayFormat)])
dummiesSeries <- xts(
  dummies[,-(1:2),with=F]
  ,order.by = dummies[,date])

martaEncoding <- "[A-X].*_*[A-X].*"

# all that are holidays- cluster holidays later to improve accuracy
#summarise all holidays- kind of crude, better to split by religious/national and by apriori "impact
# e.g. easter, christmass, new year, 3rd march etc national ones are stronger than rest but meh
goodCols <- setdiff(grep(martaEncoding,colnames(dummies),value = T),c("Speedy_Working_Day","Working_Saturday"))
goodCols

# grep("[A-X].*_*[A-X].*","Asdkf_Balsdfj")
# create the "is the day any sort of holiday" variable---------------
dummies[,isAnyHoliday:=apply(.SD,1,max),.SDcols= goodCols ]# since 0 and 1 max works here

# dummies[,dtime:=strptime(time_id,dayFormat,timezone)]

nrow(dummies)
# grep(hubPattern,r)
speedy <- read_excel("sectors.xlsx") %>% as.data.table()

speedy[, time_id:=as.character(time_id) ]
speedy[,
       levels(factor(origin))
       ]

hubPattern <-  "(\\d{1,2}\\-\\d{1,2})"
extractHub <- function(st) {
  # st <- speedy$origin[1]
  ind <- str_locate(st,hubPattern)
  if(length(ind)==0)0
  as.integer(paste0(str_sub(st,1,1),str_sub(st,3,ind[2])))
}
#create ids from the UTF- formatted names------------
#SLOW! HOW TO MAKE FASTER??
origins <- sapply(speedy$origin, extractHub)
destinations <- sapply(speedy$destination, extractHub)
speedy[,org:=origins]
speedy[,dst:=destinations]
speedy[,total:=express+economy]

speedy$org %>% class=="integer"

# speedy[,extractHub(origins)] %>% table()
# sanity check if all sectors appear for all dates ------------------------
table(speedy[,.N,by=time_id]$N)

speedyWithHolidays <- merge(speedy,dummies,by="time_id",all.x = TRUE,all.y = FALSE)

# ups!!!- sanity check fails!!! (not by much though)!!!!!!!!!!!!- most still 0s, am i wrong?-------
speedyWithHolidays[Speedy_Working_Day==0,summary(economy)]
speedyWithHolidays[Speedy_Working_Day==0,table(economy)]
speedyWithHolidays[Speedy_Working_Day==0,table(express)]


# exploratory sometime it happened they worked- maybe just 2 days?----
holidaysWrong <- speedyWithHolidays[Speedy_Working_Day==0&(economy>0|express>0),.(time_id,origin,destination,express,economy)]
holidaysWrong



# null speedy non-working days (comment out if nessesary) -----------------
speedyWithHolidays[,express:=ifelse(Speedy_Working_Day>0,express,0)]
speedyWithHolidays[,economy:=ifelse(Speedy_Working_Day>0,economy,0)]





# speedyzoo <- xts(speedy[,.(origin,destination,express,)]) 
# speedyCast <- dcast.data.table(speedy,time_id+economy+express~origin+destination,value.var = c("economy","express"))[order(time_id)]
# speedyCast <- dcast.data.table(speedy,time_id~org+dst,value.var = c("economy","express"))[order(time_id)]
# speedyCast <- dcast.data.table(
#   speedyWithHolidays,time_id~origin+
    # destination,value.var = c("economy","express"),fun.aggregate = list(sum,sum))[order(time_id)]




# convert from long to wide format ----------------------------------------
speedyCast <- dcast.data.table(
  speedyWithHolidays,time_id~org + dst,value.var = c("economy","express","total"),
  fun.aggregate = list(sum,sum,sum))[order(time_id)]


# create the aggregated data ----------------------------------------------
speedyAgg <- speedy[,.(time_id,totalEcon=sum(economy),totalExpress=sum(express),total=sum(total)),by=time_id]

speedyAggByOrigin <- speedy[,.(time_id,
                               org,
                               total=sum(total),
                               totalEcon=sum(economy),
                               totalExpress=sum(express)),by=.(time_id,org)]

speedyAggByDestination <- speedy[,.(time_id,
                                    dst, 
                                    total=sum(total),
                                    totalEcon=sum(economy),
                                    totalExpress=sum(express)),by=.(time_id,dst)]

setkey(speedyAgg,"time_id")

#exploratory
speedyCast[1:30,1:3,with=F]
colnames(speedyCast)
nrow(speedyCast)


# check -------------------------------------------------------------------
length(speedyCast$time_id)==length(unique(speedyCast$time_id))
class(speedyCast$time_id)==class(dummies$time_id)

setkey(speedyCast,"time_id")
setkey(dummies,"time_id")




# merge with the holidays data, and then add the total daily volumes as well -------------------------------------------------------------------
speedyCast <- merge(speedyCast,dummies,all.x = T)
speedyCast <- speedyCast[speedyAgg]

speedyCast[1:30,.(time_id,totalEcon,totalExpress)]
# speedyCast1

colnames(speedyCast)


# sanity checks/tests ------------------------------------------------------------
all.equal(sort(unique(speedyCast$time_id)),sort(unique(speedy$time_id)))

speedyCast[100:140,1:3,with=F]
speedy[order(time_id),][160:210,economy]
options(encoding="UTF-8")
# speedyCast[,summary("economy.1_2-1 ПЛОВДИВ_2-4 ВАРН),with=F]
# speedyCast$
 # speedyCast$economy.1_2-1 П2-1 ПЛОВДИ_2-10 РУСЕ"
# speedyCast$"economy.1_2-1 ПЛОВДИВ_2-4 ВАРНА"
# speedyCast$"economy_2-1 ПЛОВДИВ_2-4 ВАРНА"

# NB! select sector here------------------------
targetOrigin <- 22#Sofia
targetDest <- 26#Varna



colnames(speedyCast)

speedyCast[6:10,1:5,with=F]

# explore plovdiv-varna ---------------------------------------------------

colnames(speedyCast)

# select which series to predict-econ, express or total, here -------------
expressOrEcon <- "express_sum"
# expressOrEcon <- "economy_sum"
# expressOrEcon <- "total_sum"
targetVolumedStable <- 
  speedyCast[order(time_id)][,paste0(expressOrEcon,"_",as.character(targetOrigin),"_",as.character(targetDest)),with=F]

#check
testpv <-  speedyCast[order(time_id)][,
                                  paste0("express_sum","_",as.character(targetOrigin),"_",as.character(targetDest))
                                  ,with=F
                                  ]

  (targetVolumedStable-testpv)[1:30]
targetOrigin
# targetVolumeStable <- speedy[order(time_id)][targetOrigin==org&targetDest==dst,economy]
targetVolumeStable[1:30]

#24 may i 6 sept are on same date and bridges are all same always, same as 3.3 and 22.9 todo



tsSeries <- xts(cbind(
  # speedyCast$"economy_2-2 СОФИЯ_2-1 ПЛОВДИВ",
  # speedyCast$"economy_sum_2-4 ВАРНА_2-1 ПЛОВДИВ",
  targetVolumedStable,
  speedyCast[,.(
    isAnyHoliday ,
    Christmas_Day,
    Christmas_Eve,
    Speedy_Working_Day
    , Monday_Holiday
    , Thursday_Holiday, Wednesday_Holiday, 
    Thursday_Holiday,Friday_Holiday,Saturday_Holiday,Sunday_Holiday 
    ,Working_Saturday #and whatever else
                )]),strptime(speedyCast$time_id,dayFormat))
tsSeries[,-1]


# add opposite direction as predictor ----------
oppositeVolumesData <- speedyCast[order(time_id)][,
                                  paste0(expressOrEcon,"_",as.character(targetDest),"_",as.character(targetOrigin))
                                  ,with=F
                                  ]
# speedyAggByOrigin[]

tsSeriesAggregated <-lag.xts(
  xts(
  cbind(
    speedyCast[order(time_id)][,.(totalEcon,totalExpress)]
    ,totalOrigin=speedyAggByOrigin[order(time_id)][org==targetOrigin,.(totExpOrigin=totalExpress,totEconOrigin=totalEcon)],
            totalDest=speedyAggByDestination[order(time_id)][dst==targetDest,.(totEconOrigin=totalExpress,totEconDest=totalEcon)],
    oppositeVolumesData# remove if desired
  )
      , strptime(speedyCast[order(time_id)]$time_id,dayFormat)) ,1) %>% na.fill(0)


# plot.xts(xtsSeries["2015/"])
y <- tsSeries[,1]


# here an tacit assumption is that our series has the same index/size as whatever we wish to predict
# fit <- ets(y)
# plot(fit)
# fc <- forecast(fit)
# plot(fc)


# havent looked too closely here ------------------------------------------
# z <- fourier(ts(x, frequency=365.25), K=5)
# zf <- fourierf(ts(x, frequency=365.25), K=5, h=100)
# fit <- auto.arima(y, xreg=cbind(z,holiday), seasonal=FALSE)
# fit <- auto.arima(y,xreg = tsSeries[,2:3],seasonal = TRUE)
# summary(fit)
# fc <- forecast(fit, xreg=cbind(zf,holidayf), h=100)
# 
# taylor.fit <- msts(y,seasonal.periods = c(7,30,365))
# plot(taylor.fit)
# summary(taylor.fit)
# tf <- tbats(taylor.fit)
# 
# taylorfttt <- forecast(tf)
# plot(taylorfttt)

# embed(
#   tsSeries["2013-12-29/2015-10-01",2:3] #%>% nrow
#   ,3)# %>% 
#   nrow

# functions to construct the time-based lags of what we're predicting----------------
getLastWeek <- function(x) {
  # x <- 10
  currInd <- index(y[x]) 
  dayweekind <- .indexwday(y[x]) 
  indx <- which((.index(y)<currInd)&(.indexwday(y)==dayweekind))
  if (length(indx)==0) {
    NA
  }else{
    as.numeric(y[last(indx)])
  }}


# getLastWeek(1)
# getLastWeek(90)
# getLastWeekSeriesData <- sapply(1:nrow(y),getLastWeek)
# lastWeekSeries <- xts(getLastWeekSeriesData,order.by = index(y),whc=1)

getLastMonth <- function(x) {
  currInd <- .index(y[x]) 
  dayMonthIdx <- .indexmday(y[x]) 
  indx <- which((.index(y)<currInd)&(.indexmday(y)==dayMonthIdx))
  if (length(indx)<1) {
    NA
  }else{
    as.numeric(y[ last(indx)])
  }}

getLastMonthSeriesData <- sapply(1:nrow(y),getLastMonth)
lastMonthSeries <- xts(getLastMonthSeriesData,order.by = index(y))



#same date in year
getLastYear <- function(x) {
  # x <- 500
  currInd <-index(y)[x]
  dayMonthIdx <- .indexmday(y[x]) 
  monthIdx <- .indexmon(y[x]) 
  indx <- which((.index(y)<currInd)&(.indexmday(y)==dayMonthIdx)&(.indexmon(y)==monthIdx))
  if (length(indx)==0) {
    NA
  }else{
    as.numeric(y[last(indx)])
  }}

getLastYearSeriesData <- sapply(1:nrow(y),getLastYear)
lastYearSameDaySeries <- xts(getLastYearSeriesData,order.by = index(y))
lastYearSameDaySeries



# sapply(1:nrow(y),function(x) {
#  currInd <- index(y[x]) 
#  dayweekind <- .indexwday(y[x]) 
#  indx <- which((.index(y)<currInd)&(.indexwday(y)==dayweekind))
#  if (length(indx)==0) {
#    0.0
#  }else{
#    y[last(indx)]
#  }
# })

# constructing the 6 weekly lags+1 monthly +1 yearly, 0 NA fill -----------
constructAdditionalData <- function(baseSeries=y){
  # can simply do this, cause we rely on having daily observations for each sector, and all weeks have 7 days (doh:P)
  weeklyLags <- do.call(merge.xts,
                        lapply(1:7,function(i){
                          ijj <- 7*i
                          lag(baseSeries,ijj)
                        })
  ) 
  
  getLastMonthSeriesData <- sapply(1:nrow(baseSeries),getLastMonth)
  lastMonthSeries <- xts(getLastMonthSeriesData,order.by = index(baseSeries))
  
  getLastYearSeriesData <- sapply(1:nrow(baseSeries),getLastYear)
  lastYearSameDaySeries <- xts(getLastYearSeriesData,order.by = index(baseSeries))
  merge.xts(lastMonthSeries,weeklyLags,lastYearSameDaySeries) %>% na.fill(0)
  # lastMonthSeries %>% na.fill(0)
}

additionalYBasedData <- constructAdditionalData(y)
#check
(index(additionalYBasedData)==index(y) )%>% all()

# change periods here------------------
ytrainingPeriod <- "2013-10-30/2014-08-17"
ytestingPeriod <- "2014-08-18/2014-10-10"
#NB!!
#construct a TS class that is suitable for Arima to detect seasonality on, 
# though it breaks the real time indices
yhat <- ts(coredata(tsSeries[ytrainingPeriod,1]),start=2013,frequency = 7)
# yhat <- tsSeries[ytrainingPeriod,1]
yhattest <- tsSeries[ytestingPeriod,1]
# mod <- auto.arima(y["2014-01-01/2015-10-01"],xreg = embed(tsSeries["2013-12-29/2015-10-01",2:3],4),seasonal = T)
nrow(yhat)+nrow(yhattest)-nrow(tsSeries)




# todo- make this simpler using datetime arithmetics, rather than doing it always by hand
#periods mismatch due to the embedding

holidayPeriod <- "2013-10-28/2014-08-19"
holidayPeriodTest <- "2014-08-16/2014-10-12"
embedLen <- 5
holidayRelated <- embed(tsSeries[holidayPeriod,-1],embedLen)
holidayRelatedTest <-  embed(tsSeries[holidayPeriodTest,-1],embedLen)
nrow(holidayRelated)+nrow(holidayRelatedTest)-nrow(tsSeries)


indexTZ(additionalYBasedData) = Sys.getenv("TZ")# timezone bug on next lines without this
additionalYBasedDataTrain <- additionalYBasedData[ytrainingPeriod]
additionalYBasedDataTest <- additionalYBasedData[ytestingPeriod]


nrow(additionalYBasedDataTest)==nrow(yhattest)
nrow(holidayRelatedTest)==nrow(yhattest)

# add overall volume-past 2 days ------------------------------------------------------

# aggregateStatsPeriod<-"2015-02-01/2015-09-17"
# aggregateStatsTestPeriod<-"2015-09-18/2015-10-14"
aggregateStats2lags <- (
  merge.xts(tsSeriesAggregated,
                                 merge.xts( 
                                  lag.xts(tsSeriesAggregated,1),
                                  lag.xts(tsSeriesAggregated,2),
                                  lag.xts(tsSeriesAggregated,3),
                                  lag.xts(tsSeriesAggregated,4),
                                  lag.xts(tsSeriesAggregated,5)
                                  )
                                 
) %>% na.fill(0))#todo explore other na.fill optionhs, such as na.locf

aggregateStatsTrain <- aggregateStats2lags[ytrainingPeriod,]
aggregateStatsTest <- aggregateStats2lags[ytestingPeriod,]

# must be true
nrow(aggregateStatsTest)==nrow(yhattest)
nrow(holidayRelatedTest)==nrow(yhattest)

nrow(aggregateStatsTrain)+nrow(aggregateStatsTest)-nrow(tsSeries)
nrow(additionalYBasedDataTrain)==nrow(yhat)
nrow(aggregateStatsTrain) ==nrow(yhat)
nrow(holidayRelated)==nrow(yhat)


# regressorTable <- cbind( coredata(holidayRelated),coredata(aggregateStatsTrain))
# regressorTabletest <- cbind(coredata(holidayRelatedTest),coredata(aggregateStatsTest))

# create design matrices --------------------------------------------------
regressorTable <- cbind( 
  coredata(additionalYBasedDataTrain)
  ,          coredata(holidayRelated)
  ,coredata(aggregateStatsTrain)
)

regressorTabletest <- cbind(
  coredata(additionalYBasedDataTest)# ,
  ,coredata(holidayRelatedTest)
  ,coredata(aggregateStatsTest)
)

#checks
ncol(regressorTable)==ncol(regressorTabletest)
# nrow(regressorTable)
# nrow(regressorTabletest)
# ncol(regressorTabletest)
# ncol(regressorTabletest)
# regressorTable <- jitter(regressorTable,1e-6) 


toKeep <- !apply(regressorTable==0,2,all)
regressorTable <- regressorTable[,toKeep]
regressorTabletest <- regressorTabletest[,toKeep]

B <- pracma::rref(regressorTable)
B

getLIRows <- apply(B, 1, function(x){o<- which(x==1)
if (length(o)>0) {
  o[1]                    
}else{
  0
}}) %>% setdiff(0)

getLIRows
regressorTable <- regressorTable[,getLIRows]
regressorTabletest <- regressorTabletest[,getLIRows]


dim(regressorTable)
dim(regressorTabletest)
length(yhat)

#getLIRows
# LIRows <- colnames(regressorTable)[getLIRows[getLIRows!=0]] %>%intersect(
#   colnames(numericData)[sapply(colnames(numericData), function(x)(!min(numericData[[x]]==max(numericData[[x]]))))]
# )
# LIR 
#chooseWhich <-which(getLIRows==1:ncol(transformedDataFrame))
#chooseWhich
# colnames(transformedDataFrame)[LIRows]
# numericColumnsToDeleteDataInd <- which()

# main call to the model.-------------------
# sometimes this breaks, but the commented call bellow doesn't . use the commented if so
mod <- auto.arima(yhat,xreg = regressorTable,seasonal = T,ic="aic",test="adf",allowmean = TRUE,allowdrift = TRUE,
                  max.p=5,max.q = 5,max.d=5,max.D=6,max.P=5,max.Q = 8)
mod <- auto.arima(yhat,xreg = regressorTable,seasonal = T,allowmean = TRUE,allowdrift = TRUE)
summary(mod)
# fct <- forecast(mod,xreg=embed(tsSeries['2015-08-29/',2:3],4))

fct <- forecast(mod,xreg=regressorTabletest)
# summary(fct)
plot(resid(fct))


regdf <- as.data.table(regressorTable)
regdftst <- as.data.table(regressorTabletest)



# matplot(cbind(coredata(fct$mean),coredata(yhattest),coredata(fct$upper[,2])
#               # ,fct$upper[,1]
# ),type = "l",col = c("blue","red","green"))
# matplot(cbind(coredata(fct$mean),coredata(yhattest)
#               # ,fct$upper[,1]
# ),type = "l",col = c("blue","red"))
# auto.arima()
# plot(fct$mean-coredata(yhattest))
# plot(density(fct$mean-coredata(yhattest)))
# plot(abs(fct$mean/coredata(yhattest)),ylim=c(0,1))
pred <- pmax(0,fct$mean)

matplot(cbind(pred,coredata(yhattest)
              # ,fct$upper[,1]
),type = "l",col = c("blue","red"))

err <- sum(abs((pred)-coredata(yhattest)))/sum(yhattest)
err

# normality check ---
tseries::jarque.bera.test(pred-coredata(yhattest))
tseries::jarque.bera.test(pred-coredata(yhattest))
plot(density(resid(fct)))
plot(resid(fct))
pacf(resid(fct))
acf(resid(fct))
acf(yhat)

expressVars <- grep("express",colnames(regdf),value = T)[1:6]
continteractionsString<-paste("~.+(",paste(expressVars,collapse="+"),")^2")#,"-",paste(expressVars,collapse="+"))
continteractionsString

# mmatr <- sparse.model.matrix(as.formula(continteractionsString),regdf)
# mmatrtest <- sparse.model.matrix(as.formula(continteractionsString),regdftst)
mmatr <- regressorTable
mmatrtest <- regressorTabletest


newy <- resid(fct)
# doParallel::stopImplicitCluster()
# doParallel::registerDoParallel(2)
# fit1newest <-cv.glmnet(mmatr,newy,standardize=T,alpha=0.95,family="gaussian",
#                        maxit=300000)


fit1newest <-gbm.fit(mmatr,newy,distribution="tdist",n.trees = 100)

predictions <- as.vector(predict(fit1newest,mmatrtest,n.trees = 100))
newpred <-pmax(0,pred+predictions) 

  
  

matplot(cbind(newpred,coredata(yhattest)
              # ,fct$upper[,1]
),type = "l",col = c("blue","red"))

err1 <- sum(abs((newpred)-coredata(yhattest)))/sum(yhattest)
err1
err
plot(newpred-yhattest)

tseries::jarque.bera.test(newpred-yhattest)
plot(density(newpred-yhattest))
plot(newpred-yhattest)
pacf(newpred-yhattest)
acf(newpred-yhattest)
