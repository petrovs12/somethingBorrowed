if (!require("pacman")) install.packages("pacman")
pacman::p_load("readxl","stringr","lubridate","data.table","zoo","xts","dplyr","magrittr","forecast")
# timezone <- "EST"
timezone <- 
  dayFormat <- "%Y%m%d"

dummies <- read_excel("Dummy_variables_v2.xlsx")
dummies <- as.data.table(dummies)
xts(dummies[,-"time_id",with=F],order.by = dummies[,strptime(time_id,dayFormat)])
dummiesSeries <- xts(
  dummies[,-(1:2),with=F]
  ,order.by = dummies[,date])

martaEncoding <- "[A-X].*_*[A-X].*"
goodCols <- setdiff(grep(martaEncoding,colnames(dummies),value = T),c("Speedy_Working_Day","Working_Holiday"))

grep("[A-X].*_*[A-X].*","Asdkf_Balsdfj")

#summarise all holidays- kind of crude, better to split by religious/national and by apriori "impact
# e.g. easter, christmass, new year, 3rd march etc national ones are stronger than rest but meh
dummies[,isAnyHoliday:=apply(.SD,1,max),.SDcols= goodCols ]# since 0 and 1 max works here

# dummies[,dtime:=strptime(time_id,dayFormat,timezone)]

nrow(dummies)
grep(hubPattern,r)
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
#SLOW! HOW TO MAKE FASTER??------------
origins <- sapply(speedy$origin, extractHub)
destinations <- sapply(speedy$destination, extractHub)
speedy[,org:=origins]
speedy[,dst:=destinations]
speedy[,total:=express+economy]

speedy$org %>% class

# speedy[,extractHub(origins)] %>% table()
# sanity check if all sectors appear for all dates ------------------------
table(speedy[,.N,by=time_id]$N)

speedyWithHolidays <- merge(speedy,dummies,by="time_id",all.x = TRUE,all.y = FALSE)
speedyWithHolidays[Speedy_Working_Day==0,summary(economy)]

# ups!!!- sanity check fails!!! (not by much though)!!!!!!!!!!!!- most still 0s, am i wrong?
speedyWithHolidays[Speedy_Working_Day==0,table(economy)]
speedyWithHolidays[Speedy_Working_Day==0,table(express)]

# sometime it happened they worked- maybe just 2 days?
holidaysWrong <- speedyWithHolidays[Speedy_Working_Day==0&(economy>0|express>0),.(time_id,origin,destination,express,economy)]
holidaysWrong <- speedyWithHolidays[Speedy_Working_Day==0&(economy>0|express>0),.(time_id,origin,destination,express,economy)]



# null speedy non-working days (comment out if nessesary) -----------------
speedyWithHolidays[,express:=ifelse(Speedy_Working_Day>0,express,0)]
speedyWithHolidays[,economy:=ifelse(Speedy_Working_Day>0,economy,0)]





# speedyzoo <- xts(speedy[,.(origin,destination,express,)]) 
# speedyCast <- dcast.data.table(speedy,time_id+economy+express~origin+destination,value.var = c("economy","express"))[order(time_id)]
# speedyCast <- dcast.data.table(speedy,time_id~org+dst,value.var = c("economy","express"))[order(time_id)]
# speedyCast <- dcast.data.table(
#   speedyWithHolidays,time_id~origin+
    # destination,value.var = c("economy","express"),fun.aggregate = list(sum,sum))[order(time_id)]
speedyCast <- dcast.data.table(speedyWithHolidays,time_id~org + dst,value.var = c("economy","express","total"),fun.aggregate = list(sum,sum,sum))[order(time_id)]

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

speedyAgg[,tot:=totalEcon+totalExpress]
speedyAggByOrigin[,tot:=totalEcon+totalExpress]
speedyAggByDestination[,tot:=totalEcon+totalExpress]


setkey(speedyAgg,"time_id")

speedyCast[1:30,1:3,with=F]
colnames(speedyCast)
nrow(speedyCast)


# check -------------------------------------------------------------------
length(speedyCast$time_id)==length(unique(speedyCast$time_id))

setkey(speedyCast,"time_id")
setkey(dummies,"time_id")
class(speedyCast$time_id)==class(dummies$time_id)


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
speedyCast[,summary(`economy.1_2-1 ПЛОВДИВ_2-4 ВАРНА`),with=F]
# speedyCast$"economy.1_2-1 П2-1 ПЛОВДИ_2-10 РУСЕ"
# speedyCast$"economy.1_2-1 ПЛОВДИВ_2-4 ВАРНА"
# speedyCast$"economy_2-1 ПЛОВДИВ_2-4 ВАРНА"

targetOrigin <- 21#Plovdiv
targetDest <- 24#Varna



colnames(speedyCast)

speedyCast[6:10,1:5,with=F]

# explore plovdiv-varna ---------------------------------------------------

colnames(speedyCast)
targetVolumedStable <- speedyCast[,
                                  paste0("economy_sum","_",as.character(targetOrigin),"_",as.character(targetDest))
                                  ,with=F
                                  ]



tsSeries <- xts(cbind(
  # speedyCast$"economy_2-2 СОФИЯ_2-1 ПЛОВДИВ",
  # speedyCast$"economy_sum_2-4 ВАРНА_2-1 ПЛОВДИВ",
  targetVolumedStable,
  speedyCast[,.(isAnyHoliday,Speedy_Working_Day,Working_Saturday)]),strptime(speedyCast$time_id,dayFormat))

# speedyAggByOrigin[]
tsSeriesAggregated <-lag.xts(
  xts(
  # cbind(
    speedyCast[,.(totalEcon,totalExpress)]
    # ,
  #           totalOrigin=speedyAggByOrigin[order(time_id)][org==targetOrigin,total],
  #           totalDest=speedyAggByDestination[order(time_id)][dst==targetDest,total]
  # )
      , strptime(speedyCast$time_id,dayFormat)) ,1) %>% na.fill(0)


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
                        lapply(3:7,function(i){
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
(index(additionalYBasedData)==index(y) )%>% all()


ytrainingPeriod <- "2015-05-01/2015-08-17"
ytestingPeriod <- "2015-08-18/2015-10-14"
yhat <- ts(coredata(tsSeries[ytrainingPeriod,1]),start=2013,frequency = 7)
# yhat <- tsSeries[ytrainingPeriod,1]
yhattest <- tsSeries[ytestingPeriod,1]
# mod <- auto.arima(y["2014-01-01/2015-10-01"],xreg = embed(tsSeries["2013-12-29/2015-10-01",2:3],4),seasonal = T)
nrow(yhat)+nrow(yhattest)-nrow(tsSeries)


holidayPeriod <- "2015-04-28/2015-08-19"
holidayPeriodTest <- "2015-08-15/2015-10-16"
embedLen <- 6
holidayRelated <- embed(tsSeries[holidayPeriod,-1],embedLen)
holidayRelatedTest <-  embed(tsSeries[holidayPeriodTest,-1],embedLen)
nrow(holidayRelated)+nrow(holidayRelatedTest)-nrow(tsSeries)
  
indexTZ(additionalYBasedData) = Sys.getenv("TZ")


additionalYBasedDataTrain <- additionalYBasedData[ytrainingPeriod]
additionalYBasedDataTest <- additionalYBasedData[ytestingPeriod]


nrow(additionalYBasedDataTest)==nrow(yhattest)
nrow(holidayRelatedTest)==nrow(yhattest)

# add overall volume-past 2 days ------------------------------------------------------

aggregateStatsPeriod<-"2015-05-01/2015-08-17"
aggregateStatsTestPeriod<-"2015-08-18/2015-10-14"
aggregateStats2lags <- (merge.xts(tsSeriesAggregated,
                                  lag.xts(tsSeriesAggregated,1)) %>% na.fill(0))
aggregateStatsTrain <- aggregateStats2lags[aggregateStatsPeriod]
aggregateStatsTest <- aggregateStats2lags[aggregateStatsTestPeriod]

nrow(aggregateStatsTest)==nrow(yhattest)
nrow(holidayRelatedTest)==nrow(yhattest)

nrow(aggregateStatsTrain)+nrow(aggregateStatsTest)-nrow(tsSeries)
nrow(additionalYBasedDataTrain)==nrow(yhat)
nrow(aggregateStatsTrain) ==nrow(yhat)
nrow(holidayRelated)==nrow(yhat)


# regressorTable <- cbind( coredata(holidayRelated),coredata(aggregateStatsTrain))
# regressorTabletest <- cbind(coredata(holidayRelatedTest),coredata(aggregateStatsTest))

regressorTable <- cbind( coredata(additionalYBasedDataTrain) , coredata(holidayRelated),coredata(aggregateStatsTrain))
regressorTabletest <- cbind(
  coredata(additionalYBasedDataTest)
  ,coredata(holidayRelatedTest),coredata(aggregateStatsTest))
ncol(regressorTable)==ncol(regressorTabletest)
nrow(regressorTable)
nrow(regressorTabletest)
ncol(regressorTabletest)

mod <- auto.arima(yhat,xreg = regressorTable,seasonal = T,ic="aic",test="adf",allowmean = TRUE,allowdrift = TRUE,
                  max.p=5,max.q = 5,max.d=5,max.D=6,max.P=5,max.Q = 8)
mod <- auto.arima(yhat,xreg = regressorTable,seasonal = T,allowmean = TRUE,allowdrift = TRUE)
summary(mod)
# fct <- forecast(mod,xreg=embed(tsSeries['2015-08-29/',2:3],4))
fct <- forecast(mod,xreg=regressorTabletest)
plot(resid(fct))

matplot(cbind(coredata(fct$mean),coredata(yhattest),coredata(fct$upper[,2])
              # ,fct$upper[,1]
),type = "l",col = c("blue","red","green"))
# auto.arima()
plot(fct$mean-coredata(yhattest))
plot(density(fct$mean-coredata(yhattest)))
plot(abs(fct$mean/coredata(yhattest)),ylim=c(0,1))





