#############################################################################
############################### Data Preparation ############################
#############################################################################
# The original data preparation file written by members of the data 
# preparation task group.
# In the context of basket value prediction, adjustments were made only in 
# order to partition the data according to the distribution of the basket 
# value variable.
# This file is called by the Model Estimation procedure.
# Set Paths if used standalone.



#options(stringsAsFactors = FALSE)

pack = c("caret", "data.table", "splitstackshape", "stats", "arules", "klaR")


lapply(pack, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

lapply(pack, require, character.only = TRUE)

setwd(path.data)



train <- read.table("DMC_2015_orders_train.txt", header=TRUE, sep="|", stringsAsFactors=FALSE)
test <- read.table("DMC_2015_orders_class.txt", header=TRUE, sep="|",stringsAsFactors=FALSE)

data.part  = createDataPartition(y = train$basketValue,
                                 p = 0.8, list = FALSE)
data.part  = as.vector(data.part)

data.train = train[  data.part, ]
indi       = c(1:dim(train)[1])
data.test  = train[! indi %in% data.part, ]

data.train$trainingSetIndex = 1
data.test$trainingSetIndex  = 0
test$trainingSetIndex       = -1

total <- rbind(data.train, data.test, test)

setwd(path.source)
# brand "" as "unknown"
total$brand1[total$brand1==""] <- "unknown"
total$brand2[total$brand2==""] <- "unknown"
total$brand3[total$brand3==""] <- "unknown"

# prepare time variables
# lag time since last buy

total <- data.table(total)
total[, lastBuy := c(NA, orderTime[-.N]), by = userID] 
total[, lastBasketValue := c(NA, basketValue[-.N]), by = userID]
total <- as.data.frame(total)

# format variables as time with strptime
total$orderTime <- strptime(total$orderTime, format="%Y-%m-%d %H:%M:%S")
total$couponsReceived <- strptime(total$couponsReceived, format="%Y-%m-%d %H:%M:%S")
total$lastBuy <- strptime(total$lastBuy, format="%Y-%m-%d %H:%M:%S")
# extract information on weekday and time of day
total$orderWeekday <- weekdays(total$orderTime)
total$receivedWeekday <- weekdays(total$couponsReceived)
total$orderDaytime <- total$orderTime$hour
total$receivedDaytime <- total$couponsReceived$hour
total$orderedDayOfMonth <- total$orderTime$mday
# calculate the time difference between receiving and using
total$timeDifference <- as.numeric(difftime(total$orderTime,total$couponsReceived, units="mins"))

total$timeDiffLastBuy <- as.numeric(difftime(total$orderTime,total$lastBuy, units="days"))
total$timeDiffLastBuy <- as.character(cut(total$timeDiffLastBuy,breaks=c(0:21,seq(22,50,2),68),labels=c(paste("vor",c(0:21,seq(22,49,2),68),"Tagen",sep=""))))
total$timeDiffLastBuy[is.na(total$timeDiffLastBuy)] <- "unknown"

# calculate consumption time
total$lastBasketConsumption <- total$lastBasketValue/as.numeric(difftime(total$orderTime,total$lastBuy,units="days"))

# dummy for multiple order with same coupon package
total$multiPurchases<- as.numeric(duplicated(total$userID)=="TRUE" & duplicated(total$couponsReceived)=="TRUE")

# time difference categories
total$timeDifferenceCategories<- cut(total$timeDifference,breaks=c(seq(0,50,10),seq(55,600,300),seq(800,5000,1000),seq(6000,11000,2500))
                                     ,labels=c(paste("difference",c(seq(0,50,10),seq(55,600,300),seq(800,5000,1000),seq(6000,8500,2500)),sep="")))

# calculate several combinations of the price variables
total$priceRatio1 <- total$basePrice1/total$price1
total$sumPrice1 <- total$price1+total$basePrice1
total$rewardPrice1 <- total$price1 * total$reward1
total$rewardBasePrice1 <- total$basePrice1 * total$reward1
total$priceRatio2 <- total$basePrice2/total$price2
total$sumPrice2 <- total$price2+total$basePrice2
total$rewardPrice2 <- total$price2 * total$reward2
total$rewardBasePrice2 <- total$basePrice2 * total$reward2
total$priceRatio3 <- total$basePrice3/total$price3
total$sumPrice3 <- total$price3+total$basePrice3
total$rewardPrice3 <- total$price3 * total$reward3
total$rewardBasePrice3 <- total$basePrice3 * total$reward3

# Create a productID
total$productID1 <- paste(total$price1,total$basePrice1,total$productGroup1, total$categoryIDs1,sep="")
total$productID1 <- paste("product",as.numeric(factor(total$productID1)))
total$productID2 <- paste(total$price2,total$basePrice2,total$productGroup2, total$categoryIDs2,sep="")
total$productID2 <- paste("product",as.numeric(factor(total$productID2)))
total$productID3 <- paste(total$price3,total$basePrice3,total$productGroup3, total$categoryIDs3,sep="")
total$productID3 <- paste("product",as.numeric(factor(total$productID3)))

total$maxPrice <- max(total$price1,total$price2,total$price3)
total$minPrice <- min(total$price1,total$price2,total$price3)
total$priceSpread <- total$maxPrice-total$minPrice
total$sumPrices <- total$price1+total$price2+total$price3
total$sumBasePrices <- total$basePrice1+total$basePrice2+total$basePrice3

# sum of premium product coupons
total$sumPremium <- total$premiumProduct1 + total$premiumProduct2 + total$premiumProduct3

# Calculate how often each unique customer is in the train dataset
# total<- total[order(total$userID),]
shoppingFreq <- as.data.frame(table(total$userID))
colnames(shoppingFreq) <- c("userID", "shoppingFreq")
total<- merge(total,shoppingFreq, by="userID")


# calculate same brands, etc. for each 1,2,3 separately
total$equalOtherBrands1 <- 0 + (total$brand1==total$brand2) + (total$brand1==total$brand3)
total$equalOtherBrands2 <- 0 + (total$brand2==total$brand1) + (total$brand2==total$brand3)
total$equalOtherBrands3 <- 0 + (total$brand3==total$brand1) + (total$brand3==total$brand2)
total$equalOtherProductGroup1 <- 0 + (total$productGroup1==total$productGroup2) + (total$productGroup1==total$productGroup3)
total$equalOtherProductGroup2 <- 0 + (total$productGroup2==total$productGroup1) + (total$productGroup2==total$productGroup3)
total$equalOtherProductGroup3 <- 0 + (total$productGroup3==total$productGroup1) + (total$productGroup3==total$productGroup2)

#############################


dmcTrain <- subset(total, total$trainingSetIndex==1)
# calculate user specific max basket value (base rate if no information on user)
couponRateFrame <- as.data.frame(tapply(dmcTrain$basketValue, dmcTrain$userID, function(x)max(x,na.rm=TRUE)))
couponRateFrame$userID <- rownames(couponRateFrame)
rownames(couponRateFrame) <- NULL
colnames(couponRateFrame) <- c("userMaxBasketValue", "userID")
couponRateFrame$userMaxBasketValue[is.infinite(couponRateFrame$userMaxBasketValue)] <- median(couponRateFrame$userMaxBasketValue, na.rm=TRUE)
total <- merge(total,couponRateFrame,by="userID", all.x=TRUE)
total$userMaxBasketValue[is.na(total$userMaxBasketValue)] <- median(couponRateFrame$userMaxBasketValue, na.rm=TRUE)

# calculate user specific min basket value (base rate if no information on user)
couponRateFrame <- as.data.frame(tapply(dmcTrain$basketValue, dmcTrain$userID, function(x)min(x,na.rm=TRUE)))
couponRateFrame$userID <- rownames(couponRateFrame)
rownames(couponRateFrame) <- NULL
colnames(couponRateFrame) <- c("userMinBasketValue", "userID")
couponRateFrame$userMinBasketValue[is.infinite(couponRateFrame$userMinBasketValue)] <- median(couponRateFrame$userMinBasketValue, na.rm=TRUE)
total <- merge(total,couponRateFrame,by="userID", all.x=TRUE)
total$userMinBasketValue[is.na(total$userMinBasketValue)] <- median(couponRateFrame$userMinBasketValue, na.rm=TRUE)

# calculate user specific basket value variance (base rate if no information on user)
couponRateFrame <- as.data.frame(tapply(dmcTrain$basketValue, dmcTrain$userID, function(x)var(x,na.rm=TRUE)))
couponRateFrame$userID <- rownames(couponRateFrame)
rownames(couponRateFrame) <- NULL
colnames(couponRateFrame) <- c("userBasketValueVar", "userID")
couponRateFrame$userBasketValueVar[is.na(couponRateFrame$userBasketValueVar)] <- 0
total <- merge(total,couponRateFrame,by="userID", all.x=TRUE)
total$userBasketValueVar[is.na(total$userBasketValueVar)] <- 0

source("merge_over_coupons.R")

# cluster data for both datasets
source("cluster.R")
#
dmcTrain <- subset(total,total$trainingSetIndex==1 & total$basketValue<700)
mergedTrain <- subset(merged,merged$trainingSetIndex==1 & total$basketValue<700)
# cluster specific median basketValue
for (cluster in grep("Cluster", colnames(merged),value=TRUE)){
  basketValueFrame <- as.data.frame(tapply(mergedTrain$basketValue, mergedTrain[, cluster], function(x)median(x,na.rm=TRUE)))
  basketValueFrame$shoppingFreq <- rownames(basketValueFrame)
  rownames(basketValueFrame) <- NULL
  colnames(basketValueFrame) <- c(paste("baseline",sub("Cluster","Clust",cluster),sep=""), as.character(cluster))
  basketValueFrame[is.nan(basketValueFrame[,paste("baseline",sub("Cluster","Clust",cluster),sep="")]),paste("baseline",sub("Cluster","Clust",cluster),sep="")] <- median(mergedTrain$basketValue, na.rm=TRUE)
  merged <- merge(merged,basketValueFrame,by=as.character(cluster), all.x=TRUE)
  total<- merge(total,basketValueFrame,by=as.character(cluster),all.x=TRUE)
}

# calculate user specific coupon rate (base rate if no information on user)
couponRateFrame <- as.data.frame(tapply(dmcTrain$coupon1Used, dmcTrain$userID, function(x)mean(x,na.rm=TRUE)))
couponRateFrame$userID <- rownames(couponRateFrame)
rownames(couponRateFrame) <- NULL
colnames(couponRateFrame) <- c("userCouponRate1", "userID")
#couponRateFrame$userCouponRate1[is.nan(couponRateFrame$userCouponRate1)] <- mean(dmcTrain$coupon1Used, na.rm=TRUE)
total <- merge(total,couponRateFrame,by="userID", all.x=TRUE)
total$userCouponRate1[is.na(total$userCouponRate1)] <- mean(dmcTrain$coupon1Used, na.rm=TRUE)

couponRateFrame <- as.data.frame(tapply(dmcTrain$coupon2Used, dmcTrain$userID, function(x)mean(x,na.rm=TRUE)))
couponRateFrame$userID <- rownames(couponRateFrame)
rownames(couponRateFrame) <- NULL
colnames(couponRateFrame) <- c("userCouponRate2", "userID")
#couponRateFrame$userCouponRate2[is.nan(couponRateFrame$userCouponRate2)] <- mean(dmcTrain$coupon2Used, na.rm=TRUE)
total <- merge(total,couponRateFrame,by="userID", all.x=TRUE)
total$userCouponRate2[is.na(total$userCouponRate2)] <- mean(dmcTrain$coupon2Used, na.rm=TRUE)

couponRateFrame <- as.data.frame(tapply(dmcTrain$coupon3Used, dmcTrain$userID, function(x)mean(x,na.rm=TRUE)))
couponRateFrame$userID <- rownames(couponRateFrame)
rownames(couponRateFrame) <- NULL
colnames(couponRateFrame) <- c("userCouponRate3", "userID")
#couponRateFrame$userCouponRate3[is.nan(couponRateFrame$userCouponRate3)] <- mean(dmcTrain$coupon3Used, na.rm=TRUE)
total <- merge(total,couponRateFrame,by="userID", all.x=TRUE)
total$userCouponRate3[is.na(total$userCouponRate3)] <- mean(dmcTrain$coupon3Used, na.rm=TRUE)

# baselines for all merged and part total. Check this again.
source("baselineByPanel.R")

# Basket Value of last buy
total$lastBasketValue[is.na(total$lastBasketValue)] <- total$userMedianBasketValue[is.na(total$lastBasketValue)]
total$lastBasketValue[is.na(total$lastBasketValue)] <- median(dmcTrain$basketValue, na.rm=TRUE)
# total <-total[order(total$userID,partial=total$orderTime),]
# View(total[,c("userID","orderTime","basketValue","lastBasketValue")])
merged$lastBasketValue <- NULL
merged <- merge(merged,total[,c("userID","orderID","lastBasketValue")], by=c("userID","orderID"),all.x=TRUE)
# merged <-merged[order(merged$userID,partial=merged$orderTime),]
# View(merged[,c("userID","orderTime","basketValue","lastBasketValue")])


# Factor variable reduction by weight of evidence
source("weight_of_evidence.R")

# sort by orderID
total <- total[order(total$orderID),]
merged <- merged[order(merged$orderID),]

# remove all high-level factors
total<-total[,!colnames(total) %in%  c('couponsReceived',"orderTime", "couponID")]

merged<-merged[,!colnames(merged) %in%  c("brand", "productGroup", "categoryIDs",'couponsReceived',"orderTime"
                                          ,"couponID", "productID")]
merged <- merged[,!grepl("categoryIDs_",colnames(merged))]
merged <- merged[,!grepl("brand_",colnames(merged))]
merged <- merged[,!grepl("productGroup_",colnames(merged))]
merged <- merged[,!grepl("productID_",colnames(merged))]

# remove lastBuy
total$lastBuy <- NULL
merged$lastBuy <- NULL
total$lastBasketConsumption <- NULL
merged$lastBasketConsumption <- NULL


data = total

### Only keep data
rm(list = setdiff(ls(), c("data", "path", "path.data", "path.results", "path.source", "stempel")))

data$userID = NULL
data$lastBuy                  = NULL
data$lastBasketConsumption    = NULL
data$couponNrCouponRateTotal1 = NULL
data$couponNrCouponRateTotal2 = NULL
data$couponNrCouponRateTotal3 = NULL
data$userID                   = NULL
data$coupon1Used              = NULL
data$coupon2Used              = NULL
data$coupon3Used              = NULL
data$orderID                  = NULL

data$orderWeekday = as.factor(data$orderWeekday)
data$receivedWeekday = as.factor(data$receivedWeekday)
data$timeDiffLastBuy = as.factor(data$timeDiffLastBuy)

# Remove constant columns

#data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
 #                                      as.factor)


data = data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
