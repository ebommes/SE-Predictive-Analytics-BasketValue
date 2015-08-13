# Different baselines 

# Create a function to simplify code 
# baselineFunction <- function(data,baselineVar,categoryVar,aggFunction){
#   frame <- as.data.frame(tapply(data[,baselineVar], data[,categoryVar], 
#                                 if (aggFunction=="mean") function(x) mean(x,na.rm=TRUE)
#                                 else if (aggFunction=="median") function(x) median(x,na.rm=TRUE)
#                                 else print("aggFunction must be mean or median")))
#   frame[,categoryVar] <- rownames(frame)
#   rownames(frame) <- NULL
#   colnames(frame) <- c(paste(categoryVar,baselineVar,"Rate", sep=""),categoryVar)
#   frame[is.nan(frame[,baselineVar]),1] <- 
#     if (aggFunction=="mean") mean(data[,baselineVar], na.rm=TRUE)
#   else if (aggFunction=="median") median(data[,baselineVar], na.rm=TRUE)
#   else print("aggFunction must be mean or median")
#   return(frame)
# }

#mergedTrain <- subset(merged, trainingSetIndex==1)

total <- total[order(total$orderID),]
merged <- merged[order(merged$orderID),]
totalTrain <- subset(total,total$trainingSetIndex==1)
mergedTrain <- subset(merged,merged$trainingSetIndex==1)

# user specific average coupon use
couponRateFrame <- as.data.frame(tapply(mergedTrain$couponUsed, mergedTrain$userID, function(x)mean(x,na.rm=TRUE)))
couponRateFrame$userID <- rownames(couponRateFrame)
rownames(couponRateFrame) <- NULL
colnames(couponRateFrame) <- c("userCouponRateTotal", "userID")
couponRateFrame$userCouponRateTotal[is.nan(couponRateFrame$userCouponRateTotal)] <- mean(mergedTrain$couponUsed, na.rm=TRUE)
merged <- merge(merged,couponRateFrame,by="userID", all.x=TRUE)
merged$userCouponRateTotal[is.na(merged$userCouponRateTotal)] <- mean(mergedTrain$couponUsed, na.rm=TRUE)
total <- merge(total,couponRateFrame,by="userID",all.x=TRUE)
total$userCouponRateTotal[is.na(total$userCouponRateTotal)] <- mean(mergedTrain$couponUsed, na.rm=TRUE)

# productID specific coupon use
couponRateFrame <- as.data.frame(tapply(mergedTrain$couponUsed, mergedTrain$productID, function(x)mean(x,na.rm=TRUE)))
couponRateFrame$productID <- rownames(couponRateFrame)
rownames(couponRateFrame) <- NULL
colnames(couponRateFrame) <- c("productIDCouponRateTotal", "productID")
couponRateFrame$productIDCouponRateTotal[is.nan(couponRateFrame$productIDCouponRateTotal)] <- mean(mergedTrain$couponUsed, na.rm=TRUE)
merged <- merge(merged,couponRateFrame,by="productID", all.x=TRUE)
merged$productIDCouponRateTotal[is.na(merged$productIDCouponRateTotal)] <- mean(mergedTrain$couponUsed, na.rm=TRUE)
for (i in 1:3){
  total[,paste("productIDCouponRateTotal",i,sep="")] <- subset(merged[,"productIDCouponRateTotal"],merged$couponNr==i)
}

# coupon Nr specific coupon use
couponRateFrame <- as.data.frame(tapply(mergedTrain$couponUsed, mergedTrain$couponNr, function(x)mean(x,na.rm=TRUE)))
couponRateFrame$couponNr <- rownames(couponRateFrame)
rownames(couponRateFrame) <- NULL
colnames(couponRateFrame) <- c("couponNrCouponRateTotal", "couponNr")
couponRateFrame$couponNrCouponRateTotal[is.nan(couponRateFrame$couponNrCouponRateTotal)] <- mean(mergedTrain$couponUsed, na.rm=TRUE)
merged <- merge(merged,couponRateFrame,by="couponNr", all.x=TRUE)
merged$couponNrCouponRateTotal[is.na(merged$couponNrCouponRateTotal)] <- mean(mergedTrain$couponUsed, na.rm=TRUE)
for (i in 1:3){
  total[,paste("couponNrCouponRateTotal",i,sep="")] <- subset(merged[,"couponNrCouponRateTotal"],merged$couponNr==i)
}

# productGroup specific coupon use
couponRateFrame <- as.data.frame(tapply(mergedTrain$couponUsed, mergedTrain$productGroup, function(x)mean(x,na.rm=TRUE)))
couponRateFrame$productGroup <- rownames(couponRateFrame)
rownames(couponRateFrame) <- NULL
colnames(couponRateFrame) <- c("productGroupCouponRateTotal", "productGroup")
couponRateFrame$productGroupCouponRateTotal[is.nan(couponRateFrame$productGroupCouponRateTotal)] <- mean(mergedTrain$couponUsed, na.rm=TRUE)
merged <- merge(merged,couponRateFrame,by="productGroup", all.x=TRUE)
merged$productGroupCouponRateTotal[is.na(merged$productGroupCouponRateTotal)] <- mean(mergedTrain$couponUsed, na.rm=TRUE)
for (i in 1:3){
  total[,paste("productGroupCouponRateTotal",i,sep="")] <- subset(merged[,"productGroupCouponRateTotal"],merged$couponNr==i)
}

# brand specific coupon use
couponRateFrame <- as.data.frame(tapply(mergedTrain$couponUsed, mergedTrain$brand, function(x)mean(x,na.rm=TRUE)))
couponRateFrame$brand <- rownames(couponRateFrame)
rownames(couponRateFrame) <- NULL
colnames(couponRateFrame) <- c("brandCouponRateTotal", "brand")
couponRateFrame$brandCouponRateTotal[is.nan(couponRateFrame$brandCouponRateTotal)] <- mean(mergedTrain$couponUsed, na.rm=TRUE)
merged<- merge(merged,couponRateFrame,by="brand", all.x=TRUE)
merged$brandCouponRateTotal[is.na(merged$brandCouponRateTotal)] <- mean(mergedTrain$couponUsed, na.rm=TRUE)
for (i in 1:3){
  total[,paste("brandCouponRateTotal",i,sep="")] <- subset(merged[,"brandCouponRateTotal"],merged$couponNr==i)
}

# combined coupon use
merged$combinedCouponUse <- merged$userCouponRateTotal * merged$productIDCouponRateTotal * merged$productGroupCouponRateTotal*
  merged$couponNrCouponRateTotal * merged$brandCouponRateTotal 

# combined coupon use by Logit
mergedLogit <- subset(merged,merged$trainingSetIndex==1)
combinationLogit <- glm(couponUsed ~ userCouponRateTotal+ productIDCouponRateTotal+ productGroupCouponRateTotal+
               couponNrCouponRateTotal, data=mergedLogit, family="binomial")
merged$logitCombinedCouponUse <- predict(combinationLogit, newdata=merged, type="response")

# mean coupon use excluding brand
merged$meanCombinedCouponUse <- rowMeans(data.frame(merged$userCouponRateTotal,merged$productIDCouponRateTotal,merged$productGroupCouponRateTotal,
                                                 merged$couponNrCouponRateTotal))

# user specific average basketValue
basketValueFrame <- as.data.frame(tapply(mergedTrain$basketValue, mergedTrain$userID, function(x)mean(x,na.rm=TRUE)))
basketValueFrame$userID <- rownames(basketValueFrame)
rownames(basketValueFrame) <- NULL
colnames(basketValueFrame) <- c("userMeanBasketValue", "userID")
basketValueFrame$userMeanBasketValue[is.nan(basketValueFrame$userMeanBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)
merged <- merge(merged,basketValueFrame,by="userID", all.x=TRUE)
merged$userMeanBasketValue[is.na(merged$userMeanBasketValue)] <- mean(mergedTrain$basketValue, na.rm=TRUE)
total <- merge(total,basketValueFrame,by="userID", all.x=TRUE)
total$userMeanBasketValue[is.na(total$userMeanBasketValue)] <- mean(mergedTrain$basketValue, na.rm=TRUE)

# user specific median basketValue
basketValueFrame <- as.data.frame(tapply(mergedTrain$basketValue, mergedTrain$userID, function(x)median(x,na.rm=TRUE)))
basketValueFrame$userID <- rownames(basketValueFrame)
rownames(basketValueFrame) <- NULL
colnames(basketValueFrame) <- c("userMedianBasketValue", "userID")
basketValueFrame$userMedianBasketValue[is.na(basketValueFrame$userMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)
merged <- merge(merged,basketValueFrame,by="userID", all.x=TRUE)
merged$userMedianBasketValue[is.na(merged$userMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)
total <- merge(total,basketValueFrame,by="userID", all.x=TRUE)
total$userMedianBasketValue[is.na(total$userMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)

# weekday specific median basketValue
basketValueFrame <- as.data.frame(tapply(mergedTrain$basketValue, mergedTrain$orderWeekday, function(x)median(x,na.rm=TRUE)))
basketValueFrame$orderWeekday <- rownames(basketValueFrame)
rownames(basketValueFrame) <- NULL
colnames(basketValueFrame) <- c("weekdayMedianBasketValue", "orderWeekday")
basketValueFrame$weekdayMedianBasketValue[is.nan(basketValueFrame$weekdayMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)
merged <- merge(merged,basketValueFrame,by="orderWeekday", all.x=TRUE)
merged$weekdayMedianBasketValue[is.na(merged$weekdayMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)
total<- merge(total,basketValueFrame,by="orderWeekday", all.x=TRUE)
total$weekdayMedianBasketValue[is.na(total$weekdayMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)

# shopping frequency specific median basketValue
basketValueFrame <- as.data.frame(tapply(mergedTrain$basketValue, mergedTrain$shoppingFreq, function(x)median(x,na.rm=TRUE)))
basketValueFrame$shoppingFreq <- rownames(basketValueFrame)
rownames(basketValueFrame) <- NULL
colnames(basketValueFrame) <- c("shoppingFreqMedianBasketValue", "shoppingFreq")
basketValueFrame$shoppingFreqMedianBasketValue[is.nan(basketValueFrame$shoppingFreqMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)
merged <- merge(merged,basketValueFrame,by="shoppingFreq", all.x=TRUE)
merged$shoppingFreqMedianBasketValue[is.na(merged$shoppingFreqMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)
total<- merge(total,basketValueFrame,by="shoppingFreq", all.x=TRUE)
total$shoppingFreqMedianBasketValue[is.na(total$shoppingFreqMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)

# daytime specific median basketValue
basketValueFrame <- as.data.frame(tapply(mergedTrain$basketValue, mergedTrain$orderDaytime, function(x)median(x,na.rm=TRUE)))
basketValueFrame$shoppingFreq <- rownames(basketValueFrame)
rownames(basketValueFrame) <- NULL
colnames(basketValueFrame) <- c("orderDaytimeMedianBasketValue", "orderDaytime")
basketValueFrame$orderDaytimeMedianBasketValue[is.nan(basketValueFrame$orderDaytimeMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)
merged <- merge(merged,basketValueFrame,by="orderDaytime", all.x=TRUE)
merged$orderDaytimeMedianBasketValue[is.na(merged$orderDaytimeMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)
total<- merge(total,basketValueFrame,by="orderDaytime", all.x=TRUE)
total$orderDaytimeMedianBasketValue[is.na(total$orderDaytimeMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)

# cluster specific median basketValue
basketValueFrame <- as.data.frame(tapply(mergedTrain$basketValue, mergedTrain$CategoryClusterKNN, function(x)median(x,na.rm=TRUE)))
basketValueFrame$shoppingFreq <- rownames(basketValueFrame)
rownames(basketValueFrame) <- NULL
colnames(basketValueFrame) <- c("CategoryClusterKNNMedianBasketValue", "CategoryClusterKNN")
basketValueFrame$CategoryClusterKNNMedianBasketValue[is.nan(basketValueFrame$CategoryClusterKNNMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)
merged <- merge(merged,basketValueFrame,by="CategoryClusterKNN", all.x=TRUE)
merged$CategoryClusterKNNMedianBasketValue[is.na(merged$CategoryClusterKNNMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)
total<- merge(total,basketValueFrame,by="CategoryClusterKNN", all.x=TRUE)
total$CategoryClusterKNNMedianBasketValue[is.na(total$CategoryClusterKNNMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)

basketValueFrame <- as.data.frame(tapply(mergedTrain$basketValue, mergedTrain$productGroupClusterKNN, function(x)median(x,na.rm=TRUE)))
basketValueFrame$shoppingFreq <- rownames(basketValueFrame)
rownames(basketValueFrame) <- NULL
colnames(basketValueFrame) <- c("productGroupClusterKNNMedianBasketValue", "productGroupClusterKNN")
basketValueFrame$productGroupClusterKNNMedianBasketValue[is.nan(basketValueFrame$productGroupClusterKNNMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)
merged <- merge(merged,basketValueFrame,by="productGroupClusterKNN", all.x=TRUE)
merged$productGroupClusterKNNMedianBasketValue[is.na(merged$productGroupClusterKNNMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)
total<- merge(total,basketValueFrame,by="productGroupClusterKNN", all.x=TRUE)
total$productGroupClusterKNNMedianBasketValue[is.na(total$productGroupClusterKNNMedianBasketValue)] <- median(mergedTrain$basketValue, na.rm=TRUE)


# Other ways for improved baserates
# panelReg <- glm(basketValue~userID+0, family="gaussian",data=merged)

