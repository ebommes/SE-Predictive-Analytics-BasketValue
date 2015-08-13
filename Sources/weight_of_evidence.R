# Weight of evidence
largeFactors = c("brand", "productGroup", "categoryIDs","userID"
                 ,"couponID", "productID","reward","orderWeekday","receivedWeekday","orderDaytime","receivedDaytime"
                 ,"orderedDayOfMonth","timeDifferenceCategories", "timeDiffLastBuy"
                 ,grep("Cluster",colnames(merged), value=TRUE))
for (var in c(largeFactors, "couponUsed")){
  merged[,var] <- factor(merged[,var])
}

merged.woe <- merged
merged.woe[merged.woe$trainingSetIndex!=1,"couponUsed"] <- NA 
# for couponUsed
weightOfEvidence <- woe(as.formula(paste("couponUsed ~ ", paste(largeFactors, collapse= "+"))),data=merged.woe,zeroadj=0.5,appont=TRUE)

userID <- merged$userID
merged <- predict(weightOfEvidence, newdata=merged, replace=TRUE)
merged$userID <- userID

# merge the resulting WoEs with the total dataframe
woeVector <- as.vector(weightOfEvidence$woe["userID"])
names(woeVector) <- "woe.userID"
total <- merge(total,woeVector,by.x="userID",by.y="row.names", all.x=TRUE)

for (var in c("brand", "productGroup", "categoryIDs","couponID", "productID","reward")){
  for (var2 in paste(var,1:3,sep="")){
    woeVector <- as.vector(weightOfEvidence$woe[var])
    names(woeVector) <- paste("woe",var2,sep=".")
  total <- merge(total,woeVector,by.x=var2,by.y="row.names", all.x=TRUE)
  total[,var2] <- NULL
}
}
