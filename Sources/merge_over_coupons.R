#setwd("/users/maj/dropbox/Applied Business Analytics")
#total <- read.csv("data_temp/dmc_3.1.csv", stringsAsFactors=FALSE)

# 1,2,3 Variablen mergen
total<- total[order(total$orderID),]
mergedColumns <- gsub("3","",grep("3", colnames(total), value=TRUE)[!(grep("3", colnames(total), value=TRUE) %in% "coupon3Used")])
#  c("couponID","price","basePrice","reward","premiumProduct","brand","productGroup","categoryIDs","equalOtherBrands", "equalOtherProductGroup"
#                   ,"diffPrice","priceRatio","priceRewardRatio","sumPrice","rewardPrice","rewardBasePrice"
#                   ,"")
normalColumns <- colnames(total[,!(colnames(total) %in% paste(mergedColumns,rep(1:3,length(mergedColumns)),sep=""))])
merged1 <- total[, !colnames(total) %in% 
                             c(paste(mergedColumns,2,sep=""), "coupon2Used",paste(mergedColumns,3,sep=""), "coupon3Used")]
merged1$couponNr <- 1
merged2 <- total[, !colnames(total) %in% 
                   c(paste(mergedColumns,1,sep=""), "coupon1Used",paste(mergedColumns,3,sep=""), "coupon3Used")]
merged2$couponNr <- 2
merged3 <- total[, !colnames(total) %in% 
                   c(paste(mergedColumns,2,sep=""), "coupon2Used",paste(mergedColumns,1,sep=""), "coupon1Used")]
merged3$couponNr <- 3

colnames(merged1)[colnames(merged1) %in% paste(mergedColumns,1,sep="")] <- c(mergedColumns)
colnames(merged1)[colnames(merged1)=="coupon1Used"] <- c("couponUsed")
colnames(merged2)[colnames(merged2) %in% paste(mergedColumns,2,sep="")] <- c(mergedColumns)
colnames(merged2)[colnames(merged2)=="coupon2Used"] <- c("couponUsed")
colnames(merged3)[colnames(merged3) %in% paste(mergedColumns,3,sep="")] <- c(mergedColumns)
colnames(merged3)[colnames(merged3)=="coupon3Used"] <- c("couponUsed")

merged <- rbind(merged1,merged2,merged3)
rownames(merged) <- NULL
merged <- data.frame(merged)

# split categoryIDs into dummies
merged<-concat.split.expanded(merged, "categoryIDs", type="character", fill="0", drop=FALSE)
merged<-concat.split.expanded(merged, "brand", type="character", fill="0", drop=FALSE)
merged<-concat.split.expanded(merged, "productGroup", type="character", fill="0", drop=FALSE)
merged<-concat.split.expanded(merged, "productID", type="character", fill="0", drop=FALSE)






