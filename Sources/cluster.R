


######## product Group ##
mergedPG <- merged
mergedPG$productGroup <- NULL
mergedPG <- mergedPG[,grep("productGroup",colnames(mergedPG),value=TRUE)]
mergedPG <- apply(mergedPG,2,function(x)as.numeric(as.character(x)))
userCouponGroup <- by(mergedPG,merged$userID,colSums)
userCouponGroup <- as.data.frame(do.call(rbind,userCouponGroup))
productGroupVariety <- rowSums(userCouponGroup)
userCouponGroup[userCouponGroup>1] <- 1
productGroupVariety <- rowSums(userCouponGroup)/productGroupVariety

# Hierarchical cluster
productGroupDistance <- dissimilarity(as.matrix(userCouponGroup),method="cosine")
userGroupClusters <- hclust(productGroupDistance,method="ward.D2")
#plot(userGroupClusters, ann=FALSE)
userGroupCluster <- as.data.frame(cutree(userGroupClusters, k=10))
colnames(userGroupCluster) <- "productGroupClusterCosine"
userGroupCluster$productGroupVariety <- productGroupVariety
#merged <- merge(merged,userGroupCluster,by.x="userID",by.y="row.names")
#total <- merge(total,userGroupCluster,by.x="userID",by.y="row.names")

# productGroup kNN clusters
#wss <- (nrow(userCouponGroup)-1)*sum(apply(userCouponGroup,2,var))
#for (i in 5:20) wss[i] <- sum(kmeans(userCouponGroup, 
#                                     centers=i,iter.max=20)$withinss)
#plot(1:20, wss, type="b", xlab="Number of Clusters",
#     ylab="Within groups sum of squares")
# K-Means Cluster Analysis
productGroupClusterKNN <- as.data.frame(kmeans(userCouponGroup, 14)$cluster)
#productGroupClusterKNN$userID <- row.names(userCouponGroup)
colnames(productGroupClusterKNN) <- "productGroupClusterKNN"
#merged <- merge(merged,productGroupClusterKNN,by.x="userID",by.y="row.names")
#total <- merge(total,productGroupClusterKNN,by.x="userID",by.y="row.names")


######## categoryIDs  ####
mergedCID <- merged
mergedCID$categoryIDs <- NULL
mergedCID <- mergedCID[,grep("categoryID",colnames(mergedCID),value=TRUE)]
mergedCID <- apply(mergedCID,2,function(x)as.numeric(as.character(x)))
userCouponCategory <- by(mergedCID,merged$userID,colSums)
userCouponCategory <- as.data.frame(do.call(rbind,userCouponCategory))
categoryVariety <- rowSums(userCouponCategory)
userCouponCategory[userCouponCategory>1] <- 1
categoryVariety <- rowSums(userCouponCategory)/categoryVariety

# Hierarchical cluster
CategoryDistance <- dissimilarity(as.matrix(userCouponCategory),method="cosine")
userCategoryClusters <- hclust(CategoryDistance,method="ward.D2")
#plot(userCategoryClusters, ann=FALSE)
userCategoryCluster <- as.data.frame(cutree(userCategoryClusters, k=13))
colnames(userCategoryCluster) <- "CategoryClusterCosine"
userCategoryCluster$categoryVariety <- categoryVariety
#merged <- merge(merged,userCategoryCluster,by.x="userID",by.y="row.names")


# productGroup kNN clusters
# search optimal number of clusters
#wss <- (nrow(userCouponCategory)-1)*sum(apply(userCouponCategory,2,var))
#for (i in 5:20) wss[i] <- sum(kmeans(userCouponCategory, 
#                                     centers=i,iter.max=20)$withinss)
#plot(1:20, wss, type="b", xlab="Number of Clusters",
#     ylab="Within groups sum of squares")
# K-Means Cluster Analysis
categoryClusterKNN <- as.data.frame(kmeans(userCouponCategory, 10)$cluster)
#productGroupClusterKNN$userID <- row.names(userCouponCategory)
colnames(categoryClusterKNN) <- "CategoryClusterKNN"
#merged <- merge(merged,categoryClusterKNN,by.x="userID",by.y="row.names")


######## brands  ####
mergedBrand <- merged
mergedBrand$brand <- NULL
mergedBrand <- mergedBrand[,grep("brand",colnames(mergedBrand),value=TRUE)]
mergedBrand <- apply(mergedBrand,2,function(x)as.numeric(as.character(x)))
userCouponBrand <- by(mergedBrand,merged$userID,colSums)
userCouponBrand <- as.data.frame(do.call(rbind,userCouponBrand))
brandVariety <- rowSums(userCouponBrand)
userCouponBrand[userCouponBrand>1] <- 1
brandVariety <- rowSums(userCouponBrand)/brandVariety

# Hierarchical cluster
BrandDistance <- dissimilarity(as.matrix(userCouponBrand),method="cosine")
userBrandClusters <- hclust(BrandDistance,method="ward.D2")
#plot(userBrandClusters, ann=FALSE)
userBrandCluster <- as.data.frame(cutree(userBrandClusters, k=9))
colnames(userBrandCluster) <- "brandClusterCosine"
userBrandCluster$brandVariety <- brandVariety
#merged <- merge(merged,userBrandCluster,by.x="userID",by.y="row.names")

# productGroup kNN clusters
# search optimal number of clusters
#wss <- (nrow(userCouponBrand)-1)*sum(apply(userCouponBrand,2,var))
#for (i in 5:20) wss[i] <- sum(kmeans(userCouponBrand, 
#                                     centers=i,iter.max=20)$withinss)
#plot(1:20, wss, type="b", xlab="Number of Clusters",
#     ylab="Within groups sum of squares")
# K-Means Cluster Analysis
brandClusterKNN <- as.data.frame(kmeans(userCouponBrand, 12)$cluster)
#brandClusterKNN$userID <- row.names(userCouponBrand)
colnames(brandClusterKNN) <- "brandClusterKNN"
#merged <- merge(merged,brandClusterKNN,by.x="userID",by.y="row.names")

clusters <- data.frame(userGroupCluster,productGroupClusterKNN,userCategoryCluster,categoryClusterKNN,userBrandCluster,brandClusterKNN)
merged <- merge(merged,clusters,by.x="userID",by.y="row.names")
total <- merge(total,clusters,by.x="userID",by.y="row.names")

for (var in c("productGroupClusterCosine",  "productGroupClusterKNN" ,"CategoryClusterCosine" , "CategoryClusterKNN" ,  "brandClusterCosine", "brandClusterKNN")){
  total[,var] <- as.factor(total[,var])
  merged[,var] <- as.factor(merged[,var])
}


# clusterForestBasketValue <- rpart(as.formula(c("basketValue ~",paste("productGroupClusterCosine",  "productGroupClusterKNN" ,"CategoryClusterCosine" , "CategoryClusterKNN" ,  "brandClusterCosine", "brandClusterKNN",sep="+")))
#                                              , data=dmcTrain,cp=0.001)
# total$clusterBasketValue <- predict(clusterForestBasketValue,newdata=total)

