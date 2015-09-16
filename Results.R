##############################################################################
################################ Results #####################################
##############################################################################
# With this file the results of the simulation are evaluated. Models and 
# ensembles are loaded from "/Predictions". Minimal ACs for base classifiers 
# are determined as well as summary statistics for ensembles. Several plots 
# for ACs are created.
##############################################################################



install.packages("psych")
library(psych)


# Set Paths
path         = "H:/Projects/Predictive Analytics"
path.data    = paste(path, "/DMC2015", sep = "")
path.source  = paste(path, "/Sources", sep = "")
path.results = paste(path, "/Estimated Models", sep = "")
path.pred    = paste(path, "/Predictions", sep = "")

# Functions
AC = function(y, yhat){ 
  denom = mean(y)
  crit  = ( abs(y - yhat) / denom )^2
  crit  = sum(crit)
  return(crit)
}


ACs = function(y, yhat){ 
  denom = mean(y)
  crit  = ( abs(y - yhat) / denom )^2
  crit  = colSums(crit)
  return(crit)
}

setwd(path.pred)

set = list.files(path.pred)
set = substr(set, 1, 14 )
set = set[-which(set %in% c("EStrain.rds","EStest.rds"))]
for(i in 1:length(set)){
  print(i)
  set.pred = readRDS(paste(set, ".rds", sep = "")[i])
  
  test.pred  = set.pred$model.test
  test.bv    = set.pred$bv.test
  
  val.pred   = set.pred$model.val
  val.bv     = set.pred$bv.val

  for(j in 1:ncol(test.pred)){
    if(j == 1){
      AC.test = ACs(test.bv,test.pred[j])
      AC.val  = ACs(test.bv,test.pred[j])
    }
    else{
      AC.test = c(AC.test, ACs(test.bv,test.pred[j]))
      AC.val  = c(AC.val,ACs(val.bv,val.pred[j]))
    }
  }
  
  if(i == 1){
    AC.test.df = as.data.frame(t(AC.test))
    AC.val.df  = as.data.frame(t(AC.val))
  }
  else{
    AC.test.df = rbind(AC.test.df,AC.test)
    AC.val.df = rbind(AC.val.df,AC.val)
  }
}


AC.test.df = cbind(set = as.character(set),AC.test.df)
AC.val.df = cbind(set = as.character(set),AC.val.df)



### Find minimum over all models
subset = AC.test.df[1:98,2:15]
min.models = AC.test.df$set[which(subset == min(subset))]
min(subset)
min.models


### compare single models and ensembles
AC.es.train.on.val = readRDS("EStrain.rds")
AC.es.test.on.val  = readRDS("EStest.rds")
AC.es.train.on.test = readRDS("EStrainontest.rds")


boxplot(AC.es.train.on.test, 
        col = "lightblue",
        main = "Boxplot of ACs for ensembles on hold-out sample",
        las = 1)
describe(AC.es.train.on.test)

### AC of baseline prediction
AC.baseline.val  = AC(val.bv,rep(mean(val.bv),669))
lm1 = lm(AC.es.train.on.val~AC.es.test.on.val)

par(mfrow =c(2,1))
plot(AC.es.on.test,AC.es.on.val,
     las = 1,
     main = "ACs of ensembles",
     xlab = "AC for hold-out sample",
     ylab = "AC for validation set",
     mgp  = c(3, .6, 0))
abline(h = AC.baseline.val, col = "red")
abline(h = mean(AC.es.on.val), col = "green")


plot(AC.es.on.test,AC.es.on.val,
     las = 1,
     xlab = "AC for hold-out sample",
     ylab = "AC for validation set",
     xlim = c(0,AC.baseline.test),
     mgp  = c(3, .6, 0))
abline(h = AC.baseline.val, col = "red")
abline(h = mean(AC.es.on.val), col = "green")


par(mfrow =c(1,1))
plot(AC.es.train.on.val,AC.es.test.on.val,
     las = 1,
     main = "ACs of ensembles predictions on validation set",
     xlab = "training set ensembles",
     ylab = "hold-out sample ensembles",
     mgp  = c(3, .6, 0))
abline(h = AC.baseline.val,
       v = AC.baseline.val, 
       col = "red")
abline(h = mean(AC.es.test.on.val),
       v = mean(AC.es.train.on.val), 
       col = "green")
abline(a = lm1$coefficients[1],
     b=lm1$coefficients[2],
     col="blue")


# cor(AC.es.train.on.val,AC.es.test.on.val)
# chisq.test(AC.es.train.on.val,AC.es.test.on.val)
# 
# hist(AC.es.on.val,breaks="FD")
# hist(AC.es.on.val[which(AC.es.on.test < 3500)],breaks="FD")

