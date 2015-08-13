#options(stringsAsFactors = FALSE)
### Libraries
pack = c("earth", "elasticnet", "leaps", "kernlab", "ipred", "plyr", "rpart",
         "kknn", "nnet", "brnn", "frbs", "RSNNS", "foreach", "caret", "gbm",
         "randomForest", "RRF", "party", "doParallel", "data.table",
         "splitstackshape", "stats", "arules", "klaR", "elasticnet")


lapply(pack, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})


lapply(pack, library, character.only = TRUE)

path         = "/users/EB/Google Drive/Projects/etc/SE PM"
path.data    = paste(path, "/DMC2015", sep = "")
path.source  = paste(path, "/Sources", sep = "")
path.results = paste(path, "/Estimated Models", sep = "")


model.par = function(cl = 6, models){

    c.acs = function(data, lev = NULL, model = NULL){
      denom       = mean(data$obs)
      crit        = ( abs(data$obs - data$pred) / denom )^2
      crit        = sum(crit)
      names(crit) = "DMC"
      return(crit)
    }

    model.save = function(model, data, pack){
        L = list(model = model, data = data)
        f = list.files(pattern = ".rds")
        if(length(f) != 0){
            f = as.numeric(gsub(".rds", "", f, fixed = TRUE))
            i = max(f) + 1
        }else{
            i = 1
        }
        
        saveRDS(L, file = paste(i, ".rds", sep = ""))
    }

    setwd(path)
    print("Prep Data")
    source("Data Preparation.R")

    data.train = data[data$trainingSetIndex == 1, ] 
    c.model    = models
    m          = length(c.model)
    c.fc       = trainControl(returnData = TRUE, savePredictions = TRUE,
                              number = 5, summaryFunction = c.acs)

    cl = makeCluster(cl)
    registerDoParallel(cl)

    print("Start Estimation")
    
    c.train = foreach(i = 1:m, .packages = pack)%dopar%{

        c.train.m = try(train(basketValue ~ ., data = data.train, method = c.model[i],
                              trControl = c.fc, tuneLength = 2,
                              maximize = FALSE, metric = "DMC"))
        c.train.m
    }

    stopCluster(cl)
    setwd(path.results)
    model.save(c.train, data)
}

# Use 6 processors


#loop
p    = 10
pack = c("earth", "elasticnet", "leaps", "kernlab", "ipred", "plyr", "rpart",
         "kknn", "nnet", "brnn", "frbs", "RSNNS", "foreach", "caret", "gbm",
         "randomForest", "RRF", "party", "doParallel", "data.table",
         "splitstackshape","stats","arules","klaR", "elasticnet")

foreach(j = 1:p, .packages = pack) %do% {
    model.par(cl     = 6,
              models = c("parRF", "RRF", "rf", "cforest", "ctree", "blackboost",
                         "RRFglobal", "mlp", "mlpWeightDecay", "gbm", "qrf",
                         "glmboost", "cubist", "svmRadialCost", "svmRadial"),
              pack   = pack)
}



