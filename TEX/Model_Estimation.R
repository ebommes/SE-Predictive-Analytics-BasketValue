#####################################################
################# Model Estimation ##################
#####################################################
# This file carries out the model estimation.
# After calling Data Preparation.R, 14 different 
# prediction models are trained in parallel on the 
# randomly partitioned dataset and then saved to
# "/Estimated Models".
#####################################################

pack1 = 
c("earth","elasticnet","leaps","kernlab","ipred",
"plyr","rpart", "kknn", "nnet","brnn", "frbs",
"RSNNS","foreach","caret","gbm","randomForest",
"RRF","party", "quantregForest", "mboost", "Cubist")

pack2 = c("earth", "elasticnet", "leaps", "kernlab", 
"ipred", "plyr", "rpart","kknn", "nnet", "brnn", 
"frbs", "RSNNS", "foreach", "caret", "gbm",
"randomForest", "RRF", "party", "doParallel", 
"data.table","splitstackshape", "stats", "arules", 
"klaR", "elasticnet")

pack = unique(c(pack1, pack2))

lapply(pack, function(x) if (!(x %in% 
installed.packages())) {
  install.packages(x)
})

lapply(pack, library, character.only = TRUE)

# Set Paths
path         = "H:/Projects/Predictive Analytics"
path.data    = paste(path, "/DMC2015", sep = "")
path.source  = paste(path, "/Sources", sep = "")
path.results = paste(path, "/Estimated Models", 
sep = "")

# Here: Loop
for(iter in 1:50){
print(iter)
stempel = as.character(Sys.time())
stempel = gsub(":", "", stempel, fixed = TRUE)
stempel = gsub("-", "", stempel, fixed = TRUE)
stempel = gsub(" ", "", stempel, fixed = TRUE)
# Prep data
setwd(path)
print("Prep Data")
source("Data Preparation.R")
setwd(path.results)
saveRDS(data, file = paste(stempel, "data.rds", 
sep = ""))

models = c( "ctree", "blackboost", "RRFglobal", 
"mlp", "mlpWeightDecay", "gbm", "qrf", "glmboost", 
"cubist", "svmRadialCost", "svmRadial", "RRF", 
"rf", "cforest")

# models = c("blackboost", "mlp")

# DMC metric
c.acs = function(data, lev = NULL, model = NULL){
  denom       = mean(data$obs)
  crit        = ( abs(data$obs - data$pred) / 
  denom )^2
  crit        = sum(crit)
  names(crit) = "DMC"
  return(crit)
}

# Save model
model.save = function(model, data){
    L = list(model = model, data = data)
    f = list.files(pattern = ".rds")
    if(length(f) != 0){
        f = as.numeric(gsub(".rds", "", f, 
        fixed = TRUE))
        i = max(f) + 1
    }else{
        i = 1
    }
    
    saveRDS(L, file = paste(i, ".rds", sep = ""))
}

data.train = data[data$trainingSetIndex == 1, ] 
c.model    = models
m          = length(c.model)
c.fc       = trainControl(returnData = TRUE, 
savePredictions = TRUE, number = 5, 
summaryFunction = c.acs)

L = list()
print("Start Estimation")

#c.train = foreach(i = 1:m, .packages = pack)
%dopar%{for(i in 1:m){
    print(paste("Estimate model", c.model[i]))
    print(paste("     Start time", Sys.time()))
    cl = 50
    fl = makeCluster(cl)
    registerDoParallel(fl)
    c.train.m = try(train(basketValue ~ ., 
    data = data.train, method = c.model[i],
    trControl = c.fc, tuneLength = 2,
    maximize = FALSE, metric = "DMC"))
    stopCluster(fl)
    print(paste("     End time", Sys.time()))
    # L[i] = c.train.m
    saveRDS(c.train.m, file = paste(stempel, 
    c.model[i], ".rds", sep = ""))
}
}
