##############################################################################
########################### Test Prediction ##################################
##############################################################################
# This file is used to carry out the predictions on test and validation set.
# For each trained model of a randomly partitioned data set stored in 
# "/Estimated Models", the basket value in test and validation set are 
# predicted. Finally, for each data set, a list is built which includes model 
# predictions on training, test and validation set as well es the basket 
# values from the DMC data.


pack1 = c("earth","elasticnet","leaps","kernlab","ipred",
  		  "plyr","rpart", "kknn", "nnet","brnn", "frbs",
  		  "RSNNS","foreach","caret","gbm","randomForest",
 		  "RRF","party", "quantregForest", "mboost", "Cubist")

pack2 = c("earth", "elasticnet", "leaps", "kernlab", "ipred", "plyr", "rpart",
          "kknn", "nnet", "brnn", "frbs", "RSNNS", "foreach", "caret", "gbm",
          "randomForest", "RRF", "party", "doParallel", "data.table",
          "splitstackshape", "stats", "arules", "klaR", "elasticnet")

pack = unique(c(pack1, pack2))

lapply(pack, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

lapply(pack, library, character.only = TRUE)

models = c("blackboost", "cforest", "ctree", "cubist", "gbm", "glmboost", "mlp", "mlpWeightDecay",
	       "qrf", "rf", "RRF","RRFglobal", "svmRadial","svmRadialCost")

ex     = c("set.dat.train", "set.dat.test", "set.dat.val", "models")


# Set Paths
path         = "/Volumes/bommesel.hub/Projects/Predictive Analytics"
path.data    = paste(path, "/DMC2015", sep = "")
path.source  = paste(path, "/Sources", sep = "")
path.results = paste(path, "/Estimated Models", sep = "")
path.pred    = paste(path, "/Predictions", sep = "")

# Basket values validation set
setwd(path.data)
bv.val = read.csv("basketvalues.csv", sep = ";", dec = ",")
bv.val = bv.val$x


# Model list and unique time stamps
set.est   = list.files(path.results)
set.ind   = summary(as.factor(substr(set.est, 1, 14 ))) == 15
set.compl = names(set.ind[set.ind == TRUE])

# Don't extract if we already have it
set.done  = list.files(path.pred)
set.done  = substr(set.done, 1, 14)
set.compl = set.compl[!set.compl %in% set.done]


fl = detectCores()
### Loop over completed estimations
for(i in 2:length(set.compl)){
	setwd(path.results)
	set           = set.compl[i]
	print(set)
	set.dat       = readRDS(paste(set, "data.rds", sep = ""))
	set.dat.train = set.dat[set.dat$trainingSetIndex ==  1, ]
	set.dat.test  = set.dat[set.dat$trainingSetIndex ==  0, ]
	set.dat.val   = set.dat[set.dat$trainingSetIndex == -1, ]

	cl = fl
	cl = makeCluster(cl)
	registerDoParallel(cl)

	L = foreach(j = 1:length(models), .packages = pack, .export = ex) %dopar% {
		model.name  = models[j]
		model.sav   = paste(set, model.name, ".rds", sep = "")
		model       = readRDS(model.sav)
		model.train = predict(model, newdata = set.dat.train)
		model.test  = predict(model, newdata = set.dat.test)
		model.val   = predict(model, newdata = set.dat.val)

		list(model.train = model.train, model.test = model.test, model.val = model.val)
	}
	stopCluster(cl)

	names(L)    = models
	model.train = do.call(cbind.data.frame, sapply(L, function(x) x[1]))
	model.test  = do.call(cbind.data.frame, sapply(L, function(x) x[2]))
	model.val   = do.call(cbind.data.frame, sapply(L, function(x) x[3]))

	L = list(model.train = model.train, model.test = model.test, model.val = model.val,
		bv.train = set.dat.train$basketValue, bv.val = bv.val,
		bv.test = set.dat.test$basketValue, models = models)

	setwd(path.pred)
	saveRDS(L, paste(set, ".rds", sep = ""))
}
