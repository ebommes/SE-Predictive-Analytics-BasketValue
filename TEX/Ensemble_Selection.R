#####################################################
################# Ensemble Selection ################
#####################################################
# With this file, the ensemble selection for each 
# random partion is realized.
# The algorithm used for ensembling is the greedy
# optimization hill-climbing technique with bagging.
# The model prediction lists are loaded from 
# "/Predictions". A data frame including DMC measures 
# for each ensemble built from a partition is saved
# to "/Predictions".
#####################################################

pack = unique("compiler")

lapply(pack, function(x) if (!(x %in%
installed.packages())) {
  install.packages(x)
})

lapply(pack, library, character.only = TRUE)

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

greedOpt = function(X, Y, iter = 100L){
  
  # Initialize Variables
  N           = ncol(X)
  weights     = rep(0L, N)
  pred        = 0 * X
  sum.weights = 0L
  
  while(sum.weights < iter) {
    # No. of Models/Slots in Ensemble
    sum.weights   = sum.weights + 1L
    
    # "Trick": Compute Matrix with predictions 
    # of each Candidate-Ensemble
    pred          = (pred + X) * (1L / sum.weights)
    
    # Compute Prediction error of Candidate-Ensemble
    # Here: Use our Score Function?
    errors        = ACs(Y, pred)
    
    # Choose Candidate-Ensemble with smallest Error
    best          = which.min(errors)
    
    # increase Weight of best Model in Ensemble 
    # Model
    weights[best] = weights[best] + 1L
    
    # Correctly weight Prediction (and Use only 
    # chosen ensemble) for next step
    pred          = pred[, best] * sum.weights
  }
  
  # Return the weight of each of M Models in 
  # Ensemble Model
  return(weights / sum.weights)
}
# Compile Function
greedOptc = cmpfun(greedOpt)

# Function for bagged Ensemble Selection
BES = function(X, Y, b = 10L, p = 0.5, r = 100L){
  i = 0L
  N = nrow(X)
  M = ncol(X)
  W = matrix(rbinom(b * M, 1, p), ncol = M)
  
  while(i < b){
    i         = i + 1L
    ind       = which(W[i, ] == 1)
    W[i, ind] = W[i, ind] * 
    greedOptc(X[, ind], Y, r)
  }
  
  return(colSums(W)/b)
}
BES = cmpfun(BES)

# Set Paths
path         = "/Volumes/bommesel.hub/Projects/
Predictive Analytics"
path.data    = paste(path, "/DMC2015", sep = "")
path.source  = paste(path, "/Sources", sep = "")
path.results = paste(path, "/Estimated Models", 
sep = "")
path.pred    = paste(path, "/Predictions", sep = "")

# Ensemble setup (Settings for ES without Bagging)
b = 10L  # Number of Samples
p = 0.5  # Probability that a Model is selected as 
# Part of Sample
r = 500L # How often should be averaged per 
# Ensemble?


setwd(path.pred)

set = list.files(path.pred)
set = set[-which(set %in% c("EStrain.rds",
"EStest.rds","EStrainontest.rds"))]
set = substr(set, 1, 14 )

for(i in 1:length(set)){
	print(i)
	set.pred = 
    readRDS(paste(set, ".rds", sep = "")[i])

	### Run Ensemble selection on Training set -> 
    ### Evaluate with test and validation set
	train.pred = set.pred$model.train
	train.bv   = set.pred$bv.train

	test.pred  = set.pred$model.test
	test.bv    = set.pred$bv.test

	# Get Ensemble weights
	train.weight = 
    BES(train.pred, train.bv, b, p, r)
	test.weight  = 
    BES(test.pred,   test.bv, b, p, r)

	# Weight Predictions for validation set
	val.pred    = set.pred$model.val
	val.bv      = set.pred$bv.val
	val.n       = length(val.bv)

	# Multiply weights with model predictions
	
	test.es.train.pred = sweep(test.pred, MARGIN = 2,
    STATS = train.weight, FUN = "*")
	test.es.train.pred = rowSums(test.es.train.pred)

	val.es.train.pred = sweep(val.pred, MARGIN = 2, 
    STATS = train.weight, FUN = "*") 
	val.es.train.pred = rowSums(val.es.train.pred)

	val.es.test.pred  = sweep(val.pred, MARGIN = 2, 
    STATS = test.weight, FUN = "*") 
	val.es.test.pred  = rowSums(val.es.test.pred)

	# Assess model fit

	if(i == 1){
		AC.test.train = AC(test.bv, 
        test.es.train.pred)
		AC.val.train = AC(val.bv, val.es.train.pred)
		AC.val.test  = AC(val.bv, val.es.test.pred)
	}else{
		AC.test.train = c(AC.test.train,
        AC(test.bv,test.es.train.pred))
		AC.val.train = c(AC.val.train,
        AC(val.bv,val.es.train.pred))
		AC.val.test  = c(AC.val.test,  AC(val.bv,
        val.es.test.pred))
	}
}

saveRDS(AC.test.train, "EStrainontest.rds")
saveRDS(AC.val.train, "EStrain.rds")
saveRDS(AC.val.test,  "EStest.rds")