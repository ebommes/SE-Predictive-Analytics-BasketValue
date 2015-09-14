#####################################################
############ Prediction Extraction ##################
#####################################################
# This file loads the pre-calculated models from 
# "/Estimated Models" and extracts there predictions 
# on the training set. Extracted prediction are 
# saved to "/Predictions".
#####################################################

pack =  
c("earth","elasticnet","leaps","kernlab","ipred",
"plyr","rpart", "kknn", "nnet","brnn", "frbs",
"RSNNS","foreach","caret","gbm","randomForest",
"RRF","party", "Cubist")

for (p in pack) {
  if (!require(p, character.only = T)) 
{install.packages(p);
	library(p, character.only = T)}
}
rm(p,pack)


path = "~/Desktop/DMC/Estimated Models"
path.output = "~/Desktop/DMC/Predictions"

setwd(path)

#Model list and unique time stamps
files= list.files(path)
tmp_stamp = 
as.factor(unique(substr(files, 1, 14 )))

#use only stamps for complete model sets
for(n in 1:length(files)){
    for(i in 1:length(tmp_stamp)){
        freq = 
        count(grepl(tmp_stamp[tnp_stamp=i], files))
            if (freq[2,2]==15 & n==1){
              compl = 
              as.character(paste(tmp_stamp[
              tmp_stamp=i]))
            }
            if(freq[2,2]==15){
               compl2 = 
               as.character(paste(tmp_stamp[
               tmp_stamp=i]))
               compl = cbind(compl, compl2)}
    }
}
stamp= as.factor(unique(compl[1,]))

models = as.factor(c("blackboost.rds", 
"cforest.rds", "ctree.rds","cubist.rds","gbm.rds",
"glmboost.rds","mlp.rds","mlpWeightDecay.rds",
"qrf.rds", "rf.rds","RRF.rds","RRFglobal.rds", 
"svmRadial.rds","svmRadialCost.rds"))

setwd(path)

#predictions for models
for (i in 1:nlevels(stamp)){
    ID = stamp[stamp=i]
    data = readRDS(paste(as.character(ID), 
    "data.rds", sep = ""))
    train=data[data$trainingSetIndex==1,]
  
        for (n in 1:length(models)){
             name = paste(as.character(ID), 
             as.character(models[[n]]), sep = "")
             readRDS(name)
             
              if(n == 1 & i==1 ){
                   col = paste(substr(name, 15, 
                   nchar(name)-4), i, sep="_")
                   print(paste(Sys.time(), col, 
                   sep=" "))      
                   mod = readRDS(name)
                   tmp = data.frame(predict(mod, 
                   newdata=train))
                   tmpcol = as.vector(col)
               }
               else{
                   col = paste(substr(name, 15, 
                   nchar(name)-4), i, sep="_")
                   print(paste(Sys.time(), col, 
                   sep=" "))      
                   mod = readRDS(name)
                   tmp2 = data.frame(predict(mod, 
                   newdata=train))
                   tmp  = cbind(tmp, tmp2)
                   tmpcol2 = as.vector(col)
                   tmpcol = as.vector(c(tmpcol,
                   tmpcol2))
                   colnames(tmp) = tmpcol
               }
        }
}

setwd(path.output)
write.csv2(tmp, "predictions.csv", row.names = FALSE)

