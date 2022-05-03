# *****************************************************************************
# R Script implementing conventional random cross-validation.  
# Related to the manuscript "Dealing with clustered samples for assessing map 
# accuracy by cross-validation".
# Contact: Sytze de Bruin, Wageningen University, Laboratory of Geo-information
# Science and Remote Sensing, email: sytze.debruin@wur.nl
# May 3, 2022
# *****************************************************************************

# ****** load required libraries *******
library(ranger)
library(sperrorest)


# ************ GLOBALS ***************
samples   <- c("clusterMedium", "clusterStrong", "clusterGapped", "regular", 
               "simpleRandom")
infolder  <- "../samples"
outfolder <- "../CVresults"
startseed <- 1234567
n_CV      <- 100  # number of cross validation replications
n_samp    <- 100  # # number of sample replicates (for each design)


# create outfolders if they don't exist
if(!dir.exists(outfolder))
  dir.create(outfolder)

if(!dir.exists(paste0(outfolder, "/spatial")))
  dir.create(paste0(outfolder, "/spatial"))


# ************ FUNCTIONS ***************

err_fu <- function(obs, pred){
  rmse <- sqrt(mean((obs-pred)^2))
  muref <- mean(obs)
  SSR <- sum((obs - pred)^2)
  SST <- sum((obs - muref)^2)
  mec <- 1 - SSR/SST
  me <- mean(obs - pred)
  list(me = me, rmse = rmse, mec = mec)
}


predfun <- function(object, newdata){
  pred <- predict(object, newdata)
  pred[[1]]
}


spatialCV <- function(smpl, number, variate, seed){
  
  fname <- paste0(variate, "data", sprintf("%03d", number), ".Rdata")
  f_in <- file.path(infolder,smpl,fname)
  load(f_in)
  
  if(variate == "AGB"){
    fo <- as.formula(paste0("agb~", paste(names(AGBdata)[-1], collapse = "+")))
    tst <- sperrorest(fo, data=AGBdata, model_fun=ranger, 
                      model_args=list(respect.unordered.factors=TRUE),
                      pred_fun=predfun, 
                      smp_fun=partition_kmeans, coords=c("xcoord", "ycoord"),
                      smp_args=list(balancing_steps = 1, seed1=seed, 
                                    repetition=1:n_CV, iter.max = 50), 
                      err_fun = err_fu)
  } else{
    fo <- as.formula(paste0("ocs~", paste(names(OCSdata)[-1], collapse = "+")))
    tst <- sperrorest(fo, data=OCSdata, model_fun=ranger, 
                      model_args=list(respect.unordered.factors=TRUE),
                      pred_fun=predfun, 
                      smp_fun=partition_kmeans, coords=c("xcoord", "ycoord"),
                      smp_args=list(balancing_steps = 1, seed1=seed, 
                                    repetition=1:n_CV, iter.max = 50), 
                      err_fun = err_fu)
  }

  ME   <- tst$error_rep$test_me
  RMSE <- tst$error_rep$test_rmse
  MEC  <- tst$error_rep$test_mec
  rm(tst)
  
  fname <-  paste0(variate, "_", smpl, sprintf("%03d", number), ".Rdata")
  f_out <- file.path(outfolder, "spatial", fname)
  save(MEC, ME, RMSE, file=f_out)
  
}


# ************ CALL THE FUNCTIONS ************ 
for(smpl in samples){
  for(i in 1:n_samp){
    spatialCV(smpl, i, "AGB", startseed)
    spatialCV(smpl, i, "OCS", startseed)
  }
}

