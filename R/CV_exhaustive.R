# *****************************************************************************
# R Script implementing exhaustive validation, which is used as the reference.  
# Related to the paper "Dealing with clustered samples for assessing map 
# accuracy by cross-validation".
# Contact: Sytze de Bruin, Wageningen University, Laboratory of Geo-information
# Science and Remote Sensing, email: sytze.debruin@wur.nl
# May 3, 2022
# *****************************************************************************

# ****** load required libraries *******
library(ranger)
library(terra)


# ************ GLOBALS ***************
samples   <- c("clusterMedium", "clusterStrong", "clusterGapped", "regular", 
               "simpleRandom")
infolder1 <- "../data"
infolder2 <- "../samples"
outfolder <- "../CVresults"
startseed <- 1234567
n_samp    <- 100  # number of sample replicates (for each design)


# create outfolders if they don't exist
if(!dir.exists(outfolder))
  dir.create(outfolder)

if(!dir.exists(paste0(outfolder, "/exhaustive")))
  dir.create(paste0(outfolder, "/exhaustive"))


# download data from https://doi.org/10.5281/zenodo.6513429
# ****** load input raster data ******
msk <- rast(file.path(infolder1, "TOTmask.tif"))
AGBstack <- rast(file.path(infolder1, "AGBstack.tif"))
OCSstack <- rast(file.path(infolder1, "OCSstack.tif"))

OCt <- rast(file.path(infolder1, "ocs.tif"))
AGt <- rast(file.path(infolder1, "agb.tif"))
OCS <- mask(OCt, msk, filename="tmpocs.tif", overwrite=T)
AGB <- mask(AGt, msk, filename="tmpagb.tif", overwrite=T)
rm(AGt, OCt)


# ************ FUNCTIONS ***************
predfun <- function(object, newdata){
  pred <- predict(object, newdata)
  pred[[1]]
}

mecfu <- function(ref, pred){
  muref <- global(ref, "mean", na.rm=T)[[1]]
  residsq <- (ref - pred)^2
  SSR <- global(residsq, "sum", na.rm=T)[[1]]
  rm(residsq)
  residsq <- (ref - muref)^2
  SST <- global(residsq, "sum", na.rm=T)[[1]]
  1 - SSR/SST
}

rmsefu <- function(ref, pred){
  residsq <- (ref - pred)^2
  sqrt(global(residsq, "mean", na.rm=T)[[1]])
}

mefu <- function(ref, pred){
  resmap <- ref - pred
  global(resmap, "mean", na.rm=T)[[1]]
}

exhaustive <- function(smpl, number, variate, seed){
  
  fname1 <- paste0(variate, "data", sprintf("%03d", number), ".Rdata")
  fname2 <- paste0(variate, smpl, sprintf("%03d", number), ".tif")
  
  f_in  <- file.path(infolder2,smpl,fname1)
  f_out <- file.path(outfolder, "exhaustive", fname2)
  
  load(f_in)
  
  set.seed(seed)
  
  if(variate == "AGB"){
    RFmodel <- ranger(agb~., AGBdata, 
                      respect.unordered.factors=TRUE)
    map  <- predict(AGBstack, RFmodel, predfun, filename=f_out, overwrite=T,
                    na.rm=T)
    ME   <- mefu(AGB, map)
    RMSE <- rmsefu(AGB, map)
    MEC  <- mecfu(AGB, map)
  } else {
    RFmodel <- ranger(ocs~., OCSdata, 
                      respect.unordered.factors=TRUE)
    map  <- predict(OCSstack, RFmodel, predfun, filename=f_out, overwrite=T,
                    na.rm=T)
    ME   <- mefu(OCS, map)
    RMSE <- rmsefu(OCS, map)
    MEC  <- mecfu(OCS, map)
  }
  
  fname <-  paste0(variate, "_", smpl, sprintf("%03d", number), ".Rdata")
  f_out <- file.path(outfolder,"exhaustive", fname)
  save(MEC, ME, RMSE, file=f_out)
}


# ************ CALL THE FUNCTIONS ************ 
for(smpl in samples){
  for(i in 1:n_samp){
    exhaustive(smpl, i, "AGB", startseed)
    exhaustive(smpl, i, "OCS", startseed)
  }
}
