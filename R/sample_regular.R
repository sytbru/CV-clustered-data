# *****************************************************************************
# R Script for preparing systematic random samples for subsequent analysis  
# related to the manuscript "Dealing with clustered samples for assessing map 
# accuracy by cross-validation".
# Contact: Sytze de Bruin, Wageningen University, Laboratory of Geo-information
# Science and Remote Sensing, email: sytze.debruin@wur.nl
# May 3, 2022
# *****************************************************************************

# ****** load required library *******
library(terra)


# ************ GLOBALS ***************
infolder  <- "../data"
outfolder <- "../samples"
n_samp    <- 100  # number of sample replicates


# ********* load input data **********
# download data from https://doi.org/10.5281/zenodo.6513429

msk <- rast(file.path(infolder, "TOTmask.tif"))

AGBstack <- rast(file.path(infolder, "AGBstack.tif"))
OCSstack <- rast(file.path(infolder, "OCSstack.tif"))


# create outfolders if they don't exist
if(!dir.exists(outfolder))
  dir.create(outfolder)

if(!dir.exists(paste0(outfolder, "/regular")))
  dir.create(paste0(outfolder, "/regular"))


# ******* create the samples ********

# sample size over entire extent, aiming for 5000 points within the study area
# found by earlier iterations
esamp <- 12400  

set.seed(1234567)

for (i in 1:n_samp){
  extSmp <- spatSample(msk, esamp, method="regular", as.points=T)
  maxShft <- 11500 # min(distance(extSmp)) * 0.5
  shftSmp <- shift(extSmp, runif(1, -maxShft, maxShft), 
                   runif(1, -maxShft, maxShft))
  tst <- extract(msk, shftSmp)
  idx <- which(tst[,2] == 1)
  shftSmp <- shftSmp[idx,]
  
  AGBdata <- extract(AGBstack, shftSmp)
  OCSdata <- extract(OCSstack, shftSmp)
  AGBdata$ID <- NULL
  OCSdata$ID <- NULL
  AGBdata$glc2017 <- factor(AGBdata$glc2017, levels=1:8)
  OCSdata$glc2017 <- factor(OCSdata$glc2017, levels=1:8)
  
  fname <- paste0("AGBdata", sprintf("%03d", i), ".Rdata")
  save(AGBdata, file=file.path(outfolder, "regular", fname))
  fname <- paste0("OCSdata", sprintf("%03d", i), ".Rdata")
  save(OCSdata, file=file.path(outfolder, "regular", fname))
}
