# *****************************************************************************
# R Script for preparing simple random samples (SRS) for subsequent analysis 
# related to the paper "Dealing with clustered samples for assessing map 
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

if(!dir.exists(paste0(outfolder, "/simpleRandom")))
  dir.create(paste0(outfolder, "/simpleRandom"))

# ******* create the samples ********
set.seed(1234567)

for (i in 1:n_samp){
  idx <- as.integer(spatSample(msk, 5000, method="random", 
                                na.rm=T, cells=T)[,1])
  AGBdata <- extract(AGBstack, idx)
  OCSdata <- extract(OCSstack, idx)
  AGBdata$glc2017 <- factor(AGBdata$glc2017, levels=1:8)
  OCSdata$glc2017 <- factor(OCSdata$glc2017, levels=1:8)
  
  fname <- paste0("AGBdata", sprintf("%03d", i), ".Rdata")
  save(AGBdata, file=file.path(outfolder, "simpleRandom", fname))
  fname <- paste0("OCSdata", sprintf("%03d", i), ".Rdata")
  save(OCSdata, file=file.path(outfolder, "simpleRandom", fname))
}
