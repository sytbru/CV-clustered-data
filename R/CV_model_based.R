# *****************************************************************************
# R Script implementing homoscedastic model-based based cross-validation.  
# Related to the paper "Dealing with clustered samples for assessing map 
# accuracy by cross-validation".
# Contact: Sytze de Bruin, Wageningen University, Laboratory of Geo-information
# Science and Remote Sensing, email: sytze.debruin@wur.nl
# May 3, 2022
# *****************************************************************************

# ****** load required libraries *******
library(sf)
library(gstat)
library(terra)
library(ranger)


# ************ GLOBALS ***************

infolder1 <- "../data"
infolder2 <- "../CVresults/random"
outfolder <- "../CVresults/modelbased"
nsim <- 500 # number of sequential Gaussian simulations
i_CV <- 1:10 * 10 # cross validation replications analysed
startseed <- 1234567


# check whether infolder2 exists; if not, stop
if(!dir.exists(infolder2)){
  cat('First run "CV_random.R"\n\n')
  stop(paste("directory", infolder2, "does not exist")) 
}

# create outfolder if needed
if(!dir.exists(outfolder))
  dir.create(outfolder, recursive=T)


# ************ FUNCTION ***************

getVgm <- function(pts, i, variate){
  fo <- formula(paste0(names(pts)[i],"~1"))
  bnds <- c(1:5 * 5000, 3:20 * 10000)
  if(variate=="AGB"){
    vgmod <- vgm(200, "Exp", 40000, add.to = vgm(150, "Sph", 15000, 300))
  } else {
    vgmod <- vgm(10, "Exp", 20000, 12)
  }
  vg  <- variogram(fo, pts, boundaries=bnds)
  vgmod <- tryCatch(fit.variogram(vg, vgmod), warning = function(w) w)
  if (is(vgmod, "warning")){
    vgmod <- vgm(200, "Exp", 40000, 500)
    vgmod <- tryCatch(fit.variogram(vg, vgmod), warning = function(w) w)
    if (is(vgmod, "warning")){
      vgmod <- vgm(300, "Nug", 0)
      vgmod <- fit.variogram(vg, vgmod)
    }
  }
  vgmod$range[vgmod$range < 0] <- 1 # last resort
  return(list(vg, vgmod))
}



# ************ MAIN ************ 

# simulation grid (fixed)
agg_area <- rast(file.path(infolder1, "aggArea.tif"))
agg_pnts <- as.points(agg_area)
sf_pnts <- st_as_sf(agg_pnts)
rm(agg_area)

# covariates on the grid
AGBstack <- rast(file.path(infolder1, "AGBstack.tif"))
COVdata <- extract(AGBstack, agg_pnts)
COVdata$ID <- NULL
COVdata$agb <- NULL
rm(agg_pnts, AGBstack)

nanID <- which(apply(COVdata, 1, function(x) any(is.na(x))))
COVdata <- COVdata[-nanID,]
sf_pnts <- sf_pnts[-nanID,]

# find files with all design realizations
f_ins <- list.files(infolder2, glob2rx("pts*.Rdata"))

# loop over all files
for(f_in in f_ins){
  
  # retrieve strings for naming purposes
  lchar <- nchar(f_in)
  f_out <- substr(f_in, 4, lchar)
  variate <- substr(f_in, 4, 6)
  design <- substr(f_in, 8, lchar-9)
  number <- substr(f_in, lchar-8, lchar-6) 

  # load CV data
  load(file.path(infolder2, f_in))  #pts_df
  
  pts_df$x <- pts_df$x*1000
  pts_df$y <- pts_df$y*1000
  pts_df <- st_as_sf(x=pts_df, coords=c("x", "y"))
  
  st_crs(pts_df) <- st_crs(sf_pnts)
  
  # Predict variate at prediction grid locations
  # get sample data
  fsamp <- paste0(variate, "data", number, ".Rdata")
  load(file.path("../samples", design, fsamp))

  # fit RF on the entire sample
  set.seed(startseed)
  if(variate == "AGB"){
    RFmodel <- ranger(agb~., AGBdata, respect.unordered.factors=TRUE)
  } else{
    RFmodel <- ranger(ocs~., OCSdata, respect.unordered.factors=TRUE)
  }
  
  # predict on grid
  preds <- predict(RFmodel, COVdata)[[1]]

  # run the approach for a selection of folds set by i_CV
  RMSEs <- numeric()
  MECs <- numeric()
  vgs <- list()
  ilist <- 1
  
  for(icv in i_CV){
    
    # variogram modelling
    vg <- getVgm(pts_df, icv, variate)
    
    # kriging on the prediction grid (coarser than original map)
    fo <- formula(paste0(names(pts_df)[icv], "~1"))
    set.seed(startseed)
    resmaps <- krige(fo, pts_df, newdata=sf_pnts, nsim=nsim, model=vg[[2]], 
                     nmax=75)
    resmaps$geometry <- NULL
    
    refs <- resmaps + preds
    
    SST <- apply(refs, 2, function(x) sum((x - mean(x))^2))
    
    RMSEs <- rbind(RMSEs,
                   apply(resmaps, 2, function(x) sqrt(mean(x^2))))
    MECs  <- rbind(MECs,
                   1 - apply(resmaps, 2, 
                             function(x) sum(x^2))/SST)
    vgs[ilist] <- list(list(vg = vg[[1]], vgmod = vg[[2]]))
    ilist <- ilist + 1
  }
  
  save(RMSEs, MECs, vgs, RFmodel, file = file.path(outfolder, f_out))

}

