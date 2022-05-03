# *****************************************************************************
# R Script exemplifying the creation of the figures used in the paper  
# "Dealing with clustered samples for assessing map accuracy by cross-
# validation". This script assumes (without checking) that the full set of 
# results has already been generated.
# Contact: Sytze de Bruin, Wageningen University, Laboratory of Geo-information
# Science and Remote Sensing, email: sytze.debruin@wur.nl
# May 3, 2022
# *****************************************************************************


# ************************************************************************
# ******************************* Figure 1 *******************************
# ************************************************************************

library(sf)
library(terra)
library(viridis)

infolder  <- "../data"
outfolder <- "../Figs"

if(!dir.exists(outfolder))
  dir.create(outfolder, recursive=T)

# *********** reference variables ***********

png(file.path(outfolder, "Fig1.png"), 1700, 600)
par(mfrow=c(1,2))

agbCols <- colorRampPalette(c("#954535", "#FFFF8F", "#228B22", "#355E3B"))
agb <- rast(file.path(infolder, "agb.tif"))
plot(agb, col=agbCols(100),
     mar=c(1, 1, 1, 6.5), plg=list(cex=1.75), axes = F, 
     range = c(0,420)) # pax=list(labels=F, tick=F),
text(4071500, 3400000, "AGB [Mg/ha]", cex=2)
text(2600000, 3400000, "(a)", cex=2.5)
lines(x=c(3711500, 3711500, 4211500, 4211500), 
      y=c(1550500, 1509500, 1509500, 1550500), lwd= 2)
text(3961500, 1620500, "500 km", cex=2)


ocsCols <- colorRampPalette(c("#FF3131", "#FBEC5D", "#50C878", "#008000"))
ocs <- rast(file.path(infolder, "ocs.tif"))
plot(ocs, col=ocsCols(100), mar=c(1, 1, 1, 6.5), plg=list(cex=1.75), axes=F,
     range= c(0, 155))
text(4071500, 3400000, "OCS [Mg/ha]", cex=2)
text(2600000, 3400000, "(b)", cex=2.5)
dev.off()

# ************************************************************************
# ******************************* Figure 2 *******************************
# ************************************************************************

library(terra)

msk <- rast(file.path(infolder, "TOTmask.tif"))

smpls <- c("simpleRandom", "regular", "clusterMedium", "clusterStrong",
           "clusterGapped")

png(file.path(outfolder, "fig2.png"), 2400, 1800)
par(mfrow=c(3,3))
j <- 1
for(smpl in smpls){
  if (smpl=="regular"){
    i <- 1
    subfig <- paste0("(", letters[j], ")")
    load(file.path("samples", smpl, paste0("AGBdata", 
                                           sprintf("%03d", i), ".Rdata")))
    pts <- cbind(AGBdata$xcoord * 1000, AGBdata$ycoord * 1000)
    plot(msk, col="light grey", legend=F, axes=F)
    points(pts, pch=16, cex=0.5)
    text(2650000, 3440000, subfig, cex= 5)
    j <- j + 1
  } else for(i in c(1,5)){
        subfig <- paste0("(", letters[j], ")")
        # fname <- paste0(smpl, sprintf("%02d", i), ".png")
        load(file.path("samples", smpl, paste0("AGBdata", 
                                               sprintf("%03d", i), ".Rdata")))
        pts <- cbind(AGBdata$xcoord * 1000, AGBdata$ycoord * 1000)
        # png(file.path(outfolder, "Fig2", fname), 800, 600)
        plot(msk, col="light grey", legend=F, axes=F)
        points(pts, pch=16, cex=0.5)
        text(2650000, 3440000, subfig, cex= 5)
        
        j <- j + 1
      }
}
dev.off()

# ************************************************************************
# ******************************* Figure 3 *******************************
# ************************************************************************

library(sf)
library(terra)
library(viridis)


# make palette AGB

agbCols <- colorRampPalette(c("#954535", "#FFFF8F", "#228B22", "#355E3B"))


# *********** predicted AGB ***********

# simple random sample
agb_sc1 <- rast("../CVresults/exhaustive/AGBsimpleRandom001.tif")

png(file.path(outfolder, "AGBpredSRS.png"), 850, 600)
plot(agb_sc1, col=agbCols(100), mar=c(1, 1, 1, 6.5), plg=list(cex=1.75), axes=F,
     range= c(0, 420))
text(2600000, 3400000, "(a)", cex=2.5)
dev.off()

# moderately clustered sample
agb_sc3 <- rast("../CVresults/exhaustive/AGBclusterMedium001.tif")

png(file.path(outfolder, "AGBpredMCL.png"), 850, 600)
plot(agb_sc3, col=agbCols(100), mar=c(1, 1, 1, 6.5), plg=list(cex=1.75), axes=F,
     range= c(0, 420))
text(2600000, 3400000, "(b)", cex=2.5)
dev.off()

# strongly clustered sample
agb_sc4 <- rast("../CVresults/exhaustive/AGBclusterStrong001.tif")

png(file.path(outfolder, "AGBpredSCL.png"), 850, 600)
plot(agb_sc4, col=agbCols(100), mar=c(1, 1, 1, 6.5), plg=list(cex=1.75), axes=F,
     range= c(0, 420))
text(2600000, 3400000, "(c)", cex=2.5)
dev.off()

# strongly clustered sample with gaps
agb_sc5 <- rast("../CVresults/exhaustive/OCSclusterGapped001.tif")

png(file.path(outfolder, "AGBpredGCL.png"), 850, 600)
plot(agb_sc5, col=agbCols(100), mar=c(1, 1, 1, 6.5), plg=list(cex=1.75), axes=F,
     range= c(0, 420))
text(2600000, 3400000, "(d)", cex=2.5)
dev.off()



# ************************************************************************
# ******************************* Figure 4 *******************************
# ************************************************************************

library(openxlsx)


infolder1 <- "../CVresults"
infolder2 <- "../material"

# create infolder2 if needed
if(!dir.exists(infolder2))
  dir.create(infolder2, recursive=T)

colnms <- c("variate", "design", "number", "RMSE", "MEC")
outtab <- data.frame(matrix(NA, 0, 5))
names(outtab) <- colnms

p <- file.path(infolder1, "exhaustive")
f_ins <- list.files(p, glob2rx("???_*.Rdata"))
for(f_in in f_ins){
  lchar <- nchar(f_in)
  variate <- substr(f_in, 1, 3)
  design <- substr(f_in, 5, lchar-9)
  number <- as.numeric(substr(f_in, lchar-8, lchar-6))
  load(file.path(p, f_in))
  MEC <- mean(MEC)
  RMSE <- mean(RMSE)
  
  newrow <- data.frame(variate = variate, design = design,
                       number = number, RMSE = RMSE, MEC = MEC)
  outtab <- rbind(outtab, newrow)
}

write.xlsx(outtab, file.path(infolder2, "exhaustive100.xlsx"), overwrite = T)

# ***************************************************
# ************ BOX PLOTS (EXHAUSTIVE) ***************
# ***************************************************

# get and convert data
intab <- read.xlsx(file.path(infolder2, "exhaustive100.xlsx"))
smtab <- read.xlsx(file.path(infolder2, "exhaustive_summary.xlsx"))
intab$design[intab$design == "simpleRandom"] <- "SRS"
smtab$design[smtab$design == "simpleRandom"] <- "SRS"
intab$design[intab$design == "regular"] <- "syst"
smtab$design[smtab$design == "regular"] <- "syst"
intab$design[intab$design == "clusterGapped"] <- "clustGap"
smtab$design[smtab$design == "clusterGapped"] <- "clustGap"
intab$design[intab$design == "clusterMedium"] <- "clustMed"
smtab$design[smtab$design == "clusterMedium"] <- "clustMed"
intab$design[intab$design == "clusterStrong"] <- "clustStr"
smtab$design[smtab$design == "clusterStrong"] <- "clustStr"
smpls <- c("SRS", "syst", "clustMed", "clustStr", "clustGap")
smtab$design <- factor(smtab$design, levels=smpls)
intab$design <- factor(intab$design, levels=smpls)

windows(12,8)
par(mfrow=c(2,2), mar=c(4, 3, 3, 1))

# RMSE
idx <- which(intab$variate == "AGB")
intab$design <- factor(intab$design, levels=smpls)
plot(intab$design[idx], intab$RMSE[idx], range=0, main="RMSE AGB [Mg/ha]", 
     xlab="", ylab="", cex.axis=1.2, cex.main=1.2, font.main=3)
idx <- which(smtab$variate == "AGB")
points(smtab$design[idx], smtab$RMSE[idx], col="black", bg="white", 
       cex=1.5, pch=21)
mtext("(a)", 3, adj = 0, padj = 0, cex=1.15, font=1)


idx <- which(intab$variate == "OCS")
intab$design <- factor(intab$design, levels=smpls)
plot(intab$design[idx], intab$RMSE[idx], range=0, main="RMSE OCS [Mg/ha]",
     xlab="", ylab="", cex.axis=1.2, cex.main=1.2)
idx <- which(smtab$variate == "OCS")
points(smtab$design[idx], smtab$RMSE[idx], col="black", bg="white", 
       cex=1.5, pch=21)
mtext("(b)", 3, adj = 0, padj = 0, cex=1.15, font=1)


# MEC
idx <- which(intab$variate == "AGB")
plot(intab$design[idx], intab$MEC[idx], range=0, main="MEC AGB [-]", 
     xlab="", ylab="", cex.axis=1.2, cex.main=1.2)
idx <- which(smtab$variate == "AGB")
points(smtab$design[idx], smtab$MEC[idx], col="black", bg="white", 
       cex=1.5, pch=21)
mtext("(c)", 3, adj = 0, padj = 0, cex=1.15, font=1)


idx <- which(intab$variate == "OCS")
intab$design <- factor(intab$design, levels=smpls)
plot(intab$design[idx], intab$MEC[idx], range=0, main="MEC OCS [-]", 
     xlab="", ylab="", cex.axis=1.2, cex.main=1.2)
idx <- which(smtab$variate == "OCS")
points(smtab$design[idx], smtab$MEC[idx], col="black", bg="white", 
       cex=1.5, pch=21)
mtext("(d)", 3, adj = 0, padj = 0, cex=1.15, font=1)


# ************************************************************************
# ******************************* Figure 5 *******************************
# ************************************************************************

library(spatstat)
library(terra)
library(sf)
library(viridis)

# ************ GLOBALS ***************

infolder1 <- "../data"
infolder2 <- "../CVresults/random"
outfolder <- "../Figs"

# ************ FUNCTIONS ***************

getDensity <- function(x, y, sf_pol, rsl){
  win <- as.owin(sf_pol)
  spp <- ppp(x, y, win)
  s <- bw.CvL(spp)
  den <- density.ppp(spp, eps=rsl, sigma=s, positive=T)
  denmat <- as.matrix.im(den)
  denrot <- transmat(denmat, from="spatstat", to="Europe")
  denext <- ext(round(c(den$xrange, den$yrange),0))
  denrast<- rast(denrot, crs=crs(sf_pol))
  ext(denrast) <- denext
  return(denrast)
}


# ************ CALL THE FUNCTION ************ 

studarea <- st_union(st_read(file.path(infolder1, "strata.shp")))
polbuf <- st_buffer(studarea, 5000)
polbuf <- st_buffer(polbuf,  -2500)
rm(studarea)

f_ins <- c("ptsAGB_simpleRandom001.Rdata", "ptsAGB_regular001.Rdata", 
           "ptsAGB_clusterMedium005.Rdata", "ptsAGB_clusterStrong005.Rdata",
           "ptsAGB_clusterGapped005.Rdata")

i <- 1
for(f_in in f_ins){
  lchar <- nchar(f_in)
  f_out1 <- paste0("dens_", substr(f_in, 8, lchar - 6), ".png")
  f_out2 <- paste0("dens_", substr(f_in, 8, lchar - 6), ".tif")
  # load CV data
  load(file.path(infolder2, f_in))  #pts_df

  dens <- getDensity(pts_df$x*1000, pts_df$y*1000,
                     polbuf, 2500)
  dens <- dens * 1e9
  writeRaster(dens, file.path(outfolder, f_out2), T)
  # dens <- rast(file.path(outfolder, f_out2))
  
  png(file.path(outfolder, f_out1), 800, 600)
  plot(dens, col=viridis(50), range=c(0.0, 10), axes=F, plg=list(cex=1.75))
  text(2750000, 3400000, paste0("(", letters[i], ")"), cex=2.5)
  dev.off()
  i <- i + 1
}


# ************************************************************************
# ******************************* Figure 6 *******************************
# ************************************************************************

infolder1 <- "../Figs"
infolder2 <- "../CVresults/random"
infolder3 <- "../CVresults/modelbased"
outfolder <- "../Figs"

i_CV <- 1:10 * 10



png(file.path(outfolder, "Fig6.png"), 1200, 750)
par(mar=c(5, 5,  2, 1), mfrow=c(2,2))

# ****** simple random ******

dens <- rast(file.path(infolder1, "dens_simpleRandom001.tif"))

load(file.path(infolder2, "ptsAGB_simpleRandom001.Rdata"))

# compute sampling intensity and point weights
pts_df$x <- pts_df$x*1000
pts_df$y <- pts_df$y*1000

pts_df$dens <- extract(dens, pts_df[, 1:2])[,2]

qs <- quantile(pts_df$dens, 1:99 * 0.01)

pts_df$intvl <- findInterval(pts_df$dens, qs)
avgdens <- aggregate(pts_df$dens, list(pts_df$intvl), mean)[,2]*1e-9

mn <- min(avgdens) * 0.67
mx <- max(avgdens) + mn * 0.5
xx <- seq(mn, mx, length.out=100)

plot(NULL, NULL, xlim=c(1.5e-10, 1e-8), ylim=c(8, 45), frame.plot=TRUE, 
     axes = F, main="Simple random sample", cex.main=2, ylab="sd(residual)",
     cex.lab=2, xlab="")
axis(side=1, labels=FALSE)
axis(side=2, labels=TRUE, cex.axis=2)

for(i in 1:10){
  j <- i_CV[i]
  sdresid <- aggregate(pts_df[,j+2], list(pts_df$intvl), sd)[,2]
  loessmod <- loess(sdresid~avgdens, degree=0, span=0.5, surface="direct")
  yy <- predict(loessmod, data.frame(avgdens=xx))
  points(avgdens, sdresid, pch=16, cex=0.8, col=rgb(0,0, 0, 0.2))
  lines(xx, yy, col=rgb(1,0.2, 0.2, 0.2), lwd=2)
}
mtext("(a)", 3, adj = 0, padj = 0, cex=2, font=1)


# ****** moderately clustered ******

dens <- rast(file.path(infolder1, "dens_clusterMedium005.tif"))

load(file.path(infolder2, "ptsAGB_clusterMedium005.Rdata"))

# compute sampling intensity and point weights
pts_df$x <- pts_df$x*1000
pts_df$y <- pts_df$y*1000

pts_df$dens <- extract(dens, pts_df[, 1:2])[,2]

qs <- quantile(pts_df$dens, 1:99 * 0.01)

pts_df$intvl <- findInterval(pts_df$dens, qs)
avgdens <- aggregate(pts_df$dens, list(pts_df$intvl), mean)[,2]*1e-9

mn <- min(avgdens) * 0.67
mx <- max(avgdens) + mn * 0.5
xx <- seq(mn, mx, length.out=100)

plot(NULL, NULL, xlim=c(1.5e-10, 1e-8), ylim=c(8, 45), frame.plot=TRUE, 
     axes = F, main="Moderately clustered sample", cex.main=2, xlab="", ylab="")
axis(side=1, labels=FALSE)
axis(side=2, labels=FALSE)

for(i in 1:10){
  j <- i_CV[i]
  sdresid <- aggregate(pts_df[,j+2], list(pts_df$intvl), sd)[,2]
  loessmod <- loess(sdresid~avgdens, degree=0, span=0.5, surface="direct")
  yy <- predict(loessmod, data.frame(avgdens=xx))
  points(avgdens, sdresid, pch=16, cex=0.8, col=rgb(0,0, 0, 0.2))
  lines(xx, yy, col=rgb(1,0.2, 0.2, 0.2), lwd=2)
}
mtext("(b)", 3, adj = 0, padj = 0, cex=2, font=1)


# ****** strongly clustered ******

dens <- rast(file.path(infolder1, "dens_clusterStrong005.tif"))

load(file.path(infolder2, "ptsAGB_clusterStrong005.Rdata"))

# compute sampling intensity and point weights
pts_df$x <- pts_df$x*1000
pts_df$y <- pts_df$y*1000

pts_df$dens <- extract(dens, pts_df[, 1:2])[,2]

qs <- quantile(pts_df$dens, 1:99 * 0.01)

pts_df$intvl <- findInterval(pts_df$dens, qs)
avgdens <- aggregate(pts_df$dens, list(pts_df$intvl), mean)[,2]*1e-9

mn <- min(avgdens) * 0.67
mx <- max(avgdens) + mn * 0.5
xx <- seq(mn, mx, length.out=100)

plot(NULL, NULL, xlim=c(1.5e-10, 1e-8), ylim=c(8, 45), frame.plot=TRUE, 
     axes = F, main="Strongly clustered sample", cex.main=2, cex.lab=2,
     xlab="mean sampling density", ylab="")
axis(side=1, labels=TRUE, cex.axis=2)
axis(side=2, labels=FALSE)

for(i in 1:10){
  j <- i_CV[i]
  sdresid <- aggregate(pts_df[,j+2], list(pts_df$intvl), sd)[,2]
  loessmod <- loess(sdresid~avgdens, degree=0, span=0.5, surface="direct")
  yy <- predict(loessmod, data.frame(avgdens=xx))
  points(avgdens, sdresid, pch=16, cex=0.8, col=rgb(0,0, 0, 0.2))
  lines(xx, yy, col=rgb(1,0.2, 0.2, 0.2), lwd=2)
}
mtext("(c)", 3, adj = 0, padj = 0, cex=2, font=1)


# ****** strongly clustered with gaps ******

dens <- rast(file.path(infolder1, "dens_clusterGapped05.tif"))

load(file.path(infolder2, "ptsAGB_clusterGapped005.Rdata"))

# compute sampling intensity and point weights
pts_df$x <- pts_df$x*1000
pts_df$y <- pts_df$y*1000

pts_df$dens <- extract(dens, pts_df[, 1:2])[,2]

qs <- quantile(pts_df$dens, 1:99 * 0.01)

pts_df$intvl <- findInterval(pts_df$dens, qs)
avgdens <- aggregate(pts_df$dens, list(pts_df$intvl), mean)[,2]*1e-9

mn <- min(avgdens) * 0.67
mx <- max(avgdens) + mn * 0.5
xx <- seq(mn, mx, length.out=100)

plot(NULL, NULL, xlim=c(1.5e-10, 1e-8), ylim=c(8, 45), frame.plot=TRUE, 
     axes = F, main="Strongly clustered, gapped sample", cex.main=2, xlab="",
     ylab="")
axis(side=1, labels=FALSE)
axis(side=2, labels=FALSE)

for(i in 1:10){
  j <- i_CV[i]
  sdresid <- aggregate(pts_df[,j+2], list(pts_df$intvl), sd)[,2]
  loessmod <- loess(sdresid~avgdens, degree=0, span=0.5, surface="direct")
  yy <- predict(loessmod, data.frame(avgdens=xx))
  points(avgdens, sdresid, pch=16, cex=0.8, col=rgb(0,0, 0, 0.2))
  lines(xx, yy, col=rgb(1,0.2, 0.2, 0.2), lwd=2)
}
mtext("(d)", 3, adj = 0, padj = 0, cex=2, font=1)

dev.off()



# ************************************************************************
# ******************************* Figure 7 *******************************
# ************************************************************************


library(gstat)

windows(12,8)
par(mfrow=c(2,3), mar=c(4.5, 4.5, 1.5, 1.75))
xx <- c(0.001, 1:200 * 1000)

# simple random
load(file.path(infolder, "modelbased/AGB_simpleRandom001.Rdata"))

plot(NULL, NULL, xlim=c(0, 200000), ylim=c(0, 750), xlab="", 
     ylab = "semivariance", las=1, xaxs="i", main="Simple random",
     cex.axis=1.2, cex.lab=1.2, cex.main= 1.2)

for(i in 1:10){
  vg  <- vgs[[i]][[1]]
  mdl <- vgs[[i]][[2]]
  lines(xx, gstat::variogramLine(mdl, dist_vector = xx)$gamma, 
        col=rgb(0.8,0, 0, 0.2), lwd=2)
  points(vg$dist, vg$gamma, pch=16, cex=0.7, col=rgb(0,0, 0, 0.2))
}
mtext("(a)", 3, adj = 0, padj = 0, cex=1.15, font=1)

# systematic
load(file.path(infolder, "modelbased/AGB_regular001.Rdata"))

plot(NULL, NULL, xlim=c(0, 200000), ylim=c(0, 750), xlab="", 
     ylab = "", las=1, xaxs="i", main="Systematic",
     cex.axis=1.2, cex.lab=1.2, cex.main= 1.2)

for(i in 1:10){
  vg  <- vgs[[i]][[1]]
  mdl <- vgs[[i]][[2]]
  lines(xx, gstat::variogramLine(mdl, dist_vector = xx)$gamma, 
        col=rgb(0.8,0, 0, 0.2), lwd=2)
  points(vg$dist, vg$gamma, pch=16, cex=0.7, col=rgb(0,0, 0, 0.2))
}
mtext("(b)", 3, adj = 0, padj = 0, cex=1.15, font=1)


# moderately clustered
load(file.path(infolder, "modelbased/AGB_clusterMedium005.Rdata"))

plot(NULL, NULL, xlim=c(0, 200000), ylim=c(0, 750), xlab="distance [m]", 
     ylab = "", las=1, xaxs="i", main="Moderately clustered",
     cex.axis=1.2, cex.lab=1.2, cex.main= 1.2)

for(i in 1:10){
  vg  <- vgs[[i]][[1]]
  mdl <- vgs[[i]][[2]]
  lines(xx, gstat::variogramLine(mdl, dist_vector = xx)$gamma, 
        col=rgb(0.8,0, 0, 0.2), lwd=2)
  points(vg$dist, vg$gamma, pch=16, cex=0.7, col=rgb(0,0, 0, 0.2))
}
mtext("(c)", 3, adj = 0, padj = 0, cex=1.15, font=1)


# strongly clustered
load(file.path(infolder, "modelbased/AGB_clusterStrong005.Rdata"))

plot(NULL, NULL, xlim=c(0, 200000), ylim=c(0, 750), xlab="", 
     ylab = "", las=1, xaxs="i", main="Strongly clustered",
     cex.axis=1.2, cex.lab=1.2, cex.main= 1.2)

for(i in 1:10){
  vg  <- vgs[[i]][[1]]
  mdl <- vgs[[i]][[2]]
  lines(xx, gstat::variogramLine(mdl, dist_vector = xx)$gamma, 
        col=rgb(0.8,0, 0, 0.2), lwd=2)
  points(vg$dist, vg$gamma, pch=16, cex=0.7, col=rgb(0,0, 0, 0.2))
}
mtext("(d)", 3, adj = 0, padj = 0, cex=1.15, font=1)

# strongly clustered, gapped
load(file.path(infolder, "modelbased/AGB_clusterGapped005.Rdata"))

plot(NULL, NULL, xlim=c(0, 200000), ylim=c(0, 750), xlab="", 
     ylab = "", las=1, xaxs="i", main="Strongly clustered, gapped",
     cex.axis=1.2, cex.lab=1.2, cex.main= 1.2)

for(i in 1:10){
  vg  <- vgs[[i]][[1]]
  mdl <- vgs[[i]][[2]]
  lines(xx, gstat::variogramLine(mdl, dist_vector = xx)$gamma, 
        col=rgb(0.8,0, 0, 0.2), lwd=2)
  points(vg$dist, vg$gamma, pch=16, cex=0.7, col=rgb(0,0, 0, 0.2))
}
mtext("(e)", 3, adj = 0, padj = 0, cex=1.15, font=1)



# ************************************************************************
# ***************************** Figure 7 OCS *****************************
# ************************************************************************

library(gstat)

windows(12,8)
par(mfrow=c(2,3), mar=c(4.5, 4.5, 1.5, 1.75))
xx <- c(0.001, 1:200 * 1000)

# simple random
load(file.path(infolder, "modelbased/OCS_simpleRandom001.Rdata"))

plot(NULL, NULL, xlim=c(0, 200000), ylim=c(0, 45), xlab="", 
     ylab = "semivariance", las=1, xaxs="i", main="Simple random",
     cex.axis=1.2, cex.lab=1.2, cex.main= 1.2)

for(i in 1:10){
  vg  <- vgs[[i]][[1]]
  mdl <- vgs[[i]][[2]]
  lines(xx, gstat::variogramLine(mdl, dist_vector = xx)$gamma, 
        col=rgb(0.8,0, 0, 0.2), lwd=2)
  points(vg$dist, vg$gamma, pch=16, cex=0.7, col=rgb(0,0, 0, 0.2))
}
mtext("(a)", 3, adj = 0, padj = 0, cex=1.15, font=1)

# systematic
load(file.path(infolder, "modelbased/OCS_regular001.Rdata"))

plot(NULL, NULL, xlim=c(0, 200000), ylim=c(0, 45), xlab="", 
     ylab = "", las=1, xaxs="i", main="Systematic",
     cex.axis=1.2, cex.lab=1.2, cex.main= 1.2)

for(i in 1:10){
  vg  <- vgs[[i]][[1]]
  mdl <- vgs[[i]][[2]]
  lines(xx, gstat::variogramLine(mdl, dist_vector = xx)$gamma, 
        col=rgb(0.8,0, 0, 0.2), lwd=2)
  points(vg$dist, vg$gamma, pch=16, cex=0.7, col=rgb(0,0, 0, 0.2))
}
mtext("(b)", 3, adj = 0, padj = 0, cex=1.15, font=1)


# moderately clustered
load(file.path(infolder, "modelbased/OCS_clusterMedium005.Rdata"))

plot(NULL, NULL, xlim=c(0, 200000), ylim=c(0, 45), xlab="distance [m]", 
     ylab = "", las=1, xaxs="i", main="Moderately clustered",
     cex.axis=1.2, cex.lab=1.2, cex.main= 1.2)

for(i in 1:10){
  vg  <- vgs[[i]][[1]]
  mdl <- vgs[[i]][[2]]
  lines(xx, gstat::variogramLine(mdl, dist_vector = xx)$gamma, 
        col=rgb(0.8,0, 0, 0.2), lwd=2)
  points(vg$dist, vg$gamma, pch=16, cex=0.7, col=rgb(0,0, 0, 0.2))
}
mtext("(c)", 3, adj = 0, padj = 0, cex=1.15, font=1)


# strongly clustered
load(file.path(infolder, "modelbased/OCS_clusterStrong005.Rdata"))

plot(NULL, NULL, xlim=c(0, 200000), ylim=c(0, 45), xlab="", 
     ylab = "", las=1, xaxs="i", main="Strongly clustered",
     cex.axis=1.2, cex.lab=1.2, cex.main= 1.2)

for(i in 1:10){
  vg  <- vgs[[i]][[1]]
  mdl <- vgs[[i]][[2]]
  lines(xx, gstat::variogramLine(mdl, dist_vector = xx)$gamma, 
        col=rgb(0.8,0, 0, 0.2), lwd=2)
  points(vg$dist, vg$gamma, pch=16, cex=0.7, col=rgb(0,0, 0, 0.2))
}
mtext("(d)", 3, adj = 0, padj = 0, cex=1.15, font=1)

# strongly clustered, gapped
load(file.path(infolder, "modelbased/OCS_clusterGapped005.Rdata"))

plot(NULL, NULL, xlim=c(0, 200000), ylim=c(0, 45), xlab="", 
     ylab = "", las=1, xaxs="i", main="Strongly clustered, gapped",
     cex.axis=1.2, cex.lab=1.2, cex.main= 1.2)

for(i in 1:10){
  vg  <- vgs[[i]][[1]]
  mdl <- vgs[[i]][[2]]
  lines(xx, gstat::variogramLine(mdl, dist_vector = xx)$gamma, 
        col=rgb(0.8,0, 0, 0.2), lwd=2)
  points(vg$dist, vg$gamma, pch=16, cex=0.7, col=rgb(0,0, 0, 0.2))
}
mtext("(e)", 3, adj = 0, padj = 0, cex=1.15, font=1)


# ************************************************************************
# ******************************* Figure 8 *******************************
# ************************************************************************

windows(12,8)
par(mfrow=c(2,3), mar=c(4.5, 4.5, 1.5, 1.75))
xx <- c(0.001, 1:200 * 1000)

# simple random
load(file.path(infolder, "heteroscedastic/AGB_simpleRandom001.Rdata"))

plot(NULL, NULL, xlim=c(0, 200000), ylim=c(0, 1.25), xlab="", 
     ylab = "semivariance", las=1, xaxs="i", main="Simple random",
     cex.axis=1.2, cex.lab=1.2, cex.main= 1.2)

for(i in 1:10){
  vg  <- vgs[[i]][[1]]
  mdl <- vgs[[i]][[2]]
  lines(xx, gstat::variogramLine(mdl, dist_vector = xx)$gamma, 
        col=rgb(0.8,0, 0, 0.2), lwd=2)
  points(vg$dist, vg$gamma, pch=16, cex=0.7, col=rgb(0,0, 0, 0.2))
}
mtext("(a)", 3, adj = 0, padj = 0, cex=1.15, font=1)


# systematic
load(file.path(infolder, "heteroscedastic/AGB_regular001.Rdata"))

plot(NULL, NULL, xlim=c(0, 200000), ylim=c(0, 1.25), xlab="", 
     ylab = "", las=1, xaxs="i", main="Systematic",
     cex.axis=1.2, cex.lab=1.2, cex.main= 1.2)

for(i in 1:10){
  vg  <- vgs[[i]][[1]]
  mdl <- vgs[[i]][[2]]
  lines(xx, gstat::variogramLine(mdl, dist_vector = xx)$gamma, 
        col=rgb(0.8,0, 0, 0.2), lwd=2)
  points(vg$dist, vg$gamma, pch=16, cex=0.7, col=rgb(0,0, 0, 0.2))
}
mtext("(b)", 3, adj = 0, padj = 0, cex=1.15, font=1)


# moderately clustered
load(file.path(infolder, "heteroscedastic/AGB_clusterMedium005.Rdata"))

plot(NULL, NULL, xlim=c(0, 200000), ylim=c(0, 1.25), xlab="distance [m]", 
     ylab = "", las=1, xaxs="i", main="Moderately clustered",
     cex.axis=1.2, cex.lab=1.2, cex.main= 1.2)

for(i in 1:10){
  vg  <- vgs[[i]][[1]]
  mdl <- vgs[[i]][[2]]
  lines(xx, gstat::variogramLine(mdl, dist_vector = xx)$gamma, 
        col=rgb(0.8,0, 0, 0.2), lwd=2)
  points(vg$dist, vg$gamma, pch=16, cex=0.7, col=rgb(0,0, 0, 0.2))
}
mtext("(c)", 3, adj = 0, padj = 0, cex=1.15, font=1)

# strongly clustered
load(file.path(infolder, "heteroscedastic/AGB_clusterStrong005.Rdata"))

plot(NULL, NULL, xlim=c(0, 200000), ylim=c(0, 1.25), xlab="", 
     ylab = "", las=1, xaxs="i", main="Strongly clustered",
     cex.axis=1.2, cex.lab=1.2, cex.main= 1.2)

for(i in 1:10){
  vg  <- vgs[[i]][[1]]
  mdl <- vgs[[i]][[2]]
  lines(xx, gstat::variogramLine(mdl, dist_vector = xx)$gamma, 
        col=rgb(0.8,0, 0, 0.2), lwd=2)
  points(vg$dist, vg$gamma, pch=16, cex=0.7, col=rgb(0,0, 0, 0.2))
}
mtext("(d)", 3, adj = 0, padj = 0, cex=1.15, font=1)


# strongly clustered, gapped sample
load(file.path(infolder, "heteroscedastic/AGB_clusterGapped005.Rdata"))

plot(NULL, NULL, xlim=c(0, 200000), ylim=c(0, 1.25), xlab="", 
     ylab = "", las=1, xaxs="i", main="Strongly clustered, gapped",
     cex.axis=1.2, cex.lab=1.2, cex.main= 1.2)

for(i in 1:10){
  vg  <- vgs[[i]][[1]]
  mdl <- vgs[[i]][[2]]
  lines(xx, gstat::variogramLine(mdl, dist_vector = xx)$gamma, 
        col=rgb(0.8,0, 0, 0.2), lwd=2)
  points(vg$dist, vg$gamma, pch=16, cex=0.7, col=rgb(0,0, 0, 0.2))
}
mtext("(e)", 3, adj = 0, padj = 0, cex=1.15, font=1)


# ************************************************************************
# ******************************* Figure 9 *******************************
# ************************************************************************
library(openxlsx)

infolder  <- "../CVresults"
outfolder <- "../material"

mets <- c("exhaustive", "random", "spatial", "intensity",
          "modelbased", "heteroscedastic")

colnms <- c("method", "variate", "design", "number", "RMSE", "MEC")
outtab <- data.frame(matrix(NA, 0, 10))
names(outtab) <- colnms

for(m in mets){
  p <- file.path(infolder, m)
  f_ins <- list.files(p, glob2rx("???_*.Rdata"))
  for(f_in in f_ins){
    lchar <- nchar(f_in)
    variate <- substr(f_in, 1, 3)
    design <- substr(f_in, 5, lchar-9)
    number <- as.numeric(substr(f_in, lchar-8, lchar-6))
    load(file.path(p, f_in))
    if(m == "modelbased" | m == "heteroscedastic"){
      MEC    <- mean(MECs)
      RMSE   <- mean(RMSEs)
    } else{
      if(length(MEC) > 1){
        MEC  <- mean(MEC)
        RMSE <- mean(RMSE)
      }
    }
    
    newrow <- data.frame(method = m, variate = variate, design = design,
                         number = number, RMSE = RMSE, MEC = MEC)
    outtab <- rbind(outtab, newrow)
  }
}

write.xlsx(outtab, file.path(outfolder, "outtab100.xlsx"), overwrite = T)


# ***************************************************
# ******** PLOTS ACCURACY METRICS CV METHODS ********
# ***************************************************

outtab <- read.xlsx(file.path(outfolder, "outtab100.xlsx"))
outtab$methodID <- with(outtab, ifelse(method == "random", 3, 
                                       ifelse(method == "spatial", 8,
                                       ifelse(method == "intensity", 13,
                                       ifelse(method == "modelbased", 18,
                                       ifelse(method == "heteroscedastic", 23, 
                                       0))))))

# relative RMSE & MEC
outtab$rRMSE <- NA
outtab$rMEC  <- NA
for(variate in c("AGB", "OCS")){
  for(design in unique(outtab$design)){
    for(number in 1:100){
      idx1 <- which(outtab$design == design & outtab$variate == variate & 
                      outtab$number == number)
      idx2 <- which(outtab$design == design & outtab$variate == variate & 
                      outtab$methodID == 0 & outtab$number == number)
      
      outtab$rRMSE[idx1] <- 100 * (outtab$RMSE[idx1] - outtab$RMSE[idx2])/
        outtab$RMSE[idx2]
      outtab$rMEC[idx1] <- 100 * (outtab$MEC[idx1] - outtab$MEC[idx2])/
        outtab$MEC[idx2]
    }
  }
}

idx  <- which(outtab$design ==  "simpleRandom")
outtab$methodID[idx] <- outtab$methodID[idx] - 2

idx  <- which(outtab$design ==  "regular")
outtab$methodID[idx] <- outtab$methodID[idx] - 1

# idx  <- which(outtab$design ==  "clusterMedium")
# outtab$methodID[idx] <- outtab$methodID[idx]

idx  <- which(outtab$design ==  "clusterStrong")
outtab$methodID[idx] <- outtab$methodID[idx] + 1

idx  <- which(outtab$design ==  "clusterGapped")
outtab$methodID[idx] <- outtab$methodID[idx] + 2

xx <- c(1:5, 6:10, 11:15, 16:20, 21:25)


windows(12,8)
par(mfrow=c(2,2), cex.axis=1.1, cex.lab=1.2, mar=c(4.5, 4.5, 1.5, 1))
cols <- rep(c("white", "brown", "pink", "orange", "lightgreen"), 4)

# ******** AGB RMSE ********
boxplot(rRMSE~methodID, data=outtab, 
        subset = which(outtab$variate == "AGB" & outtab$methodID > 0.5),
        xlab="", ylab = "relative RMSE [%]", col=cols, range=0, xaxt="n",
        main = "AGB", boxwex= 0.8, ylim=c(-74, 62))
idx <- which(outtab$variate == "AGB" & outtab$methodID > 0.5)
yy <- aggregate(outtab$rRMSE[idx], 
                by=list(outtab$methodID[idx]), mean)$x
abline(v = c(5.5, 10.5, 15.5, 20.5), lty=3, lwd = 3, col="lightgrey")
abline(h = 0, lty=2, col="red", lwd=2)

points(xx, yy, col="black", bg=cols, lwd=1.5,
       cex=1.2, pch=21)

axis(side=1, at=c(3, 8, 13, 18, 23), cex=1.2,
     labels=c("conventional", "spatial", "weighted", "hom.sced.", 
              "het.sced."))

mtext("(a)", 3, adj = 0, padj = 0, cex=1.15, font=1)


# ******** OCS RMSE ********

boxplot(rRMSE~methodID, data=outtab, 
        subset = which(outtab$variate == "OCS" & outtab$methodID > 0.5),
        xlab="", ylab = "relative RMSE [%]", col=cols, range=0, xaxt="n",
        main = "OCS", boxwex= 0.8, ylim=c(-74, 62))
idx <- which(outtab$variate == "OCS" & outtab$methodID > 0.5)
yy <- aggregate(outtab$rRMSE[idx], 
                by=list(outtab$methodID[idx]), mean)$x
abline(v = c(5.5, 10.5, 15.5, 20.5), lty=3, lwd = 3, col="lightgrey")
abline(h = 0, lty=2, col="red", lwd=2)

points(xx, yy, col="black", bg=cols, lwd=1.5,
       cex=1.2, pch=21)

axis(side=1, at=c(3, 8, 13, 18, 23), cex=1.2,
     labels=c("conventional", "spatial", "weighted", "hom.sced.", 
              "het.sced."))

mtext("(b)", 3, adj = 0, padj = 0, cex=1.15, font=1)

legend("topright", c("SRS", "syst", "clustMed",
                     "clustStr", "clustGap"), fill=cols, 
       cex = 1.1, bg="white")


# ******** AGB MEC ********
boxplot(rMEC~methodID, data=outtab, 
        subset = which(outtab$variate == "AGB" & 
                         outtab$methodID > 0.5),  
        xlab="", ylab = "relative MEC [%]",
        col=cols, range=0, xaxt="n",
        at = xx, main = "AGB",
        boxwex= 0.8, ylim=c(-49,40))
idx <- which(outtab$variate == "AGB" & outtab$methodID > 0.5)
yy <- aggregate(outtab$rMEC[idx], 
                by=list(outtab$methodID[idx]), mean)$x
abline(v = c(5.5, 10.5, 15.5, 20.5), lty=3, lwd = 3, col="lightgrey")
abline(h = 0, lty=2, col="red", lwd=2)

points(xx, yy, col="black", bg=cols, lwd=1.5,
       cex=1.2, pch=21)

axis(side=1, at=c(3, 8, 13, 18, 23), cex=1.2,
     labels=c("conventional", "spatial", "weighted", "hom.sced.", 
              "het.sced."))

mtext("(c)", 3, adj = 0, padj = 0, cex=1.15, font=1)


# ******** OCS MEC ********
boxplot(rMEC~methodID, data=outtab, 
        subset = which(outtab$variate == "OCS" & 
                         outtab$methodID > 0.5),  
        xlab="", ylab = "relative MEC [%]",
        col=cols, range=0, xaxt="n",
        at = xx, main = "OCS",
        boxwex= 0.8, ylim=c(-49,40))
idx <- which(outtab$variate == "OCS" & outtab$methodID > 0.5)
yy <- aggregate(outtab$rMEC[idx], 
                by=list(outtab$methodID[idx]), mean)$x
abline(v = c(5.5, 10.5, 15.5, 20.5), lty=3, lwd = 3, col="lightgrey")
abline(h = 0, lty=2, col="red", lwd=2)

points(xx, yy, col="black", bg=cols, lwd=1.5,
       cex=1.2, pch=21)

axis(side=1, at=c(3, 8, 13, 18, 23), cex=1.2,
     labels=c("conventional", "spatial", "weighted", "hom.sced.", 
              "het.sced."))

mtext("(d)", 3, adj = 0, padj = 0, cex=1.15, font=1)


