#Description:-------------------------------------------------------

# This script is for data exploration of Robbie Hember's climate rasters


#Packages:-------------------------------------------------------

library(raster)


#Location:-------------------------------------------------------

rswddir <- "M:/RasterDBs/BC1ha_rswd_mon_norm_1971to2000_si_hist"

setwd(rswddir)

#Import:

testBC1 <- raster("BC1ha_rswd_mon_norm_1971to2000_si_hist_c_6.tif")

rswd_list <- list.files(rswddir, pattern = ".tif$", full.names = TRUE)

rswd_stack <- stack(rswd_list)
getwd()
