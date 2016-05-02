#Required packages
library(rgdal)
library(sp)
library(raster)
library(snow)
library(spatial.tools)

#Set temp file... has to have >50gb of room
rasterOptions(tmpdir="H:/Sooke/tempfiles")

##################################################################################################
## SETUP ## Generates 30m raster of a specific forest district in Province of Saskatchewan
# Total batchcrop time at 30m on A125345 workstation (30 cluster): 7.226512 (setup+batchcrop)
##################################################################################################
#Subset Sask area to include only 'WCL Prince Albert FMA' forest district
beginCluster(30)
startTime <- Sys.time()
rootDir <- ("H:/Sooke")
rasterTemplate <- raster("H:/Sooke/input/BC1ha_tmean_mon_norm_1971to2000_si_hist/BC1ha_tmean_mon_norm_1971to2000_si_hist_c_1.tif") #land2005c6
rastersToCropDir <- ("H:/Sooke/input/BC1ha_tmean_mon_norm_1971to2000_si_hist")
WSA_polys <- readOGR(dsn = paste(rootDir, "reference", sep = "/"),
                      layer = "WSA_study_area") # add SK vector Sask province
WSA_polys <- spTransform(WSA_polys, crs(rasterTemplate)) # reproject Sask_area to Recliner inputs

sooke_area <- WSA_polys[WSA_polys$Short_Labe == 'Sooke',]


sooke_areaRaster <- rasterize(sooke_area, rasterTemplate, field=1) # rasterize Saskarea using 30 template
## This creates a bunch of temp files
sooke_areaRaster <- crop(sooke_areaRaster, sooke_area, snap='out') # remove NA areas from study area
sooke_areaRasterExpand <- buffer(sooke_areaRaster,doEdge=TRUE, width=100) # add pixels to outer edge to ensure full
#  coverage within province/forest district polygon
sooke_areaRaster <- projectRaster(sooke_areaRasterExpand, rasterTemplate)
sooke_areaRaster_100 <- sooke_areaRaster #set up Reference layer for BatchCrop
setwd(paste(rootDir, "reference", sep="/"))
writeRaster(sooke_areaRaster_100, "sooke_areaRaster_100.tif", overwrite=TRUE) # change name to reflect forest district (line 24)

endTime <- Sys.time()
elapsedTime <- endTime - startTime
# 4.526815 hours to here
###################################################################################################
# If SETUP is already completed, add study area raster to run BatchCrop on list of rasters
# Time for BatchCrop with 6 bands of LANDSAT rasters (e.g. 1 year's worth) :11.06133 hours
setwd("H:/Saskatchewan/bsmiley_work/Sask/Sask_area_datasets")
Crop30 <- raster("Crop30_PaFMA.tif") # add template raster (30m res)
OutPrj= "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
OutRes = 30
startTime <- Sys.time()
beginCluster(40)
# location of rasters to be clipped
setwd("H:/Saskatchewan/TestingRasters/ToClip")
#Run BatchCrop (For clarity, replace OutName with name of forest district clipped by)
BatchCrop(Reference=Crop30,OutName=PrinceAlbertFMA, OutPrj= "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 
+lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", OutRes=30)
endTime <- Sys.time()
elapsedTime_6 <- endTime - startTime
removeTmpFiles_edit(0)
###################################################################################################
##################################################################################################
#New and improved removeTmpFiles fun (old one was broken)
##################################################################################################
removeTmpFiles_edit<-function(h=24)
{
  warnopt <- getOption("warn")
  on.exit(options(warn = warnopt))
  tmpdir <- tmpDir(create = FALSE)
  if (!is.na(tmpdir)) {
    d <- (tmpdir)
    f <- list.files(path=d, pattern='[.]gr[di]', full.names=TRUE, 
                    include.dirs=TRUE)
    fin <- file.info(f)
    dif <- Sys.time() - fin$mtime
    dif <- as.numeric(dif, units = "hours")
    dif[is.na(dif)] <- h + 1
    f <- f[dif > h]
    if (length(f) > 1) {
      unlink(f, recursive = TRUE)
    }
  }
  options(warn = warnopt)
}
###################################################################################################
#BatchCrop is a function that takes a list of rasters, crops them to a reference study area
# and reprojects them. Below the function is a set up script which adds the rasterizes a reference
#layer from a shp file of Sask and masks out non-forest areas. Also below is the BatchCrop execution
#script
###################################################################################################
BatchCrop<-function(Reference,OutName,OutPrj,OutRes){
  filenames <- list.files(full.names=FALSE)   #Extract list of  file names from working directory
  library(raster) #Calls 'raster' library
  #Function 'f1' imports data listed in 'filenames' and assigns projection
  f1<-function(x,z) {
    y <- raster(x)
    projection(y) <- CRS(z)
    return(y)
  }
  import <- mclapply(filenames,f1,projection(Crop30))
  f2<-function(x,y) {
    x<-projectRaster(x, crs=OutPrj, res=OutRes, method="ngb")
    return(x)
  } 
  closeAllConnections()
  beginCluster(30)
  output <- mclapply(import,f2,OutPrj)
  multiply2<-function(x,y) {
    origin(y) <- 10 # give 30m reference same origin as X
    x <- x * y # this stays at 250m and change resolution to 30m when running BatchCrop
    #.... still will have extra area outside of Sask but this can be clipped after
    return(x)
  }  
  clipped <- mclapply(import,multiply2,Crop30)    #Clip AGAIN using 30m resolution
  #Use a 'for' loop to iterate writeRaster function for all cropped layers
  for(i in (1:max(length(filenames)))){
    writeRaster(clipped[[i]],paste(deparse(substitute(OutName)),filenames[i]), format='GTiff',
                datatype='INT2U')
  }
}