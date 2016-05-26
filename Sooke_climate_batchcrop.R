#This script has 3 components:
# 1) Creates a study area raster to the extent selected using the raster parameters 
#    (resolution, origin, projection, etc.) of an input raster. It also adds an extra 
#    boundary pixel to ensure there are no gaps between the study area extent and the 
#    Clipped rasters. This raster is later used to clip a list of input rasters.

# 2) Run BatchCrop script
#     This section sets input and output folders and parameters and runs the two
#     functions in section 3

# 3) Contains two functions:
#     a) a function embedded in (b) function that removes large temporary raster files
#      created during processing
#     b) the BatchCrop function that reprojects and crops the list of rasters to the
#      study area raster. 

#Required packages
library(rgdal)
library(sp)
library(raster)
library(snow)
library(spatial.tools)

#Set temp file... has to have >50gb of room
rasterOptions(tmpdir="H:/Sooke/temp_files")

#Identify input/output-----------------------
beginCluster(30)
startTime <- Sys.time()
rootDir <- ("H:/Sooke")
rasterTemplate <- raster("H:/Sooke/input/test/BC1ha_rswd_mon_norm_1971to2000_si_hist_c_1.tif")
rastersToCropDir <- ("H:/Sooke/input/test")
WSA_polys <- readOGR(dsn = paste(rootDir, "reference", sep = "/"),
                      layer = "WSA_study_areas")

#sync the projections of the study area and rasters to be clipped------------
WSA_polys <- spTransform(WSA_polys, crs(rasterTemplate)) # reproject Sask_area to Recliner inputs

sooke_area <- WSA_polys[WSA_polys$Short_Labe == 'Sooke',]


sooke_areaRaster <- rasterize(sooke_area, rasterTemplate) # rasterize Saskarea using 30 template
## This creates a bunch of temp files
sooke_areaRaster <- crop(sooke_areaRaster, sooke_area, snap='near') # remove NA areas from study area
sooke_areaRaster2 <- extend(sooke_areaRaster, 1, value=NA)
sooke_areaRasterExpand <- buffer(sooke_areaRaster2,doEdge=TRUE, width=200) # add pixels to outer edge to ensure full
#  coverage within province/forest district polygon
sooke_areaRaster_1ha <- sooke_areaRasterExpand #set up Reference layer for BatchCrop
setwd(paste(rootDir, "reference", sep="/"))
writeRaster(sooke_areaRaster_1ha, "sooke_areaRaster_1ha.tif", overwrite=TRUE) # change name to reflect forest district (line 24)

endTime <- Sys.time()
templateTime <- endTime - startTime
# 4.526815 hours to here
###################################################################################################
# If SETUP is already completed, add study area raster to run BatchCrop on list of rasters
# Time for BatchCrop with 6 bands of LANDSAT rasters (e.g. 1 year's worth) :11.06133 hours
rootDir <- ("H:/Sooke")
rastersToCropDir <- ("H:/Sooke/input/test")
outdir <- ("H:/Sooke/output/test/")
setwd(paste(rootDir, "reference", sep="/"))
studyArea <- raster("sooke_areaRaster_1ha.tif") # add template raster (30m res)
OutPrj= "+proj=aea +lat_1=50 +lat_2=58.5
          +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 
          +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
OutRes = 100
startTime <- Sys.time()
beginCluster(40)
# location of rasters to be clipped
setwd(rastersToCropDir)
startTime <- Sys.time()
#Run BatchCrop (For clarity, replace OutName with name of forest district clipped by)
BatchCrop(Reference=studyArea,OutName=Sooke, OutPrj= "+proj=aea +lat_1=50 +lat_2=58.5
          +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 
          +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", OutRes=100)
endTime <- Sys.time()
Croptime_1 <- endTime - startTime
# removeTmpFiles_edit(0)
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
  filenames <- list.files(full.names=FALSE, pattern = ".tif$")   #Extract list of  file names from working directory
  library(raster) #Calls 'raster' library
  #Function 'f1' imports data listed in 'filenames' and assigns projection
  f1<-function(x,z) {
    y <- raster(x)
    projection(y) <- CRS(z)
    return(y)
  }
  import <- mclapply(filenames,f1,projection(studyArea))
  f2<-function(x,y) {
    x<-projectRaster(x, crs=OutPrj, res=OutRes, method="ngb")
    return(x)
  } 
  closeAllConnections()
  beginCluster(30)
  output <- mclapply(import,f2,OutPrj)
  multiply2<-function(x,y) {
    # origin(y) <- 10 # give 30m reference same origin as X
    x <- x * y # this stays at 250m and change resolution to 30m when running BatchCrop
    #.... still will have extra area outside of Sask but this can be clipped after
    return(x)
  }  
  clipped <- mclapply(import,multiply2,studyArea)    #Clip AGAIN using 30m resolution
  #Use a 'for' loop to iterate writeRaster function for all cropped layers
  for(i in (1:max(length(filenames)))){
    writeRaster(clipped[[i]],paste(outdir,deparse(substitute(OutName)),"_",filenames[i], 
                                   sep=""), format='GTiff',datatype='INT2U')
  }
}