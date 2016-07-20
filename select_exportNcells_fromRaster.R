#This function takes the N highest values in a raster, selects, and exports only those cells
# to a new Raster

#Author: Byron Smiley
#Date: July 20th 2016


#packages
library(raster)
library(dplyr)



#Variables
indir <- "C:/Byron/temp/"
outdir <- "C:/Byron/temp/"
origRas <- raster(paste(indir, "parameter_groups.tiff", sep=""))
Ncells <- 1000


#Example run function:
selectN_highCells(originalRas = origRas, Ncells=Ncells, final=final)

#Function
selectN_highCells <- function(originalRas, Ncells, final) {
  newdf <- as.matrix(origRas)
  newdf <- sort(newdf,decreasing=TRUE) %>%
    head(Ncells) %>%
    append(0) %>%
    sort(decreasing=FALSE)
  newdf <- newdf[c(1,2,Ncells+1)]
  
  rastemp <- cut(origRas, newdf,right=FALSE)
  
  clTable <- matrix(c(0,1,1,2,NA,1),nrow=2, ncol=3)
  
  rastemp <- reclassify(rastemp, clTable)
  final <- origRas * rastemp
  plot(final)
  writeRaster(final, paste(outdir, "newRas_highCells",Ncells,sep=""), format="GTiff",
              dataType=dataType(origRas), overwrite=TRUE)
}
