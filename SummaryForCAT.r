#------------------------------------------------------------------
# This script is intended to help people summarise their hours
# if they use the "My Work Clock" app for androids, and get files sent to
# their emails in a .csv format
#
# February 23rd, 2016
# CBoisvenue
#----------------------------------------------------------------

# packages to load
require(data.table)

# directories/names etc
indir <- "C:/Byron/temp/"
pers <- "SomeGuy"

# read in file
in.dat <- fread(paste(indir,"data.csv",sep=""),sep=",",header=TRUE)

# get rid of columns not needed and format others
in.dat<-in.dat[,.(date = as.Date(`Start time`),week = week(as.Date(`Start time`)),hours=`Time (hours)`,Job)]

#total by week
weektotal <- in.dat[,.(weekhours = sum(hours)),by=week]
jobweek <- in.dat[,.(jobhours = sum(hours)),by=.(Job,week)]
setkey(weektotal,week)
setkey(jobweek,week)
dat.out1 <- merge(jobweek,weektotal)

# we want % of total time
dat.out <- dat.out1[,.(week,Job,perc = jobhours/weekhours)]

write.table(dat.out,file = paste(indir,pers,".csv",sep=""),sep=",",row.names=FALSE)
