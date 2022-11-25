## to make the installation of the "oneClass" package work there are a couple of packages
## required. First of all you will need to install "devtools". Next you can run the line
## install_github("benmack/oneClass") - this is likely to results in an error message
## telling you that some package could not be loaded / is missing.
## you should always install the packages that are mentioned to be missing and then load
## the packages and then rerun the line of code.
## you might need to do this a couple of times until oneClass is installed correctly.

require("devtools")
install_github("benmack/oneClass")
require("raster")
require("rgdal")
require("glcm")
require("e1071")
require("oneClass")


#####
##### load Sentinel-1 image
#####

# jump into directory
setwd("/home/nonameornot/KIT/14_Sentinel_1/01_S1_Segmentation")
#load image
img <- stack("scaled_S1.tif")
# plot the two bands of the image
plot(img)


#####
##### classify segments with one class classifier
#####

# load segments (positive samples + all samples)

# jump into directory
setwd("I:/KIT_Forschung/23_Sentinel_1/1_S1_segm_oneClass/segmentation")
# load segments (whole image)
segm <- readOGR(".", "small")
# load training segments of target class
segm_tr <- readOGR(".", "small_tr")
 

# extract values from Radar image by overlapping the image with the segments
# and extracting the mean raster value for each segment
# this step might need a couple of minutes
tr_data <- extract(img, segm_tr, fun=mean)
all_data <- extract(img, segm, fun=mean)

# save objects to save time in case the processing will be repeated later
 
save(tr_data, file="tr_data2.R") 
save(all_data, file="all_data2.R")

# see information in the extracted data
head(all_data)

# copy extracted values of training segments into a new variable
tr_data1 <- tr_data[,1:2]
# transform the new variable in a dataframe
tr_data2 <- as.data.frame(tr_data1)
# add a target class to the dataframe
tr_data2$class <- 'wald'

# repeat the same for the file containing all segments
all_data <- all_data[,1:2]
all_data2 <- as.data.frame(all_data)
all_data2$class <- 'abs'
 
# sub sample the file containing all segments to have a sample for the "background"
# in the one class classifier
s1 <- sample(1:1502, 150)
ys <- all_data2[s1,]


################ code von Steffi (reduziert)

# copy prepared training data in a new variable called "pres" which stands for presence
pres <- tr_data2

# copy background data in a new variable called "backgr" which stands for background
backgr <- ys

# merge the two datasets into a single variable called "new"
new <- rbind (pres,backgr)

# get out only numerical values from the just created variables (this is just to prepare
# the data in a way the algorithm can understand it - dont worry too much about it)
trdata <- new[,1:2]
 
# prepare algorithm by defining the target class
tr.new <- puFactor(new[, 3], positive='wald')
 
# set output directory 
setwd("I:/KIT_Forschung/23_Sentinel_1/1_S1_segm_oneClass/oneclass/output")
 
### ocsvm = one class svm 

# train the one class classifier with the just prepared data
ocsvm.fit <- trainOcc(x=trdata, y=tr.new, method="ocsvm")

# predict the trained classifier to the data extracted from all segments
ocsvm.pred <- predict(ocsvm.fit,all_data2[,1:2])
 
# check the model results to identify a suitable threshold

hist(ocsvm.fit)

# define a threshold to transfer the continuous outputs of the one class classifier
# into discrete classes => use the graph created with the preceding line to define
# an appropriate threshold (see also oneClassIntro.pdf )
ocsvm.bin <- ocsvm.pred > 0

# attach the results to a new shapefile
# copy the segment shapefile into a new variable
segm_out <- segm 
# attach model results to the new variable (in a column in the attribut table called
# "occ_cl")
segm_out$occ_cl <- ocsvm.bin
 
head(segm_out)

# write the new variable/shapefile with the attached classification results to a shapefile
writeOGR(segm_out, dsn="occ_classification.shp", layer="test", driver='ESRI Shapefile', delete_dsn=T)
 
 