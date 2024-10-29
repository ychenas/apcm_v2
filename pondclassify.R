#
fun.lu.type <- function (xmin=234, xmax=234, ymin=209, ymax=209, wrk_yr=2015, aoi_reg = c("TAIPEI"))
# start the fun.lu.type
{
#aoi_reg="XY_ID"
#wrk_yr=2013
#xmin=224;xmax=224;ymin=194;ymax=194

#load libraries  
library(raster)
library(tidyverse)
library(rpart)
#library(rpart.plot)
library(rgdal)
#set AOI region 
print(aoi_reg)
   #set AOI with specific region
   if (aoi_reg == "TAIPEI") {
       xmin=234;xmax=237; ymin=209; ymax=212
     }else if (aoi_reg=="TAOYUAN") {
               xmin=231; xmax=234; ymin=208; ymax=210 
     }else if (aoi_reg=="NORTH") { 
               xmin=224; xmax=240; ymin=205; ymax=212
     }else if (aoi_reg=="CENTRAL") {
               xmin=224; xmax=230; ymin=195; ymax=204
     }else if (aoi_reg=="EAST") {
               xmin=231; xmax=240; ymin=195; ymax=204
     }else if (aoi_reg=="SOUTH") {
               xmin=224; xmax=240; ymin=185; ymax=194
     }else if (aoi_reg=="TAIWAN"){
               xmin=224; xmax=240; ymin=185; ymax=212
     }else {
       xmin=xmin;xmax=xmax;ymin=ymin;ymax=ymax
     } 
   
#else if(aoi_reg=="TAOYUAN") {xmin=231; xmax=234; ymin=208; ymax=210}
#   else if(aoi_reg=="TAIWAN")  {xmin=224; xmax=240; ymin=185; ymax=212}
#   else if(aoi_reg=="NORTH")   {xmin=224; xmax=240; ymin=205; ymax=212}
#   else if(aoi_reg=="CENTRAL") {xmin=224; xmax=240; ymin=195; ymax=204}
#   else if(aoi_reg=="SOUTH")   {xmin=224; xmax=240; ymin=185; ymax=194}
 
#--------------------#classification decision tree#--------------------#
tree <- TRUE
while (tree==TRUE){

  #create allaoi vector to store all aoi
  allaoi <- vector()
  for (x in xmin:xmax){
    for (y in ymin:ymax){
      #append aoi to allaoi
      allaoi <- append(allaoi, paste(x,"_",y,sep=""))
    }
  }
  str_yr <- min(wrk_yr)
  end_yr <- max(wrk_yr)
  
  #classification algorithm for future images
  for (yr in str_yr:end_yr){ #doesn't iterate yr 2018 because when iterating 2017, 2018 will be counted in
    print(yr)
    
    if (yr==2017){
      taoyuanpond <- raster(paste("/home/u3176872/LULCC/plots/classificationresults/taiwan/",yr,"/tree20/2017and2018_taoyuan_tree20.tif", sep=""))
    }else{
      taoyuanpond <- raster(paste("/home/u3176872/LULCC/plots/classificationresults/taiwan/",yr,"/tree20/", yr, "_taoyuan_tree20.tif", sep=""))
    }
    
    #create a matrix of index to attach to taoyuanpond raster to give index value
    indexmatrix <- matrix(1:(dim(taoyuanpond)[1]*dim(taoyuanpond)[2]),nrow=dim(taoyuanpond)[1],ncol=dim(taoyuanpond)[2],byrow = TRUE)
    #convert matrix to raster
    indexraster <- raster(indexmatrix)
    #set coordinate system and extent of index raster just like taoyuanpond classification raster
    crs(indexraster) <- crs(taoyuanpond)
    extent(indexraster) <- extent(extent(taoyuanpond)[1],extent(taoyuanpond)[2],
                                  extent(taoyuanpond)[3],extent(taoyuanpond)[4])
    #stack taoyuanpond classification raster with index raster just created
    rasterstack <- stack(taoyuanpond,indexraster)
    #change raster layers' names
    names(rasterstack) <- c("ltype","index")
    
    #----------#see size of pond#----------#
    #import taoyuan pond centroids coordinates csv;this version of csv's pond name column is taken out and replaced as number because csv can't read in chinese
    centroidpoints <- read.csv("/home/u3176872/LULCC/sourcefiles/pondcentroid.csv", header=T, encoding = "UTF-8",sep = ",")
    coordinates(centroidpoints)= ~ TWD97X + TWD97Y
    crs(centroidpoints) <- crs(taoyuanpond)
    #extract value of taoyuanpond raster (land type and index) through overlapping point of centroid
    rasValue <- raster::extract(rasterstack, centroidpoints)
    #create combinePointValue dataframe that stores the centroid coordinates and extracted index+landtype value from taoyuanpond raster
    combinePointValue <- cbind(as.data.frame(centroidpoints),rasValue)
    #create new pondarea column to combinePointValue dataframe
    combinePointValue["pondarea"] <- NA
    
    boxrange<-75
    #use 75 because on Taoyuan gov website it says largest pond is 198900 m2, which is 198900/6/6=5525 pixels, which in a matrix square of 75 pixels per side (75*75=5625)
    box <- (75-1)/2
    for (pixel in 1:length(combinePointValue$index)){ #length(combinePointValue$index)=223
      # for (pixel in 201:203){
      print(pixel)
      if (is.na(combinePointValue$ltype[pixel])){
        next
      }else if (combinePointValue$ltype[pixel]==3){
        #convert index value to raster row and column value
        raster_row <- (combinePointValue$index[pixel]%/%(dim(taoyuanpond)[2]))+1
        raster_col <- combinePointValue$index[pixel]-((combinePointValue$index[pixel]%/%(dim(taoyuanpond)[2]))*dim(taoyuanpond)[2])
        #clip a 75x75 matrix with center at centroid; the min max functions in the extent function is to deal with situations where centroid is close to border therefore cannot extend 75x75 matrix outwards
        clipmatrix <- crop(rasterstack, extent(rasterstack, max(1,raster_row-box), min(dim(taoyuanpond)[1],raster_row+box), max(1,raster_col-box), min(dim(taoyuanpond)[2],raster_col+box))) #row min, row max, col min, col max
        # print(clipmatrix)
        # print(raster_row)
        # print(raster_col)
        for (row in 1:boxrange){
          for (col in 1:boxrange){
            if (is.na(clipmatrix[row,col][1])){
              next
            }else if (clipmatrix[row,col][1]==3){
              #templist evaluating left right top bottom pixels of clipmatrix[row,col][1] to see if they are also water ltype
              #solve issues regarding border pixels - templist[1]=0 means that there is no left pixels, clipmatrix[row,col][1] is at left border of matrix
              #                                       templist[2]=boxrange+1 means that there is no right pixels, clipmatrix[row,col][1] is at right border of matrix
              #                                       templist[3]=0 means that there is no top pixels, clipmatrix[row,col][1] is at top border of matrix
              #                                       templist[4]=boxrange+1 means that there is no bottom pixels, clipmatrix[row,col][1] is at bottom border of matrix
              templist<-c(max(0,col-1),min(boxrange+1,col+1),max(0,row-1),min(boxrange+1,row+1)) #left right top bottom
              #if templist[1]!=0 that there is left neighboring pixel, get land type of the left pixel
              if (templist[1]!=0){ templist[1]<- clipmatrix[row,templist[1]][1] }
              #if templist[2]!=(boxrange+1) that there is right neighboring pixel, get land type of the right pixel
              if (templist[2]!=(boxrange+1)){ templist[2]<-clipmatrix[row,templist[2]][1] }
              #if templist[3]!=0 that there is top neighboring pixel, get land type of the top pixel
              if (templist[3]!=0){ templist[3]<-clipmatrix[templist[3],col][1] }
              #if templist[4]!=(boxrange+1) that there is bottom neighboring pixel, get land type of the bottom pixel
              if (templist[4]!=(boxrange+1)){ templist[4]<-clipmatrix[templist[4],col][1] }
              #set templist[1]/templist[3] as 0 is there are no left/top neighbor pixels
              templist[c(1,3)][templist[c(1,3)]==0]<- 0
              #set templist[2]/templist[4] as 0 is there are no right/bottom neighbor pixels
              templist[c(2,4)][templist[c(2,4)]==boxrange+1]<- 0
              #count the number water pixels surrounding the iterated pixel; if there is no surrounding water pixels, the center pixel will not be counted as pond
              if ((3 %in% templist)==FALSE){
                clipmatrix[row,col][1]<-NA #set those pixels that don't contain neighboring water pixels as NA
              } #end of if statement
            } #end of else-if statement
          } #end of col-loop
        } #end of row-loop
        #create classifiedlandtype dataframe out of the land type column of clipmatrix dataframe to later count how many water pixels are in it to calculate pond size
        classifiedlandtype <- as.data.frame(clipmatrix$ltype)
        # print((sum(clipmatrix==3,na.rm=TRUE)))
        #count the number of occurence of 3 within the clipped 75x75 matrix to see how many water pixels are counted as pond and *6*6 to get total area of pond (pixel resolution is 6x6m)
        combinePointValue$pondarea[pixel] <- (sum(classifiedlandtype==3,na.rm=TRUE))*6*6
        
        clipmatrix <- as.data.frame(clipmatrix)
        #subset clipmatrix to only those have water land type (later to be converted to pond pixels)
        tempmatrix <- subset(clipmatrix, clipmatrix$ltype==3)
        #bind all counted pond pixels into single dataframe "wholelist" that contains all pond pixels index
        if (exists("wholelist")==FALSE){ #set the first tempmatrix created as wholelist since it is empty
          wholelist <- tempmatrix
        }else{ #stack the following pixel's tempmatrix created to wholelist since it is not empty anymore
          wholelist <- rbind(wholelist,tempmatrix)
        }
      } #end of else-if statement
    } #end of pixel-loop
    
    taoyuanpond <- replace(taoyuanpond,wholelist$index,8)
    #transfer water pixels to pond pixels under the scenario that the pond is only one pixel standing by its own that is deleted in the above clipmatrix for-loop
    subsetdf <- combinePointValue[!is.na(combinePointValue$ltype)&combinePointValue$ltype==3,]
    taoyuanpond <- replace(taoyuanpond,subsetdf$index,8)
    
    #create raster of reclassified taoyuan image (forest: 1; builtup: 2; water: 3; agri: 4; unknown:5; grassland: 6; unclassified: 7; pond: 8)
    if (yr==2017){
      writeRaster(taoyuanpond,paste("/home/u3176872/LULCC/plots/classificationresults/taiwan/",yr,"/tree20/2017and2018_taoyuan_tree20_pond.tif", sep=""))
    }else{
      writeRaster(taoyuanpond,paste("/home/u3176872/LULCC/plots/classificationresults/taiwan/",yr,"/tree20/", yr, "_taoyuan_tree20_pond.tif", sep=""))
    }
    
    rm(wholelist)
  } #end of yr-for loop
  
  tree <- FALSE
} #end of tree while loop


#return()
## end of function fun.lu.type 
}


#========== Set the script to Auto RUN=========================== 
# Satrt the funtion byt the specific arguments 
# If you want to submit the file by queue to obelix/300T,.etc. serve rname), you need to apply the 
# following lines, which allowed this R-script can be called under the shell/bash script
# with the arguments sending by specific batch jobs  
#
args<-commandArgs(TRUE)
print(args)
fun.lu.type(args[1], args[2], args[3], args[4], args[5], args[6]) 
#
#========== End ================================================



