# Author name: Yi-Ying Chen 
# Email: yiyingchen@gate.sinica.edu.tw
# 
# load R library 

#
fun.up.scale <- function (img_index=20, 
                          img_path=c("/work/vivianlin0921/PCA/classificationresults/taiwan/2021/trythreshold/NIRthreshold/2021"),
                          ref_buf=250, 
                          wrk_yr=2013)
# start the fun.up.scale
{

library("sp")
library("raster")
library("rgdal") #readOGR
library("rgeos") #gCentroid
library("viridis") 
library("proj4")
#library("snow")
#library("tidyverse")

#wrk_yr=wrk_yr 
#ref_buf=250
#wrk_yr=2023


#create the output directory 
out_dir = paste("./df_table_",wrk_yr,"/",sep="")
dir.create(out_dir)

#set image path 
#image_path = c("/work/vivianlin0921/PCA/classificationresults/taiwan/2013/all/2013")
#image_path = c("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/2021/trythreshold/NIRthreshold/")
image_path = paste("/lfs/home/ychen/lfs_dir/Satellite/SPOT_CSRSR/grid_box_lulcc/LULCC_6m/2023/tree20", sep="")

#image_path = img_path


#image_path = img_path 
image_subname = c(".tif")

#ref_buf=250
#img_index = 172
# find the bufer the points in the mesh
#brd=200

set_index=as.integer(img_index)
set_buf = as.integer(ref_buf)

#set_buf=250
ii=0 

# load finish net at 500m by 500m spacing 

mesh.500m = readOGR(verbose = FALSE, 
            "/lfs/home/ychen/lfs_dir/GIS/Taiwan_Fishnet/500m/taiwan_raster_t97.shp")
#mesh.500m = readOGR(verbose = FALSE, 
#            "/lfs/home/ychen/scripts/R/Rscripts/SPOT_CLASS/fishnet/mesh/500m/taiwan_raster_WGS84.shp")
#convert the projection into twd97
#mesh.500m =  spTransform(mesh.500m, sp::CRS("+init=epsg:3826"))  

mesh.12km = readOGR(verbose = FALSE,
             "/lfs/home/ychen/lfs_dir/GIS/Taiwan_Fishnet/12km/SPOT_12km_TW_MESH_260_images.shp")
#convert the projection into twd97
mesh.12km =  spTransform(mesh.12km, sp::CRS("+init=epsg:3826"))  

# get gelocation of center point  of the grid.
cent.xy.500m = gCentroid(mesh.500m, byid=TRUE)
# create dataframe for xy coordinate
df.500m <- as.data.frame(cent.xy.500m)
# Using add_column()
df.500m.share <- data.frame(x=df.500m$x, y=df.500m$y,forest = 0, agri=0, water=0, built=0, other=0  )
cent.xy.12km = gCentroid(mesh.12km, byid=TRUE)

# worlk on the mesh tables

#for (i in 1:260) {
  # the grid box xy
  #   pt4    pt3
  #  
  ##  pt1/5    pt2   
#  xmax=max(mesh.12km@polygons[[i]]@Polygons[[1]]@coords[,1])
#  xmin=min(mesh.12km@polygons[[i]]@Polygons[[1]]@coords[,1])
  #
#  ymax=max(mesh.12km@polygons[[i]]@Polygons[[1]]@coords[,2])
#  ymin=min(mesh.12km@polygons[[i]]@Polygons[[1]]@coords[,2])
  #
#  print(paste("xmin:",xmin, "xmax:",xmax, sep=" "))
#  print(paste("ymin:",ymin, "ymax;",ymax, sep=" ")) 
 
#  gd.pts <- subset (df.500m, (df.500m$x > xmin & df.500m$x < xmax & df.500m$y > ymin & df.500m$y < ymax )) 
#  print(paste("Total 500m-grid mech for each SPOT image : ", length(gd.pts$x),sep=""))
  #points( gd.pts, bg=my.col[i],pch=22, col=NA,cex=1.0)

#}

#plot 500 points
#points (x=df.500m$x, y=df.500m$y, cex=0.3, pch=16)
#print(paste("Total points:", length(df.500m$y), sep="")) 

# set color palette
#  my.col.5c <- c("#439c6e","#e86d5f","#8bd2e8","#f0d86e","#999999") 
      #dark green for forest, red for builtup, blue for water, orange for agri, grey for unknown   
#  my.col.forest<- colorRampPalette(c("gray","lightgreen","#439c6e"))(101)
  #my.col <- viridis(n=259, alpha=1, direction=1, option="H") # D for viridus  H for turbo/rainbow



df.500m <- data.frame()
wrk_img <- list()
ref.pt <- list()
df.500m.share.wrk <- list()

#df.imesh.xyid <- data.frame()
#df.meshxy.tmp <- data.frame())

#for (imesh in 1:259) {
for (imesh in as.integer(set_index) )  {

print(paste("Working on imesh:",imesh,".", sep=" "))
xid=mesh.12km@data$XID[imesh]
yid=mesh.12km@data$YID[imesh]

print( paste("mesh_index:",imesh, "XID:", xid, " YID:", yid, sep="") )

  # assign mapping information  
 #  df.meshxy.tmp$mesh <- imesh
 #  df.meshxy.tmp$xid <- xid
 #  df.meshxy.tmp$yid <- yid 

   
   #update the data.frame 
 #  df.imesh.xyid <- rbind(df.imesh.xyid, df.meshxy.tmp)
   
}


#set xy id for the wrking image 
xid <- formatC(xid,format="s",width=3)
yid <- formatC(yid,format="s",width=3) 
img_path <- c( paste(image_path,"/",wrk_yr,"_",xid,"_",yid,"_tree20" ,image_subname,sep="") )

  print(paste("Working on the image file:",img_path,sep=""))

# chueck classificed image file
    if( file.exists(img_path) ) {
 
    #initiated the ii index based on image 
    ii = ii + 1 
    wrk_img[[ii]] <- raster(x = img_path)
 
   } else{
     print(paste("can't find the image file:",img_path,sep=""))
     ##---exist the loop--- for next iteration 
     next
   }

# find id within the image
g_xmax <- wrk_img[[ii]]@extent@xmax
g_xmin <- wrk_img[[ii]]@extent@xmin
g_ymax <- wrk_img[[ii]]@extent@ymax
g_ymin <- wrk_img[[ii]]@extent@ymin

   
    #do nothing and go to next interation
    if (length(df.500m.share$x) == 0 ){
     #reset ii =ii -1
     ii = ii-1 
     print("no referenc grid @500m, reset index")
     next
    }

    # get the subset table for working on the selected image
    df.500m.share.wrk[[ii]] <- subset( df.500m.share, 
        (df.500m.share$x < g_xmax & df.500m.share$x > g_xmin & df.500m.share$y < g_ymax & df.500m.share$y > g_ymin))
    
    #exist for on reference points
    if (length(df.500m.share.wrk[[ii]]$x) == 0 ) {
      #reset ii =ii -1
      ii = ii-1 
      print("no reference points @6m, reset index")
      next
    }


print(paste("wrking images:",ii, sep="") )
#print(df.500m.share.wrk[[ii]])

  
#create spatial point obj
 ref.pt[[ii]] <- SpatialPoints(coords=cbind(df.500m.share.wrk[[ii]]$x,df.500m.share.wrk[[ii]]$y), proj4string= sp::CRS("+init=epsg:3826")  ) 
#
 tot.pt <- length(ref.pt[[ii]])

   # A quick plot for checking where are we processing the images. 
   if (ii == 1) {
     plot(mesh.12km, axes=TRUE, xlim=c(12.0e+4,47.0e+4), ylim=c(24.0e+5,28.00e+5)) 
     # plot(mesh.12km, axes=TRUE, xlim=c(wrk_img[[ii]]@extent@xmin-brd,wrk_img[[ii]]@extent@xmax+brd), 
     #      ylim=c(wrk_img[[ii]]@extent@ymin-brd,wrk_img[[ii]]@extent@ymax+brd))
   }else{
     plot(wrk_img[[ii]], col=my.col.5c, add=T , zlim=c(1,5))
   }



ld.do  <- TRUE
if (ld.do) {
print(paste("singal cpu start: ", Sys.time(),sep="")) 

  for ( j in 1: length(ref.pt[[ii]]) ) {
  #  print(paste("progresssing: ", formatC((i/tot.pt)*100, digits=1, width = 4, format = "fg"),
  #               "%",sep=""))
  #set buffer distance as 250m from the center :wq!
  #      pp <- extract( wrk_img[[ii]],  ref.pt[[ii]][j] , buffer=250)
        pp <- extract( wrk_img[[ii]],  ref.pt[[ii]][j] , buffer=set_buf)
  #       pp <- extract( wrk_img[[ii]],  ref.pt[[ii]][j] )
  #classify as forest=1, builtup=2, water=3, agri=4, unknown=5
  ##forest type
  tot.n <- length(which(!is.na(as.array(unlist(pp[[ii]]))))) 

  #forest 
  df.500m.share.wrk[[ii]]$forest[j]= length(which(pp[[ii]]==1))/ tot.n
    if (is.na(df.500m.share.wrk[[ii]]$forest[j])) df.500m.share.wrk[[ii]]$forest[j]=0
  #builtup
  df.500m.share.wrk[[ii]]$built[j] = length(which(pp[[ii]]==2))/ tot.n
    if (is.na(df.500m.share.wrk[[ii]]$built[j])) df.500m.share.wrk[[ii]]$built[j]=0
  #water 
   df.500m.share.wrk[[ii]]$water[j] = length(which(pp[[ii]]==3))/ tot.n
    if (is.na(df.500m.share.wrk[[ii]]$water[j])) df.500m.share.wrk[[ii]]$water[j]=0
  #agri
  df.500m.share.wrk[[ii]]$agri[j]  = length(which(pp[[ii]]==4))/ tot.n
    if (is.na(df.500m.share.wrk[[ii]]$agri[j])) df.500m.share.wrk[[ii]]$agri[j]=0
  #unknown
  df.500m.share.wrk[[ii]]$other[j] = length(which(pp[[ii]]==5))/ tot.n
    if (is.na(df.500m.share.wrk[[ii]]$other[j])) df.500m.share.wrk[[ii]]$other[j]=0

  #grass
  df.500m.share.wrk[[ii]]$grass[j]  = length(which(pp[[ii]]>=6))/ tot.n
    if (is.na(df.500m.share.wrk[[ii]]$grass[j])) df.500m.share.wrk[[ii]]$grass[j]=0
 
   #combine other and forest 
   print(paste("sampling number for each grid:", tot.n, sep=" "))

  } #end for j 

#### combine table to the whole island ###
#update the share percentage in the original dataframe "df.500m.share" by the subset table "df.500m.share.gd"
#
   for (igd in 1: length(df.500m.share.wrk[[ii]]$x) ) {
   x.wrk <- df.500m.share.wrk[[ii]]$x[igd]
   y.wrk <- df.500m.share.wrk[[ii]]$y[igd]  
   #replace the value for the grid  
   df.500m.share[(df.500m.share$x== x.wrk & df.500m.share$y== y.wrk), ] <- df.500m.share.wrk[[ii]][igd,]
   #print(df.500m.share.gd[igd,])
   }

   #ouput result of working image  
   print(df.500m.share.wrk[[ii]])
   
   #update the data.frame 
   df.500m <- rbind(df.500m, df.500m.share.wrk[[ii]])
   
} # end if ld.do 



#save/ouput the df.500m.share table
img_txt=sprintf("%03d",as.integer(img_index))

df.path = paste(out_dir,"/","df.500m.share.",img_txt,".csv",sep="") 

print(paste("csv_file:",df.path,sep="") )
print(df.500m)

#save(df.500m.share, file = "data.frame.500m.share.rda")
write.csv(df.500m, file=df.path, row.names=FALSE)

#conver dataframe to raster object 
#raster.lulcc <- rasterFromXYZ(df.500m, crs=sp::CRS("+init=epsg:3826"))
#save the ratser as netCDf file
#writeRaster(raster.lulcc, paste(out_dir,"lulcc_500m_twd97.",img_txt,".nc",sep=""), 
#        overwrite=TRUE, format="CDF", varname="LU", varunit="fraction", 
#	longname="Landuse/cover type derived from SPOT images 6m and upscale to 500m grid, Forest=1, Builtup=2, Water=3, Agri=4, Unkn.=5 ",
#	xname="x", yname="y", zname="Coverage")


#dev.new()
#dev.off()
print(paste("singal cpu end: ", Sys.time(),sep=" ")) 


#write out mapping information between mesh, xid and yid information 
#write.csv(x=df.imesh.xyid, file = "./mesh_xid_yid.csv", sep = ",",
#                 eol = "\n", na = "NA", dec = ".", row.names = FALSE,
#                 col.names = TRUE, qmethod = c("escape", "double"))



} # end of funtion 


#========== Set the script to Auto RUN=========================== 
# Satrt the funtion byt the specific arguments 
# If you want to submit the file by queue to obelix/300T,.etc. serve rname), you need to apply the 
# following lines, which allowed this R-script can be called under the shell/bash script
# with the arguments sending by specific batch jobs  
#
#args<-commandArgs(TRUE)
#print(args)
#fun.up.scale(args[1], args[2], args[3], args[4]) 
#
#========== End ================================================




