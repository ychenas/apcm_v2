
 library("raster")
 library("rgdal")
  #import canopy height model raster for future usage
  #canopy <- raster(x="./sourcefiles/canopyheight_233_210.tif") #data resolution: 6m
#   canopy <- raster(x="/lfs/home/ychen/lfs_dir/Satellite/SPOT_CSRSR/sourcefiles/canopyheight_model/lidar_chikueiwang_20m/20m_CHM_resample6m.tif")
    dtm  <- raster(x="/lfs/home/ychen/lfs_dir/GIS/TaiwanDTM/xport1.flt")



    crs(dtm)<-"+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs" 
    #crs(canopy)<-"+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs" 
 
  #set to subset region  
     #   xmin=121.0  ==> 250000.000
     #   xmax=122.0  ==> 351745.080
     #   ymin=23.0   ==> 2544283.125
     #   ymax=24.0   ==> 2655384.288
  #   e <- as(extent(-16, -7.25, 4, 12.75), 'SpatialPolygons')
       
     #read the gid information  
     ref_map <- raster(x="/lfs/home/ychen/lfs_dir/Satellite/SPOT_CSRSR/grid_box_lulcc/LULCC_6m/2022/2022_237_203_tree20.tif")
  
      xmin <- extent(ref_map)[1]
      xmax <- extent(ref_map)[2]
      ymin <- extent(ref_map)[3]
      ymax <- extent(ref_map)[4]
   ref_row <- dim(ref_map)[1]
   ref_col <- dim(ref_map)[2]
    #set_refextent
    ref_extent <- extent(xmin,xmax,ymin,ymax)
    
#initiate the_sub_map 
    sub_map <- raster(nrow=ref_row, ncol=ref_col)
 
      #  xmin <-  282000.000
      #  xmax <-  332000.000
      #  ymin <- 2650000.000
      #  ymax <- 2680000.000
     
      e <- as(extent(xmin, xmax, ymin, ymax), 'SpatialPolygons')
      
      crs(e) <- crs(dtm)
      #create extent object that holds xmin xmax ymin ymax values that you want to set future rasters to-
      #(this is to create an universal extent for all images under this AOI so later these rasters can be stacked)
      #crop canopy raster based on the extent of AOI, therefore extent of outputraster (classified result) raster
 #     subset_canopy <- crop(canopy, extent(canopy, xmin,xmax,ymin,ymax))
   # subset_dtm <- crop(dtm, extent(xmin, xmax, ymin, ymax) )
 
      subset_dtm <- crop(dtm,e)

      #set projection as TWD97
      crs(subset_dtm) <- "+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
      
      subset_slp <- terrain(subset_dtm,opt="slope", unit="degrees")

      #resample the subset raster map to 6m 
#      sub_map <- resample(sub_dtm, sub_map, method='bilinear') 


      #output classification result as tif raster
      writeRaster(subset_dtm,paste("./subset_map/subset_dtm_6m.tif",sep=""), format="GTiff", overwrite=TRUE)
   
      writeRaster(subset_slp,paste("./subset_map/subset_slp_6m.tif",sep=""), format="GTiff", overwrite=TRUE)



 
