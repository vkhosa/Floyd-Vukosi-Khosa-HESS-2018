setwd("C:\\Users\\VKhosa\\Documents\\sm_grid_data_daily")
library(raster)
library(lubridate)
library(openair)
library(date)
library(rgdal)

files <- list.files(pattern = "*.tif")

cordex_25=brick(files[4], varname= "wb1_ave")


getSSI<-function(files,cordex_25){
  
  #************************************************************************************
  # Arguments: .tif tiles contating daily data for CCAM-CABLE and GLEAM models
  #            at 25*25 km2 resolution
  #
  #returns: raster object with gridded SSI calculated per pixel
  #***********************************************************************************
  
  #Output data frame
  Out.DTF <- as.data.frame(matrix(nrow=0,ncol=3))
  names(Out.DTF) <-c("long","lat","z")
  #coro <-raster(cordex_25)
  
  
  SM_spdf <- as(cordex_25, "SpatialPixelsDataFrame")
  SM_df <- as.data.frame(test_spdf)
  
  #time_seq <- seq(from = as.POSIXct("2001-01-01"), to = as.POSIXct("2015-12-31"), by = "day")
  
  t_seq<-seq.Date(as.Date("2011-01-01"), as.Date("2014-12-31"), by="day")
  names(SM_df)<-c(as.character(t_seq),"lon","lat")
  
  test_df2<- test_df[0,]
  
  #data frame with data formated as month_day
  mm_dd <- t(as.data.frame(substr(as.character(t_seq),6,10),as.character(t_seq)))
  mm_dd$lon<-"NA"
  mm_dd$lat<-"NA"
  names(mm_dd)<-c(as.character(t_seq),"lon","lat")
  
  
  test_df<-rbind(mm_dd,SM_df)
  
  outdata.SM<-as.data.frame(matrix(nrow =0,ncol = length(names(mm_dd))))
  
  names(outdata.SM)<-c("lon","lat",as.character(t_seq))
  
  
  # create progress bar
  
  pb <- winProgressBar(title = "progress bar", min = 3,
                       max = length(test_df[,1]), width = 300)
  
  for(i in 3:length(test_df[,1])){
    
    out.dtf <- as.data.frame(matrix(nrow = 1,ncol = 1))
    out.dtf$lon<-test_df[i,length(names(test_df))-1]
    out.dtf$lat<-test_df[i,length(names(test_df))]
    out.dtf <- out.dtf[-1]
    
    days<- unique(as.character(as.data.frame(substr(as.character(t_seq),6,10),as.character(t_seq))[,1]))
    
    for( d in days){ 
      
      #data frame with with three columns: dayMonth, soil moisture values, date
      row.seq <- as.data.frame(t(test_df[c(1,i),1:length(t_seq)]),row.names = T)
      row.seq$date<-t_seq
      
      names(row.seq)<-c("ddmm","sm","date")
      
      # data for all years beloning to a specific day
      row.seq.av<- row.seq[row.seq$ddmm==d,]
      
      
      SMData.list<-as.numeric(as.character(row.seq.av$sm))
      
      # calculate mean for the day
      SM_ave <- mean(SMData.list)
      # calculate standard deviation for the day
      SM_std <-sd(SMData.list)
      
      date.list<-as.character(row.seq.av[row.seq.av$ddmm==d,"date"])
      
      #calculate SSI
      sSI.anom <- ( SMData.list-SM_ave )/SM_std
      SSI.anom <- as.character(SSI.anom)
      
      temp.dtf<-t(as.data.frame(SSI.anom,as.character(date.list)))
      
      row.names(temp.dtf)<-NULL
      
      out.dtf<-cbind(out.dtf,temp.dtf)
      
      setWinProgressBar(pb, i, title=paste( round(i/length(test_df[,1])*100, 0),
                                            "% done"))
      
    }
    
    #rbind into the outer data frame, define
    
    outdata.all<-rbind(outdata.SM,out.dtf)
    
  }
  
  close(pb)
  
}
