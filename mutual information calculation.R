setwd("C:\\Users\\vkhosa\\Documents\\Work-GC&ED\\Data\\Soil_Mosture_comparison\\grid-2011-2014-daily")
library(raster)
library(lubridate)
library(openair)

files <- list.files(pattern = "*-b.tif")


getMI<-function(files){
  
  #************************************************************************************
  # Arguments: .tif tiles contating daily data for CCAM-CABLE and GLEAM models
  #            at 25*25 km2 resolution
  #
  #returns: raster object with gridded Mutual information calculated per pixel
  #************************************************************************************
  
  # Reads the CCAM-CABLE files
  cordex_25=brick(files[1])
  
  n<-length(files)
  n<-3
  #loop through the GLEAM files
  for (i in 2:n){
    
    
    sm <- brick(files[i])
    
    
    calc.MI <- function(sm, cordex_25){
      # output template
      cor.map <- raster(sm)
      # combine stacks
      T12 <- stack(cordex_25,sm)
      
      rnlayers=nlayers(T12)
      # the function takes a vector, partitions it in half, then correlates
      # the two sections, returning the correlation coefficient. 
      
      
      
      calc.eachpixel.MI <- function(myvec,na.rm=T){
        #**************************************************************************
        # Aguments: vector of stacked two model data representing each pixel
        #
        # Returns: Mutual information (MI) between the two models
        #**************************************************************************
        
        # separate the two model data into independent vectors
        myvecT1<-myvec[1:(length(myvec)/2)]
        myvecT2<-myvec[(length(myvec)/2+1):length(myvec)]
        
        
        dates.list <-as.POSIXct(seq.Date(from=as.Date("2011-01-01"), to=as.Date("2014-12-31"), by="day"))
        
        
        #greate data frame with data and two models columns
        SPDATA_DF.Out <- as.data.frame(matrix(nrow=length(dates.list),ncol = 3))
        colnames(SPDATA_DF.Out) <- c("date","cable","gleam")
        SPDATA_DF.Out$date <- dates.list
        SPDATA_DF.Out$cable <-  myvecT1
        SPDATA_DF.Out$gleam <- myvecT2
        
        # aggregate daily data to monthly
        SPDATA_DF.Out <- timeAverage(SPDATA_DF.Out, avg.time = "1 month", data.thresh = 80,start.date=as.Date("2011-01-01"))
        
        if((sum((is.na(SPDATA_DF.Out$cable))/length(myvec))*100==0)&&(sum((is.na(SPDATA_DF.Out$gleam))/length(myvec))*100==0)){
          
          #time series detrending functions
          data.ts = ts(SPDATA_DF.Out$cable, frequency=12, start=c(2001,1), end=c(2014,12))
          data.ts2 = ts(SPDATA_DF.Out$gleam, frequency=12, start=c(2001,1), end=c(2014,12))
          fit = stl(data.ts, s.window=7, t.window=11)
          fit2 = stl(data.ts2, s.window=7, t.window=11)
          residual <- fit$time.series[,3]
          residual2 <- fit2$time.series[,3]
          
          #call the mutual information computing function
          res<- mi.data(residual,residual2,discretization.method = "sturges")
          
        }
        
        else{
          
          
          res <- NA
          
        }
        
        return(res) 
      }
      #}
      # apply the function above to each cell and write the MI
      # to the output template. 
      
      MI.map <- stackApply(T12, indices = rep(1, rnlayers),fun =  calc.eachpixel.MI, na.rm = FALSE)
      
      return(MI.map)
      
    }
    
    MI=calc.MI(sm, cordex_25)
    
    writeRaster(MI, filename=paste(files[i],"-MI.tif",sep=""), format="GTiff", overwrite=TRUE,options="INTERLEAVE=BAND")
    
  }
}

getMI(files)