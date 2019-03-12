library("WaveletComp")

setwd("C:\\Users\\vkhosa\\Documents\\Work-GC&ED\\Data\\Soil_Mosture_comparison")

dayta <- read.csv("SSM-comparison-all-products-skukuza-monthly-imputed.csv")

getWaveltAnalysys<-function(dayta){
  #*****************************************************************************************
  #Argument:      dayta has columns: "date","CCAM.CABLE","VIC"
  #
  #Returnss:      (1) plots of cross-wavelet analyis
  #               (2) Numarical value of timelag
  #****************************************************************************************
  
  
  
  dayta$date=as.POSIXct(dayta$date, tz = "Africa/Johannesburg", format = "%d/%m/%Y")
  
  #cross wavelet  transform
  my.wc <- analyze.coherency(dayta, my.pair = c("CCAM.CABLE","in.situ.imp"),
                             loess.span = 0,
                             dt = 1, dj = 1/100,
                             lowerPeriod = 1/2,
                             make.pval = TRUE, n.sim = 10)
  op <- par(no.readonly = TRUE, font.axis = 2, font.lab = 2) # manipulating labels
  
  # Prints the cross-wavelet plot
  wc.image(my.wc, n.levels = 250,
           legend.params = list(lab = "cross-wavelet power levels"),
           timelab = "", periodlab = "period (months)",
           label.time.axis = TRUE,show.date = TRUE, date.format = "%F %T")
  
  ### extracting phase angles
  row.closest.to.11 <- which.min((my.wc$Period - 11)^2) # locate period closest to 11
  angle.series <- my.wc$Angle[row.closest.to.11,] # determine angles
  angle.series[1:6] <- NA # omit 6 values at the beginning and 6 values at the end (outside of coi)
  angle.series[(length(angle.series) - 5):length(angle.series)] <- NA 
  lead.time.in.minutes <- 30 * (12 * (angle.series / (2*pi))) # convert to minutes: 2*pi corresponds to 12 hours, because period = 12
  
  timelag<-mean(lead.time.in.minutes,na.rm = TRUE)
  
  return(timelag)
  
}


getWaveltAnalysys(dayta)