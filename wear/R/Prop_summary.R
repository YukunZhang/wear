#' Activity Summary--Proportions and Percentiles
#'
#' Summarize activity measures using proportions and percentiles
#' @param final.dat cleaned final data that is between record getup time and sleep time
#' @param record.sleep.time Log file of sleep time
#' @param record.getup.time Log file of getup time
#' @param ll the id of getup time record
#' @return ... ...
#' @export

prop_summary=function(final.dat,record.getup.time, record.sleep.time,ll)
  
{
  temp.mat<-subset(final.dat,final.dat[,1]>record.getup.time[ll] & final.dat[,1]<record.sleep.time[ll] )
  if(nrow(temp.mat)==0)next
  hour.char<-as.numeric(format(as.POSIXlt(temp.mat$date.time*24*60*60, origin = ISOdatetime(1899,12,30,0,0,0)),"%H"))
  temp.sed<-subset(temp.mat,temp.mat$ActivityCode==0)$Interval
  sed.hour<- sum(temp.sed) /60/60
  length.temp.sed<-length(temp.sed)
  num.changes.from.sed.to.non.sed<- length.temp.sed
  total.sed.time<-sed.hour
  total.number.of.sed.bouts<-num.changes.from.sed.to.non.sed
  mean.sed.bout.length<- mean(temp.sed) /60/60
  prop.of.sed.time.greater.20min<- 100*length(temp.sed[temp.sed>20*60])/length.temp.sed
  prop.of.sed.time.greater.60min<- 100*length(temp.sed[temp.sed>60*60])/length.temp.sed
  prop.of.sed.time.greater.120min<- 100*length(temp.sed[temp.sed>120*60])/length.temp.sed
  
  total.sed.time.greater.20min<- sum(temp.sed[temp.sed>20*60])/60/60
  total.sed.time.greater.60min<- sum(temp.sed[temp.sed>60*60])/60/60
  total.sed.time.greater.120min<- sum(temp.sed[temp.sed>120*60])/60/60
  
  quantile.temp<-quantile(temp.sed, probs = c(0.05,0.25,0.5,0.75,0.95))/60/60
  percentile.sed.time.5<- quantile.temp[1]
  percentile.sed.time.25<- quantile.temp[2]
  percentile.sed.time.50<- quantile.temp[3]
  percentile.sed.time.75<- quantile.temp[4]
  percentile.sed.time.95<- quantile.temp[5]
  
  alpha.sed<- 1+ 1/mean(log(temp.sed/ min(temp.sed)))
  gini.index.sed<- gini(temp.sed)
  
  prop.sed.time.6.12<- 100*sum(subset(temp.mat,temp.mat$ActivityCode==0 & hour.char>=6 & hour.char<12)$Interval) /(sum(subset(temp.mat,hour.char>=6 & hour.char<12)$Interval)+0.0001)   ###prevent this value is zero
  prop.sed.time.12.18<- 100*sum(subset(temp.mat,temp.mat$ActivityCode==0 & hour.char>=12 & hour.char<18)$Interval) /(sum(subset(temp.mat,hour.char>=12 & hour.char<18)$Interval)+0.0001)
  prop.sed.time.18.22<- 100*sum(subset(temp.mat,temp.mat$ActivityCode==0 & hour.char>=18 & hour.char<22)$Interval) /(sum(subset(temp.mat,hour.char>=18 & hour.char<22)$Interval)+0.0001)
  table2<-c(88888888,total.sed.time,total.number.of.sed.bouts,mean.sed.bout.length,prop.of.sed.time.greater.20min,prop.of.sed.time.greater.60min,prop.of.sed.time.greater.120min,total.sed.time.greater.20min,total.sed.time.greater.60min,total.sed.time.greater.120min,percentile.sed.time.5,percentile.sed.time.25,percentile.sed.time.50,percentile.sed.time.75,percentile.sed.time.95,alpha.sed,gini.index.sed,prop.sed.time.6.12,prop.sed.time.12.18,prop.sed.time.18.22)
  table2.label<-c("88888888","total.sed.time","total.number.of.sed.bouts","mean.sed.bout.length" ,"prop.of.sed.time.greater.20min","prop.of.sed.time.greater.60min","prop.of.sed.time.greater.120min","total.sed.time.greater.20min","total.sed.time.greater.60min","total.sed.time.greater.120min","percentile.sed.time.5","percentile.sed.time.25","percentile.sed.time.50","percentile.sed.time.75","percentile.sed.time.95","alpha.sed","gini.index.sed","prop.sed.time.6.12","prop.sed.time.12.18","prop.sed.time.18.22")
  
  out=list( total.sed.time=total.sed.time,total.number.of.sed.bouts=total.number.of.sed.bouts,mean.sed.bout.length,prop.of.sed.time.greater.20min=mean.sed.bout.length,prop.of.sed.time.greater.20min,prop.of.sed.time.greater.60min=prop.of.sed.time.greater.60min, prop.of.sed.time.greater.120min= prop.of.sed.time.greater.120min,total.sed.time.greater.20min=total.sed.time.greater.20min,total.sed.time.greater.60min=total.sed.time.greater.60min,total.sed.time.greater.120min=total.sed.time.greater.120min,percentile.sed.time.5,percentile.sed.time.25=percentile.sed.time.5,percentile.sed.time.25,percentile.sed.time.50=percentile.sed.time.50,percentile.sed.time.75=percentile.sed.time.75,percentile.sed.time.95=percentile.sed.time.95,alpha.sed=alpha.sed,gini.index.sed=gini.index.sed,prop.sed.time.6.12=prop.sed.time.6.12,prop.sed.time.12.18=prop.sed.time.12.18,prop.sed.time.18.22=prop.sed.time.18.22,table2=table2,table2.label=table2.label)
  return(out)
}