#' Activity Summary--Standing and Stepping
#'
#' Summarize Standing and Stepping activity measures
#' @param final.dat cleaned final data that is between record getup time and sleep time
#' @param record.sleep.time Log file of sleep time
#' @param record.getup.time Log file of getup time
#' @param ll the id of getup time record
#' @return ... ...
#' @export

Activity_summary=function(final.dat,record.getup.time, record.sleep.time,ll){
  temp.mat<-subset(final.dat,final.dat[,1]>record.getup.time[ll] & final.dat[,1]<record.sleep.time[ll] )
  if(nrow(temp.mat)==0)next
  
  temp.mat.for.activity<- temp.mat
  temp.mat.for.activity$Activity[temp.mat.for.activity$Activity==1]<-2
  end.pos<-cumsum(rle(temp.mat.for.activity$Activity)$lengths)
  start.pos<-c(0,end.pos[1:(length(end.pos)-1)])+1
  ############### for each runs, handle the data
  handle.runs<- sapply(1:length(end.pos),function(x,data.mat=temp.mat.for.activity)
  {
    select.data<-data.mat[start.pos[x]:end.pos[x],]
    combine.data<- c(min(select.data$date.time),sum(select.data$Interval),max(select.data$Activity),sum(select.data$METs) )
    return(combine.data)
  }, simplify=F
  )
  ############### combine each run
  combined.temp.mat.for.activity<-data.frame(do.call(rbind,handle.runs))
  colnames(combined.temp.mat.for.activity)<-c("date.time", "Interval", "ActivityCode","METs")
  ###############
  ###############Calculation
  ###############
  ###
  temp.activity<-subset(combined.temp.mat.for.activity,combined.temp.mat.for.activity$ActivityCode==2)$Interval
  ###
  
  length.temp.activity<-length(temp.activity)
  total.number.of.activity.bouts<- length.temp.activity
  mean.activity.bout.length<- mean(temp.activity) /60/60
  
  prop.of.activity.time.greater.5min<- 100*length(temp.activity[temp.activity>5*60])/length.temp.activity
  prop.of.activity.time.greater.10min<- 100*length(temp.activity[temp.activity>10*60])/length.temp.activity
  prop.of.activity.time.greater.30min<- 100*length(temp.activity[temp.activity>30*60])/length.temp.activity
  
  total.activity.time.greater.5min<- sum(temp.activity[temp.activity>5*60])/60/60
  total.activity.time.greater.10min<- sum(temp.activity[temp.activity>10*60])/60/60
  total.activity.time.greater.30min<- sum(temp.activity[temp.activity>30*60])/60/60
  
  quantile.activity.temp<-quantile(temp.activity, probs = c(0.05,0.25,0.5,0.75,0.95))/60/60
  percentile.activity.time.5<- quantile.activity.temp[1]
  percentile.activity.time.25<- quantile.activity.temp[2]
  percentile.activity.time.50<- quantile.activity.temp[3]
  percentile.activity.time.75<- quantile.activity.temp[4]
  percentile.activity.time.95<- quantile.activity.temp[5]
  
  alpha.activity<- 1+ 1/mean(log(temp.activity/ min(temp.activity)))
  gini.index.activity<- gini(temp.activity)
  step.hour<- sum(subset(temp.mat,temp.mat$ActivityCode==2)$Interval) /60/60
  stand.hour<- sum(subset(temp.mat,temp.mat$ActivityCode==1)$Interval) /60/60
  
  stepping.to.standing.ratio<- step.hour/stand.hour
  
  table4<- c(88888888,total.number.of.activity.bouts,mean.activity.bout.length,prop.of.activity.time.greater.5min,prop.of.activity.time.greater.10min,prop.of.activity.time.greater.30min,total.activity.time.greater.5min,total.activity.time.greater.10min,total.activity.time.greater.30min,percentile.activity.time.5,percentile.activity.time.25,percentile.activity.time.50,percentile.activity.time.75,percentile.activity.time.95,alpha.activity,gini.index.activity,stepping.to.standing.ratio)
  table4.label<- c("88888888","total.number.of.activity.bouts","mean.activity.bout.length","prop.of.activity.time.greater.5min","prop.of.activity.time.greater.10min","prop.of.activity.time.greater.30min","total.activity.time.greater.5min","total.activity.time.greater.10min","total.activity.time.greater.30min","percentile.activity.time.5","percentile.activity.time.25","percentile.activity.time.50","percentile.activity.time.75","percentile.activity.time.95","alpha.activity","gini.index.activity","stepping.to.standing.ratio")
  out=list( temp.activity=temp.activity,total.number.of.activity.bouts=total.number.of.activity.bouts,mean.activity.bout.length=mean.activity.bout.length,prop.of.activity.time.greater.5min=prop.of.activity.time.greater.5min,prop.of.activity.time.greater.10min=prop.of.activity.time.greater.10min,prop.of.activity.time.greater.30min=prop.of.activity.time.greater.30min,total.activity.time.greater.5min=total.activity.time.greater.5min,total.activity.time.greater.10min=total.activity.time.greater.10min,total.activity.time.greater.30min=total.activity.time.greater.30min,percentile.activity.time.5=percentile.activity.time.5,percentile.activity.time.25=percentile.activity.time.25,percentile.activity.time.50=percentile.activity.time.50,percentile.activity.time.75=percentile.activity.time.75,percentile.activity.time.95=percentile.activity.time.95,alpha.activity=alpha.activity,gini.index.activity=gini.index.activity,stepping.to.standing.ratio=stepping.to.standing.ratio,table4=table4,table4.label=table4.label)
  return(out)
}