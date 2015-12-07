#' Physical Activity Summary
#'
#' Summarize activity measures
#' @param final.dat cleaned final data that is between record getup time and sleep time
#' @param record.sleep.time Log file of sleep time
#' @param record.getup.time Log file of getup time
#' @param ll the id of getup time record
#' @return hours.worn.total Total worn hours
#' @return hours.awake    Total awake hours
#' @return sed.hour  Total sedantary hours
#' @return ... ...
#' @export
PA_summary=function (final.dat,record.sleep.time,record.getup.time,ll,person,group.char,www) {
  temp.mat<-subset(final.dat,final.dat[,1]>record.getup.time[ll] & final.dat[,1]<record.sleep.time[ll] )
  
  if(nrow(temp.mat)==0)next
  ###################################################
  time.char<-as.POSIXlt(record.getup.time[ll]*24*60*60, origin = ISOdatetime(1899,12,30,0,0,0))
  hour.char<-as.numeric(format(as.POSIXlt(temp.mat$date.time*24*60*60, origin = ISOdatetime(1899,12,30,0,0,0)),"%H"))
  ###
  temp.sed<-subset(temp.mat,temp.mat$ActivityCode==0)$Interval
  length.temp.sed<-length(temp.sed)
  time.char<-as.POSIXlt(record.getup.time[ll]*24*60*60, origin = ISOdatetime(1899,12,30,0,0,0))
  month<-as.numeric(format(time.char,"%m"))
  day<-as.numeric(format(time.char,"%d"))
  year<-as.numeric(format(time.char,"%Y"))
  hours.worn.total<- sum(temp.mat$Interval)/60/60
  hours.awake<- (record.sleep.time[ll]-record.getup.time[ll])*24
  sed.hour<- sum(temp.sed) /60/60
  stand.hour<- sum(subset(temp.mat,temp.mat$ActivityCode==1)$Interval) /60/60
  step.hour<- sum(subset(temp.mat,temp.mat$ActivityCode==2)$Interval) /60/60
  num.changes.from.sed.to.non.sed<- length.temp.sed
  ######## Code update here !
  step.count.total<- 2*nrow(subset(temp.mat,temp.mat$ActivityCode==2))  ##### in the event file, one row with ActivityCode==2 means two steps
  ########
  gini.index<- gini(temp.sed)
  num.hour.over.3.METs<-  sum(temp.mat$Interval[(temp.mat$METs/temp.mat$Interval)*60*60>3])/60/60
  MET.hours<- sum(temp.mat$METs)
  valid.day<-ifelse( (hours.worn.total<10 & hours.worn.total/hours.awake>0.8 & step.count.total>200) | (hours.worn.total>=10 & step.count.total>200 ),1,0    )
  dayofweek<-as.numeric(format(time.char,"%w"))
  weekday.or.weekend<- ifelse( dayofweek!=0 & dayofweek!=6,1,0)
  #table1<-c(person,group.char,www,month,day,year,hours.worn.total,hours.awake,sed.hour,stand.hour,step.hour,num.changes.from.sed.to.non.sed,step.count.total,gini.index,num.hour.over.3.METs,MET.hours,valid.day,dayofweek,weekday.or.weekend)
  table1<-c(person,group.char,www,month,day,year,hours.worn.total,hours.awake,sed.hour,stand.hour,step.hour,num.changes.from.sed.to.non.sed,step.count.total,gini.index,num.hour.over.3.METs,MET.hours,valid.day,dayofweek,weekday.or.weekend)
  table1.label<-c("id","group","week","month","day","year","hours.worn.total","hours.awake","sed.hour","stand.hour","step.hour","num.changes.from.sed.to.non.sed","step.count.total","gini.index","num.hour.over.3.METs","MET.hours","valid.day","dayofweek","weekday.or.weekend")
  
  out=list(  temp.mat= temp.mat,temp.sed= temp.sed,length.temp.sed=length.temp.sed,hour.char=hour.char, step.count.total= step.count.total,month = month, day = day,year=year,hours.worn.total=hours.worn.total, hours.awake=hours.awake, sed.hour=sed.hour, stand.hour=stand.hour, step.hour=step.hour, num.changes.from.sed.to.non.sed=num.changes.from.sed.to.non.sed, gini.index=gini.index,num.hour.over.3.METs=num.hour.over.3.METs,MET.hours=MET.hours,valid.day=valid.day,dayofweek=dayofweek,weekday.or.weekend=weekday.or.weekend,table1=table1,table1.label=table1.label)
  #table1.label<-c("id","group","week","month","day","year","hours.worn.total","hours.awake","sed.hour","stand.hour","step.hour","num.changes.from.sed.to.non.sed","step.count.total","gini.index","num.hour.over.3.METs","MET.hours","valid.day","dayofweek","weekday.or.weekend")
  #num.hour.over.3.METs=num.hour.over.3.METs,
  return(out)
}