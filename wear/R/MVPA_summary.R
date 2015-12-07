#' Activity Summary--MVPA
#'
#' Summarize activity measures using MVPA
#' @param final.dat cleaned final data that is between record getup time and sleep time
#' @param record.sleep.time Log file of sleep time
#' @param record.getup.time Log file of getup time
#' @param ll the id of getup time record
#' @return ... ...
#' @export

MVPA_summary=function(final.dat,record.getup.time, record.sleep.time,ll,temp.activity)
{  temp.mat<-subset(final.dat,final.dat[,1]>record.getup.time[ll] & final.dat[,1]<record.sleep.time[ll] )
if(nrow(temp.mat)==0)next

mvpa.sporadic.interval<- 1/4          ### 1 means 1 minute; 0.5 means 30 seconds; 1/6 means 10 seconds


mvpa.1min.mat<-temp.mat
start.time<-mvpa.1min.mat$date.time[1]
end.time<-  mvpa.1min.mat$date.time[nrow(mvpa.1min.mat)]
num.1min.interval<- trunc( (end.time-start.time)*24*60*60/(60*mvpa.sporadic.interval) )   ###point 1
#(end.time-start.time) != sum(temp.mat$Interval)/60/60/24 if there is take off
interval.length<-mvpa.sporadic.interval*60/60/60/24
interval.1min.start<-start.time+interval.length*(1:(num.1min.interval))
##################
mvpa.record.start.time<-c(start.time, interval.1min.start)
mvpa.record.end.time<-c(interval.1min.start,end.time)
################## if there is take off, they won't be in combine.original.pseudo.mat
combine.original.pseudo.mat<-do.call(rbind,sapply(1: length(mvpa.record.start.time),function(ll){
  temp.mat<-subset(mvpa.1min.mat,mvpa.1min.mat[,1]+mvpa.1min.mat[,2]/24/60/60>mvpa.record.start.time[ll] & mvpa.1min.mat[,1]<mvpa.record.end.time[ll] )
  if(nrow(temp.mat)==0) return (NULL)
  if(temp.mat[nrow(temp.mat),1]+(temp.mat[nrow(temp.mat),2]/24/60/60)> mvpa.record.end.time[ll])  ###if this activity is the last one and it surpass the take off log time
  {
    temp.mat[nrow(temp.mat),4]<-temp.mat[nrow(temp.mat),4]*(mvpa.record.end.time[ll]-temp.mat[nrow(temp.mat),1])/(temp.mat[nrow(temp.mat),2]/24/60/60)
    temp.mat[nrow(temp.mat),2]<- (mvpa.record.end.time[ll]-temp.mat[nrow(temp.mat),1])*24*60*60
  }
  if(temp.mat[1,1]<mvpa.record.start.time[ll])   ###if this activity is the first one and itis earlier than the take on log time
  {
    temp.mat[1,4]<-temp.mat[1,4]* (temp.mat[1,2]-(mvpa.record.start.time[ll]- temp.mat[1,1])*24*60*60)/temp.mat[1,2]
    temp.mat[1,2]<-temp.mat[1,2]-(mvpa.record.start.time[ll]- temp.mat[1,1])*24*60*60
    temp.mat[1,1]<-mvpa.record.start.time[ll]
  }
  
  return(cbind(temp.mat,ll))
}, simplify = F)  )

colnames(combine.original.pseudo.mat)<-c("date.time","Interval","ActivityCode", "METs","one.minute.interval")
############################################################
############################################################ step2 summary 1 min intervals
############################################################
one.minute.collection<-by(combine.original.pseudo.mat,combine.original.pseudo.mat$one.minute.interval,function(s)c(min(s$date.time),sum(s$METs)*(60/mvpa.sporadic.interval),unique(s$one.minute.interval),sum(s$Interval)  )) ###point 3
one.minute.mat<-do.call(rbind,one.minute.collection)
one.minute.mat<-subset(one.minute.mat,one.minute.mat[,3]!=0 & one.minute.mat[,4]>(60*mvpa.sporadic.interval*0.9) & one.minute.mat[,4]<(60*mvpa.sporadic.interval*1.1)     )  ### one.minute.mat[,4] is the true length, it may not be exactly 30 second, can have a few seconds bias
#########################
if(trunc(nrow(one.minute.mat)/(10/mvpa.sporadic.interval))==0)  {
  ten.minute.vec<-rep(1,nrow(one.minute.mat))
  if(nrow(one.minute.mat)==1) ten.minute.mat<- data.frame(t(c(one.minute.mat[1:length(ten.minute.vec),],ten.minute.vec)))
  if(nrow(one.minute.mat)>1) ten.minute.mat<- data.frame(cbind(one.minute.mat[1:length(ten.minute.vec),],ten.minute.vec))
} else
{
  ten.minute.vec<-rep(1:trunc(nrow(one.minute.mat)/(10/mvpa.sporadic.interval)),each=  (10/mvpa.sporadic.interval)    )    ##### 30s to 10 min ###point 4
  ten.minute.mat<- data.frame(cbind(one.minute.mat[1:length(ten.minute.vec),],ten.minute.vec))
}

colnames(ten.minute.mat)<-c("date.time","mets","one.minute.interval","interval.length","ten.minute.interval")
############################################################
############################################################ step3 summary 10 min intervals
############################################################
#### if in 10 minutes, 8 minutes have METs>3, it is MVPA bout; if less than 8minutes, they are counted as mvpa sporadic.
is.mvpa<-function(s) if(s>=(8/mvpa.sporadic.interval)    ) return(1) else return(0)   ###point 5  ### this is for MVPA long bout
ten.minute.collection<- data.frame(do.call(rbind,by(ten.minute.mat,ten.minute.mat$ten.minute.interval,function(s)c(min(s$date.time), is.mvpa(length(which(s$mets>=3))), mean(s$mets),length(which(s$mets>=3)), abs(max(s$date.time)-min(s$date.time)-sum(s$interval.length[1:(length(s$interval.length)-1)])/24/60/60 )   ))))
colnames(ten.minute.collection)<-c("date.time","mvpa","mets","mvpa.sporadic","is.interval.valid")  #### is.interval.valid is to avoid the wear off during the day problem
ten.minute.collection<-subset(ten.minute.collection,is.interval.valid<0.003) ###if the interval has 5 minutes take off, we do not take it

############################################################
############################################################ step4 MVPA information
############################################################
#### total time
Total.MVPA.Long.Bout.time<-nrow(subset(ten.minute.collection,mvpa==1))/6  ###by hours
Total.MVPA.Sporadic.time<-sum(subset(ten.minute.collection,mvpa.sporadic>0 & mvpa!=1)$mvpa.sporadic )/(60/mvpa.sporadic.interval) ###by hours ###point 6
Total.MVPA.time<- Total.MVPA.Long.Bout.time+Total.MVPA.Sporadic.time
Total.light.time<- sum(temp.activity) /60/60-Total.MVPA.time
#### Long Bouts+Sporadic.time runs
Long.Bouts.and.Sporadic.run<- rle(ifelse( one.minute.mat[,2]>=3,1,0))
Total.Number.of.MVPA.Long.Bouts.and.Sporadic<-  length(which(Long.Bouts.and.Sporadic.run$values==1))

run.for.Long.Bouts.and.Sporadic.mvpa<- Long.Bouts.and.Sporadic.run$lengths[which(Long.Bouts.and.Sporadic.run$values==1)]/ (60/mvpa.sporadic.interval) ###by hours ###point 7

if(Total.MVPA.time==0)  Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.2<-0 else  Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.2<-100*length(run.for.Long.Bouts.and.Sporadic.mvpa[run.for.Long.Bouts.and.Sporadic.mvpa>1/30])/Total.Number.of.MVPA.Long.Bouts.and.Sporadic
if(Total.MVPA.time==0)  Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.5<-0 else  Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.5<-100*length(run.for.Long.Bouts.and.Sporadic.mvpa[run.for.Long.Bouts.and.Sporadic.mvpa>1/12])/Total.Number.of.MVPA.Long.Bouts.and.Sporadic
if(Total.MVPA.time==0)  Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.10<-0 else Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.10<-100*length(run.for.Long.Bouts.and.Sporadic.mvpa[run.for.Long.Bouts.and.Sporadic.mvpa>1/6])/Total.Number.of.MVPA.Long.Bouts.and.Sporadic


#### Long Bouts Only runs
run.for.mvpa<-rle(ten.minute.collection$mvpa)

Total.Number.of.MVPA.Long.Bouts<-  length(which(run.for.mvpa$values==1))
temp.mvpa.long.bout<-run.for.mvpa$lengths[which(run.for.mvpa$values==1)]/6 ###by hours
if(Total.Number.of.MVPA.Long.Bouts==0) Mean.MVPA.Long.Bout.Length<-0 else Mean.MVPA.Long.Bout.Length<-mean(temp.mvpa.long.bout)
####
if(Total.Number.of.MVPA.Long.Bouts==0)  Proportion.of.MVPA.Long.Bouts.greater.10<-0 else  Proportion.of.MVPA.Long.Bouts.greater.10<-100*length(temp.mvpa.long.bout[temp.mvpa.long.bout>1/6])/Total.Number.of.MVPA.Long.Bouts
if(Total.Number.of.MVPA.Long.Bouts==0)  Proportion.of.MVPA.Long.Bouts.greater.20<-0 else  Proportion.of.MVPA.Long.Bouts.greater.20<-100*length(temp.mvpa.long.bout[temp.mvpa.long.bout>2/6])/Total.Number.of.MVPA.Long.Bouts
#### percentile is not meaningful, always too short
####if(Total.Number.of.MVPA.Long.Bouts==0)
#### {
####percentile.MVPA.Long.Bouts.time.5<- 0
####percentile.MVPA.Long.Bouts.time.25<- 0
####percentile.MVPA.Long.Bouts.time.50<- 0
####percentile.MVPA.Long.Bouts.time.75<- 0
####percentile.MVPA.Long.Bouts.time.95<- 0
#### } else {
####  Percentiles.of.MVPA.Long.Bout.Length.quantile<- quantile(temp.mvpa.long.bout, probs = c(0.05,0.25,0.5,0.75,0.95))
####percentile.MVPA.Long.Bouts.time.5<- Percentiles.of.MVPA.Long.Bout.Length.quantile[1]
####percentile.MVPA.Long.Bouts.time.25<- Percentiles.of.MVPA.Long.Bout.Length.quantile[2]
####percentile.MVPA.Long.Bouts.time.50<- Percentiles.of.MVPA.Long.Bout.Length.quantile[3]
####percentile.MVPA.Long.Bouts.time.75<- Percentiles.of.MVPA.Long.Bout.Length.quantile[4]
####percentile.MVPA.Long.Bouts.time.95<- Percentiles.of.MVPA.Long.Bout.Length.quantile[5]
####        }


####################################################
#################################################### MET.value
####################################################
Highest.MET.value.15s<- max(one.minute.mat[,2])
Highest.MET.value.10min<- max(ten.minute.collection[,3])
################################################ MET.value from MVPA
Total.MET.hrs.Long.Bouts.and.Sporadic.mvpa<- sum((one.minute.mat[,2]/60/60*one.minute.mat[,4])[one.minute.mat[,2]>=3])
Total.MET.hrs.Long.Bouts<- sum((subset(ten.minute.collection,mvpa==1))$mets/60*10)


table5<- c(88888888,Total.light.time,Total.MVPA.time, Total.MVPA.Long.Bout.time,Total.MVPA.Sporadic.time,Total.Number.of.MVPA.Long.Bouts.and.Sporadic,Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.2,Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.5,Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.10, Total.Number.of.MVPA.Long.Bouts,Mean.MVPA.Long.Bout.Length, Proportion.of.MVPA.Long.Bouts.greater.10,Proportion.of.MVPA.Long.Bouts.greater.20, Highest.MET.value.15s,Highest.MET.value.10min, Total.MET.hrs.Long.Bouts.and.Sporadic.mvpa,Total.MET.hrs.Long.Bouts)
table5.label<- c("88888888","Total.light.time","Total.MVPA.time","Total.MVPA.Long.Bout.time","Total.MVPA.Sporadic.time","Total.Number.of.MVPA.Long.Bouts.and.Sporadic","Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.2","Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.5","Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.10","Total.Number.of.MVPA.Long.Bouts","Mean.MVPA.Long.Bout.Length","Proportion.of.MVPA.Long.Bouts.greater.10","Proportion.of.MVPA.Long.Bouts.greater.20","Highest.MET.value.15s","Highest.MET.value.10min","Total.MET.hrs.Long.Bouts.and.Sporadic.mvpa","Total.MET.hrs.Long.Bouts")
out=list( Total.light.time=Total.light.time,Total.MVPA.time=Total.MVPA.time, Total.MVPA.Long.Bout.time=Total.MVPA.Long.Bout.time,Total.MVPA.Sporadic.time=Total.MVPA.Sporadic.time,Total.Number.of.MVPA.Long.Bouts.and.Sporadic=Total.Number.of.MVPA.Long.Bouts.and.Sporadic,Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.2=Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.2,Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.5=Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.5,Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.10=Proportion.of.MVPA.Long.Bouts.and.Sporadic.greater.10, Total.Number.of.MVPA.Long.Bouts=Total.Number.of.MVPA.Long.Bouts,Mean.MVPA.Long.Bout.Length=Mean.MVPA.Long.Bout.Length, Proportion.of.MVPA.Long.Bouts.greater.10=Proportion.of.MVPA.Long.Bouts.greater.10,Proportion.of.MVPA.Long.Bouts.greater.20=Proportion.of.MVPA.Long.Bouts.greater.20, Highest.MET.value.15s= Highest.MET.value.15s,Highest.MET.value.10min=Highest.MET.value.10min, Total.MET.hrs.Long.Bouts.and.Sporadic.mvpa= Total.MET.hrs.Long.Bouts.and.Sporadic.mvpa,Total.MET.hrs.Long.Bouts=Total.MET.hrs.Long.Bouts,table5=table5,table5.label=table5.label)
return(out)

}