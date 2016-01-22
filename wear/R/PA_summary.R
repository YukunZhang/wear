#' Physical Activity Summary-Summations
#'
#' Summarize activity measures
#' @param final_dat cleaned final data that is between record getup time and sleep time
#' @return \code{hours_worn_total} Total worn hours
#' @return \code{sed_hour}  Total sedantary hours
#' @return \code{stand_hour} Total stand hours
#' @return \code{step_hour} Total step hours
#' @return \code{num_changes_from_sed_to_non_sed} Numbers of changing from sedantary to non sadantary status
#' @return \code{step_count_total} Total Step count
#' @return \code{num_hour_over_3_METs} Number of hours that Metabolic Equivalent of Task (MET) is over 3
#' @return \code{MET_hours}  Total METs
#' @return \code{table} A table that shows all above results
#' @details All numbers are calculated in the given time period (day, hour, etc_)_ Total sedentary/standing/stepping hours are obtained from the summation of the duration times for sedentary/standing/stepping activities in the given time period_

#' @examples  data(sampledata);PA_summary(sampledata)
#' @export
PA_summary=function (final_dat) {

  dat=final_dat
  final_dat=clean_time(dat)
table1=c()
      ###################################################
  for (i in unique(final_dat$new_date)){
    final_dat_oneday=final_dat[final_dat$new_date==i,]
  time_char<-as.POSIXlt(i*24*60*60, origin = ISOdatetime(1899,12,30,0,0,0))

  #hour_char<-as.numeric(format(as.POSIXlt(temp_mat$date_time*24*60*60, origin = ISOdatetime(1899,12,30,0,0,0)),"%H"))
  ###
  temp_sed<-subset(final_dat_oneday,final_dat_oneday$ActivityCode==0)$Interval
  length_temp_sed<-length(temp_sed)
  month<-as.numeric(format(time_char,"%m"))
   day<-as.numeric(format(time_char,"%d"))
   year<-as.numeric(format(time_char,"%Y"))
  hours_worn_total<- sum(final_dat_oneday$Interval)/60/60
  #hours_awake<- (record_sleep_time[ll]-record_getup_time[ll])*24
  sed_hour<- sum(temp_sed) /60/60
  stand_hour<- sum(subset(final_dat_oneday,final_dat_oneday$ActivityCode==1)$Interval) /60/60
  step_hour<- sum(subset(final_dat_oneday,final_dat_oneday$ActivityCode==2)$Interval) /60/60
  num_changes_from_sed_to_non_sed<- length_temp_sed
  ######## Code update here !
  step_count_total<- 2*nrow(subset(final_dat_oneday,final_dat_oneday$ActivityCode==2))  ##### in the event file, one row with ActivityCode==2 means two steps
  ########
 # gini_index<- gini(temp_sed)
  num_hour_over_3_METs<-  sum(final_dat_oneday$Interval[(final_dat_oneday$METs/final_dat_oneday$Interval)*60*60>3])/60/60
  MET_hours<- sum(final_dat_oneday$METs)
  #valid_day<-ifelse( (hours_worn_total<10 & hours_worn_total/hours_awake>0_8 & step_count_total>200) | (hours_worn_total>=10 & step_count_total>200 ),1,0    )
  dayofweek<-as.numeric(format(time_char,"%w"))
  weekday_or_weekend<- ifelse( dayofweek!=0 & dayofweek!=6,1,0)
  #table1<-c(person,group_char,www,month,day,year,hours_worn_total,hours_awake,sed_hour,stand_hour,step_hour,num_changes_from_sed_to_non_sed,step_count_total,num_hour_over_3_METs,MET_hours,valid_day,dayofweek,weekday_or_weekend)
   table<-cbind(year,month,day,dayofweek,weekday_or_weekend,hours_worn_total,sed_hour,stand_hour,step_hour,num_changes_from_sed_to_non_sed,step_count_total,num_hour_over_3_METs,MET_hours)
  #print(table)

  table1=rbind(table1,table)

  }

  colnames(table1)<-c("year","month","day","dayofweek","weekday_or_weekend","hours_worn_total","sed_hour","stand_hour","step_hour","num_changes_from_sed_to_non_sed","step_count_total","num_hour_over_3_METs","MET_hours")

 # table<-cbind(year,month,day,dayofweek,weekday_or_weekend,hours_worn_total,sed_hour,stand_hour,step_hour,num_changes_from_sed_to_non_sed,step_count_total,num_hour_over_3_METs,MET_hours)
  #colnames(table)<-c("year","month","day","dayofweek","weekday_or_weekend","hours_worn_total","sed_hour","stand_hour","step_hour","num_changes_from_sed_to_non_sed","step_count_total","num_hour_over_3_METs","MET_hours")

  #out=list(year=year,month=month, day=day,dayofweek=dayofweek, weekday_or_weekend,hours_worn_total=hours_worn_total, sed_hour=sed_hour, stand_hour=stand_hour, step_hour=step_hour, num_changes_from_sed_to_non_sed=num_changes_from_sed_to_non_sed,step_count_total=step_count_total,num_hour_over_3_METs=num_hour_over_3_METs,MET_hours=MET_hours,table=table)

  #table1_label<-c("id","group","week","month","day","year","hours_worn_total","hours_awake","sed_hour","stand_hour","step_hour","num_changes_from_sed_to_non_sed","step_count_total","num_hour_over_3_METs","MET_hours","valid_day","dayofweek","weekday_or_weekend")
  #num_hour_over_3_METs=num_hour_over_3_METs
  return(table1)
}
