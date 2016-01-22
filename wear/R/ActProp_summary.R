#' Physical Activity Summary--Activity Proportions
#'
#' Summarize sedentary, standing, and stepping activity measures
#' @param final_dat cleaned final data that is between record getup time and sleep time
#' @return \code{perc_sedentary} Proportion of sedentary
#' @return \code{perc_stand} Proportion of standing
#' @return \code{perc_step} Proportion of stepping
#' @return \code{step_per_day}  Number of steps during the given period of time
#' @return \code{break_per_day} Number of interrupting sedentary behavior during the given period of time
#' @return \code{break_rate} Rate of interrupting sedentary behavior
#' @return \code{MET_hour} METs hours
#' @examples  data(sampledata);ActProp_summary(sampledata)
#' @details  Proportion of sedentary/standing/stepping is the ratio of the total durations of sedentary/standing/step to the total worn time_
#' @details  For number of steps, the number of stepping records multiplied by 2_ This is because every two steps lead to one row's record in the event files_
#' @details Number of interrupting sedentary behavior is approximated by the number of  sedentary bouts_
#' @details Rate of interrupting sedentary behavior is the ratio of the daily number of interrupting sedentary behavior to total sedentary hours_
#' @details	 METs hours is the summation of METs hours from all activity records_

#'@export
#'

ActProp_summary=function (final_dat) {

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
    step_count_total<- 2*nrow(subset(final_dat_oneday,final_dat_oneday$ActivityCode==2))  ##### in the event file, one row with ActivityCode==2 means two steps

    # gini_index<- gini(temp_sed)
    MET_hour<- sum(final_dat_oneday$METs)
    #valid_day<-ifelse( (hours_worn_total<10 & hours_worn_total/hours_awake>0_8 & step_count_total>200) | (hours_worn_total>=10 & step_count_total>200 ),1,0    )
    dayofweek<-as.numeric(format(time_char,"%w"))
    weekday_or_weekend<- ifelse( dayofweek!=0 & dayofweek!=6,1,0)
    perc_sedentary<- 100*sed_hour/hours_worn_total
    perc_stand<- 100*stand_hour/hours_worn_total
    perc_step<- 100*step_hour/hours_worn_total
    step_per_day<-step_count_total
    break_per_day<-num_changes_from_sed_to_non_sed
    break_rate<-break_per_day/sed_hour
     #table1<-c(person,group_char,www,month,day,year,hours_worn_total,hours_awake,sed_hour,stand_hour,step_hour,num_changes_from_sed_to_non_sed,step_count_total,num_hour_over_3_METs,MET_hours,valid_day,dayofweek,weekday_or_weekend)
    table<-cbind(year,month,day,dayofweek,weekday_or_weekend,perc_sedentary,perc_stand,perc_step,step_per_day,break_per_day,break_rate,MET_hour)
    #print(table)

    table1=rbind(table1,table)

  }

  colnames(table1)<-c("year","month","day","dayofweek","weekday_or_weekend","perc_sedentary","perc_stand","perc_step","step_per_day","break_per_day","break_rate","MET_hour")

  # table<-cbind(year,month,day,dayofweek,weekday_or_weekend,hours_worn_total,sed_hour,stand_hour,step_hour,num_changes_from_sed_to_non_sed,step_count_total,num_hour_over_3_METs,MET_hours)
  #colnames(table)<-c("year","month","day","dayofweek","weekday_or_weekend","hours_worn_total","sed_hour","stand_hour","step_hour","num_changes_from_sed_to_non_sed","step_count_total","num_hour_over_3_METs","MET_hours")

  #out=list(year=year,month=month, day=day,dayofweek=dayofweek, weekday_or_weekend,hours_worn_total=hours_worn_total, sed_hour=sed_hour, stand_hour=stand_hour, step_hour=step_hour, num_changes_from_sed_to_non_sed=num_changes_from_sed_to_non_sed,step_count_total=step_count_total,num_hour_over_3_METs=num_hour_over_3_METs,MET_hours=MET_hours,table=table)

  #table1_label<-c("id","group","week","month","day","year","hours_worn_total","hours_awake","sed_hour","stand_hour","step_hour","num_changes_from_sed_to_non_sed","step_count_total","num_hour_over_3_METs","MET_hours","valid_day","dayofweek","weekday_or_weekend")
  #num_hour_over_3_METs=num_hour_over_3_METs
  return(table1)
}
