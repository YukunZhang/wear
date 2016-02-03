#' Standing/light intensity Summary
#'
#' Summarize Standing and light intensity activity measures
#' @param final_dat cleaned final data that is between record getup time and sleep time

#' @return \code{total_number_of_activity_bouts} Total events that are standing or stepping
#' @return \code{mean_activity_bout_length} Average length of activity bout
#' @return \code{prop_of_activity_time_greater_5min} The ratio of the number of active bouts greater than 5 min to the total number of active events
#' @return \code{prop_of_activity_time_greater_10min} The ratio of the number of active bouts greater than 10 min to the total number of active events.
#' @return \code{prop_of_activity_time_greater_30min} The ratio of the number of active bouts greater than 30 min to the total number of active events.
#' @return \code{total_activity_time_greater_5min} The summation of the active durations which are greater than 5 min
#' @return \code{total_activity_time_greater_10min} The summation of the active durations which are greater than 10 min
#' @return \code{total_activity_time_greater_30min} The summation of the active durations which are greater than 30 min
#' @return \code{percentile_activity_time_5} 5\% percentile of activity bouth length
#' @return \code{percentile_activity_time_25} 25\% percentile of activity bout length
#' @return \code{percentile_activity_time_50} 50\% percentile of activity bout length
#' @return \code{percentile_activity_time_75} 75\% percentile of activity bout length
#' @return \code{percentile_activity_time_95} 95\% percentile of activity bout length
#' @return \code{alpha_activity} alpha of activity time, see details
#' @return \code{stepping_to_standing_ratio} Ratio of stepping to standing, the ratio of total stepping hours to standing hours
#' @return \code{table} A table that combine all outputs
#' @details  \code{alpha_activity} is defined by \code{1+1/M}, where \code{M} is the average of \code{log(activity bout length /minimum activity bout length)}

#' @examples  data(sample_event);data(sample_bed_time);data(sample_takeon_time);SLI_summary(sample_event,sample_takeon_log,sample_bed_time)
#' @importFrom stats quantile
#' @export
#'

SLI_summary=function(final_dat,takeoff.time,bed.time){
  #library(reldist)
  final_dat=clean_time(final_dat,takeoff.time)
  table1=c()
  record.getup<-bed.time
  ###
  record.getup.time<-c()
  record.sleep.time<-c()
  for (kk in 1:nrow(record.getup))
  {
    temp.getup.time<-as.numeric(as.POSIXlt(strptime(as.character(paste(as.character(record.getup[kk,3] ),as.character(record.getup[kk,4]))),"%m/%d/%Y %H:%M:%S"))+2209136400)/24/60/60
    record.getup.time<-c(record.getup.time,temp.getup.time)
    temp.sleep.time<-as.numeric(as.POSIXlt(strptime(as.character(paste(as.character(record.getup[kk,5] ),as.character(record.getup[kk,6]))),"%m/%d/%Y %H:%M:%S"))+2209136400)/24/60/60
    record.sleep.time<-c(record.sleep.time,temp.sleep.time)
  }

  temp_mat_for_activity<- final_dat
  temp_mat_for_activity$Activity[temp_mat_for_activity$Activity==1]<-2
  end_pos<-cumsum(rle(temp_mat_for_activity$Activity)$lengths)
  start_pos<-c(0,end_pos[1:(length(end_pos)-1)])+1
  ############### for each runs, handle the data
  handle_runs<- sapply(1:length(end_pos),function(x,data_mat=temp_mat_for_activity)
  {
    select_data<-data_mat[start_pos[x]:end_pos[x],]
    combine_data<- c(min(select_data$date_time),sum(select_data$Interval),max(select_data$Activity),sum(select_data$METs),min(select_data$new_date) )
    return(combine_data)
  }, simplify=F
  )
  ############### combine each run
  combined_temp_mat_for_activity<-data.frame(do.call(rbind,handle_runs))
  colnames(combined_temp_mat_for_activity)<-c("date_time", "Interval", "ActivityCode","METs","new_date")
  ###############
  table1=c()
  ###################################################
  ll=0 #count for getup time

  for (i in unique(combined_temp_mat_for_activity$new_date)){
    ll=ll+1
    temp_mat_oneday=final_dat[final_dat$new_date==i,]
  temp_mat_oneday<-temp_mat_oneday[temp_mat_oneday[,1]>record.getup.time[ll] &temp_mat_oneday[,1]<record.sleep.time[ll], ]

    final_dat_oneday=combined_temp_mat_for_activity[combined_temp_mat_for_activity$new_date==i,]
    final_dat_oneday<-final_dat_oneday[final_dat_oneday[,1]>record.getup.time[ll] &final_dat_oneday[,1]<record.sleep.time[ll], ]

    time_char<-as.POSIXlt(i*24*60*60, origin = ISOdatetime(1899,12,30,0,0,0))
    month<-as.numeric(format(time_char,"%m"))
    day<-as.numeric(format(time_char,"%d"))
    year<-as.numeric(format(time_char,"%Y"))



  ###############Calculation
  ###############
  ###
  temp_activity<-subset(final_dat_oneday,final_dat_oneday$ActivityCode==2)$Interval
  ###
  length_temp_activity<-length(temp_activity)
  total_number_of_activity_bouts<- length_temp_activity
  mean_activity_bout_length<- mean(temp_activity) /60/60

  prop_of_activity_time_greater_5min<- 100*length(temp_activity[temp_activity>5*60])/length_temp_activity
  prop_of_activity_time_greater_10min<- 100*length(temp_activity[temp_activity>10*60])/length_temp_activity
  prop_of_activity_time_greater_30min<- 100*length(temp_activity[temp_activity>30*60])/length_temp_activity

  total_activity_time_greater_5min<- sum(temp_activity[temp_activity>5*60])/60/60
  total_activity_time_greater_10min<- sum(temp_activity[temp_activity>10*60])/60/60
  total_activity_time_greater_30min<- sum(temp_activity[temp_activity>30*60])/60/60

  quantile_activity_temp<-quantile(temp_activity, probs = c(0.05,0.25,0.5,0.75,0.95))/60/60
  percentile_activity_time_5<- quantile_activity_temp[1]
  percentile_activity_time_25<- quantile_activity_temp[2]
  percentile_activity_time_50<- quantile_activity_temp[3]
  percentile_activity_time_75<- quantile_activity_temp[4]
  percentile_activity_time_95<- quantile_activity_temp[5]

  alpha_activity<- 1+ 1/mean(log(temp_activity/ min(temp_activity)))
  #gini_index_activity<- gini(temp_activity)
  step_hour<- sum(subset(temp_mat_oneday,temp_mat_oneday$ActivityCode==2)$Interval) /60/60
  stand_hour<- sum(subset(temp_mat_oneday,temp_mat_oneday$ActivityCode==1)$Interval) /60/60

  stepping_to_standing_ratio<- step_hour/stand_hour

  table<- cbind(year,month,day,total_number_of_activity_bouts,mean_activity_bout_length,prop_of_activity_time_greater_5min,prop_of_activity_time_greater_10min,prop_of_activity_time_greater_30min,total_activity_time_greater_5min,total_activity_time_greater_10min,total_activity_time_greater_30min,percentile_activity_time_5,percentile_activity_time_25,percentile_activity_time_50,percentile_activity_time_75,percentile_activity_time_95,alpha_activity,stepping_to_standing_ratio)
  row.names(table)=NULL
  table1=rbind(table1,table)
  #print(table1)

  }
  colnames(table1)<- c("year","month","day","total_number_of_activity_bouts","mean_activity_bout_length","prop_of_activity_time_greater_5min","prop_of_activity_time_greater_10min","prop_of_activity_time_greater_30min","total_activity_time_greater_5min","total_activity_time_greater_10min","total_activity_time_greater_30min","percentile_activity_time_5","percentile_activity_time_25","percentile_activity_time_50","percentile_activity_time_75","percentile_activity_time_95","alpha_activity","stepping_to_standing_ratio")

  #out=list( total_number_of_activity_bouts=total_number_of_activity_bouts,mean_activity_bout_length=mean_activity_bout_length,prop_of_activity_time_greater_5min=prop_of_activity_time_greater_5min,prop_of_activity_time_greater_10min=prop_of_activity_time_greater_10min,prop_of_activity_time_greater_30min=prop_of_activity_time_greater_30min,total_activity_time_greater_5min=total_activity_time_greater_5min,total_activity_time_greater_10min=total_activity_time_greater_10min,total_activity_time_greater_30min=total_activity_time_greater_30min,percentile_activity_time_5=percentile_activity_time_5,percentile_activity_time_25=percentile_activity_time_25,percentile_activity_time_50=percentile_activity_time_50,percentile_activity_time_75=percentile_activity_time_75,percentile_activity_time_95=percentile_activity_time_95,alpha_activity=alpha_activity,stepping_to_standing_ratio=stepping_to_standing_ratio,table=table)
  return(table1)
}
