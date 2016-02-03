#' Physical Activity Summary--Sedentary Features
#'
#' Summarize sendentary features using proportions and percentiles
#' @param final_dat cleaned final data that is between record getup time and sleep time

#' @return \code{total_sed_time} Total sedantary hour, same as PA_summary function output sed_hour
#' @return \code{total_number_of_sed_bouts} Total number of sedentary bouts_ It counts the number of sedentary recordings
#' @return \code{break_per_day} Number of interrupting sedentary behavior during the given period of time
#' @return \code{break_rate} Rate of interrupting sedentary behavior
#' @return \code{mean_sed_bout_length} Mean sedentary bout length_ It is the average of the duration time for all sedentary activities
#' @return \code{prop_of_sed_time_greater_20min} Proportions of sedentary bout greater than 20 minutes
#' @return \code{prop_of_sed_time_greater_60min} Proportions of sedentary bout greater than 60 minutes
#' @return \code{prop_of_sed_time_greater_120min} Proportions of sedentary bout greater than 120 minutes
#' @return \code{total_sed_time_greater_20min} Total sedentary time greater than 20 minutes
#' @return \code{total_sed_time_greater_60min} Total sedentary time greater than 60 minutes
#' @return \code{total_sed_time_greater_120min} Total sedentary time greater than 120 minutes
#' @return \code{percentile_sed_time_5}  5\% Percentile of sedentary time
#' @return \code{percentile_sed_time_25} 25\% Percentile of sedentary time
#' @return \code{percentile_sed_time_50} 50\% Percentile of sedentary time
#' @return \code{percentile_sed_time_75} 75\% Percentile of sedentary time
#' @return \code{percentile_sed_time_95} 95\% Percentile of sedentary time
#' @return \code{alpha_sed} alpha of sendataty time, see details_
#' @return \code{prop_sed_time_6_12} Proportions of sedentary time between 6:00-12:00
#' @return \code{prop_sed_time_12_18} Proportions of sedentary time between 12:00-18:00
#' @return \code{prop_sed_time_18_22} Proportions of sedentary time between 18:00-22:00
#' @details Proportions of sedentary bout greater than 20/60/120 minutes is the ratio of the number of sedentary bouts greater than 20/60/120 minutes to the total number of sedentary recordings_
#' @details Total sedentary time greater than 20/60/120 minutes is the summation of the sedentary durations which are greater than 20/60/120 minutes_
#' @details To calculate 5\%/25\%/___/95\% percentile of sedentary time, all of the recorded sedentary durations are listed and R function \emph{quantile} is used to find the percentiles_
#' @details  alpha_sed is defined by \code{1+1/M}, where \code{M} is the average of \code{log(sedentary bout length /minimum sedentary bout length)}_
#' @details Proportions of sedentary time between 6:00-12:00/12:00-18:00/18:00-22:00 is the ratio of the sedentary durations to the total activity durations between 6:00-12:00/12:00-18:00/18:00-22:00_


#' @examples  data(sample_event);data(sample_bed_time);data(sample_takeon_time);Sedentary_summary(sample_event,sample_takeon_log,sample_bed_time)
#' @importFrom stats quantile
#' @export

Sedentary_summary=function(final_dat,takeoff.time,bed.time){

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
  ###################################################
  ll=0 #count for getup time

  for (i in unique(final_dat$new_date)){
    ll=ll+1
    temp_mat_oneday=final_dat[final_dat$new_date==i,]
    temp_mat_oneday<-temp_mat_oneday[temp_mat_oneday[,1]>record.getup.time[ll] &temp_mat_oneday[,1]<record.sleep.time[ll], ]

    time_char<-as.POSIXlt(i*24*60*60, origin = ISOdatetime(1899,12,30,0,0,0))
    month<-as.numeric(format(time_char,"%m"))
    day<-as.numeric(format(time_char,"%d"))
    year<-as.numeric(format(time_char,"%Y"))
  hour_char<-as.numeric(format(as.POSIXlt(temp_mat_oneday$date_time*24*60*60, origin = ISOdatetime(1899,12,30,0,0,0)),"%H"))
  temp_sed<-subset(temp_mat_oneday,temp_mat_oneday$ActivityCode==0)$Interval
  length_temp_sed<-length(temp_sed)
  num_changes_from_sed_to_non_sed<- length_temp_sed

  sed_hour<- sum(temp_sed) /60/60
  length_temp_sed<-length(temp_sed)
  num_changes_from_sed_to_non_sed<- length_temp_sed
  total_sed_time<-sed_hour
  total_number_of_sed_bouts<-num_changes_from_sed_to_non_sed
  mean_sed_bout_length<- mean(temp_sed) /60/60
  prop_of_sed_time_greater_20min<- 100*length(temp_sed[temp_sed>20*60])/length_temp_sed
  prop_of_sed_time_greater_60min<- 100*length(temp_sed[temp_sed>60*60])/length_temp_sed
  prop_of_sed_time_greater_120min<- 100*length(temp_sed[temp_sed>120*60])/length_temp_sed

  total_sed_time_greater_20min<- sum(temp_sed[temp_sed>20*60])/60/60
  total_sed_time_greater_60min<- sum(temp_sed[temp_sed>60*60])/60/60
  total_sed_time_greater_120min<- sum(temp_sed[temp_sed>120*60])/60/60

  quantile_temp<-quantile(temp_sed, probs = c(0.05,0.25,0.5,0.75,0.95))/60/60
  percentile_sed_time_5<- quantile_temp[1]
  percentile_sed_time_25<- quantile_temp[2]
  percentile_sed_time_50<- quantile_temp[3]
  percentile_sed_time_75<- quantile_temp[4]
  percentile_sed_time_95<- quantile_temp[5]

  alpha_sed<- 1+ 1/mean(log(temp_sed/ min(temp_sed)))
  #gini_index_sed<- gini(temp_sed)

  prop_sed_time_6_12<- 100*sum(subset(temp_mat_oneday,temp_mat_oneday$ActivityCode==0 & hour_char>=6 & hour_char<12)$Interval) /(sum(subset(temp_mat_oneday,hour_char>=6 & hour_char<12)$Interval)+0.0001)   ###prevent this value is zero
  prop_sed_time_12_18<- 100*sum(subset(temp_mat_oneday,temp_mat_oneday$ActivityCode==0 & hour_char>=12 & hour_char<18)$Interval) /(sum(subset(temp_mat_oneday,hour_char>=12 & hour_char<18)$Interval)+0.0001)
  prop_sed_time_18_22<- 100*sum(subset(temp_mat_oneday,temp_mat_oneday$ActivityCode==0 & hour_char>=18 & hour_char<22)$Interval) /(sum(subset(temp_mat_oneday,hour_char>=18 & hour_char<22)$Interval)+0.0001)

  break_per_day<-num_changes_from_sed_to_non_sed
  break_rate<-break_per_day/sed_hour
  table<- cbind(year,month,day,total_sed_time,total_number_of_sed_bouts,break_per_day,break_rate,mean_sed_bout_length,prop_of_sed_time_greater_20min,prop_of_sed_time_greater_60min,prop_of_sed_time_greater_120min,total_sed_time_greater_20min,total_sed_time_greater_60min,total_sed_time_greater_120min,percentile_sed_time_5,percentile_sed_time_25,percentile_sed_time_50,percentile_sed_time_75,percentile_sed_time_95,alpha_sed,prop_sed_time_6_12,prop_sed_time_12_18,prop_sed_time_18_22)
  row.names(table)=NULL
  table1=rbind(table1,table)
  #print(table1)

  }
  colnames(table1)<- c("year","month","day","total_sed_time","total_number_of_sed_bouts","break_per_day","break_rate","mean_sed_bout_length" ,"prop_of_sed_time_greater_20min","prop_of_sed_time_greater_60min","prop_of_sed_time_greater_120min","total_sed_time_greater_20min","total_sed_time_greater_60min","total_sed_time_greater_120min","percentile_sed_time_5","percentile_sed_time_25","percentile_sed_time_50","percentile_sed_time_75","percentile_sed_time_95","alpha_sed","prop_sed_time_6_12","prop_sed_time_12_18","prop_sed_time_18_22")

  #out=list( total_number_of_activity_bouts=total_number_of_activity_bouts,mean_activity_bout_length=mean_activity_bout_length,prop_of_activity_time_greater_5min=prop_of_activity_time_greater_5min,prop_of_activity_time_greater_10min=prop_of_activity_time_greater_10min,prop_of_activity_time_greater_30min=prop_of_activity_time_greater_30min,total_activity_time_greater_5min=total_activity_time_greater_5min,total_activity_time_greater_10min=total_activity_time_greater_10min,total_activity_time_greater_30min=total_activity_time_greater_30min,percentile_activity_time_5=percentile_activity_time_5,percentile_activity_time_25=percentile_activity_time_25,percentile_activity_time_50=percentile_activity_time_50,percentile_activity_time_75=percentile_activity_time_75,percentile_activity_time_95=percentile_activity_time_95,alpha_activity=alpha_activity,stepping_to_standing_ratio=stepping_to_standing_ratio,table=table)
  return(table1)
}

