#' Cut time outside range of log record
#'
#'  Delete wrong data, all records should between log time for take on and take off. For the case that start in log time but last long outside the log, we just leave the time in the interval.
#' @param ll Row number of record get up time
#' @return Data with right take on and take off time
#' @export

time_consistent=function(dat, record.start.time, record.end.time,ll){
  temp.mat<-subset(dat,dat[,1]+dat[,3]/24/60/60>record.start.time[ll] & dat[,1]<record.end.time[ll] )
  if(nrow(temp.mat)==0) return (NULL)
  if(temp.mat[nrow(temp.mat),1]+(temp.mat[nrow(temp.mat),3]/24/60/60)> record.end.time[ll])  ###if this activity is the last one and it surpass the take off log time
  {
    temp.mat[nrow(temp.mat),6]<-temp.mat[nrow(temp.mat),6]*(record.end.time[ll]-temp.mat[nrow(temp.mat),1])/(temp.mat[nrow(temp.mat),3]/24/60/60)
    temp.mat[nrow(temp.mat),3]<- (record.end.time[ll]-temp.mat[nrow(temp.mat),1])*24*60*60
  }
  if(temp.mat[1,1]<record.start.time[ll])   ###if this activity is the first one and itis earlier than the take on log time
  {
    temp.mat[1,6]<-temp.mat[1,6]* (temp.mat[1,3]-(record.start.time[ll]- temp.mat[1,1])*24*60*60)/temp.mat[1,3]
    temp.mat[1,3]<-temp.mat[1,3]-(record.start.time[ll]- temp.mat[1,1])*24*60*60
    temp.mat[1,1]<-record.start.time[ll]
  }

  return(temp.mat)
}
