clean_time=function(dat){

if(is.numeric(dat$Time)==F) ####if  is.numeric(dat$Time)==F, we need further modification of time in next step
{
  ee<-as.character(dat$Time)
  max_length<-max(nchar(ee))
  ee[nchar(ee)!=max_length]<-"#1899-12-30 00:00:00#"
  #### use character type, may not be good
  ee_new<- (as.numeric( as.POSIXlt( substr(ee, 2, max_length-1 )  )    )+2209136400)/24/60/60
  #### use interval type, this is the best
  start_ee<-  min(which(dat[,2]>0))-1
  if (start_ee>1)
  {
    ee_new_int_type <-c( ee_new[1:((start_ee)-1)],ee_new[start_ee]+(dat$DataCount[start_ee:nrow(dat)]/10/24/60/60)  )
  }else
  {    ee_new_int_type <-ee_new[start_ee]+(dat$DataCount[start_ee:nrow(dat)]/10/24/60/60)
  } #### if interval type has large difference with character type, use character type
  int_dif_char<-which(abs(ee_new-ee_new_int_type)>0.1 )
  ee_new_int_type[int_dif_char]<-ee_new[int_dif_char]
  ####
  dat<-cbind(ee_new_int_type,dat[,2:6])
}
final_dat<-dat[,c(1,3,4,6)]
colnames(final_dat)<-c("date_time","Interval","ActivityCode", "METs")
final_dat=final_dat[final_dat$date_time!=0,] #delete rows with time as #1899-12-30 00:00:00#

new_date=0
final_dat=cbind(final_dat,new_date)
for (i in 1:nrow(final_dat)){
  #eg: 2010-03-31 23:59:59 is 40268.9583217593; 2010-04-01 00:00:00 is 40268.9583333333 but this is count as 2010-03-31
  if(final_dat$date_time[i]-floor(final_dat$date_time[i])<=0.125){ #eg: 2010-04-01 04:00:00 is 40269.125 but it is counted as 2010-03-31 so overwrite the date using 2010-03-31
    final_dat$new_date[i]= floor(final_dat$date_time[i])-1+0.125 }else{
      final_dat$new_date[i]= floor(final_dat$date_time[i])+0.125}
}
return(final_dat)
}
