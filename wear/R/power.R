#' Report participant's wear summary
#'
#' Import daily raw results and summarize them, export a summary table
#' @param takeoff.time  The log file of taking off time
#' @param bed.time  The log file of sleeping time
#' @param n number of observations/participants
#' @param folder.to.export.APST.csvfile.and.RDatafile  The folder to export files
#' @return the summary of wear
#' @export


#######################################################################################################
#######################################################################################################
wear=function(takeoff.time,bed.time,folder.to.export.APST.csvfile.and.RDatafile){

  for (person in 1:2)
  {
    #######################################################read data
    ######## person<-1
    id<-person
    if(nchar(id)==1) id.char<-paste("0",id,sep="") else  id.char<-as.character(id)
    #a<-try(eval(parse(text=paste("dat<-read.csv('APST",id.char,"W12_event.csv')",sep=""))))
    if(week==0)a<-try(eval(parse(text=paste("dat<-read.csv('APST",id.char,"B_event.csv')",sep="")))) else a<-try(eval(parse(text=paste("dat<-read.csv('APST",id.char,"W",week,"_event.csv')",sep=""))))
    if(class(a)=="try-error") next

    #######################################################build record's begin and end time from log file/// May not be true!// trust data only
    record<-subset(takeoff.time,takeoff.time$id==person)

    ###
    if(nrow(record)==0)next
    ###
    record.start.time<-c()
    record.end.time<-c()
    for (kk in 1:nrow(record))
    {
      temp.start.time<-as.numeric(as.POSIXlt(strptime(as.character(paste(as.character(record[kk,3] ),as.character(record[kk,4]))),"%m/%d/%Y %H:%M:%S"))+2209190400)/24/60/60
      record.start.time<-c(record.start.time,temp.start.time)

      temp.end.time<-as.numeric(as.POSIXlt( strptime(as.character(paste(as.character(record[kk,5] ),as.character(record[kk,6]))),"%m/%d/%Y %H:%M:%S")  )+2209190400)/24/60/60
      record.end.time<-c(record.end.time,temp.end.time)
    }
    ##########################################  match get up time with take on time/// if multiple take on time in a day, the second take on time is seen as get up time
    #####list days take on
    #####time.char<-as.numeric(format(as.POSIXlt(record.start.time*24*60*60, origin = ISOdatetime(1899,12,30,0,0,0) ),"%d"))
    ###
    record.getup<-subset(bed.time,bed.time$id==person)
    ###
    record.getup.time<-c()
    record.sleep.time<-c()
    for (kk in 1:nrow(record.getup))
    {
      temp.getup.time<-as.numeric(as.POSIXlt(strptime(as.character(paste(as.character(record.getup[kk,3] ),as.character(record.getup[kk,4]))),"%m/%d/%Y %H:%M:%S"))+2209190400)/24/60/60
      record.getup.time<-c(record.getup.time,temp.getup.time)
      temp.sleep.time<-as.numeric(as.POSIXlt(strptime(as.character(paste(as.character(record.getup[kk,5] ),as.character(record.getup[kk,6]))),"%m/%d/%Y %H:%M:%S"))+2209190400)/24/60/60
      record.sleep.time<-c(record.sleep.time,temp.sleep.time)
    }
    #######################################################
    ##########################################  match get up time with take on time/// if multiple take on time in a day, the second take on time is seen as get up time
    #######################################################
    ####################################################### Unify date structure  delete
    print(is.numeric(dat$Time))
    if(is.numeric(dat$Time)==F) ####if  is.numeric(dat$Time)==F, we need further modification of time in next step
    {
      ee<-as.character(dat$Time)
      max.length<-max(nchar(ee))
      print(max.length)
      ee[nchar(ee)!=max.length]<-"#1899-12-30 00:00:00#"
      #### use character type, may not be good
      ee.new<- (as.numeric( as.POSIXlt( substr(ee, 2, max.length-1 )  )    )+2209190400)/24/60/60
      #### use interval type, this is the best
      start.ee<-  min(which(dat[,2]>0))-1
      print(start.ee)
      ee.new.int.type <-c( ee.new[1:((start.ee)-1)],ee.new[start.ee]+(dat$DataCount[start.ee:nrow(dat)]/10/24/60/60)  )
      #### if interval type has large difference with character type, use character type
      int.dif.char<-which(abs(ee.new-ee.new.int.type)>0.1 )
      ee.new.int.type[int.dif.char]<-ee.new[int.dif.char]
      ####
      dat<-cbind(ee.new.int.type,dat[,2:6])
    }

    dat<- dat[,1:6]  #### this works for both time good and bad data
    #######################################################  delete wrong data, al records should between log time for take on and take off
    #######################################################  For the case that start in log time but last long outside the log, we just leave the time in the interval
    final.dat<-do.call(rbind,sapply( 1: length(record.start.time),function(ll){
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
    }, simplify = F))



    if(length(final.dat)==0)next
    #######################################################
    ####################################################### Dataset with Records

    final.dat<-final.dat[,c(1,3,4,6)]
    colnames(final.dat)<-c("date.time","Interval","ActivityCode", "METs")
    if(is.numeric(dat[,1])==T) eval(parse(text=paste("APST",id.char,"week",week,"<-final.dat",sep=""))) else   next
    #######################################################
    ####################################################### Summary Statistics
    #######################################################
    for (ll in 1: length(record.getup.time) )
    {
      ## ll=1

      ###################################################
      ###################################################
      ################################################### For Table part 1 evan_final_2_23
      ###################################################
      ### group
      group.char<- as.character(subset(import.APST.csvfile.groups,id==person)$group)
      if (length(group.char)==0) group.char<- NA                                                                    ##### some id does not have group
      ###

      out1=PA_summary(final.dat,record.sleep.time,record.getup.time,ll,person,group.char,www)
month=out1$month
day=out1$day
year=out1$year
hours.worn.total=out1$hours.worn.total
hours.awake=out1$hours.awake
sed.hour=out1$sed.hour
stand.hour=out1$stand.hour
step.hour=out1$step.hour
num.changes.from.sed.to.non.sed=out1$num.changes.from.sed.to.non.sed
gini.index=out1$gini.index
num.hour.over.3.METs=out1$num.hour.over.3.METs
MET.hours=out1$MET.hours
valid.day=out1$valid.day
dayofweek=out1$dayofweek
weekday.or.weekend=out1$weekday.or.weekend
temp.sed=out1$ temp.sed
length.temp.sed=out1$length.temp.sed
hour.char=out1$hour.char
step.count.total=out1$ step.count.total
temp.mat=out1$ temp.mat
table1=out1$table1
table1.label=out1$table1.label
      ###################################################
      ################################################### For Table part 2  Doc to fill
      ###################################################
out2=prop_summary(final.dat,record.getup.time, record.sleep.time,ll)

table2=out2$table2
table2.label=out2$table2.label
  ###################################################
      ################################################### For Table part 3  Sarah's table
      ###################################################

      perc.sedentary<- 100*sed.hour/hours.worn.total
      perc.stand<- 100*stand.hour/hours.worn.total
      perc.step<- 100*step.hour/hours.worn.total
      step.per.day<-step.count.total
      break.per.day<-num.changes.from.sed.to.non.sed
      break.rate<-break.per.day/sed.hour
      MET.hour<- MET.hours

      table3<- c(88888888,perc.sedentary,perc.stand,perc.step,step.per.day,break.per.day,break.rate,MET.hour)
      table3.label<- c("88888888","perc.sedentary","perc.stand","perc.step","step.per.day","break.per.day","break.rate","MET.hour")

      ###################################################
      ################################################### Calculate Activity information
      ###################################################
      ### for standing and stepping, we have to combine the data
      ### since this is for activity, we take standing and stepping as the same activity
      ############### start and ends of each runs
     out4=Activity_summary(final.dat,record.getup.time, record.sleep.time,ll)
      table4=out4$table4
      table4.label=out4$table4.label
      temp.activity=out4$temp.activity
        ####################
      ################################################################################################
      ###################################################Calculate MVPA information##################
      ################################################################################################
      ############################################################
      ############################################################ step 1, build 1min intervals.
      ############################################################
      ### The default X interval is 1 min/ the suggested intervals can be 30s, 10s
      ###
     out5=MVPA_summary(final.dat,record.getup.time, record.sleep.time,ll,temp.activity)
      table5=out5$table5
      table5.label=out5$table5.label
      ###################################################
      ###################################################
      ################################################### For Sum Table
      ###################################################
      temp.Summary.Statistics.Table<- t(c(table1,table2,table3,table4,table5))
      colnames(temp.Summary.Statistics.Table)<-c(table1.label,table2.label,table3.label,table4.label,table5.label)

      Summary.Statistics.Table<-rbind(Summary.Statistics.Table,temp.Summary.Statistics.Table)
    }

  }

  setwd(folder.to.export.APST.csvfile.and.RDatafile)
  if(week==0)write.table(Summary.Statistics.Table, file ="Summary.Statistics.Table.csv", append=T, sep=',', row.names = F)
  if(week>0)write.table(Summary.Statistics.Table, file ="Summary.Statistics.Table.csv", append=T, sep=',',col.names=F, row.names = F)

  #rm(list= ls()[grep("APST", ls(),invert=T )])


}

#eval(parse(text=paste("save.image('Combine.RData')",sep="")))

