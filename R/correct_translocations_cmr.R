
correct_translocation_cmr<-function(data){
  
data$State[data$GLT=="UR2" & data$Year==2021]<-1
data$State[data$GLT=="PT5" & data$Year>2002]<-1
data$State[data$GLT=="OL11" & data$Year>2001]<-1
data$State[data$GLT=="MP192" & data$Year>2021]<-2
data$State[data$GLT=="JG7" & data$Year>2001]<-1
data$State[data$GLT=="JG6" & data$Year>2001]<-1
data$State[data$GLT=="JG5" & data$Year>2001]<-1
data$State[data$GLT=="JG4" & data$Year>2001]<-1
data$State[data$GLT=="FP10" & data$Year>2020]<-2
data$State[data$GLT=="FA41" & data$Year>2021]<-1
data$State[data$GLT=="FA3" & data$Year>2011]<-1
data$State[data$GLT=="FA33" & data$Year>2018]<-1
data$State[data$GLT=="FA35" & data$Year>2019]<-1
data$State[data$GLT=="FA39" & data$Year>2020]<-1
data$State[data$GLT=="FA41" & data$Year>2020]<-1
data$State[data$GLT=="FA10" & data$Year>2014]<-1
data$State[data$GLT=="AF17" & data$Year>2011]<-1
data$State[data$GLT=="FA15" & data$Year>2017]<-1
data$State[data$GLT=="FA16" & data$Year>2017]<-1
data$State[data$GLT=="FA21" & data$Year>2017]<-1 #correction oct 2023
data$State[data$GLT=="FA20" & data$Year>2016]<-1 #correction Oct 2023


return(data)

}