revalue_birth<-function(data){

plyr::revalue(as.factor(data$Birth),
  c("'02/93" = "02/93",
"'10/09" = "10/09",
"10/'4" = "10/14",
"?/02" = "06/02",
"?/14"="06/14",
"1015" = "10/15",
"?/16" = "06/16",
"RC1" = "07/16",
"RC2" = "05/17",
"RC3" = "05/17",
"?/17" = "03/17",   #birth between 01 and 05/17 based on observations
"43809" = "10/12",
"10.20" = "10/20",
"?/20" = "07/20",
"?/21" = "11/21",
"10/ 21" = "10/21",
"'07/22" = "07/22",
"'04/22" = "04/22"))


DATA$Birth[which(DATA$GLT=="SI10" | DATA$GLT=="SI11")]<-"11/07"
DATA$Birth[which(DATA$GLT=="SI12" | DATA$GLT=="SI13")]<-"12/07"

DATA$Birth[which(DATA$GLT=="RT20" | DATA$GLT=="RT21")]<-"06/07"

DATA$Birth[which(DATA$GLT=="MP44")]<-"02/10"

DATA$Birth[which(DATA$GLT=="JP19")]<-"07/10"

DATA$Birth[which(DATA$GLT=="JP20")]<-"09/10"



#issue with FA20 (2 birth dates!!)
}
