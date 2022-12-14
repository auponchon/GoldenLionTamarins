extract_long_data <- function(file) {
  
  
  ifelse("Plan1" %in% excel_sheets(file),
    blocks<-read_excel(file,sheet="Plan1", n_max = 274),
 blocks<-read_excel(file,sheet="Sheet1", n_max = 274))
  
  mino<-min(which(blocks[ ,1]=="Grupo"))
  
  #cut dataset in 2 blocks before binding the second one under first one if 8 columns
 if(!is.na(blocks[mino,8])){
  
  
  block1<-blocks[,c(1:8)]
  block2<-blocks[,c(10:17)]
  
  names(block1)<-names(block2)<-c("Grupo",	"GLT",	"Nyanzol"	,"Tattoo",	"Sexo",	"Idade",	"Birth",	"Radio")
  block <- rbind(block1, block2)
  
 }
  
  #cut dataset in 2 blocks before binding the second one under first one if 7 columns
 if(is.na(blocks[mino,8])){
        block1<-blocks[,c(1:7)]
        block2<-blocks[,c(9:15)]
        
names(block1)<-names(block2)<-c("Grupo",	"GLT",	"Nyanzol"	,"Tattoo",	"Sexo",	"Birth",	"Radio")
block <- rbind(block1, block2) %>% 
  mutate(Idade=NA) %>% 
  dplyr::select("Grupo",	"GLT",	"Nyanzol"	,"Tattoo",	"Sexo",	"Idade",	"Birth",	"Radio")
    
block$GLT[block$GLT=="Mico"|block$GLT=="mico"]<-"GLT"
   
  }
  
  #order column names and filter data
  block.full <- block  %>%
    dplyr::filter_all(any_vars(!is.na(.))) %>%    #remove empty rows
    dplyr::mutate(Region=ifelse(!is.na(Grupo) & is.na(GLT),   #associate region
                                Grupo,NA)) %>% 
    fill(Region) %>%
    dplyr::filter(GLT != "GLT" ) %>% #remove headers
    mutate(Disp = ifelse(Grupo %in% dispersers |
                           Radio %in% dispersers |
                           grepl("disp", Grupo, fixed = T) |           #identify dispersal
                           grepl("disp", Radio, fixed = T) ,
                        "1",
                         "0"),
           Death = ifelse(grepl("ied", Grupo, fixed = T) |
                            grepl("ied", Radio, fixed = T) |       #identify dead ind
                            Grupo %in% dead,
                          "1",
                          "0"),
           Other = ifelse(
             Grupo %in% solo,
             "Alone",                                               #account for other comments (lonely ind)
             ifelse(Grupo == "não capt.",
                    "Not capt.",
                    ifelse(Grupo == "Expulsa", 
                           "Expulsed",
                           NA))),
           DateEvent = ifelse(
             grepl("/", Grupo, fixed = T),
             str_extract(Grupo, "[0-9]{2}/[0-9]{2}/[0-9]{2}"),        #date of dispersal or death if available
             ifelse(
               grepl("/", Radio, fixed = T),
               str_extract(Radio, "[0-9]{2}/[0-9]{2}/[0-9]{2}"),
               NA))) 
  
  
  
 clean<- block.full  %>% 
    mutate(Grupo=ifelse(Radio!="Disp" & Disp!="0" |                  #remove names other than group names
                          Radio!="Disp" & !is.na(DateEvent) | 
                          Grupo %in% names |
                          Death=="1" |
                          grepl("/", Grupo, fixed = T) |
                          !is.na(Other),
                        NA, 
                        Grupo)) %>% 
    fill(Grupo) %>%    #fill empty group column with site name
    mutate(Year=as.numeric(gsub(".*?([0-9]+).*", "\\1", file)),
           DateObs=as.Date(str_extract(file,
                                       "[0-9]{2}-[0-9]{2}-[0-9]{4}"),     #extract date
                           format="%d-%m-%Y")) %>% 
    mutate(Group=ifelse(Disp!="1" & Death !="1",                          #identify final group name
                        Grupo,
                        NA))%>% 
   dplyr::select(Year,DateObs,Region,Group,Grupo, GLT, Nyanzol,Tattoo,Sexo,  #reorder columns
                 Idade,Birth,Radio,Disp,Death,Other,DateEvent)
  

  
  return(clean)
}

