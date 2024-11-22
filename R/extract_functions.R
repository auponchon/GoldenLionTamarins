extract_long_data <- function(file) {
  
  library(lubridate)
  
  ifelse("Plan1" %in% excel_sheets(file),
    blocks<-read_excel(file,sheet="Plan1", n_max = 460),
    ifelse("Table 1" %in% excel_sheets(file),
           blocks<-read_excel(file,sheet="Table 1", n_max = 460),
 blocks<-read_excel(file,sheet="Sheet1", n_max = 460)))
  
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
  dplyr::mutate(Idade=NA) %>% 
  dplyr::select("Grupo",	"GLT",	"Nyanzol"	,"Tattoo",	"Sexo",	"Idade",	"Birth",	"Radio")
    
block$GLT[block$GLT=="Mico"| block$GLT=="mico"]<-"GLT"
   
  }
  
  #order column names and filter data
  block.full <- block  %>%
    dplyr::filter_all(any_vars(!is.na(.))) %>%    #remove empty rows
    dplyr::mutate(Region=ifelse(!is.na(Grupo) & is.na(GLT),   #associate region
                                Grupo,NA)) %>% 
    fill(Region) %>%
    dplyr::filter(GLT != "GLT" ) %>% #remove headers
    dplyr::mutate(Disp = ifelse(Grupo %in% dispersers |
                           Radio %in% dispersers |
                           grepl("isp", Grupo, fixed = T) |           #identify dispersal
                           grepl("isp", Radio, fixed = T) | 
                             Grupo %in% solo ,
                        "1",
                         "0"),
           Death = ifelse(grepl("ied", Grupo, fixed = T) |
                            grepl("ied", Radio, fixed = T) |       #identify dead ind
                            Grupo %in% dead,
                          "1",
                          "0"),
           Solo = ifelse( Grupo %in% solo,
                          "1",                                               #account for other comments (lonely ind)
                           "0"),
           DateEvent = ifelse(
             grepl("/", Grupo, fixed = T),
             str_extract(Grupo, "[0-9]{2}/[0-9]{2}/[0-9]{2}"),        #date of dispersal or death if available
             ifelse(
               grepl("/", Radio, fixed = T),
               str_extract(Radio, "[0-9]{2}/[0-9]{2}/[0-9]{2}"),
               NA))) 
  

  
clean<- block.full  %>%
  dplyr::mutate(Group=ifelse(Grupo %in% dispersers | 
                          grepl("isp", Grupo, fixed = T) | 
                        grepl("ied", Grupo, fixed = T) |    #remove names other than group names
                         Grupo %in% names |
                          Grupo %in% dead |
                          Grupo %in% solo,
                       NA,
                       Grupo),
          Idade = ifelse(GLT=="IN",
                         "IN",
                         Idade)) %>%
  fill(Group) %>%    #fill empty group column with site name
  dplyr::mutate(DateObs=as.Date(str_extract(file,
                                      "[0-9]{2}-[0-9]{2}-[0-9]{4}"),     #extract date from file name
                          format="%d-%m-%Y")) %>%
  dplyr::mutate(Group=ifelse(Disp!="1" & Death !="1" & Solo!="1",                          #identify final group name
                       Group,
                       NA),
                Year=year(DateObs),
                DateObs=as.Date(DateObs,format="%Y-%m"),
                File="Obs") %>%
  dplyr::select(Year,DateObs,Region,Group,Grupo, GLT, Nyanzol,Tattoo,Sexo,Radio,  #reorder columns
                Idade,Birth,Disp,Death,Solo,DateEvent,File) %>%
   dplyr::arrange(DateObs,Group,GLT) %>% 
  dplyr::distinct(.,DateObs,Region,Group,GLT, .keep_all=T)

  return(clean)
}


