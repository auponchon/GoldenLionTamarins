extract_long_data <- function(file) {
  
  
  ifelse("Plan1" %in% excel_sheets(file),
    blocks<-read_excel(file,sheet="Plan1", n_max = 260),
 blocks<-read_excel(file,sheet="Sheet1", n_max = 260))
  
  #cut dataset in 2 blocks before binding the second one under first one
 if(!is.na(blocks[4,8])){
  
  
  block1<-blocks[,c(1:8)]
  block2<-blocks[,c(10:17)]
  
  names(block1)<-names(block2)<-c("Grupo",	"GLT",	"Nyanzol"	,"Tattoo",	"Sexo",	"Idade",	"Birth",	"Radio")
  block <- rbind(block1, block2)
  
 }
  
  
 if(is.na(blocks[4,8])){
        block1<-blocks[,c(1:7)]
        block2<-blocks[,c(9:15)]
        
names(block1)<-names(block2)<-c("Grupo",	"GLT",	"Nyanzol"	,"Tattoo",	"Sexo",	"Birth",	"Radio")
block <- rbind(block1, block2) %>% 
  mutate(Idade=NA) %>% 
  dplyr::select("Grupo",	"GLT",	"Nyanzol"	,"Tattoo",	"Sexo",	"Idade",	"Birth",	"Radio")
    
    
  }
  
  #order column names and filter data
  block.full <- block  %>%
    dplyr::filter_all(any_vars(!is.na(.))) %>%    #remove empty rows
    dplyr::mutate(Region=ifelse(!is.na(Grupo) & is.na(GLT),   #associate region
                                Grupo,NA)) %>% 
    fill(Region) %>%
    dplyr::filter(GLT != "GLT") %>% #remove headers
    mutate(Disp = ifelse(Grupo %in% dispersers |
                           Radio %in% dispersers |
                           grepl("disp", Grupo, fixed = T) |
                           grepl("disp", Radio, fixed = T) ,
                         "Disp",
                         NA),
           Death = ifelse(grepl("Died", Grupo, fixed = T) |
                            grepl("Died", Radio, fixed = T) |
                            Grupo %in% dead,
                          "Dead",
                          NA),
           Other = ifelse(
             Grupo %in% solo,
             "Alone",
             ifelse(Grupo == "não capt.",
                    "Not capt.",
                    ifelse(Grupo == "Expulsa", 
                           "Expulsed",
                           NA))),
           DateEvent = ifelse(
             grepl("/", Grupo, fixed = T),
             str_extract(Grupo, "[0-9]{2}/[0-9]{2}/[0-9]{2}"),
             ifelse(
               grepl("/", Radio, fixed = T),
               str_extract(Radio, "[0-9]{2}/[0-9]{2}/[0-9]{2}"),
               NA)))
  
  
  
 clean<- block.full  %>% 
    mutate(Grupo=ifelse(Radio!="disp" & !is.na(Disp) | 
                          Radio!="disp" & !is.na(DateEvent) | 
                          Grupo %in% names,
                        NA, 
                        Grupo)) %>% 
    fill(Grupo) %>%    #fill empty group column with site name
    mutate(Year=as.numeric(gsub(".*?([0-9]+).*", "\\1", file)),
           DateObs=as.Date(str_extract(file,
                                       "[0-9]{2}-[0-9]{2}-[0-9]{4}"),
                           format="%d-%m-%Y")) %>% 
    mutate(Group=ifelse(Disp!="Disp",Grupo,NA))
  

  
  return(clean)
}

