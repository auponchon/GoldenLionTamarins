extract_disp_data <- function(filedisp) {
  
  all<-read_excel(filedisp,
                    sheet="data",
                    col_types = c(rep("text",4),
                                  "date",
                                  rep("text",5),
                                  "date",
                                  rep("text",3),
                                  "date")) %>% 
    dplyr::filter(!is.na(Individual) & Include=="yes") %>% 
    dplyr::rename(GLT=Individual,
                  Tattoo=Tatoo) 
  
  
  
  block.emigr<-all %>% 
    dplyr::rename(Group=GroupEmigr,
                  Region=Fragment.of.emigration,
                  DateObs=Estimated.date.of.emigration) %>% 
    dplyr::mutate(Year=year(DateObs),
                  Idade=NA,
                  Disp="0",
                  Death="0",
                  Solo="0") %>% 
    dplyr::select(Year,DateObs,Region,Group,GLT,Tattoo,Sex,Idade,Disp,Death,Solo)
    
    
    
  block.immigr<-all %>% 
    dplyr::rename(Group=GroupImmigr,
                  Region=Fragment.of.immigration,
                  DateObs=Estimated.date.of.Immigration) %>% 
    dplyr::mutate(Year=year(DateObs),
                  Idade=NA,
                  Disp="0",
                  Death="0",
                  Solo="0") %>% 
    dplyr::select(Year,DateObs,Region,Group,GLT,Tattoo,Sex,Idade,Disp,Death,Solo)
  
  
block<-block.emigr %>% 
  rbind(block.immigr) %>% 
  dplyr::mutate(DateObs=as.Date(DateObs,format="%Y-%M-%d")) %>% 
  dplyr::arrange(DateObs,Group,GLT)
   
  return(block)
}
