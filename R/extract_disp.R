extract_disp_data <- function(filedisp) {
  
  block<-read_excel(filedisp,
                    sheet="todos",
                    range = cell_cols("A:G"),
                    col_types=c("text","text","text","date",
                                "text","text","text")) %>% 
    dplyr::rename(Region=Local,
                  Group=Grupo,
                  DateObs=Date,
                  Tattoo=Tatoo) %>% 
    dplyr::mutate(Year=year(DateObs),
                  Idade=NA,
                  Disp="0",
                  Death="0",
                  Solo="0",
                  DateObs=as.Date(DateObs,format="%Y-%m")) %>% 
    dplyr::select(Year,DateObs,Region,Group,GLT,Tattoo,Sex,Idade,Disp,Death,Solo) %>% 
    dplyr::arrange(DateObs,GLT) %>% 
    dplyr::filter(!is.na(GLT))# & Year > 2000)
  
  
  return(block)
}
