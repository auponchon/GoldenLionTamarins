proj<-"+proj=utm +zone=23 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"
library(sf)

get_group_size<-function(dataset){
  
  #calculate group size at each first observation in the year
  IND<-dataset %>% 
    dplyr::filter(!GLT %in% bad) %>% 
    dplyr::filter(Disp!=1 & Death=="0" & Solo=="0") %>% 
    dplyr::filter(Year> 2009) %>% 
    # dplyr::filter( Year > 2009 & 
    #                  GLT %in% unique(dataset$GLT
    #                                  [dataset$UMMPs=="Uniao I" |
    #                                      dataset$UMMPs=="Poco das Antas" |
    #                                      dataset$UMMPs=="Imbau II" |
    #                                      dataset$UMMPs=="Imbau I" |
    #                                      dataset$Group=="LB" |
    #                                      dataset$Group=="EB" |
    #                                      dataset$Group=="EX" |
    #                                      dataset$Group=="AX2"])) %>%
    dplyr::arrange(DateObs)
  
  
  group.size<- IND %>% 
    group_by(Group,Year,DateObs) %>% 
    count() %>% 
    dplyr::arrange(DateObs) %>% 
    group_by(Group,Year) %>% 
    slice_min(DateObs) %>% 
    dplyr::select(-DateObs) %>% 
    ungroup()
  
  
  return(group.size)
}

#calculate distance between monitored groups
get_distance<-function(){
#load all coordinates of groups
loc<-read.table(here::here("data","rawData","Landscape","RegionsName.csv"),
                header=T,sep=";") %>% 
  dplyr::rename(Long=CENTROIDE.X.UTM.SAD69.23S,
                Lat=CENTROIDE.Y.UTM.SAD69.23S,
                Group=Abreviation) %>% 
  dplyr::select(-Platform,-City) %>% 
  # dplyr::filter(Group %in% gp) %>% 
  dplyr::filter_all(any_vars(!is.na(.))) %>% 
  sf::st_as_sf(., coords = c("Long","Lat")) %>% 
  sf::st_set_crs(proj) 

dista<-st_distance(loc)/1000 

rownames(dista)<-loc$Group
colnames(dista)<-loc$Group

disto<-dista %>% 
  as.data.frame.table(.) %>% 
  dplyr::rename(FromGp=Var1,
                ToGp=Var2,
                Dist=Freq) %>% 
  dplyr::mutate(Dist=as.numeric(Dist)) 

return(disto)
}
