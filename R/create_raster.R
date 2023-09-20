#projection for Brazil
source(here::here("R","merge_ummp.R"))
proj<-"+proj=utm +zone=23 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

#forest patches from shapefile
ummp<-return_complete_ummp() #add vendaval and boa esperanza which are missing in shapefile

#remove accents in names
ummp$UMMPs<-plyr::revalue(ummp$UMMPs, c("Imbaú I" = "Imbau I",
                                        "Imbaú II" = "Imbau II",
                                        "Imbaú III" = "Imbau III",
                                        "Poço das Antas" = "Poco das Antas",
                                        "União I" = "Uniao I",
                                        "União II" = "Uniao II"))

create_raster_stack<-function(layer,extreg){
  
xx<-raster(layer) %>% 
  raster::crop(.,extreg) %>%    #crop to the large-scale study region
  projectRaster(., crs=proj ,method="ngb",res=30) %>% # %>%  #project 
  raster::crop(.,ummp)  #crop to forest fragments
#  raster::aggregate(., fact=3,fun=max) %>% 
#  mask(.,ummp)


forest<-c(3,4,5,49)
notforest<-c(9,11,12,13,29)
buildings<-c(22,23,24,25,30)
agri<-c(14,15,41,21,39)
water<-c(31,32,33)

#reassign codes for different habitats
# 0 = sea
# 3 = 5 = 49 = forest
# 9 = 11 = 21 = 29 = vegetation not forest
# 15 = 24 = 25 = buildings
# 41 = agriculture 
# 12 = 13 = 30 = 31 = 32 = 41 = NA
# 33 = water

xx[xx==0]<-NA
xx[xx %in% forest]<-1
xx[xx %in% notforest]<-2
xx[xx %in% agri]<-3
xx[xx %in% water]<-4
xx[xx %in% buildings]<-5

return(xx)

 }
 
