library(here)
library(raster)
library(tidyverse)
library(sf)
library(rcartocolor)
library(ggspatial)

source(here::here("R","merge_ummp.R"))

extreg<-extent(-42.7,-41.9,-22.9,-22.3)
extregproj<-extent(740500, 809200,7481000, 7524000)

#extentinset
proj<-"+proj=utm +zone=23 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"


#get groups and regions retained in the final dataset from 2009 to 2022
ind<-read.csv(here::here("data","NewlyCreatedData","CMR",
                         "CMR_3states_AD_Sex_UniaoI_PDA_ImbauI_II_LB_EB_EX_AX2_2009-2022.csv"),
              header=T,
              sep=";") 
load(here::here("data","NewlyCreatedData","data_clean_long_final.RData"))
gp.reg<-data.clean.final %>% 
  dplyr::filter(GLT%in% ind$GLT & Year > 2008) %>% 
  distinct(Group, UMMPs) %>% 
  dplyr::filter(!is.na(Group) | !is.na(UMMPs))



#load all coordinates of groups
loc<-read.table(here::here("data","rawData","Landscape","RegionsName.csv"),
                header=T,sep=";") %>% 
  dplyr::rename(Long=CENTROIDE.X.UTM.SAD69.23S,
                Lat=CENTROIDE.Y.UTM.SAD69.23S,
                Group=Abreviation) %>% 
  dplyr::select(-Platform,-City) %>% 
  dplyr::filter(Group %in% gp.reg$Group) %>% 
  dplyr::filter_all(any_vars(!is.na(.))) %>% 
  sf::st_as_sf(., coords = c("Long","Lat")) %>% 
  sf::st_set_crs(31983) 

#raster land cover for each year obtained from BioMAPS
# load(here::here("data","NewlyCreatedData","Landscape raster","Rasters","LandUseStack.RData"))
# 
# hab_df<-coverStack[[max(length(coverStack))]] %>% 
#            as(., "SpatialPixelsDataFrame") %>% 
#              as.data.frame(.)
# colnames(hab_df) <- c("value", "x", "y")

#conservation units (UMMPs)
ummp<-return_complete_ummp() #add vendaval and boa esperanza which are missing in shapefile

#remove accents in names
ummp$UMMPs<-plyr::revalue(ummp$UMMPs, c("Imbaú I" = "Imbau I",
                                        "Imbaú II" = "Imbau II",
                                        "Imbaú III" = "Imbau III",
                                        "Poço das Antas" = "Poco das Antas",
                                        "União I" = "Uniao I",
                                        "União II" = "Uniao II"))

ummp.sub<-subset(ummp, as.character(ummp$UMMPs) %in% as.character(gp.reg$UMMPs))
  

#forest patches from shapefile
land<-sf::read_sf(here::here("data","RawData","Landscape","Shapefiles Landscape AMLD", 
                             "SIG-EDUC Redescobrindo 2021 - Fragmentos de Vegetação.shp"))



  


# brazil<-sf::read_sf(here("data","RawData","Landscape","Shapefiles Landscape AMLD",
#                          "World_WGS84_Fine_Reso.shp")) %>% 
#   sf::st_transform(31983) 

gg<-ggplot()+
    geom_sf(data=land,
            fill="grey80",
            col="grey80") +
  geom_sf(data=ummp,
          mapping=aes(fill=UMMPs),alpha=0.5,
          fill="grey30") +
   geom_sf(data=ummp.sub,
           mapping=aes(fill=UMMPs),alpha=0.5)+
    geom_sf(data=loc,
            shape=15,
            show.legend = F)+
 
  annotation_scale(location = "br", 
                   width_hint = 0.15,
                   pad_x = unit(0.7, "cm"),
                   bar_cols = "black") +
  annotation_north_arrow(location = "br", 
                         which_north = "true",
                         style = ggspatial::north_arrow_nautical(), 
                         pad_y = unit(0.8, "cm")) +
  
  theme_bw() +
  theme(legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0, 0), "lines"),
        axis.text = element_text(size=10),
        plot.title=element_text(hjust=0.5,
                                face = "bold",
                                size=12))
#  inset(braz,)
print(gg)    


