library(tidyverse)
library(here)


library(terra)
library(landscapemetrics)
source(here::here("R","fct_dilatation_erosion.R"))
source(here::here("R","create_raster.R"))
  source(here::here("R","progress_bar.R"))

get_patch_size<-function(package){


#extent of study area in geographic coordinates
extreg<-terra::ext(-42.7,-41.9,-22.8,-22.3)

#raw raster land cover obtained from BioMAPS for 2010 to 2021
rast<-list.files(here::here("data","RawData","Landscape","Rasters land cover"),
                 full.names=T, pattern=".tif")[21:32]

#get locations of groups and set them as shape layer
loc<-read.table(here::here("data","rawData","Landscape","RegionsName.csv"),
                header=T,sep=";") %>% 
  dplyr::rename(Long=CENTROIDE.X.UTM.SAD69.23S,
                Lat=CENTROIDE.Y.UTM.SAD69.23S,
                Group=Abreviation) %>% 
  dplyr::select(-Platform,-City) %>% 
  dplyr::filter_all(any_vars(!is.na(.))) %>% 
  sf::st_as_sf(., coords = c("Long","Lat")) %>% 
  sf::st_set_crs(31983)

#homogenize land cover from 2010 (21 to 2021)
coverStack<-purrr::map(rast,create_raster_stack,extreg=extreg,.progress=T)

patchsize<-NULL

for (i in 1:length(coverStack)){

  setTxtProgressBar(progress_bar(length(coverStack)),i)  
  
  
hab<-coverStack[[i]]
dil_hab<-dilatation_erosion(hab,seuil=100)

if (package=="terra"){
#get patch size from terra package
land.patch<-terra::patches(dil_hab,
        directions = 8, 
        zeroAsNA=TRUE, 
        allowGaps=FALSE)
taille_patch_terra <- zonal(cellSize(x=land.patch, unit="m"), land.patch, sum, as.raster=TRUE)

patchsize.gp<-terra::extract(taille_patch_terra/10000,terra::vect(loc), ID=T, bind=T) 
patchsize.gp$Year<-parse_number(rast[i])
}
  
 if (package=="landscapemetrics") {
#get patch size from landscapemetrics package
# zz<- landscapemetrics::get_patches(dil_hab,class=1)
# zzz<-zz$layer_1$class_1
# xx<-landscapemetrics::spatialize_lsm(zzz, what = "lsm_p_area")
# taille_patch_landsp<-xx$layer_1$lsm_p_area

   patchsize.gp<-extract_lsm(dil_hab,loc, extract_id = loc$Group, directions=8, what = "lsm_p_area")   
   patchsize.gp$Year<-parse_number(rast[i])

 } 


patchsize<- c(patchsize,patchsize.gp)
}

return(patchsize)

}
