############################################################################################

library(deldir)

#1) RANDOM SAMPLING

tirage_pts_aleatoires <- function(r,n){
  r <- as(r, "Raster")

  # mes_points_raster = RasterLayer
  mes_points_raster = sampleRandom(r, size=n, na.rm=TRUE, ext=NULL, cells=FALSE, rowcol=FALSE, xy=FALSE, sp=FALSE, asRaster=TRUE,overwrite=TRUE)
  
  # mes_points_xy = matrix des coordonnées de chaque point
  mes_points_xy = sampleRandom(r, size=n, na.rm=TRUE, ext=NULL, cells=FALSE, rowcol=FALSE, xy=TRUE, sp=FALSE, asRaster=FALSE,overwrite=TRUE)
  
  #Affichage des patchs et des points tirés
  # plot(r,main = "ETAPE 7 : SAMPLE POINTS IN LARGEST PATCHES")
  #  plot(ummp,add=T)
  # plot(mes_points_raster,col="black",add=T,pch=16)

  
  return(mes_points_xy)
}


#2) REGULAR POINT SAMPLES

tirage_pts_reguliers <- function(r,n){

  #mes_points_reguliers_view : permet de visualiser la grille de points
  mes_points_reguliers_view = spatSample(r, size=n, method="regular", replace=FALSE, na.rm=TRUE, 
                                    as.raster=FALSE, as.df=TRUE, as.points=TRUE, values=TRUE, cells=FALSE, 
                                    xy=FALSE, ext=NULL, warn=TRUE, weights=NULL, exp=5, exhaustive=FALSE)
  
  # mes_points_reguliers : permet de récupérer les coordoonées des points tirés
  mes_points_reguliers = spatSample(r, size=n, method="regular", replace=FALSE, na.rm=TRUE, 
                                    as.raster=FALSE, as.df=FALSE, as.points=FALSE, values=FALSE, cells=FALSE, 
                                    xy=TRUE, ext=NULL, warn=TRUE, weights=NULL, exp=5, exhaustive=FALSE)
   plot(r,main = "ETAPE 7 : Tirage regulier de points dans les patchs dont la taille est supérieur au seuil maximal")
   plot(mes_points_reguliers_view,col="black",add=T,pch=20) 
  # 
  return(mes_points_reguliers)
}


#3) CREATING POLYGONS 

tess2spat <- function(obj,idvec=NULL) {
  K <- nrow(obj$summary)
  if (is.null(idvec)) { idvec <- 1:K }
  partition <- vector(mode="list", length=K)
  xy <- lapply(tile.list(obj), "[", i=3:4)
  #form Polygons
  for (i in 1:length(xy)) {
    pcrds <- unique(cbind(xy[[i]]$x, xy[[i]]$y))
    pcrds <- rbind(pcrds, pcrds[ 1,])
    colnames(pcrds) <- c("X","Y")
    
    partition[[i]] <- sp::Polygons(list(sp::Polygon(pcrds)), ID=as.character(idvec[i]))
  }
  partition <- sp::SpatialPolygons(partition)
  partition
}


#4) DEFINING PATCHES BASED ON TESSALATION POLYGONS 
decoupage_patchs <- function(tesselation,id_max) {

  #Définition des polygones de tesselation
  tessalation_polygones = tess2spat(tesselation,idvec=NULL)
  tessalation_polygones = as(tessalation_polygones,"SpatialPolygonsDataFrame")
 #  writeOGR(tessalation_polygones, dsn = '.', layer = 'poly', driver = "ESRI Shapefile",overwrite = T)
   polygones = vect(tessalation_polygones)
  
  r[r > 0] <- 1
  
  #polygones[,1] = 1:nrow(data.frame(polygones))
  polygones[,1] = (id_max+1):(nrow(data.frame(polygones))+id_max)
  names(polygones)[1]="id"
  
  #r = rast(r)
  patch_raster <- rasterize(polygones, r*0, field = "id")
  r2 = patch_raster*r
  
  return(r2)
}



#5) CHECK PATCH SIZE DEPENDING ON THE NUMBER OF POINT SAMPLES

verif_decoupage <- function(new_patchs) {

  freq_patchs = freq(new_patchs)
  #View(freq_patchs)
  
  coupe <- cut(freq_patchs$count,c(0,2.25,25,Inf),labels=c('TOO SMALL','OK','TOO LARGE'))
  stats_taille<-table(coupe)
  
  par(mfrow=c(2,2))
  boxplot(freq_patchs$count)
  hist(freq_patchs$count)
  barplot(stats_taille)
  pie(stats_taille,labels = paste0(round(100 * stats_taille/sum(stats_taille), 1), "%"))

}