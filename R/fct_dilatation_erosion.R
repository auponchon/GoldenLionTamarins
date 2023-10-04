############################################################################################

# Mailys Quaerys - Mai 2023
"
Fonction qui permet de réaliser la procédure de dilatation - erosion
"

############################################################################################


#FONCTION
choix_seuil <- function(file){
  
  
  # Load Packages
  library(raster)
  library(terra)
  
  ### Importation du paysage
  paysage = rast(file)
 # plot(paysage)
  
  ### Binarisation du paysage : matrice - habitat
  habitat = paysage
  #plot(habitat)
  #legende <- c("Matrice", "Habitat")
  #plot(habitat, type = "classes", levels = legende,plg = list(cex = 0.7)
  
  ### Remplacement des 0 (=matrice) en NA : OK
  habitat[habitat > 1] <- NA
  #plot(habitat)
  
  
  ### Calcul de la distance à l'habitat
  "Utilisation de la fonction 'distance' du package 'raster' qui renvoie pour chaque cellule
qui contient la valeur 'NA' la plus petite distance à une cellule qui ne contient pas 'NA'"
  distances = distance(habitat)
 # plot(distances)
  # Enregistrer le raster de distance dans un fichier
  #writeRaster(x = distances, filename = "distances_habitat.tif")
  
  
  ### Seuillage de la distance à l'habitat (en nombre de cellules et choix du seuil par l'utilisateur)
  par (mfrow=c (5,2))
  for (seuil in seq(50,300,50)){
    #On donne la valeur 0 lorsque l'on est au dessous du seuil, et 1 quuand on est en dessus
    distances_hab = distances>seuil
    filename = paste("Dilatation",seuil, sep="_")
    plot(distances_hab,
         type = "classes",
         main = filename,
         levels = c("Habitat","Matrice"),
         plg = list(cex = 0.7) )
  }
  
  seuil = as.integer(readline("Choisir le seuil souhaité : "))
  seuil
  
  return(seuil)
}





#FONCTION
dilatation_erosion <- function(file,seuil){
  

  # Load Packages
  library(raster)
  library(terra)

  ### Importation du paysage
  #paysage<-rast(file)
  paysage<- file
  #plot(paysage)

  ### Binarisation du paysage : matrice - habitat
  habitat = paysage
  #plot(habitat)
  #legende <- c("Matrice", "Habitat")
  #plot(habitat, type = "classes", levels = legende,plg = list(cex = 0.7)
  
  ### Remplacement des 0 (=matrice) en NA
  habitat[habitat > 1] <- NA
  #plot(habitat)

  ### Calcul de la distance à l'habitat
  "Utilisation de la fonction 'distance' du package 'raster' qui renvoie pour chaque cellule
  qui contient la valeur 'NA' la plus petite distance à une cellule qui ne contient pas 'NA'"
  distances = distance(habitat)
 # plot(distances)
  # Enregistrer le raster de distance dans un fichier
  #writeRaster(x = distances, filename = "distances_habitat.tif")

  ### Seuillage de la distance à l'habitat (en nombre de cellules)
  #On donne la valeur 0 lorsque l'on est au dessous du seuil, et 1 quuand on est en dessus
  # print("Pour rappel, le seuil choisi est :")
  # print(seuil)
  distances_hab = distances>seuil
  #plot(distances_hab) 

  ### Remplacement des 0 (=habitat et distance inférieure au seuil) en NA
  distances_hab[distances_hab[[1]] == 0] <- NA
  #plot(distances_hab)

  ### Calcul de la distance au non habitat
  distancesnonhabitat = distance(distances_hab)
 # plot(distancesnonhabitat)
  # Enregistrer le raster de distance dans un fichier
  #writeRaster(x = distancesnonhabitat, filename = "distances_non_habitat.tif")

  ### Seuillage de la distance à l'habitat (en nombre de cellules)
  #On donne la valeur 1 lorsque l'on est au dessous du seuil, et 0 quuand on est en dessus
  final_rast = distancesnonhabitat>seuil
  #plot(final_rast) 
  
  return(final_rast)
}



get_largest_dilated_habitat_surface<-function(rasterstack,seuil){
  
  size<-data.frame(Year=NULL,Size=NULL)
  
  
  for (i in 1:length(rasterstack)){
    
    #extract raster for year i
    habitat<-rasterstack[[i]] 
    
    #filter only suitable habitat (code=1)
    habitat[habitat>1]<-0
    
    #smoth the landscape at a scale of 100m
    dilatation_erosion_paysage = dilatation_erosion(habitat,seuil)
    #get total surface of habitat for the given year
    line<-cbind(names(habitat),sum(values(habitat),na.rm=T))
    size<-rbind(size,line)
    
    print(names(habitat))
  }
  
  return(size)
  
}

