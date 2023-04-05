
extract_capture_data <- function(fileadd) {

block<-read_excel(fileadd,
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


revalue_old_regions<-function(data){
  
  #"ARU", "A.SA", 
  
  data$Region<-plyr::revalue(as.factor(data$Region), 
                             c("BOI B" = "Poco das Antas",
                               "FAZ. 2I" =	"Faz. 2 Irmaos",
                      #         "FAZ. 2E" = "Faz. Estreito",
                               "FAZ. AF" = "Faz. Afetiva",
                               "FAZ.AF" = "Faz. Afetiva",
                               "FAZ. BE" = "Faz. Boa Esperanca",
                               "FAZ.BE" = "Faz. Boa Esperanca",
                       #        "FAZ. BR" = "Faz. Bom Retiro",
                               "FAZ. CNC" = "Faz. Cabana nova Conquista",
                               "B-SJ" = "Faz. Cabana nova Conquista",
                   #            "FAZ. BV" = "Faz. Sao Francisco",
                                "FAZ. CQ" = "Faz. Coqueiros",
                                "FAZ. ES" = "Faz. Estreito",
                               "FAZ.EXT" = "Faz. Estreito",          
                               "FAZ. IG" = "Faz. Igarape",
                               "FAZ.IGA"= "Faz. Igarape",
                               "FAZ. IU" = "Faz. Iguape",
                               "FAZ.NC" = "Faz. Nova Esperanca",
                               "FAZ.NE" = "Faz. Nova Esperanca",
                               "FAZ. PP" = "Faz. Pacoty",
                               "FAZ. PI" = "Faz. Pacoty",
                               "FAZ.PAC" = "Faz. Pacoty",
                               "FAZ. RV" = "Faz. Rio Vermelho", 
                               "FAZ.RV" = "Faz. Rio Vermelho", 
                               "FAZ. SB" = "Faz. St Bira",
                               "FAZ. SF" =  "Faz. Sao Francisco",
                               "FAZ. SH" = "Faz. Sta Helena",
                                "FAZ. SH2" = "Faz. Sta Helena 2",
                  #             "FAZ. SJ" = "Faz. Boa Esperanca", for FX group and Faz. Sao Joao for IR
                        #       "FAZ. SM" = "Faz. Vale do Cedro",
                               "FAZ.STA.Fé" = "Faz. Sta Fe",
                               "FAZ.STA. B."= "Faz. St Bira",
                                "FAZ.STA.HEL" = "Faz. Sta Helena",
                               "FAZ. TAM" = "Faz. Tamarins",
                               "FAZ.TAM" = "Faz. Tamarins",
                               "FAZ. V" = "Faz. Vendaval",
                               "FAZ. VC" = "Faz. Vale do Cedro",
                                "FAZ.VC" = "Faz. Vale do Cedro",
                  #              "FAZ. IU" = "",
                  #            "PEDR" = "Poco das Antas",
                   #            "REBIO" = "Rebio Uniao",
                               "REBIO." = "Rebio Uniao",
                               "RBIO UNIAO" = "Rebio Uniao",
                               "RBIO P. ANT."= "Poco das Antas",
                               "ST. CB" = "St. Cisne Branco",
                        #       "ST. JO" = "Sitio Joedi",  JOAO or JOEDI?
                               "ST. JO" = "Sitio Joedi",
                               "ST. JOEDI" = "Sitio Joedi",
                               "ST. PROF"="St. Professor", 
                               "SIT.P" = "St. Professor", 
                               "ST. 3I" = "Sitio 4 Irmaos",
                               "ST.3I"= "Sitio 4 Irmaos",
                               "ST. ELIELTON" ="St. do Eliel"))
                   #           "VIV 2" = "Poco das Antas"))
                               
                               
  data$Group<-plyr::revalue(as.factor(data$Group), 
                             c("AXII"="AX2",      
                               "BBII"= "BB2",
                               "FU" = "Fusao",
                               "JA" = "Jaguar",
                               "LAII" = "LA2",
                               "MFII" = "MF2",
                               "MLII" = "ML2",
                               "RIS" = "RI",
                #               "RIS2" = "RI2",
                               "SEDE" = "SD",
                               "STA Fé"	= "Sta Fe"))
  
  data$Region[which(data$Group=="AX" | 
                      data$Group=="SI" | 
                      data$Group=="SI2")] <- "Faz. Sta Helena 2"
  
  return(data)
}

revalue_names_capture<-function(data){
  data$GLT[which(data$Tattoo=="2405")]<-"2405"
  data$GLT[which(data$Tattoo=="2155")]<-"2155"
  data$GLT[which(data$GLT=="2317")]<-data$Tattoo[which(data$GLT=="2317")]

  return(data)
  
}
  
