library(plyr)
library(lubridate)

revalue_region<-function(data){

data$Region<-plyr::revalue(as.factor(data$Region), 
c("2 Irmãos" =	"Faz. 2 Irmaos",
 "Afetiva" =	"Faz. Afetiva",
 "Boa Esperança" =	"Faz. Boa Esperanca",
 "Dois Irmãos"=	"Faz. 2 Irmaos",
 "F .Tamarins"=	"Faz. Tamarins",
 "F.2 Irmãos"=	"Faz. 2 Irmaos",
# "F. 2 Irmãos" = "Faz. 2 Irmaos",
 "Faz. 2 lrmaos" = "Faz. 2 Irmaos",
 "Faz. 2 Irmãos" = "Faz. 2 Irmaos",
# "Faz 2 Irmaos" = "Faz. 2 Irmaos",
 "Faz.2 Irmãos" = "Faz. 2 Irmaos",
 "F.Iguape"=	"Faz. Iguape",
 "F.Sta Helena"=	"Faz. Sta Helena",
 "F.Sta Helena 1"=	"Faz. Sta Helena",
 "Faz Afetiva"=	"Faz. Afetiva",
 "Faz AMLD"=	"Faz. Igarape",
# "Faz. AMLD"=	"Faz. Igarape",
 "Faz Boa Esperança"=	"Faz. Boa Esperanca",
# "Faz Bom Retiro" = "Faz. Bom Retiro",
# "Faz Coqueiros" = "Faz. Coqueiros",
 "Faz Estreito"=	"Faz. Estreito",
 "Faz Igarape"=	"Faz. Igarape",
# "Faz. Iguapé" = "Faz. Iguape",
 "Faz Nova Esperança"=	"Faz. Nova Esperanca",
 "Faz Pacoti"=	"Faz. Pacoty",
 "Faz Pacoty"=	"Faz. Pacoty",
 "Faz Rio Vermelho"=	"Faz. Rio Vermelho",
# "Faz São Francisco" = "Faz. Sao Francisco",
 "Faz. São Francisco" = "Faz. Sao Francisco",
# "Faz.sr Bira" = "Faz. St Bira",
 "Faz. St Helena 2" = "Faz. Sta Helena 2",
 "Faz S. Helena 2"=	"Faz. Sta Helena 2",
 "Faz Sta Helena 2" = "Faz. Sta Helena 2",
 "Faz St.Helena"=	"Faz. Sta Helena",
 "Faz Sta Helena"=	"Faz. Sta Helena",
# "Faz.St.Helena" = "Faz. Sta Helena",
# "Faz Santa Helena" = "Faz. Sta Helena",
 "Faz. Sta Helena 1" = "Faz. Sta Helena",
# "Faz Sta Barbara" = "Faz. Sta Barbara",
 "Faz Tamarins"=	"Faz. Tamarins",
# "Faz Vendaval"= "Faz. Vendaval",
# "Faz. AMLD"=	"Faz. Igarape",
 "Faz. Boa Esperança"=	"Faz. Boa Esperanca",
 "Faz. Cabanã nova Conquista"=	"Faz. Cabana nova Conquista",
 "Faz. Cabana nova Coquista" = "Faz. Cabana nova Conquista",
 "Faz. Nova Esperança"=	"Faz. Nova Esperanca",
 "Faz. Pacoti" = "Faz. Pacoty",
# "Faz.Sta.Helena"=	"Faz. Sta Helena",
# "Faz. St.Helena" = "Faz. Sta Helena",
 "Faz. Santa Helena2" = "Faz. Sta Helena 2",
# "Faz S. Helena 2" = "Faz. Sta Helena 2",
 "Faz. Sta. Fé"=	"Faz. Sta Fe",
 "Faz.São João"=	"Faz. Sao Joao",
# "Faz. SaoFrancisco" = "Faz. Sao Francisco",
 "Faz.Tamarins"=	"Faz. Tamarins",
 "Fazenda Afetiva"=	"Faz. Afetiva",
 "Fazenda Boa Esperança"=	"Faz. Boa Esperanca",
 "Fazenda Nova Esperança"=	"Faz. Nova Esperanca",
 "Fazenda Pacoty"=	"Faz. Pacoty",
 "Fazenda Patis"=	"Faz. Patis",
 "Fazenda Rio Verelho"=	"Faz. Rio Vermelho",
 "Fazenda Rio vermelho"=	"Faz. Rio Vermelho",
 "Fazenda Rio Vermelho"=	"Faz. Rio Vermelho",
 "Fazenda Tamarins"=	"Faz. Tamarins",
 "Igarape" = "Faz. Igarape",
 "Igarapé" = "Faz. Igarape",
 "Iguape"=	"Faz. Iguape",
# "OlhosD'agua"=	"OlhosD'agua",
 "Patis"	="Faz. Patis",
"Paulo Freire" = "Faz. Sta Helena",
 "Poço das Antas"=	"Poco das Antas",
 "Professor"	="St. Professor",
 "Rebio União"=	"Rebio Uniao",
 "Rio Vermelho"=	"Faz. Rio Vermelho",
 "Sitio 15 de Março"="Sitio 15 de Marco",
 "Sitio 4 irmãos" = "Sitio 4 Irmaos",
 "Sitio 4 Irmãos" = "Sitio 4 Irmaos",
 "Sitio Andorinhas"="St. das Andorinhas",
 "Sitio Ipê"=	"Sitio Ipe",
 "Sitio Moriá"=	"Sitio Moria",
 "St.Joedi" = "Sitio Joedi",
# "St. Joedi" =  "Sitio Joedi",
 "ST. Cisne Branco"=	"St. Cisne Branco",
 "ST Cisne Branco"="St. Cisne Branco",
 "ST. das Andorinhas"=	"St. das Andorinhas",
 "Sitio das Andorinhas"  = "St. das Andorinhas",
 "Sitio dos Jotas" = "Sitio 7 Jotas",
 "ST. do Professor"=	"St. Professor",
 "Sta Helena"=	"Faz. Sta Helena",
 "Sta Helena 1"=	"Faz. Sta Helena",
 "Sta Helena 2"=	"Faz. Sta Helena 2"))

  data$Group<-plyr::revalue(as.factor(data$Group), 
                      c("Afetiva"=	"AF",
                        "AF2" = "AFII",
                        "AXII"="AX2",
                        "Apenhiul"=	"AP",
                        "Areal"=	"AR",
                        "Atlanta"=	"AT",
                        "Beira Rio"="BR",
                        "Belvedere"=	"BV",
                        "Bira" = "BI",
                        "Biquinha" = "BQ",
                        "Brookfield" = "BK",
                        "Cahoma2" = "CA2",
                        "CLAII" = "CLA2",
                      #  "Columbia" = "CB",
                        "Corredor"=	"C.O",
                        "Daniel"=	"DN",
                        "Dublin"=	"DU",
                        "Eliel"=	"EL",
                        "Emmen"=	"EM",
                        "Emmem2" = "EM2",
                        "Emmen2" = "EM2",
                        "Estreito"="ET",
                        "Fugitivo"=	"Foo",
                        "Fugitivo 2"=	"FG2",
                        "Funil"=	"FU",
                        "Fusão"=	"Fusao",
                        "Grupo Novo"=	"GN",
                        "Iguape" = "IP",
                        "Iguape2" = "IP2",
                        "Igarape2" = "IG2",
                        "IPÊ"=	"IPE",
                        "IPII" = "IP2",
                        "Israel"=	"IR",
                        "JÁ"=	"JA",
                        "Jaguar"=	"Jaguar",
                        "Jamelao"=	"JM",
                        "Jaqueira"=	"JQ",
                        "Jersey" = "JR",
                        "Joedi" = "JD",
                        "Jorge"=	"JG",
                        "Jorge2" = "JG2",
                        "Kenedi"=	"KN",
                        "LAII" = "LA2",
                        "Lago Seco" = "LS",
                        "Lava Pé"=	"LP",
                        "Lixão" = "LX",
                        "Lixao" = "LX",
                        "Los Angeles" = "LA2",
                        "M. Barreto"= "MB",
                        "MBII" = "MB2",
                        "Madu"=	"MD",
                        "Marwell"=	"MA",
                        "Meninos"="MN",
                        "Mistura Fina"=	"MF",
                        "Olimpia"=	"OL",
                        "OLII" = "OL2",
                        "Omaha" = "OM",
                        "Omaha2" = "OM2",
                        "P. Abreu"=	"PA",
                        "Pacoti"=	"PI",
                        "Paiva" = "PV",
                        "Paulo Freire" = "PF",
                        "Passagem"=	"PSG",
                        "Pindoba"=	"PB",
                        "Pindoba-2"=	"PB2",
                        "Pindoba2"=	"PB2",
                        "Portland"=	"PT",
                        "PS" = "PSG",
                        "Prof.Herdi"=	"PH",
                        "Quelinho"=	"QL",
                        "Recanto"=	"RC",
                        "Reintro"=	"RT",
                        "Ribeirao" = "ZN",
                        "Riverbank's"="RI",
                        "RIS" = "RI",
                        "RIS2" = "RI2",
                        "RV2"=	"RVII",
                        "RSII" = "RS2",
                        "Salario" = "SL",
                        "Salario2" = "SL2",
                      #  "Sta. Fé" = "Sta Fe",
                        "St. Joedi" = "JD",
                        "STIII" = "ST3",
                        "Saquarema"=	"SQ",
                        "Seattle"=	"SE",
                        "Sede"=	"SD",
                        "SHII" = "SH2",
                        "Sidnei"=	"SI",
                        "Sidnei2"=	"SI2",
                        "SD2" = "SI2",
                        "Skank" = "KS",
                        "Skansen" = "SK",
                       "St B.Vista"=	"LD",
                       "St Boa Vista"=	"LD",
                        "Sta. Fé"	= "Sta Fe",
                        "Super"=	"SP",
                        "Tabebuia"=	"TB",
                        "Tamarins"=	"TM",
                        "TETRA" = "TT",
                        "TM-3"=	"TM3",
                        "TM2?"=	"TMII",
                        "Triplets"=	"TR",
                        "Unigranrio"=	"UR"))
  
  data$Region[which(data$Group=="AX" | 
                      data$Group=="SI" | 
                      data$Group=="SI2")] <- "Faz. Sta Helena 2"
  
  data$Region[which(data$Group=="JD"|
                    data$Group=="JD2")] <- "Sitio Joedi"
  
  data$Group[which(data$Group=="AL" & data$Region=="Faz. Vendaval")]<-"ALI"
  data$Disp[which(data$GLT=="MK10" & data$Year > 2003 & data$Group=="ML")]<-1
  data$Disp[which(data$GLT=="PA1" & data$Year > 2005 & data$Group=="PA")]<-1
  
#  data$Region[which(data$Group=="JR")] <- "Faz. Sta Helena"
  
  return(data)
}


revalue_stage<-function(data){

  clean<-data %>%
     dplyr::mutate(Idade=plyr::revalue(as.factor(Idade),
                                       c(".SA"="SA",
                                       "IN"="JU",
                                        "I" = "JU",
                                       "JJU" = "JU",
                                       "F" = "JU",
                                       "?" = NA)))

  return(clean)

}

revalue_name<-function(data){
  
  data$GLT<- str_replace(data$GLT, "i", "I")
  
  data$Tattoo[data$GLT=="TR14"]<-"TR14"
  data$Tattoo[data$Tattoo=="KES"]<-"KE5"
  data$GLT[data$Tattoo=="Cibele" ]<-"Cibele"  #MP21 & MP121
  data$GLT[data$Tattoo=="Rodrigo"]<-"Rodrigo"
  data$GLT[data$Tattoo=="Cris"]<-"Cris"  #MP23 & MP123
  data$GLT[data$Tattoo=="Angela"]<-"Angela"
  data$GLT[data$Tattoo=="Nelson"]<-"Nelson"
  data$GLT[data$Tattoo=="1318"]<-"1318"
  data$GLT[data$Tattoo=="AM1"]<-"AM1"
  
  data$GLT[data$GLT=="FA40" & data$Tattoo=="UR2"]<-"FA40.1"
  data$GLT[data$Group=="TM3" & data$GLT=="MP213"]<-"MP213.1"
  data$GLT[data$GLT=="JU"]<-"FT"
  data$GLT[data$GLT=="RT1" & data$Sexo=="F"]<-"RT1.1"
  data$GLT[data$GLT=="RT2" & data$Sexo=="F"]<-"RT2.1"
  data$GLT[data$GLT=="SK19" & data$Sexo=="F"]<-"SK19.1"
  data$GLT[data$GLT=="PI3" & data$Sexo=="F"]<-"PI3.1"
  data$GLT[data$GLT=="AFII"]<-"AF11"
  data$GLT[data$GLT=="AF?"]<-"AF17"
  data$GLT[data$GLT=="1318" & data$Sexo=="F"]<-"1318.1"
  data$GLT[data$GLT=="ST40" & data$Sexo=="F"]<-"ST40.1"
  data$GLT[data$GLT=="EM11" & data$Sexo=="M"]<-"EM11.1"
  data$GLT[data$GLT=="SF1" & data$Sex=="F"]<-"SF1.1"
  data$GLT[data$GLT=="SF4" & data$Sex=="F"]<-"SF4.1"
  data$GLT[data$GLT=="SF3" & data$Sex=="F"]<-"SF3.1"
  
  
   data$Group[data$GLT=="979"] <-"LB"
  
  
   return(data)
  
}

revalue_sex<-function(data){
  
  data$Sexo[which(data$Sexo=="f")]<-"F"
  data$Sexo[which(data$GLT=="MP62")]<-"F"
  data$Sexo[which(data$GLT=="SI18")]<-"F"
  data$Sexo[which(data$GLT=="SI19")]<-"M"
  data$Sexo[which(data$GLT=="FX1")]<-"F"
  data$Sexo[which(data$GLT=="KE4")]<-"M"
  data$Sexo[which(data$GLT=="O27")]<-"M"
  data$Sexo[which(data$GLT=="O39")]<-"M"
  data$Sexo[which(data$GLT=="PT8")]<-"M"
  data$Sexo[which(data$GLT=="ST24")]<-"M"
  data$Sexo[which(data$GLT=="ST34")]<-"M"
  data$Sexo[which(data$GLT=="TR1")]<-"M"
  data$Sexo[which(data$GLT=="E25")]<-"M"
  data$Sexo[which(data$GLT=="OL27")]<-"M"
  data$Sexo[which(data$GLT=="TR23")]<-"F"
  data$Sexo[which(data$GLT=="TR26")]<-"F"
  data$Sexo[data$GLT=="TR14"]<-"F"
  data$Sexo[data$GLT=="TR22"]<-"F"
 
  bad<-c("?","T0","-","--")
  
  clean<-data %>%
    dplyr::mutate(Sexo=ifelse(is.na(Sexo) | Sexo %in% bad,
                              "nonID",
                       ifelse(Sexo=="RF" | Sexo=="F'",
                              "F",
                       ifelse(Sexo == "RM",
                              "M",
                              Sexo)))) 
  
  clean2<-clean %>% 
    group_by(GLT) %>% 
    dplyr::mutate(Sexo=as.factor(Sexo),
                  Sex = Sexo)
                    # 
                    # ifelse("M" %in% Sexo,
                    #             "M",
                    #      ifelse("F" %in% Sexo,
                    #             "F",
                    #             "nonID")))
  
  return(clean2)
  
  
  
}

