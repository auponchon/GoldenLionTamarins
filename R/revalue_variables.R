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
"F.Iguape"=	"Faz. Iguape",
"F.Sta Helena"=	"Faz. St.Helena",
"F.Sta Helena 1"=	"Faz. Sta Helena 1",
"Faz Afetiva"=	"Faz. Afetiva",
"Faz AMLD"=	"Faz. AMLD",
"Faz. AMLD"=	"Faz. Igarape",
"Faz Boa Esperança"=	"Faz. Boa Esperanca",
"Faz Estreito"=	"Faz. Estreito",
"Faz Igarape"=	"Faz. Igarape",
"Faz Nova Esperança"=	"Faz. Nova Esperanca",
"Faz Pacoti"=	"Faz. Pacoty",
"Faz Pacoty"=	"Faz. Pacoty",
"Faz Rio Vermelho"=	"Faz. Rio Vermelho",
"Faz S. Helena 2"=	"Faz. Sta Helena 2",
"Faz St.Helena"=	"Faz. Sta Helena",
"Faz Sta Helena"=	"Faz. Sta Helena",
"Faz Tamarins"=	"Faz. Tamarins",
"Faz. AMLD"=	"Faz. Igarape",
"Faz. Boa Esperança"=	"Faz. Boa Esperanca",
"Faz. Cabanã nova Conquista"=	"Faz. Cabana nova Conquista",
"Faz. Nova Esperança"=	"Faz. Nova Esperanca",
"Faz. Sta Helena"=	"Faz. Sta Helena",
"Faz. Sta. Fé"=	"Faz. Sta Fé",
"Faz.São João"=	"Faz. Sao Joao",
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
"Iguape"=	"Faz. Iguape",
"OlhosD'agua"=	"OlhosD'agua",
"Patis"	="Faz. Patis",
"Poço das Antas"=	"Poco das Antas",
"Professor"	="St. Professor",
"Rebio União"=	"Rebio Uniao",
"Rio Vermelho"=	"Faz. Rio Vermelho",
"Sitio 15 de Março"="Sitio 15 de Marco",
"Sitio Andorinhas"="Sitio das Andorinhas",
"Sitio Ipê"=	"Sitio Ipe",
"Sitio Moriá"=	"Sitio Moria",
"ST. Cisne Branco"=	"St. Cisne Branco",
"ST. das Andorinhas"=	"Sitio das Andorinhas",
"ST. do Professor"=	"St. Professor",
"Sta Helena"=	"Faz. Sta Helena",
"Sta Helena 1"=	"Faz. Sta Helena 1",
"Sta Helena 2"=	"Faz. Sta Helena 2"))

  data$Group<-plyr::revalue(as.factor(data$Group), 
                      c("2 Irmãos"=	"DI",
                        "Afetiva"=	"AF",
                        "Apenhiul"=	"AP",
                        "Areal"=	"AR",
                        "Atlanta"=	"AT",
                        "Belvedere"=	"BV",
                        "Corredor"=	"C.O",
                        "Daniel"=	"DN",
                        "Dublin"=	"DU",
                        "Eliel"=	"EL",
                        "Emmen"=	"EM",
                        "F.2 Irmãos"=	"2 Irmãos",
                        "Faz AMLD"=	"PH2",
                        "Faz. Iguape"=	"SK3",
                        "Faz.2 Irmãos"=	"2 Irmãos",
                        "Fugitivo"=	"Foo",
                        "Fugitivo 2"=	"FG2",
                        "Funil"=	"FU",
                        "Fusão"=	"Fusao",
                        "Grupo Novo"=	"GN",
                        "IPÊ"=	"IPE",
                        "Israel"=	"IR",
                        "JÁ"=	"JA",
                        "Jaguar"=	"Jaguar",
                        "Jamelao"=	"JM",
                        "Jaqueira"=	"JQ",
                        "Jorge"=	"JG",
                        "Kenedi"=	"KN",
                        "Lava Pé"=	"LP",
                        "Madu"=	"MD",
                        "Marwell"=	"MA",
                        "Mistura Fina"=	"MF",
                        "Olimpia"=	"OL",
                        "P. Abreu"=	"PA",
                        "Pacoti"=	"PI",
                        "Passagem"=	"PSG",
                        "Pindoba"=	"PB",
                        "Pindoba-2"=	"PB2",
                        "Pindoba2"=	"PB2",
                        "Portland"=	"PT",
                        "Prof.Herdi"=	"PH",
                        "Quelinho"=	"QL",
                        "Recanto"=	"RC",
                        "Reintro"=	"RT",
                        "RV2"=	"RVII",
                        "Saquarema"=	"SQ",
                        "Seattle"=	"SE",
                        "Sede"=	"SD",
                        "Sidnei"=	"SI",
                        "Sidnei2"=	"SI2",
                        "St B.Vista"=	"LD",
                        "St Boa Vista"=	"LD",
                        "Sta. Fé"	= "Sta. Fe",
                        "Super"=	"SP",
                        "Tabebuia"=	"TB",
                        "Tamarins"=	"TM",
                        "TM-3"=	"TM3",
                        "TM2?"=	"TMII",
                        "Triplets"=	"TR",
                        "Unigranrio"=	"UR"))
  
  data$Region[which(data$Group=="AX" | 
                      data$Group=="SI" | 
                      data$Group=="SI2")] <- "Faz. Sta Helena 2"
  
  data$Region[which(data$Group=="JR")] <- "Faz. Sta Helena"
  
  return(data)
}

# revalue_birth<-function(data){
#   
# data$Birth<-plyr::revalue(as.factor(data$Birth),
#                 c("'02/93" = "02/93",
#                   "'10/09" = "09/09",
#                   "'10/10" = "09/10",
#                   "10/'4" = "10/14",
#                   "?/02" = "06/02",
#                   "?/14"="06/14",
#                   "1015" = "10/15",
#                   "?/16" = "06/16",
#                   "RC1" = "07/16",
#                   "RC2" = "05/17",
#                   "RC3" = "05/17",
#                   "?/17" = "03/17",   #birth between 01 and 05/17 based on observations
#                   "43809" = "10/12",
#                   "10.20" = "10/20",
#                   "?/20" = "07/20",
#                   "?/21" = "11/21",
#                   "10/ 21" = "10/21",
#                   "'07/22" = "07/22",
#                   "'04/22" = "04/22",
#                   "?"= NA))
#   
#   
#   data$Birth[which(data$GLT=="SI10" | data$GLT=="SI11")]<-"11/07"
#   data$Birth[which(data$GLT=="SI12" | data$GLT=="SI13")]<-"12/07"
#   
#   data$Birth[which(data$GLT=="RT20" | data$GLT=="RT21")]<-"06/07"
#   
#   data$Birth[which(data$GLT=="MP44")]<-"02/10"
#   
#   data$Birth[which(data$GLT=="JP19")]<-"07/10"
#   
#   data$Birth[which(data$GLT=="JP20"| data$GLT=="JP22")]<-"09/10"
#   data$Birth[which(data$GLT=="AF20")]<-"12/11"
#   data$Birth[which(data$GLT=="FA20")]<-"11/14"
#   data$Birth[which(data$GLT=="FA7" | data$GLT=="FA8")]<-"10/11"
#   data$Birth[which(data$GLT=="MP177")]<-"10/14"
#   data$Birth[which(data$GLT=="MP272")]<-"10/21"
#   data$Birth[which(data$GLT=="MP38"| data$GLT=="MP39")]<-"10/09"
#   
#   data$Birth<-lubridate::my(data$Birth)
# 
#   return(data)
#   
#   #issue with FA20 (2 birth dates!!)
# }

# revalue_stage<-function(data){
# 
#   clean<-data %>%
#     # dplyr::mutate(Stage=ifelse(DateObs <= (Birth + lubridate::years(1)),
#     #                       "IN",
#     #                ifelse (DateObs > (Birth + lubridate::years(1)) &
#     #                          DateObs <= (Birth + lubridate::years(2)),
#     #                        "SA",
#     #                  ifelse(DateObs > (Birth+years(2)),
#     #                        "AD",
#     #                        NA))))
#     
#   return(clean)
# 
# }

revalue_name<-function(data){
  
  data$GLT<- str_replace(data$GLT, "i", "I")
  
  data$GLT[data$Tattoo=="Cibele"]<-"Cibele"
  data$GLT[data$Tattoo=="Rodrigo"]<-"Rodrigo"
  data$GLT[data$Tattoo=="Cris"]<-"Cris"
  data$GLT[data$Tattoo=="1318"]<-"1318"
  data$GLT[data$Tattoo=="UR2"]<-"FA40"
 
   return(data)
  
}

revalue_sex<-function(data){
  
  data$Sexo[which(data$GLT=="MP62")]<-"F"
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
                  Sex =  ifelse("M" %in% Sexo,
                                "M",
                         ifelse("F" %in% Sexo,
                                "F",
                                "nonID")))
  
  return(clean2)
  
}

