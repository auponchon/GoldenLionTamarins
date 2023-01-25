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
"F. 2 Irmãos" = "Faz. 2 Irmaos",
"Faz. 2 lrmaos" = "Faz. 2 Irmaos",
"Faz. 2 Irmãos" = "Faz. 2 Irmaos",
"F.Iguape"=	"Faz. Iguape",
"F.Sta Helena"=	"Faz. St.Helena",
"F.Sta Helena 1"=	"Faz. Sta Helena 1",
"Faz. Sta Helena" = "Faz. Sta Helena 1",
"Faz Afetiva"=	"Faz. Afetiva",
"Faz AMLD"=	"Faz. AMLD",
"Faz. AMLD"=	"Faz. Igarape",
"Faz Boa Esperança"=	"Faz. Boa Esperanca",
"Faz Bom Retiro" = "Faz. Bom Retiro",
"Faz Estreito"=	"Faz. Estreito",
"Faz Igarape"=	"Faz. Igarape",
"Faz Nova Esperança"=	"Faz. Nova Esperanca",
"Faz Pacoti"=	"Faz. Pacoty",
"Faz Pacoty"=	"Faz. Pacoty",
"Faz Rio Vermelho"=	"Faz. Rio Vermelho",
"Faz São Francisco" = "Faz. Sao Francisco",
"Faz. São Francisco" = "Faz. Sao Francisco",
"Faz. St Helena 2" = "Faz. Sta Helena 2",
"Faz S. Helena 2"=	"Faz. Sta Helena 2",
"Faz St.Helena"=	"Faz. Sta Helena",
"Faz Sta Helena"=	"Faz. Sta Helena",
"Faz Santa Helena" = "Faz. Sta Helena",
"Faz Tamarins"=	"Faz. Tamarins",
"Faz Vendaval"= "Faz. Vendaval",
"Faz. AMLD"=	"Faz. Igarape",
"Faz. Boa Esperança"=	"Faz. Boa Esperanca",
"Faz. Cabanã nova Conquista"=	"Faz. Cabana nova Conquista",
"Faz. Cabana nova Coquista" = "Faz. Cabana nova Conquista",
"Faz. Nova Esperança"=	"Faz. Nova Esperanca",
"Faz. Sta Helena"=	"Faz. Sta Helena",
"Faz. Santa Helena2" = "Faz. Sta Helena 2",
"Faz S. Helena 2" = "Faz. Sta Helena 2",
"Faz. Sta. Fé"=	"Faz. Sta Fé",
"Faz.São João"=	"Faz. Sao Joao",
"Faz. SaoFrancisco" = "Faz. Sao Francisco",
"Faz.Tamarins"=	"Faz. Tamarins",
"Fazenda Afetiva"=	"Faz. Afetiva",
"Fazenda Boa Esperança"=	"Faz. Boa Esperanca",
"Fazenda Nova Esperança"=	"Faz. Nova Esperanca",
"Faz. Pacoti" = "Faz. Pacoty",
"Fazenda Pacoty"=	"Faz. Pacoty",
"Fazenda Patis"=	"Faz. Patis",
"Fazenda Rio Verelho"=	"Faz. Rio Vermelho",
"Fazenda Rio vermelho"=	"Faz. Rio Vermelho",
"Fazenda Rio Vermelho"=	"Faz. Rio Vermelho",
"Fazenda Tamarins"=	"Faz. Tamarins",
"Igarape" = "Faz. Igarape",
"Igarapé" = "Faz. Igarape",
"Iguape"=	"Faz. Iguape",
"OlhosD'agua"=	"OlhosD'agua",
"Patis"	="Faz. Patis",
"Paulo Freire" = "Faz. Sta Helena 1",
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
"St. Joedi" =  "Sitio Joedi",
"ST. Cisne Branco"=	"St. Cisne Branco",
"ST Cisne Branco"="St. Cisne Branco",
"ST. das Andorinhas"=	"St. das Andorinhas",
"Sitio das Andorinhas"  = "St. das Andorinhas",
"Sitio dos Jotas" = "Sitio 7 Jotas",
"ST. do Professor"=	"St. Professor",
"Sta Helena"=	"Faz. Sta Helena 1",
"Sta Helena 1"=	"Faz. Sta Helena 1",
"Sta Helena 2"=	"Faz. Sta Helena 2"))

  data$Group<-plyr::revalue(as.factor(data$Group), 
                      c("2 Irmãos"=	"DI",
                        "Afetiva"=	"AF",
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
                        "Columbia" = "CB",
                        "Corredor"=	"C.O",
                        "Daniel"=	"DN",
                        "Dublin"=	"DU",
                        "Eliel"=	"EL",
                        "Emmen"=	"EM",
                        "Emmem2" = "EM2",
                        "Emmen2" = "EM2",
                        "Estreito"="ET",
                        "F.2 Irmãos"=	"2 Irmaos",
                        "Faz AMLD"=	"PH2",
                        "Faz. Iguape"=	"SK3",
                        "Faz.2 Irmãos"=	"2 Irmaos",
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
                        "Riverbank's"="RI",
                        "RIS" = "RI",
                        "RIS2" = "RI2",
                        "RV2"=	"RVII",
                        "RSII" = "RS2",
                        "Salario" = "SL",
                        "S.Helena" = "SH",
                        "Salario2" = "SL2",
                        "Sta. Fé" = "Sta Fe",
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
                        "Sta. Fé"	= "Sta. Fe",
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
  
  data$GLT[data$Tattoo=="Cibele"]<-"Cibele"
  data$GLT[data$Tattoo=="Rodrigo"]<-"Rodrigo"
  data$GLT[data$Tattoo=="Cris"]<-"Cris"
  data$GLT[data$Tattoo=="1318"]<-"1318"
  data$GLT[data$Tattoo=="UR2"]<-"FA40"
  data$GLT[data$Tattoo=="AM1"]<-"AM1"
  
  data$GLT[data$GLT=="FT"]<-"?"
  data$GLT[data$Group=="TM3" & data$GLT=="MP213"]<-"MP213-1"
  data$GLT[data$GLT=="JU"]<-"FT"
  data$GLT[data$GLT=="RT1" & data$Sexo=="F"]<-"RT1-"
  data$GLT[data$GLT=="RT2" & data$Sexo=="F"]<-"RT2-"
  data$GLT[data$GLT=="SK19" & data$Sexo=="F"]<-"SK19-"
  data$GLT[data$GLT=="PI3" & data$Sexo=="F"]<-"PI3-"
  data$GLT[data$GLT=="AFII"]<-"AF11"
  
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

