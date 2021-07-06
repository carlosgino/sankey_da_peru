
library(tidyverse)
library(networkD3)

#data que fue modificada por excel, tenia deformacion de origen, es valida para analisis 
segunda_vuelta <- read_csv("~/GitHub/Elecciones/brexit_sankey_graphic/sankey_peru_da/data/segunda_vuelta.csv")


attach(vuelta2)
names(vuelta2)

#2/ Las macroregiones  comprenden los siguientes departamentos:
#Norte: Tumbes, Piura, Lambayeque, Cajamarca, La Libertad.  
#Centro: Lima ( excluye provincia de LIma), Ancash, Junín, Cerro de Pasco, Huánuco, Huancavelica, Ayacucho, Ica.  
#Sur: Arequipa, Moquegua, Tacna, Cusco, Madre de Dios, Apurímac.  
#Oriente: Loreto, Ucayali, Amazonas, San Martín. 
#Lima Metropolitana: Provincia Constitucional del Callao y provincia de Lima).						

5+8+6+4+2

ubigeo <- as.character(segunda_vuelta$ubigeo) 

vuelta2 <- read.csv("C:/Users/gigar/Documents/GitHub/Elecciones/elecciones2021_da/vuelta2.csv")

rm(vuelta2)

write.csv(vuelta2, "vuelta2.csv", row.names=FALSE) 

####################################################################################33

vuelta2 <- read_csv("vuelta2.csv")

vuelta22 <- vuelta2

vuelta22 <- vuelta2 %>%  
 select(-c(TIPO_ELECCION,MESA_DE_VOTACION,DESCRIP_ESTADO_ACTA,TIPO_OBSERVACION)) %>% 
 rename(ubigeo = UBIGEO,
         departamento = DEPARTAMENTO,
         provincia = PROVINCIA,
         distrito = DISTRITO,    
         num_electores = N_ELEC_HABIL,
         num_votos = N_CVAS,
         votos_perulibre= VOTOS_P1,
         votos_fuerzapopular = VOTOS_P2,
         votos_blanco = VOTOS_VB, 
         votos_nulos = VOTOS_VN) %>% 
  replace(is.na(.),0) %>% 
  rowwise() %>% 
  mutate(votos_validos = sum(votos_perulibre + votos_fuerzapopular, na.rm = T)) %>% 
  mutate(ausentismo = sum(num_electores - num_votos, na.rm = T))%>%
  mutate(BIN = sum(votos_blanco + votos_nulos, na.rm = T)) %>% 
  group_by(ubigeo) %>% 
  summarise(across(num_votos:BIN, ~ sum(.x, na.rm = TRUE))) 


vuelta23 <- select(vuelta2, 1:4) %>% 
  rename(ubigeo = UBIGEO,
         departamento = DEPARTAMENTO,
         provincia = PROVINCIA,
         distrito = DISTRITO)
vuelta23 <- unique(vuelta23)

vuelta24 <- inner_join(vuelta23, vuelta22, by= "ubigeo") %>% 
  select(ubigeo,departamento,provincia, distrito, num_electores, num_votos, votos_perulibre,     
         votos_fuerzapopular, votos_blanco, votos_nulos, votos_validos, ausentismo, BIN) %>% 
  arrange(ubigeo)
         

#rm(vuelta23);rm(vuelta2);rm(vuelta22)

sum(vuelta24$num_votos, na.rm=TRUE)
# 18856218

sum(vuelta24$num_electores, na.rm=TRUE)
# 25287954

##total votos peru libre
sum(vuelta24$votos_perulibre, na.rm=TRUE)
#[1] 8835970

#diferencia de votos 
8835970-8791730
#44240


#total votos fza popular
sum(vuelta24$votos_fuerzapopular, na.rm=TRUE) 
#[1] 8791730

# total votos blanco
sum(vuelta24$votos_blanco, na.rm=TRUE)
# 121478

#total votos nulos
sum(vuelta24$votos_nulos, na.rm=TRUE)
# 1107640

#total ausentismo electoral
sum(vuelta24$ausentismo, na.rm=TRUE)
#[1] 6431136

#total blanco, impugnados y nulos
sum(vuelta24$BIN, na.rm=TRUE)
#[1] 1229118

#total votos validos
sum(vuelta24$votos_validos)
# 17627700

#diferencia oficial de votos al 1ro de julio 2021
8836380-8792117
#[1] 44263



#########################
#organizando las macro regiones: Norte, tumbes, piura, lambayeque, cajamarca, la libertad

norte <- c("TUMBES", "PIURA", "LAMBAYEQUE", "CAJAMARCA", "LA LIBERTAD")
norte <- vuelta24 %>% filter(departamento %in% norte) %>% 
  mutate(macroregion = "norte")
  

#Sur: Arequipa, Moquegua, Tacna, Cusco, Madre de Dios, Apurímac.

sur <- c("AREQUIPA", "MOQUEGUA", "TACNA", "CUSCO", "MADRE DE DIOS", "APURIMAC", "PUNO")
sur <-  vuelta24 %>%  filter(departamento %in% sur) %>% mutate(macroregion = "sur")

#Oriente: Loreto, Ucayali, Amazonas, San Martín. 

oriente <- c("LORETO", "UCAYALI", "AMAZONAS", "SAN MARTIN")
oriente<-  vuelta24 %>% filter(departamento %in% oriente) %>% mutate(macroregion = "oriente")

##Lima Metropolitana: Provincia Constitucional del Callao y provincia de Lima).
limaycal <-  vuelta24 %>% filter(provincia  %in% c("LIMA", "CALLAO")) %>% mutate(macroregion = "limaycal")

#Centro: Lima ( excluye provincia de LIma), Ancash, Junín, Cerro de Pasco, Huánuco, Huancavelica, Ayacucho, Ica.  
centro_p <- c("ANCASH", "JUNIN", "PASCO", "HUANUCO", "HUANCAVELICA", "AYACUCHO", "ICA") # + LIMA_REGION

lima_dep <- vuelta24 %>% filter("LIMA"== departamento)
LIMA_REGION <- anti_join( lima_dep, limaycal)

centro_p <-  vuelta24 %>% group_by(departamento) %>% 
  filter(departamento %in% centro_p) 

centro <- full_join(LIMA_REGION, centro_p) %>% mutate(macroregion = "centro")
 
 
#exterior: peruanos residentes en los cinco continentes
exterior <- c("AFRICA", "AMERICA", "ASIA", "EUROPA", "OCEANIA")

exterior <- vuelta24 %>% filter(departamento %in% exterior) %>% mutate(macroregion = "exterior")

#rm(lima_dep);rm(centro_p); rm(LIMA_REGION)

full_vuelta24 <- bind_rows(oriente, norte,centro,limaycal,sur, exterior)


