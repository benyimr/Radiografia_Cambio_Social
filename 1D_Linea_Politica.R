rm(list=ls())

#Cargar paquetes
library(ggplot2)
library(ggthemes)
library(lme4)
library(nnet)
library(tidyverse)
library(sjmisc)
library(sjPlot)
library(sjstats)
library(survey)
library(srvyr)


#Importar bases de datos WIDE
load("ELSOC_Wide_2016_2018_v1.10_R.RData")

#Importar bases de datos LONG
#elsoc_long <- read_stata(path = "ELSOC_Long_2016_2018_Temporal.dta")

#Importar resultados fiabilidad
#fiabilidad <- read_rds(path = "Base_Global_Fiabilidad.rds")


###############################################################################################################################################################################
#PROCESAMIENTO DE BASE DE DATOS

#Renombrar datos Wide
elsoc_panel_01 <- elsoc_wide_2016_2018

#Eliminar Valores Perdidos en datos WIDE
elsoc_panel_01[elsoc_panel_01 == -888] <- NA
elsoc_panel_01[elsoc_panel_01 == -999] <- NA

#Ordenar por ID encuesta
elsoc_panel_01 %>% dplyr::arrange(desc(idencuesta)) -> elsoc_panel_01

#Filtrar datos con 3 mediciones
elsoc_panel_01 %>% dplyr::filter(tipo_atricion == 1 & tipo_caso != 2) -> elsoc_panel_02


#Eliminar Valores Perdidos en datos LONG (en diseño datos se eliminaron casos)
#elsoc_long[elsoc_long == -888] <- NA
#elsoc_long[elsoc_long == -999] <- NA

#Ordenar por ID encuesta
#elsoc_long %>% dplyr::arrange(desc(idencuesta)) -> elsoc_long


###############################################################################################################################################################################
#RECODIFICACION DE VARIABLES

#Edad
elsoc_panel_02$edad_rec_w01 <- car::recode(elsoc_panel_02$m0_edad_w01, "18:29=1;30:49=2;50:64=3;65:100=4")
elsoc_panel_02$edad_rec_w02 <- car::recode(elsoc_panel_02$m0_edad_w02, "18:29=1;30:49=2;50:64=3;65:100=4")
elsoc_panel_02$edad_rec_w03 <- car::recode(elsoc_panel_02$m0_edad_w03, "18:29=1;30:49=2;50:64=3;65:100=4")

#Participación Electoral
elsoc_panel_02$c11_w01rec1 <- car::recode(elsoc_panel_02$c11_w01, "1='No';2='Sí';3='No'")
elsoc_panel_02$c11_w03rec1 <- car::recode(elsoc_panel_02$c11_w03, "1='No';2='Sí';3='No'")

elsoc_panel_02$c11_w01rec2 <- car::recode(elsoc_panel_02$c11_w01, "1='No';2='Sí';3=NA")
elsoc_panel_02$c11_w03rec2 <- car::recode(elsoc_panel_02$c11_w03, "1='No';2='Sí';3=NA")

#Perfiles de Participación
elsoc_panel_02$temp1       <- paste0(elsoc_panel_02$c11_w01rec1,elsoc_panel_02$c11_w03rec1)
elsoc_panel_02$temp2       <- paste0(elsoc_panel_02$c11_w01rec2,elsoc_panel_02$c11_w03rec2)

elsoc_panel_02$perfil_voto1 <- car::recode(elsoc_panel_02$temp1,"'NASí'=NA;'NoNA'=NA;'NoNo'='No Votó'; 'NoSí'='Sólo 2017';'SíNA'=NA;'SíNo'='Sólo 2013';'SíSí'='Votó'")
elsoc_panel_02$perfil_voto2 <- car::recode(elsoc_panel_02$temp2,"'NANo'=NA;'NASí'=NA;'NoNA'=NA;'NoNo'='No Votó';'NoSí'='Sólo 2017';'SíNA'=NA;'SíNo'='Sólo 2013';'SíSí'='Votó'")

elsoc_panel_02$perfil_voto1a <- factor(elsoc_panel_02$perfil_voto1, levels = c("Votó","Sólo 2013","Sólo 2017","No Votó"))  
elsoc_panel_02$perfil_voto2a <- factor(elsoc_panel_02$perfil_voto2, levels = c("Votó","Sólo 2013","Sólo 2017","No Votó"))  

elsoc_panel_02$perfil_voto1b <- car::recode(elsoc_panel_02$perfil_voto1a,"'Votó'='Votante';c('Sólo 2013','Sólo 2017')='Ocasional';'No Votó'='Abstencionista'")
elsoc_panel_02$perfil_voto1b <- factor(elsoc_panel_02$perfil_voto1b, levels = c("Votante","Ocasional","Abstencionista"))
elsoc_panel_02$perfil_voto2b <- car::recode(elsoc_panel_02$perfil_voto2a,"'Votó'='Votante';c('Sólo 2013','Sólo 2017')='Ocasional';'No Votó'='Abstencionista'")
elsoc_panel_02$perfil_voto2b <- factor(elsoc_panel_02$perfil_voto2b, levels = c("Votante","Ocasional","Abstencionista"))

#Voto Retrospectivo
elsoc_panel_02$c11_w01rec2 <- as.factor(elsoc_panel_02$c11_w01rec2)
elsoc_panel_02$c11_w03rec2 <- as.factor(elsoc_panel_02$c11_w03rec2)
elsoc_panel_02$voto_w03    <- car::recode(elsoc_panel_02$c11_w03rec2,"'Sí'=1;'No'=0")
elsoc_panel_02$voto_w01    <- car::recode(elsoc_panel_02$c11_w01rec2,"'Sí'=1;'No'=0")

#Variables Modelo
elsoc_panel_02$hombre      <- car::recode(elsoc_panel_02$m0_sexo_w03,"1=1;2=0") #Hombre 2017
elsoc_panel_02$edad        <- elsoc_panel_02$m0_edad_w03                        #Edad 2017
elsoc_panel_02$edad2 <- elsoc_panel_02$edad * elsoc_panel_02$edad               #Edad SQ 2017
elsoc_panel_02$edad_tr     <- as.factor(car::recode(elsoc_panel_02$m0_edad_w03, "18:29=1;30:49=2;50:64=3;65:100=4"))    #Edad en tramos 2017
elsoc_panel_02$educ        <- as.factor(car::recode(elsoc_panel_02$m01_w03,"c(1,2,3)=0;c(4,5)=1;c(6,7)=2;c(8,9,10)=3")) #Educación 2017

elsoc_panel_02$interes  <- as.factor(car::recode(elsoc_panel_02$c13_w03,"1=0;c(2,3)=1;c(4,5)=2"))                  #Interes política 2017
elsoc_panel_02$int_prom <- round((elsoc_panel_02$c13_w01 + elsoc_panel_02$c13_w02 + elsoc_panel_02$c13_w03)/3)     #Promedio de Interés Político
elsoc_panel_02$pospol1  <- car::recode(elsoc_panel_02$c15_w03,"c(11,12)='No identifica';c(0,1,2,3)='Izquierda';    
                                      c(4,5,6)='Centro';c(7,8,9,10)='Derecha'")
elsoc_panel_02$pospol1  <- factor(elsoc_panel_02$pospol1, levels = c("No identifica","Izquierda","Centro","Derecha"))
elsoc_panel_02$pospol2  <- car::recode(elsoc_panel_02$c15_w03,"c(11,12)='No identifica';c(0,1,2,3,4)='Izquierda';  
                                      c(5)='Centro';c(6,7,8,9,10)='Derecha'")
elsoc_panel_02$pospol2  <- factor(elsoc_panel_02$pospol2, levels = c("No identifica","Izquierda","Centro","Derecha"))

elsoc_panel_02$habla1   <- car::recode(elsoc_panel_02$c14_01_w03,"1=0;c(2,3)=1;c(4,5)=2")   #Habla de Politica 2017
elsoc_panel_02$informa1 <- car::recode(elsoc_panel_02$c14_02_w03,"1=0;c(2,3)=1;c(4,5)=2")   #Informa de Politica 2017

elsoc_panel_02$pid        <- car::recode(elsoc_panel_02$c16_w03,"1:14=1;15=0") #Identificacion con Partidos 2018
elsoc_panel_02$pid_ant1   <- car::recode(elsoc_panel_02$c16_w02,"1:14=1;15=0") #Identificacion con Partidos 2017
elsoc_panel_02$pid_ant2   <- car::recode(elsoc_panel_02$c16_w01,"1:14=1;15=0") #Identificacion con Partidos 2016

elsoc_panel_02$deber    <- car::recode(elsoc_panel_02$c10_01_w03,"c(1,2,3)=0;c(4,5)=1")   #Votar es deber 2017
elsoc_panel_02$deb_prom <- round((elsoc_panel_02$c10_01_w01 + elsoc_panel_02$c10_01_w02 + elsoc_panel_02$c10_01_w03)/3) #Promedio Votar es Deber
elsoc_panel_02$eficacia <- elsoc_panel_02$c10_02_w03 + elsoc_panel_02$c10_03_w03          #Índice de eficacia 2017


#Intencion de Voto
elsoc_panel_02$c36_w02rec <- car::recode(elsoc_panel_02$c36_w02,"1='Guillier';2='Piñera';3='Sánchez';4='Goic'; 
                                         5='Enríquez-Ominami';c(6,7)='Otro candidato';8='Indeciso'; 9='Probablemente no vote';10='No votará'")
elsoc_panel_02$c36_w02rec <- factor(elsoc_panel_02$c36_w02rec,levels = c("Guillier","Piñera","Sánchez","Goic","Enríquez-Ominami",
                                                                         "Otro candidato","Indeciso","Probablemente no vote","No votará"))
elsoc_panel_02$c39_w03temp <- ifelse(elsoc_panel_02$c11_w03rec2=="No",11,elsoc_panel_02$c39_w03)
elsoc_panel_02$c39_w03rec  <- car::recode(elsoc_panel_02$c39_w03temp,"1='Goic';2='Kast';3='Piñera';4='Guillier';5='Sánchez';
                                          6='Enríquez-Ominami';c(7,8)='Otro candidato';c(9,10,11)='No votó'")
elsoc_panel_02$c39_w03rec <- factor(elsoc_panel_02$c39_w03rec,levels = c("Guillier","Piñera","Sánchez","Goic","Enríquez-Ominami",
                                                                         "Kast","Otro candidato","No votó"))


#Movimiento social más valorado 
elsoc_panel_02$c20_otro_w01temp <- car::recode(elsoc_panel_02$c20_otro_w01, "'animalista' = 10;'Anti maltrato animal'= 10;                                  
                                             'apoyar a los enfermos'= 10;'ayuda a indigente'= 10;'contra las afp'= 9;                                        
                                             'de la igual de genero'= 8;'de los pensionados'= 9;
                                             'igualdad de inclicion para personas con discapacidad'= 10;'junta de vecinos'= 10;                                      
                                             'mejor salud'= 10;'movimiento animalista' = 10;'movimiento anti afp' = 9;                                  
                                             'movimiento de defender los animales' = 10;'Movimiento derechos de las mujeres'  = 8;                  
                                             'Movimiento Mejora de sistema previsional' = 9;'Movimiento No mas AFP' = 9;                                
                                             'movimiento por aborto' = 8;'movimiento por causa de las pensiones de vejez'= 9;        
                                             'movimiento por eliminacion de afp' = 9;'movimiento social por el respeto e igualdad a la mujer'= 8;
                                             'ni una menos' = 8;'ni una menos, antifemicidio' = 8;'no  mas afp'  = 9;                                         
                                             'no  mas AFP' = 9;'no +afp'   = 9;'no mas afp' = 9;'no mas Afp' = 9;'no mas AFP' = 9;                                           
                                             'No mas AFP' = 9;'pensiones afp'= 9;'pro aborto' = 8;
                                             'protencion de los animales' = 10;'salud'= 10;''= -999")
elsoc_panel_02$c20_w01rec <- ifelse(elsoc_panel_02$c20_otro_w01temp == -999, elsoc_panel_02$c20_w01, elsoc_panel_02$c20_otro_w01temp)

elsoc_panel_02$c20_otro_w02temp <- car::recode(elsoc_panel_02$c20_otro_w02, "'Animalista'= 10;'cambio sename mas control'= 10;                            
                                           'dirigente vecinal' = 10;'discapacidad' = 10;'el del respeto por las personas' = 10;                     
                                           'ideal mas justo para las personas'= 10;'iglesia evangelica.' = 10;                                 
                                           'movimiento de integracion a los discapacitados'= 10;'movimiento por la tercera edad'= 10;                       
                                           'Movimiento por tener el derecho a elegir'= 8;'moviminto feminista'= 8;                                  
                                           'Ni Una Menos'= 8;'no a la baura de temuco'= 10;'No mas AFP'= 9;                                           
                                           'Sename ninos' = 10;'sistema salud publica mala atencion demasiada demanda'= 10;
                                           'todas'= 10;''= -999")
elsoc_panel_02$c20_w02rec <- ifelse(elsoc_panel_02$c20_otro_w02temp == -999, elsoc_panel_02$c20_w02, elsoc_panel_02$c20_otro_w02temp)


elsoc_panel_02$c20_otro_w03temp <- car::recode(elsoc_panel_02$c20_otro_w03, "'Cambio En La Salud Publica'= 10;'Comite De Vivienda'= 10;                                        
                                           'Damas De Amarillo'= 10;'Indigenas' = 4;'Movimiento Animalista'   = 10;                                  
                                           'No Permitir Muertes De Carabineros A Manos De Delincuentes'= 7;
                                           'Organizacion De Adulto Mayor'= 10;'Para La Vivienda'    = 10;'Salud'  = 10;                                                   
                                           'Servicio Social De Ayuda A Los Pobres'= 10;''= -999")
elsoc_panel_02$c20_w03rec <- ifelse(elsoc_panel_02$c20_otro_w03temp == -999, elsoc_panel_02$c20_w03, elsoc_panel_02$c20_otro_w03temp)

#Valora o no un MS
elsoc_panel_02$c20_w01rec1 <- car::recode(elsoc_panel_02$c20_w01rec,"1:10='Sí';11='No'")
elsoc_panel_02$c20_w02rec1 <- car::recode(elsoc_panel_02$c20_w02rec,"1:10='Sí';11='No'")
elsoc_panel_02$c20_w03rec1 <- car::recode(elsoc_panel_02$c20_w03rec,"1:10='Sí';11='No'")

#Más valorado
elsoc_panel_02$c20_w01rec2 <- car::recode(elsoc_panel_02$c20_w01rec, "1=2;2:10=1;11=0")
elsoc_panel_02$c20_w02rec2 <- car::recode(elsoc_panel_02$c20_w02rec, "1=2;2:10=1;11=0")
elsoc_panel_02$c20_w03rec2 <- car::recode(elsoc_panel_02$c20_w03rec, "1:7=1;8=2;9=2;10=1;11=0")

#Persistencia
elsoc_panel_02$mov_mas <- paste(elsoc_panel_02$c20_w01rec, elsoc_panel_02$c20_w02rec, elsoc_panel_02$c20_w03rec, sep = "-")
elsoc_panel_02$mov_val <- paste(elsoc_panel_02$c20_w01rec1, elsoc_panel_02$c20_w02rec1, elsoc_panel_02$c20_w03rec1, sep = "-")

#Participación en Movimientos Sociales
elsoc_panel_02$c22_w01rec <- car::recode(elsoc_panel_02$c22_w01,"1=0;c(2,3)=1;c(4,5)=2")
elsoc_panel_02$c22_w02rec <- car::recode(elsoc_panel_02$c22_w02,"1=0;c(2,3)=1;c(4,5)=2")
elsoc_panel_02$c22_w03rec <- car::recode(elsoc_panel_02$c22_w03,"1=0;c(2,3)=1;c(4,5)=2")
elsoc_panel_02$c22_prom   <- round((elsoc_panel_02$c22_w01rec + elsoc_panel_02$c22_w02rec + elsoc_panel_02$c22_w03rec)/3)

#Asiste a marchas
elsoc_panel_02$c08_02_w01rec <- car::recode(elsoc_panel_02$c08_02_w01,"1=0;c(2,3)=1;c(4,5)=1")
elsoc_panel_02$c08_02_w02rec <- car::recode(elsoc_panel_02$c08_02_w02,"1=0;c(2,3)=1;c(4,5)=1")
elsoc_panel_02$c08_02_w03rec <- car::recode(elsoc_panel_02$c08_02_w03,"1=0;c(2,3)=1;c(4,5)=1")
elsoc_panel_02$c08_02_prom   <- round((elsoc_panel_02$c08_02_w01rec + elsoc_panel_02$c08_02_w02rec + elsoc_panel_02$c08_02_w03rec)/3)

#Asiste a huelgas
elsoc_panel_02$c08_03_w01rec <- car::recode(elsoc_panel_02$c08_03_w01,"1=0;c(2,3)=1;c(4,5)=1")
elsoc_panel_02$c08_03_w02rec <- car::recode(elsoc_panel_02$c08_03_w02,"1=0;c(2,3)=1;c(4,5)=1")
elsoc_panel_02$c08_03_w03rec <- car::recode(elsoc_panel_02$c08_03_w03,"1=0;c(2,3)=1;c(4,5)=1")
elsoc_panel_02$c08_03_prom   <- round((elsoc_panel_02$c08_03_w01rec + elsoc_panel_02$c08_03_w02rec + elsoc_panel_02$c08_03_w03rec)/3)


#Imputación de respuestas Otro Partido
elsoc_panel_02$c16_otro_w01rec <- car::recode(elsoc_panel_02$c16_otro_w01, "c('autonomista','izquierda libertaria',
                                              'mir','MIR','movimiento ambientalista','partido ecologista verde')=14;'independiente'=15;else = 0")
elsoc_panel_02$c16_w01rec <- ifelse(elsoc_panel_02$c16_otro_w01rec == 0 , elsoc_panel_02$c16_w01,ifelse(elsoc_panel_02$c16_otro_w01rec == 14, 14, 15))

elsoc_panel_02$c16_otro_w02rec <- car::recode(elsoc_panel_02$c16_otro_w02, "c('ecologista','Frente Amplio','igualdad','izquierda',
                                            'izquierda libertaria','mir','partido ecologista verde','partido igualdad','por mi patria')=14;
                                            c('apoyo independiente','aun no hay ninguno que me represente','independiente',
                                            'los que no estan ligados a ningun patrpido')=15;else = 0")
elsoc_panel_02$c16_w02rec <- ifelse(elsoc_panel_02$c16_otro_w02rec == 0 , elsoc_panel_02$c16_w02, ifelse(elsoc_panel_02$c16_otro_w02rec == 14, 14, 15))

elsoc_panel_02$c16_otro_w03rec <- car::recode(elsoc_panel_02$c16_otro_w03, "c('Accion Republicana','Al De Beatriz Sanchez',
                                              'Allendista','Biblia','Frente Amplio','Igualdad','Izquierda Libertaria','La Democracia','Libertarios',
                                              'Mir','Partido Ecologista','Partido Ecologista Verde','Partido Igualdad')=14;'El De Bachelett'=12;
                                              'Evopoli'=5;'Independiente'=15;else=0")
elsoc_panel_02$c16_w03rec <- ifelse(elsoc_panel_02$c16_otro_w03rec == 0 , elsoc_panel_02$c16_w03, ifelse(elsoc_panel_02$c16_otro_w03rec == 5, 5,
                                                                                                         ifelse(elsoc_panel_02$c16_otro_w03rec ==12, 12, ifelse(elsoc_panel_02$c16_otro_w03rec == 14, 14, 15))))


#Se identifica o no con un partido
elsoc_panel_02$c16_w01rec1 <- car::recode(elsoc_panel_02$c16_w01rec, "1:14='Sí';15='No'")
elsoc_panel_02$c16_w01rec1 <- factor(elsoc_panel_02$c16_w01rec1, levels = c("Sí","No"))

elsoc_panel_02$c16_w02rec1 <- car::recode(elsoc_panel_02$c16_w02rec, "1:14='Sí';15='No'")
elsoc_panel_02$c16_w02rec1 <- factor(elsoc_panel_02$c16_w02rec1, levels = c("Sí","No"))

elsoc_panel_02$c16_w03rec1 <- car::recode(elsoc_panel_02$c16_w03rec, "1:14='Sí';15='No'")
elsoc_panel_02$c16_w03rec1 <- factor(elsoc_panel_02$c16_w03rec1, levels = c("Sí","No"))


#Cual Partido? (Subgrupos)
elsoc_panel_02$c16_w01rec2 <- car::recode(elsoc_panel_02$c16_w01rec,"c(1,2)='PC+PH';c(3,4)='PRO+RD';c(5,7,9)='EVO+AMP+PRI';
                                          c(6,8,12,13)='PPD+PDC+PS+PR';c(10,11)='RN+UDI';14='Otro';15=NA")  
elsoc_panel_02$c16_w02rec2 <- car::recode(elsoc_panel_02$c16_w02rec,"c(1,2)='PC+PH';c(3,4)='PRO+RD';c(5,7,9)='EVO+AMP+PRI';
                                          c(6,8,12,13)='PPD+PDC+PS+PR';c(10,11)='RN+UDI';14='Otro';15=NA")  
elsoc_panel_02$c16_w03rec2 <- car::recode(elsoc_panel_02$c16_w03rec,"c(1,2)='PC+PH';c(3,4)='PRO+RD';c(5,7,9)='EVO+AMP+PRI';
                                          c(6,8,12,13)='PPD+PDC+PS+PR';c(10,11)='RN+UDI';14='Otro';15=NA")  

#Persistencia de Party ID
elsoc_panel_02$pers_part_01 <- paste(elsoc_panel_02$c16_w01rec1,elsoc_panel_02$c16_w02rec1,elsoc_panel_02$c16_w03rec1,sep="-")
elsoc_panel_02$pers_part_02 <- paste(elsoc_panel_02$c16_w01rec2,elsoc_panel_02$c16_w02rec2,elsoc_panel_02$c16_w03rec2,sep="-")


#Imputación de respuestas Otra Coalicion
elsoc_panel_02$c17_otro_w01rec <- car::recode(elsoc_panel_02$c17_otro_w01, "c('amplitud','el partido progresista',
                                              'humanista','partido humanista')=4;'Independiente'=5;else=0")
elsoc_panel_02$c17_w01rec <- ifelse(elsoc_panel_02$c17_otro_w01rec == 0, elsoc_panel_02$c17_w01,ifelse(elsoc_panel_02$c17_otro_w01rec == 4, 4, 5))

elsoc_panel_02$c17_otro_w02rec <- car::recode(elsoc_panel_02$c17_otro_w02, "c('centro izquierda no nueva mayoria','socialismo tradicional')=4;
                                              'centro derecha'=1;c('ecologista','humanista ecologica','sfeir')=3;'van solos Carolina GOIC'=2;else=0")
elsoc_panel_02$c17_w02rec <- ifelse(elsoc_panel_02$c17_otro_w02rec == 0, elsoc_panel_02$c17_w02,ifelse(elsoc_panel_02$c17_otro_w02rec == 1, 1, 
                                                                                                       ifelse(elsoc_panel_02$c17_otro_w02rec == 2, 2, ifelse(elsoc_panel_02$c17_otro_w02rec == 3, 3, 4))))

elsoc_panel_02$c17_otro_w03rec <- car::recode(elsoc_panel_02$c17_otro_w03,"c('Evolucion Politica','Las De La Derecha')=1;
                                              c('La De Bachelet','La De Guiller','Ppd')=2;c('Humanista','La De Beatriz Sanchez')=3;
                                              c('Mir','Nacional Socialismo','No Recuerda')=4;else=0")
elsoc_panel_02$c17_w03rec <- ifelse(elsoc_panel_02$c17_otro_w03rec == 0, elsoc_panel_02$c17_w03,ifelse(elsoc_panel_02$c17_otro_w03rec == 1, 1, 
                                                                                                       ifelse(elsoc_panel_02$c17_otro_w03rec == 2, 2, ifelse(elsoc_panel_02$c17_otro_w03rec == 3, 3, 4))))


#Se identifica o no con una coalicion
elsoc_panel_02$c17_w01rec1 <- car::recode(elsoc_panel_02$c17_w01rec,"1:4='Sí';5='No'")
elsoc_panel_02$c17_w01rec1 <- factor(elsoc_panel_02$c17_w01rec1, levels = c("Sí","No"))

elsoc_panel_02$c17_w02rec1 <- car::recode(elsoc_panel_02$c17_w02rec,"1:4='Sí';5='No'")
elsoc_panel_02$c17_w02rec1 <- factor(elsoc_panel_02$c17_w02rec1, levels = c("Sí","No"))

elsoc_panel_02$c17_w03rec1 <- car::recode(elsoc_panel_02$c17_w03rec,"1:4='Sí';5='No'")
elsoc_panel_02$c17_w03rec1 <- factor(elsoc_panel_02$c17_w03rec1, levels = c("Sí","No"))

#Persistencia de Party ID
elsoc_panel_02$pers_coal_01 <- paste(elsoc_panel_02$c17_w01rec1,elsoc_panel_02$c17_w02rec1,elsoc_panel_02$c17_w03rec1, sep="-")
elsoc_panel_02$pers_coal_02 <- paste(elsoc_panel_02$c17_w01rec, elsoc_panel_02$c17_w02rec, elsoc_panel_02$c17_w03rec, sep="-")



###############################################################################################################################################################################
#GRÁFICOS DE FIABILIDAD

variables <- c("c05_08","c20_dic","c01","c05_01","c25","c10_02","c10_03","c05_06","c10_01","c15_dic1","c14_02","c05_02","c05_04",
               "c05_05","c05_07","c08_01","c17_dic","c22","c05_03","c16_dic","c14_01","c15_dic3","c13","c08_04","c08_03","c08_02")

fiabilidad %>% dplyr::filter(Variable %in% variables) %>% dplyr::select(Variable, Tipo, Niveles, Categoria_1,starts_with("ICC")) -> pruebas
head(pruebas)

###############################################################################################################################################################################
#PARTICIPACIÓN ELECTORAL

#Declaración de diseño muestral
m_orig_des1a <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador01_w01, data = elsoc_panel_02)
m_orig_des2a <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_panel_02)


#Asigna como No votante por edad
voto1 <- data.frame(table(elsoc_panel_02$c11_w01rec1, elsoc_panel_02$c11_w03rec1))
names(voto1) <- c("V_2013","V_2017","Frec1")
voto1$Frec2 <- data.frame(svytable(formula = ~c11_w01rec1+c11_w03rec1, design = m_orig_des1a, Ntotal = 2078))$Freq
voto1$Frec3 <- data.frame(svytable(formula = ~c11_w01rec1+c11_w03rec1, design = m_orig_des2a, Ntotal = 2078))$Freq

#Excluye no votante por edad
voto2 <- data.frame(table(elsoc_panel_02$c11_w01rec2, elsoc_panel_02$c11_w03rec2))
names(voto2) <- c("V_2013","V_2017","Frec4")
voto2$Frec5 <- data.frame(svytable(formula = ~c11_w01rec2+c11_w03rec2, design = m_orig_des1a, Ntotal = 2066))$Freq
voto2$Frec6 <- data.frame(svytable(formula = ~c11_w01rec2+c11_w03rec2, design = m_orig_des2a, Ntotal = 2066))$Freq

voto3 <- dplyr::full_join(x = voto1, y = voto2, by = c("V_2013","V_2017"))

#Exportar objeto como .csv
voto3
write_csv(voto3, path = "Voto.csv")

#Remover objetos innecesarios
rm(voto1,voto2)

#Modelos de Participación Electoral en 2017
m0 <- glm(c11_w03rec2 ~ 1 + c11_w01rec2 + hombre + edad + I(edad^2) + educ + interes + pospol1 + c01_w03 + c38_w03 + habla1 + informa1 + pid + deber + eficacia, family = binomial(link = "logit"), data = elsoc_panel_02)
m1 <- glm(c11_w03rec2 ~ 1 + c11_w01rec2 + hombre + edad + I(edad^2) + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1 + informa1 + pid + deber + eficacia, family = binomial(link = "logit"), data = elsoc_panel_02)
m2 <- glm(c11_w03rec2 ~ 1 + c11_w01rec2 + hombre + edad_tr          + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1 + informa1 + pid + deber + eficacia, family = binomial(link = "logit"), data = elsoc_panel_02)
m3 <- glm(c11_w03rec2 ~ 1 + c11_w01rec2 + hombre + edad + edad2     + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1 + informa1 + pid + deber + eficacia, family = binomial(link = "logit"), data = elsoc_panel_02)
m4 <- glm(c11_w03rec2 ~ 1 + c11_w01rec2 + hombre + edad + edad2     + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1 + informa1 + pid + pid_ant1 + deber + eficacia, family = binomial(link = "logit"), data = elsoc_panel_02)
m5 <- glm(c11_w03rec2 ~ 1 + c11_w01rec2 + hombre + edad + edad2     + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1 + informa1 + pid + pid_ant1 + pid_ant2 + deber + eficacia, family = binomial(link = "logit"), data = elsoc_panel_02)

n0 <- svyglm(voto_w03 ~ 1 + voto_w01 + hombre + edad + I(edad^2) + educ + interes + pospol1 + c01_w03 + c38_w03 + habla1 + informa1 + pid + deber + eficacia, family = binomial(link = "logit"), design = m_orig_des2a)
n1 <- svyglm(voto_w03 ~ 1 + voto_w01 + hombre + edad + I(edad^2) + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1 + informa1 + pid + deber + eficacia, family = binomial(link = "logit"), design = m_orig_des2a)
n2 <- svyglm(voto_w03 ~ 1 + voto_w01 + hombre + edad_tr          + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1 + informa1 + pid + deber + eficacia, family = binomial(link = "logit"), design = m_orig_des2a)
n3 <- svyglm(voto_w03 ~ 1 + voto_w01 + hombre + edad + edad2     + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1 + informa1 + pid + deber + eficacia, family = binomial(link = "logit"), design = m_orig_des2a)
n4 <- svyglm(voto_w03 ~ 1 + voto_w01 + hombre + edad + edad2     + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1 + informa1 + pid + pid_ant1 + deber + eficacia, family = binomial(link = "logit"), design = m_orig_des2a)
n5 <- svyglm(voto_w03 ~ 1 + voto_w01 + hombre + edad + edad2     + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1 + informa1 + pid + pid_ant1 + pid_ant2 + deber + eficacia, family = binomial(link = "logit"), design = m_orig_des2a)

#Resultados
screenreg(list(m0,m1,m2,m3,m4,m5, n0,n1,n2, n3,n4,n5))

#Probabilidad Predicha
elsoc_panel_02 %>% dplyr::select(voto_w03,voto_w01,hombre,edad, edad2, educ,interes, pospol2, c01_w03, c38_w03, habla1, informa1, pid, 
                                 deber, eficacia) %>% na.omit() ->elsoc_voto

datos.simulados_01 <- with(elsoc_voto, data.frame(voto_w03 = elsoc_voto$voto_w03, hombre = elsoc_voto$hombre, edad = elsoc_voto$edad, edad2=elsoc_voto$edad2,
                                                  educ = elsoc_voto$educ, interes = elsoc_voto$interes,pospol2 = elsoc_voto$pospol2, c01_w03 = elsoc_voto$c01_w03,
                                                  c38_w03 = elsoc_voto$c38_w03,habla1 = elsoc_voto$habla1, informa1 = elsoc_voto$informa1, pid = elsoc_voto$pid, 
                                                  deber = elsoc_voto$deber, eficacia = elsoc_voto$eficacia, voto_w01 = "0"))

datos.simulados_02 <- with(elsoc_voto, data.frame(voto_w03 = elsoc_voto$voto_w03, hombre = elsoc_voto$hombre, edad = elsoc_voto$edad, edad2=elsoc_voto$edad2,
                                                  educ = elsoc_voto$educ, interes = elsoc_voto$interes,pospol2 = elsoc_voto$pospol2, c01_w03 = elsoc_voto$c01_w03,
                                                  c38_w03 = elsoc_voto$c38_w03,habla1 = elsoc_voto$habla1, informa1 = elsoc_voto$informa1, pid = elsoc_voto$pid, 
                                                  deber = elsoc_voto$deber, eficacia = elsoc_voto$eficacia, voto_w01 = "1"))

mean(datos.simulados_02$prob, na.rm=TRUE)-mean(datos.simulados_01$prob, na.rm=TRUE)


#Modelos multinomiales
x0 <- multinom(perfil_voto2b ~ hombre + edad + edad2 + educ, data = elsoc_panel_02 )
x1 <- multinom(perfil_voto2b ~ hombre + edad + edad2 + educ + interes + pospol1 + c01_w03 + c38_w03 + habla1 + informa1 + pid + deber + eficacia, data = elsoc_panel_02 )
x2 <- multinom(perfil_voto2b ~ hombre + edad + edad2 + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1 + informa1 + pid + deber + eficacia, data = elsoc_panel_02 )

screenreg(list(x0, x1,x2), single.row = FALSE)


partic_01 <- as.data.frame(svytable(formula = ~edad_tr+perfil_voto2b,  design = m_orig_des2a, Ntotal = 2066))
partic_02 <- as.data.frame(svytable(formula = ~educ+perfil_voto2b,     design = m_orig_des2a, Ntotal = 2064))
partic_03 <- as.data.frame(svytable(formula = ~deb_prom+perfil_voto2b, design = m_orig_des2a, Ntotal = 2045))
partic_04 <- as.data.frame(svytable(formula = ~int_prom+perfil_voto2b, design = m_orig_des2a, Ntotal = 2048))
partic_05 <- as.data.frame(svytable(formula = ~hombre+perfil_voto2b,   design = m_orig_des2a, Ntotal = 2066))

#Exportar objeto como .csv
write_csv(partic_01, path = "Voto_Edad.csv")
write_csv(partic_02, path = "Voto_Educ.csv")
write_csv(partic_03, path = "Voto_Deber.csv")
write_csv(partic_04, path = "Voto_Interes.csv")
write_csv(partic_05, path = "Voto_Hombre.csv")

#Remover objetos innecesarios
rm(partic_01,partic_02,partic_03,partic_04, partic_05)

#Pruebas Estadísticas
svychisq(formula = ~edad_tr+perfil_voto2b, design = m_orig_des1a)
svychisq(formula = ~edad_tr+perfil_voto2b, design = m_orig_des1a, "adjWald")

svychisq(formula = ~educ+perfil_voto2b, design = m_orig_des1a)
svychisq(formula = ~educ+perfil_voto2b, design = m_orig_des1a, "adjWald")

svychisq(formula = ~int_prom+perfil_voto2b, design = m_orig_des1a)
svychisq(formula = ~int_prom+perfil_voto2b, design = m_orig_des1a, "adjWald")


###############################################################################################################################################################################
#VOTO: CANDIDATOS PRESIDENCIALES

elsoc_panel_02 %>% dplyr::filter(is.na(c36_w02rec)==FALSE & is.na(elsoc_panel_02$c39_w03rec)==FALSE) -> elsoc_vote_choice
design_vote_choice <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_vote_choice)


addmargins(table(elsoc_panel_02$c36_w02rec))  #2002
addmargins(table(elsoc_panel_02$c39_w03rec))  #1957

addmargins(table(elsoc_vote_choice$c36_w02rec))  #1875
addmargins(table(elsoc_vote_choice$c39_w03rec))  #1875

round(prop.table(svytable(formula = ~c11_w03rec2,  design = design_vote_choice, Ntotal = 1875))*100,1)

vote_choice1 <- as.data.frame(svytable(formula = ~c36_w02rec,  design = design_vote_choice, Ntotal = 1875))
vote_choice2 <- as.data.frame(svytable(formula = ~c39_w03rec,  design = design_vote_choice, Ntotal = 1875))
vote_choice3 <- as.data.frame(svytable(formula = ~c36_w02rec+c39_w03rec,  design = design_vote_choice, Ntotal = 1875))

#Exportar objeto como .csv
write_csv(vote_choice1, path = "Intencion_Voto.csv")
write_csv(vote_choice2, path = "Voto_Retrospectivo.csv")
write_csv(vote_choice3, path = "Voto_Pre_vs_Post.csv")

###############################################################################################################################################################################
#MOVIMIENTOS SOCIALES Y ACCIONES COLECTIVAS

#Definir Base de datos de Movimientos Sociales
elsoc_panel_02 %>% dplyr::filter(is.na(c20_w01rec)==FALSE & is.na(c20_w02rec)==FALSE & is.na(c20_w03rec)==FALSE) -> elsoc_soc_mov

#Variables de Control para Clasificación
elsoc_soc_mov$c_12   <- ifelse(elsoc_soc_mov$c20_w01rec == elsoc_soc_mov$c20_w02rec, 0, 1)
elsoc_soc_mov$c_13   <- ifelse(elsoc_soc_mov$c20_w01rec == elsoc_soc_mov$c20_w03rec, 0, 1)
elsoc_soc_mov$c_23   <- ifelse(elsoc_soc_mov$c20_w02rec == elsoc_soc_mov$c20_w03rec, 0, 1)
elsoc_soc_mov$suma1 <- elsoc_soc_mov$c_12 + elsoc_soc_mov$c_13 + elsoc_soc_mov$c_23
elsoc_soc_mov$n1    <- ifelse(elsoc_soc_mov$c20_w01rec == 11, 1, 0) 
elsoc_soc_mov$n2    <- ifelse(elsoc_soc_mov$c20_w02rec == 11, 1, 0) 
elsoc_soc_mov$n3    <- ifelse(elsoc_soc_mov$c20_w03rec == 11, 1, 0) 
elsoc_soc_mov$suma2 <- elsoc_soc_mov$n1 + elsoc_soc_mov$n2 + elsoc_soc_mov$n3 
elsoc_soc_mov$control <- paste(elsoc_soc_mov$suma1,elsoc_soc_mov$suma2,sep = "-")
elsoc_soc_mov$clasificacion <- car::recode(elsoc_soc_mov$control,"'0-0'='Tres veces el mismo';'0-3'='Ninguno tres veces';
                                           '2-0'='Repite uno y Otro';'2-1'='Repite uno y Ninguno';'2-2'='Ninguno dos veces';
                                           '3-0'='Tres distintos';'3-1'='Dos distintos y Ninguno'")

elsoc_soc_mov$clasificacion_alt <- car::recode(elsoc_soc_mov$control,"'0-0'='Tres veces el mismo';'0-3'='3 Ninguno';
                                               '2-0'='Repite uno y Otro';'2-1'='Repite uno y Ninguno';'2-2'='2 Ninguno';
                                               '3-0'='Tres distintos';'3-1'='Dos distintos y Ninguno'")


#Declaración de diseño muestral
design_soc_mov <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_soc_mov)

#Frecuencias por Olas
mov_soc <- data.frame(movimiento = c("Estudiantil","Laboral","Ambiental","Indígena","Diversidad","Anti-aborto","Anti-delincuencia",
                                     "Feminista","Pensiones","Otro","Niguno"))

mov_soc$Freq1_w01 <- table(elsoc_soc_mov$c20_w01rec)
mov_soc$Freq2_w01 <- svytable(formula = ~c20_w01rec, design = design_soc_mov, Ntotal = 2005)
mov_soc$Freq1_w02 <- table(elsoc_soc_mov$c20_w02rec)
mov_soc$Freq2_w02 <- svytable(formula = ~c20_w02rec, design = design_soc_mov, Ntotal = 2005)
mov_soc$Freq1_w03 <- table(elsoc_soc_mov$c20_w03rec)
mov_soc$Freq2_w03 <- svytable(formula = ~c20_w03rec, design = design_soc_mov, Ntotal = 2005)

write_csv(mov_soc, path = "Mov_Soc.csv")

#Persistencia de Movimiento Social
mov_mas <- data.frame(svytable(formula = ~mov_mas, design = design_soc_mov, Ntotal = 2005))

write_csv(mov_mas, path = "Mov_Soc_Persistencia.csv")

#Persistencia de Valor
mov_val <- data.frame(svytable(formula = ~mov_val, design = design_soc_mov, Ntotal = 2005))

write_csv(mov_val, path = "Mov_Val_Persistencia.csv")

#Participación en Movimientos
partic_soc_mov <- data.frame(nivel = c("Nunca","Poco","Frecuentemente"))
partic_soc_mov$Freq_w01 <- svytable(formula =~c22_w01rec, design = design_soc_mov, Ntotal = 1304)
partic_soc_mov$Freq_w02 <- svytable(formula =~c22_w02rec, design = design_soc_mov, Ntotal = 1165)
partic_soc_mov$Freq_w03 <- svytable(formula =~c22_w03rec, design = design_soc_mov, Ntotal = 1230)

write_csv(partic_soc_mov, path = "Mov_Soc_Partic_Frec.csv")


addmargins(table(elsoc_soc_mov$suma2, elsoc_soc_mov$c08_02_w03rec))
svytable(formula = ~suma2+c08_02_w01rec, design = design_soc_mov, Ntotal = 2004)
svytable(formula = ~suma2+c08_02_w02rec, design = design_soc_mov, Ntotal = 2005)
svytable(formula = ~suma2+c08_02_w03rec, design = design_soc_mov, Ntotal = 2004)

svytable(formula = ~suma2+c08_02_prom, design = design_soc_mov, Ntotal = 2003)
svytable(formula = ~suma2+c08_03_prom, design = design_soc_mov, Ntotal = 2003)

round(prop.table(svytable(formula = ~suma2+c08_02_prom, design = design_soc_mov, Ntotal = 2003),1)*100,1)
round(prop.table(svytable(formula = ~suma2+c08_03_prom, design = design_soc_mov, Ntotal = 2003),1)*100,1)

svychisq(formula = ~suma2+c08_02_prom, design = design_soc_mov)
svychisq(formula = ~suma2+c08_02_w01rec, design = design_soc_mov)
svychisq(formula = ~suma2+c08_02_w02rec, design = design_soc_mov)
svychisq(formula = ~suma2+c08_02_w03rec, design = design_soc_mov)

svychisq(formula = ~suma2+c08_03_prom, design = design_soc_mov)
svychisq(formula = ~suma2+c08_03_w01rec, design = design_soc_mov)
svychisq(formula = ~suma2+c08_03_w02rec, design = design_soc_mov)
svychisq(formula = ~suma2+c08_03_w03rec, design = design_soc_mov)

addmargins(table(elsoc_soc_mov$clasificacion, elsoc_soc_mov$c22_prom))
addmargins(table(elsoc_soc_mov$clasificacion, elsoc_soc_mov$c22_w01))
addmargins(table(elsoc_soc_mov$clasificacion, elsoc_soc_mov$c22_w02))
addmargins(table(elsoc_soc_mov$clasificacion, elsoc_soc_mov$c22_w03))

addmargins(table(elsoc_soc_mov$clasificacion, elsoc_soc_mov$c08_02_prom))
addmargins(table(elsoc_soc_mov$suma2, elsoc_soc_mov$c08_02_prom))
round(prop.table(svytable(formula = ~suma2+c08_02_prom, design = design_soc_mov, Ntotal = 2003),1)*100,1)
svychisq(formula = ~suma2+c08_02_prom, design = design_soc_mov)

addmargins(table(elsoc_soc_mov$c20_w01rec2, elsoc_soc_mov$c22_w01rec))
svychisq(formula = ~c20_w01rec2+c22_w01rec, design = design_soc_mov)
svychisq(formula = ~c20_w02rec2+c22_w02rec, design = design_soc_mov)
svychisq(formula = ~c20_w03rec2+c22_w03rec, design = design_soc_mov)

round(prop.table(svytable(formula = ~clasificacion+c08_02_prom, design = design_soc_mov, Ntotal = 2003),1)*100,1)
svychisq(formula = ~clasificacion+c08_02_prom, design = design_soc_mov)
svychisq(formula = ~clasificacion+c08_02_w01rec, design = design_soc_mov)
svychisq(formula = ~clasificacion+c08_02_w02rec, design = design_soc_mov)
svychisq(formula = ~clasificacion+c08_02_w03rec, design = design_soc_mov)

###############################################################################################################################################################################
#IDENTIFICACIÓN PARTIDARIA

#Base de datos filtradas
elsoc_panel_02 %>% dplyr::filter(is.na(c16_w01rec1)==FALSE & is.na(c16_w02rec1)==FALSE & is.na(c16_w03rec1)==FALSE) -> elsoc_partidos
elsoc_panel_02 %>% dplyr::filter(is.na(c17_w01rec1)==FALSE & is.na(c17_w02rec1)==FALSE & is.na(c17_w03rec1)==FALSE) -> elsoc_coaliciones


elsoc_partidos$pers_part_01a <- car::recode(elsoc_partidos$pers_part_01,"'No-No-No'='Nunca';c('No-No-Sí','No-Sí-No','Sí-No-No')='Dos veces No';
                                            c('No-Sí-Sí','Sí-No-Sí','Sí-Sí-No')='Dos veces Sí';'Sí-Sí-Sí'='Siempre'")
elsoc_partidos$pers_part_01a <- factor(elsoc_partidos$pers_part_01a, levels = c("Nunca","Dos veces No","Dos veces Sí","Siempre"))

elsoc_coaliciones$pers_coal_01a <- car::recode(elsoc_coaliciones$pers_coal_01,"'No-No-No'='Nunca';c('No-No-Sí','No-Sí-No','Sí-No-No')='Dos veces No';
                                            c('No-Sí-Sí','Sí-No-Sí','Sí-Sí-No')='Dos veces Sí';'Sí-Sí-Sí'='Siempre'")
elsoc_coaliciones$pers_coal_01a <- factor(elsoc_coaliciones$pers_coal_01a, levels = c("Nunca","Dos veces No","Dos veces Sí","Siempre"))

elsoc_coaliciones$nm_w01 <- car::recode(elsoc_coaliciones$c17_w01rec == 3 ,)
                                               

#Declaración de diseño muestral
design_partidos    <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_partidos)
design_coaliciones <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_coaliciones)


#Frecuencia Global de Identificación
identificacion <- data.frame(identifica = c("Sí","No"))

#Party ID sin DMC
identificacion$Freq1_w01 <-table(elsoc_partidos$c16_w01rec1)
identificacion$Freq1_w02 <-table(elsoc_partidos$c16_w02rec1)
identificacion$Freq1_w03 <-table(elsoc_partidos$c16_w03rec1)

#Party ID con DMC
identificacion$Freq2_w01 <- svytable(formula = ~c16_w01rec1, design = design_partidos, Ntotal = 1984)
identificacion$Freq2_w02 <- svytable(formula = ~c16_w02rec1, design = design_partidos, Ntotal = 1984)
identificacion$Freq2_w03 <- svytable(formula = ~c16_w03rec1, design = design_partidos, Ntotal = 1984)

#Coal ID sin DMC
identificacion$Freq3_w01 <-table(elsoc_coaliciones$c17_w01rec1)
identificacion$Freq3_w02 <-table(elsoc_coaliciones$c17_w02rec1)
identificacion$Freq3_w03 <-table(elsoc_coaliciones$c17_w03rec1)

#Coal ID con DMC
identificacion$Freq4_w01 <- svytable(formula = ~c17_w01rec1, design = design_coaliciones, Ntotal = 1890)
identificacion$Freq4_w02 <- svytable(formula = ~c17_w02rec1, design = design_coaliciones, Ntotal = 1890)
identificacion$Freq4_w03 <- svytable(formula = ~c17_w03rec1, design = design_coaliciones, Ntotal = 1890)


#Exportar objeto como .csv
identificacion
write_csv(x = identificacion, path = "Identificacion_Partidos.csv")
rm(identificacion)

#Frecuencia de Persistencia en respuestas
persistencia <- data.frame(table(elsoc_partidos$pers_part_01))
names(persistencia) <- c("patrones","Freq1")

persistencia$Freq2 <- data.frame(svytable(formula = ~pers_part_01, design = design_partidos, Ntotal = 1984))$Freq
persistencia$Freq3 <- data.frame(table(elsoc_coaliciones$pers_coal_01))$Freq
persistencia$Freq4 <- data.frame(svytable(formula = ~pers_coal_01, design = design_coaliciones, Ntotal = 1890))$Freq


#Exportar objeto como .csv
persistencia
write_csv(x = persistencia, path = "Identificacion_Partidos_Persistencia.csv")
rm(persistencia)

#ARREGLAR EDAD
svytable(formula = ~pers_part_01+edad_tr, design = design_partidos, Ntotal = 1984)
round(prop.table(svytable(formula = ~pers_part_01+edad_tr, design = design_partidos, Ntotal = 1984),1)*100,1)

addmargins(round(prop.table(svytable(formula = ~pers_part_01+edad_tr, design = design_partidos, Ntotal = 1984),1)*100,1))
addmargins(round(prop.table(svytable(formula = ~pers_part_01+educ, design = design_partidos, Ntotal = 1984),1)*100,1))


addmargins(table(elsoc_partidos$pers_part_01a,elsoc_partidos$educ))
svytable(formula = ~pers_part_01+educ, design = design_partidos, Ntotal = 1982)
svytable(formula = ~pers_part_01a+educ, design = design_partidos, Ntotal = 1982)

addmargins(table(elsoc_coaliciones$pers_coal_01a,elsoc_coaliciones$educ))
svytable(formula = ~pers_coal_01+educ, design = design_coaliciones, Ntotal = 1888)
svytable(formula = ~pers_coal_01a+educ, design = design_coaliciones, Ntotal = 1888)

svychisq(formula = ~pers_part_01a+educ, design = design_partidos)
svychisq(formula = ~pers_part_01a+educ, design = design_partidos, "adjWald")
svychisq(formula = ~pers_coal_01a+educ, design = design_coaliciones)
svychisq(formula = ~pers_coal_01a+educ, design = design_coaliciones, "adjWald")


addmargins(table(elsoc_partidos$pers_part_01a,elsoc_partidos$interes))
svytable(formula = ~pers_part_01+interes, design = design_partidos, Ntotal = 1983)
svytable(formula = ~pers_part_01a+interes, design = design_partidos, Ntotal = 1983)

addmargins(table(elsoc_coaliciones$pers_coal_01a,elsoc_coaliciones$interes))
svytable(formula = ~pers_coal_01+interes, design = design_coaliciones, Ntotal = 1888)
svytable(formula = ~pers_coal_01a+interes, design = design_coaliciones, Ntotal = 1888)


addmargins(table(elsoc_partidos$pers_part_01a,elsoc_partidos$perfil_voto2b))
svytable(formula = ~pers_part_01+perfil_voto2b, design = design_partidos, Ntotal = 1956)
svytable(formula = ~pers_part_01a+perfil_voto2b, design = design_partidos, Ntotal = 1956)

svychisq(~pers_part_01+perfil_voto2b, design = design_partidos)
svychisq(~pers_part_01a+perfil_voto2b, design = design_partidos)

addmargins(table(elsoc_coaliciones$pers_coal_01a,elsoc_coaliciones$perfil_voto2b))
svytable(formula = ~pers_coal_01+perfil_voto2b, design = design_coaliciones, Ntotal = 1865)
svytable(formula = ~pers_coal_01a+perfil_voto2b, design = design_coaliciones, Ntotal = 1865)

svychisq(~pers_coal_01+perfil_voto2b, design = design_coaliciones)
svychisq(~pers_coal_01a+perfil_voto2b, design = design_coaliciones)


elsoc_partidos$prueba1  <- ifelse(elsoc_partidos$c16_w01rec==elsoc_partidos$c16_w02rec,1,0)
elsoc_partidos$prueba2  <- ifelse(elsoc_partidos$c16_w01rec==elsoc_partidos$c16_w03rec,1,0)
elsoc_partidos$prueba3  <- ifelse(elsoc_partidos$c16_w02rec==elsoc_partidos$c16_w03rec,1,0)
elsoc_partidos$control1 <- elsoc_partidos$prueba1 + elsoc_partidos$prueba2 + elsoc_partidos$prueba3
elsoc_partidos$ning1    <- ifelse(elsoc_partidos$c16_w01rec == 15, 1,0)
elsoc_partidos$ning2    <- ifelse(elsoc_partidos$c16_w02rec == 15, 1,0)
elsoc_partidos$ning3    <- ifelse(elsoc_partidos$c16_w03rec == 15, 1,0)
elsoc_partidos$control2 <- elsoc_partidos$ning1 + elsoc_partidos$ning2 + elsoc_partidos$ning3
table(elsoc_partidos$control1 , elsoc_partidos$control2)

svytable(~c16_w01rec, design = design_partidos, Ntotal = 1984)
svytable(~c16_w02rec, design = design_partidos, Ntotal = 1984)
svytable(~c16_w03rec, design = design_partidos, Ntotal = 1984)

svytable(~control1+control2, design = design_partidos, Ntotal = 1984)
addmargins(svytable(~control1+control2, design = design_partidos, Ntotal = 1984))
298.12839/1984*100
120.62371/298.12839*100
134.54582/298.12839*100
42.95886/298.12839*100

elsoc_coaliciones$prueba1a  <- ifelse(elsoc_coaliciones$c17_w01rec==elsoc_coaliciones$c17_w02rec,1,0)
elsoc_coaliciones$prueba2a  <- ifelse(elsoc_coaliciones$c17_w01rec==elsoc_coaliciones$c17_w03rec,1,0)
elsoc_coaliciones$prueba3a  <- ifelse(elsoc_coaliciones$c17_w02rec==elsoc_coaliciones$c17_w03rec,1,0)
elsoc_coaliciones$control1a <- elsoc_coaliciones$prueba1a + elsoc_coaliciones$prueba2a + elsoc_coaliciones$prueba3a
elsoc_coaliciones$ning1a    <- ifelse(elsoc_coaliciones$c17_w01rec == 5, 1,0)
elsoc_coaliciones$ning2a    <- ifelse(elsoc_coaliciones$c17_w02rec == 5, 1,0)
elsoc_coaliciones$ning3a    <- ifelse(elsoc_coaliciones$c17_w03rec == 5, 1,0)
elsoc_coaliciones$control2a <- elsoc_coaliciones$ning1a + elsoc_coaliciones$ning2a + elsoc_coaliciones$ning3a

table(elsoc_coaliciones$control1a , elsoc_coaliciones$control2a)
svytable(~control1a+control2a, design = design_coaliciones, Ntotal = 1890)
addmargins(svytable(~control1a+control2a, design = design_coaliciones, Ntotal = 1890))
114.64838/193.77095*100

###############################################################################################################################################################################
#Modelos de Participación Electoral en 2017

library(nnet)
m0 <- multinom(pers_part_01 ~ 1 + hombre + edad + I(edad^2) + educ + interes + pospol1 + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_partidos)
m1 <- multinom(pers_part_01 ~ 1 + hombre + edad + I(edad^2) + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_partidos)

m2 <- multinom(pers_part_01a ~ 1 + hombre + edad + I(edad^2) + educ + interes + pospol1 + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_partidos)
m3 <- multinom(pers_part_01a ~ 1 + hombre + edad + I(edad^2) + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_partidos)
m4 <- multinom(pers_part_01a ~ 1 + hombre + edad_tr + educ + interes + pospol1 + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_partidos)
m5 <- multinom(pers_part_01a ~ 1 + hombre + edad_tr + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_partidos)

n0 <- multinom(pers_coal_01 ~ 1 + hombre + edad + I(edad^2) + educ + interes + pospol1 + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones)
n1 <- multinom(pers_coal_01 ~ 1 + hombre + edad + I(edad^2) + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones)

n2 <- multinom(pers_coal_01a ~ 1 + hombre + edad + I(edad^2) + educ + interes + pospol1 + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones)
n3 <- multinom(pers_coal_01a ~ 1 + hombre + edad + I(edad^2) + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones)
n4 <- multinom(pers_coal_01a ~ 1 + hombre + edad_tr + educ + interes + pospol1 + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones)
n5 <- multinom(pers_coal_01a ~ 1 + hombre + edad_tr + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones)

#Resultados
screenreg(list(m0,m1))
screenreg(list(m2,m3,m4,m5))
screenreg(list(n0,n1))
screenreg(list(n2,n3,n4,n5))


svytable(formula = ~c17_w01rec, design = design_coaliciones, Ntotal = 1890)
svytable(formula = ~c17_w02rec, design = design_coaliciones, Ntotal = 1890)
svytable(formula = ~c17_w03rec, design = design_coaliciones, Ntotal = 1890)


elsoc_coaliciones$control_fa <- paste(elsoc_coaliciones$c17_w02rec,elsoc_coaliciones$c17_w03rec,sep="-")
elsoc_coaliciones$fa <- car::recode(elsoc_coaliciones$control_fa,"c('1-1','1-2','1-5','2-1','2-2','2-5','4-5','5-1','5-2','5-4')='Menciona otra coalicion';
                                    '5-5'='No menciona ninguna';c('1-3','2-3','3-1','3-2','3-3','3-5','5-3')='Menciona al FA'")
elsoc_coaliciones$fa <- factor(elsoc_coaliciones$fa, levels = c("No menciona ninguna","Menciona otra coalicion","Menciona al FA"))
elsoc_coaliciones$fa1 <- car::recode(elsoc_coaliciones$control_fa,"c('1-1','1-2','1-5','2-1','2-2','2-5','4-5','5-1','5-2','5-4','5-5')='No';
                                    c('1-3','2-3','3-1','3-2','3-3','3-5','5-3')='Sí'")
elsoc_coaliciones$fa1 <- factor(elsoc_coaliciones$fa1, levels = c("No","Sí"))


elsoc_coaliciones$coal_w01 <- car::recode(elsoc_coaliciones$c17_w01rec,"c(1,4)='CHV+Otro';2='NM';5='Ninguno'")
elsoc_coaliciones$coal_w01 <- factor(elsoc_coaliciones$coal_w01, levels = c("CHV+Otro","NM","Ninguno"))

elsoc_coaliciones$ning1_w01 <- car::recode(elsoc_coaliciones$c17_w01rec,"c(1,2,4)='No';2='NM';5='Sí'")
elsoc_coaliciones$ning1_w01 <- factor(elsoc_coaliciones$ning1_w01, levels = c("No","Sí"))

elsoc_coaliciones$pospol1a <- ifelse(elsoc_coaliciones$pospol1=="Izquierda","Izquierda","Otro")
elsoc_coaliciones$pospol1a <- factor(elsoc_coaliciones$pospol1a, levels = c("Otro","Izquierda"))

elsoc_coaliciones$pospol2a <- ifelse(elsoc_coaliciones$pospol2=="Izquierda","Izquierda","Otro")
elsoc_coaliciones$pospol2a <- factor(elsoc_coaliciones$pospol2a, levels = c("Otro","Izquierda"))

design_coaliciones <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_coaliciones)

svytable(formula = ~control_fa, design = design_coaliciones, Ntotal = 1890)
86.302277/1890*100
svytable(formula = ~fa, design = design_coaliciones, Ntotal = 1890)
svytable(formula = ~fa+c17_w01rec, design = design_coaliciones, Ntotal = 1890)

addmargins(table(elsoc_coaliciones$fa,elsoc_coaliciones$educ))
round(prop.table(svytable(formula = ~educ+fa, design = design_coaliciones, Ntotal = 1888),1)*100,1)
svychisq(formula = ~educ+fa, design = design_coaliciones)
svytable(formula = ~educ+fa, design = design_coaliciones, Ntotal = 1888)

elsoc_coaliciones %>% dplyr::select(fa,hombre,edad,edad2,educ,interes,pospol1,pospol1a,pospol2,pospol2a,c01_w03,c38_w03,habla1,ning1_w01) %>% na.omit() -> elsoc_coaliciones1

m0 <- multinom(fa ~ 1 +                      hombre + edad + edad2 + educ + interes + pospol1  + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones1)
m2 <- multinom(fa ~ 1 + ning1_w01 +          hombre + edad + edad2 + educ + interes + pospol1a + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones1)
m4 <- multinom(fa ~ 1 + ning1_w01*pospol1a + hombre + edad + edad2 + educ + interes + pospol1a + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones1)

m1 <- multinom(fa ~ 1 +                      hombre + edad + edad2 + educ + interes + pospol2 + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones1)
m3 <- multinom(fa ~ 1 + ning1_w01 +          hombre + edad + edad2 + educ + interes + pospol2a + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones1)
m5 <- multinom(fa ~ 1 + ning1_w01*pospol2a + hombre + edad + edad2 + educ + interes + pospol2a + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones1)



datos.simulados_01 <- with(elsoc_coaliciones1, data.frame(ning1_w01="Sí",hombre=0,edad=48.69297,edad2=2371.005,educ="1",interes="0",habla1=1,pospol1a=c("Otro","Izquierda")))
predict(m2, newdata = datos.simulados_01, "probs")
predict(m4, newdata = datos.simulados_01, "probs")

datos.simulados_01a <- with(elsoc_coaliciones1, data.frame(ning1_w01="No",hombre=0,edad=48.69297,edad2=2371.005,educ="1",interes="0",habla1=1,pospol1a=c("Otro","Izquierda")))
predict(m2, newdata = datos.simulados_01a, "probs")
predict(m4, newdata = datos.simulados_01a, "probs")


datos.simulados_02 <- with(elsoc_coaliciones1, data.frame(ning1_w01="Sí",hombre=0,edad=48.69297,edad2=2371.005,educ="1",interes="0",habla1=1,pospol2a=c("Otro","Izquierda")))
predict(m3, newdata = datos.simulados_02, "probs")
predict(m5, newdata = datos.simulados_02, "probs")

datos.simulados_02a <- with(elsoc_coaliciones1, data.frame(ning1_w01="No",hombre=0,edad=48.69297,edad2=2371.005,educ="1",interes="0",habla1=1,pospol2a=c("Otro","Izquierda")))
predict(m3, newdata = datos.simulados_02a, "probs")
predict(m5, newdata = datos.simulados_02a, "probs")


screenreg(list(m0,m2,m4))
screenreg(list(m1,m3,m5))

n0 <- glm(fa1 ~ 1 + hombre + edad + edad2 + educ + interes + pospol1 + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones)
n1 <- glm(fa1 ~ 1 + hombre + edad + edad2 + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones)
n2 <- glm(fa1 ~ 1 + ning1_w01 + hombre + edad + edad2 + educ + interes + pospol1 + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones)
n3 <- glm(fa1 ~ 1 + ning1_w01 + hombre + edad + edad2 + educ + interes + pospol2 + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones)
n4 <- glm(fa1 ~ 1 + ning1_w01*pospol1a + hombre + edad + edad2 + educ + interes + pospol1a + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones)
n5 <- glm(fa1 ~ 1 + ning1_w01*pospol2a + hombre + edad + edad2 + educ + interes + pospol2a + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones)
n6 <- glm(fa1 ~ 1 + coal_w01*pospol1a + hombre + edad + edad2 + educ + interes + pospol1a + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones)
n7 <- glm(fa1 ~ 1 + coal_w01*pospol2a + hombre + edad + edad2 + educ + interes + pospol2a + c01_w03 + c38_w03 + habla1, family = binomial(link = "logit"), data = elsoc_coaliciones)

screenreg(list(n0,n2,n4,n6,n1,n3,n5,n7))

sjPlot::plot_model(m3,sort.est = TRUE)


###############################################################################################################################################################################
#ANALISIS LONGITUDINAL

#Importar bases de datos LONG
elsoc_long <- read_stata(path = "ELSOC_Long_2016_2018_Temporal.dta")

#Eliminar Valores Perdidos en datos LONG (en diseño datos se eliminaron casos)
elsoc_long[elsoc_long == -888] <- NA
elsoc_long[elsoc_long == -999] <- NA

#Ordenar por ID encuesta
elsoc_long %>% dplyr::arrange(desc(idencuesta)) -> elsoc_long


elsoc_long$c16_otro_rec <- car::recode(elsoc_long$c16_otro, "c('autonomista','izquierda libertaria','mir','MIR','movimiento ambientalista',
                                              'partido ecologista verde')=14;'independiente'=15;c('ecologista','Frente Amplio','igualdad',
                                              'izquierda','izquierda libertaria','mir','partido ecologista verde','partido igualdad',
                                              'por mi patria')=14;c('apoyo independiente','aun no hay ninguno que me represente','independiente',
                                              'los que no estan ligados a ningun patrpido')=15;c('Accion Republicana','Al De Beatriz Sanchez',
                                              'Allendista','Biblia','Frente Amplio','Igualdad','Izquierda Libertaria','La Democracia',
                                              'Libertarios','Mir','Partido Ecologista','Partido Ecologista Verde','Partido Igualdad')=14;
                                              'El De Bachelett'=12;'Evopoli'=5;'Independiente'=15;else=0")

elsoc_long$c16_rec <- ifelse(elsoc_long$c16_otro_rec == 0 , elsoc_long$c16, ifelse(elsoc_long$c16_otro_rec == 5, 5,
                                                                                   ifelse(elsoc_long$c16_otro_rec ==12, 12, ifelse(elsoc_long$c16_otro_rec == 14, 14, 15))))


#Se identifica o no con un partido
elsoc_long$c16_rec1 <- car::recode(elsoc_long$c16_rec, "1:14='Sí';15='No'")
elsoc_long$c16_rec1 <- factor(elsoc_long$c16_rec1, levels = c("Sí","No"))

elsoc_long$c16_rec2 <- car::recode(elsoc_long$c16_rec1,"c(1,2)='PC+PH';c(3,4)='PRO+RD';c(5,7,9)='EVO+AMP+PRI';
                                      c(6,8,12,13)='PPD+PDC+PS+PR';c(10,11)='RN+UDI';14='Otro';15=NA")  


#Imputación de respuestas Otra Coalicion
elsoc_long$c17_otro_rec <- car::recode(elsoc_long$c17_otro, "c('amplitud','el partido progresista','humanista','partido humanista')=4;
                                      'Independiente'=5;c('centro izquierda no nueva mayoria','socialismo tradicional')=4;'centro derecha'=1;
                                       c('ecologista','humanista ecologica','sfeir')=3;'van solos Carolina GOIC'=2;c('Evolucion Politica',
                                       'Las De La Derecha')=1;c('La De Bachelet','La De Guiller','Ppd')=2;c('Humanista','La De Beatriz Sanchez')=3;
                                              c('Mir','Nacional Socialismo','No Recuerda')=4;else=0")
elsoc_long$c17_rec <- ifelse(elsoc_long$c17_otro_rec == 0, elsoc_long$c17,ifelse(elsoc_long$c17_otro_rec == 1, 1, 
                                                                                 ifelse(elsoc_long$c17_otro_rec == 2, 2, ifelse(elsoc_long$c17_otro_rec == 3, 3, 
                                                                                                                                ifelse(elsoc_long$c17_otro_rec == 4, 4,5)))))

#Se identifica o no con una coalicion
elsoc_long$c17_rec1 <- car::recode(elsoc_long$c17_rec,"1:4='Sí';5='No'")
elsoc_long$c17_rec1 <- factor(elsoc_long$c17_rec1, levels = c("Sí","No"))

chisq.test(table(elsoc_long$wave, elsoc_long$c16_rec1))
chisq.test(table(elsoc_long$wave, elsoc_long$c17_rec1))

library(lme4)
library(texreg)
m0 <- glmer(c16_rec1 ~ 1 + wave + (1 + wave|idencuesta), data = elsoc_long, family = binomial)
n0 <- glmer(c17_rec1 ~ 1 + wave + (1 + wave|idencuesta), data = elsoc_long, family = binomial)


anova(m0)
anova(n0)
psycho::analyze(m0)

screenreg(list(m0, n0))

summary(aov(as.numeric(c16_rec1) ~ wave + Error(idencuesta/wave), data=elsoc_long))
summary(aov(as.numeric(c17_rec1) ~ wave + Error(idencuesta/wave), data=elsoc_long))

###############################################################################################################################################################################
#MOVIMIENTOS SOCIALES: OFFLINE ONLINE

elsoc_panel_02 %>% dplyr::filter(is.na(c08_04_w02)==FALSE & is.na(c08_04_w01)==FALSE & is.na(c08_01_w02)==FALSE) -> elsoc_mov_soc1 
elsoc_panel_02 %>% dplyr::filter(is.na(c08_04_w03)==FALSE & is.na(c08_04_w02)==FALSE & is.na(c08_01_w03)==FALSE) -> elsoc_mov_soc2

ms1 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador01_w01, data = elsoc_mov_soc1)
ms2 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador01_w01, data = elsoc_mov_soc2)

m0 <- lm(c08_04_w02 ~ 1 + c08_04_w01 + c08_01_w02,   data = elsoc_panel_02)
m1 <- svyglm(c08_04_w02 ~ 1 + c08_04_w01 + c08_01_w02,design =  ms1, family = gaussian)

n0 <- lm(c08_04_w03 ~ 1 + c08_04_w02 + c08_01_w03,   data = elsoc_panel_02)
n1 <- svyglm(c08_04_w03 ~ 1 + c08_04_w02 + c08_01_w03,design =  ms2, family = gaussian)

texreg::screenreg(list(m0,m1,n0,n1))

elsoc_mov_soc1$pred_m0 <- predict(m0)
elsoc_mov_soc1$pred_m1 <- predict(m1)

elsoc_mov_soc2$pred_n0 <- predict(n0)
elsoc_mov_soc2$pred_n1 <- predict(n1)

elsoc_mov_soc1 %>% dplyr::group_by(c08_02_w01) %>% summarise(pred = mean(pred_m1))
elsoc_mov_soc2 %>% dplyr::group_by(c08_02_w02) %>% summarise(pred = mean(pred_n1))


###############################################################################################################################################################################
#MOVIMIENTOS SOCIALES: NORMAS

elsoc_panel_02 %>% dplyr::filter(is.na(c22_w02)==FALSE & is.na(c22_w01)==FALSE & is.na(c23_w02)==FALSE) -> elsoc_mov_soc3
elsoc_panel_02 %>% dplyr::filter(is.na(c22_w03)==FALSE & is.na(c22_w02)==FALSE & is.na(c23_w03)==FALSE) -> elsoc_mov_soc4

ms3 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador01_w01, data = elsoc_mov_soc3)
ms4 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador01_w01, data = elsoc_mov_soc4)

p0 <- lm(c22_w02 ~ 1 + c22_w01 + c23_w02,     data = elsoc_mov_soc3)
p1 <- svyglm(c22_w02 ~ 1 + c22_w01 + c23_w02, design =  ms3, family = gaussian)

q0 <- lm(c22_w03 ~ 1 + c22_w02 + c23_w03,     data = elsoc_mov_soc4)
q1 <- svyglm(c22_w03 ~ 1 + c22_w02 + c23_w03, design =  ms4, family = gaussian)


texreg::screenreg(list(p0,p1,q0,q1))

elsoc_mov_soc3$pred_p0 <- predict(p0)
elsoc_mov_soc3$pred_p1 <- predict(p1)

elsoc_mov_soc4$pred_q0 <- predict(q0)
elsoc_mov_soc4$pred_q1 <- predict(q1)

elsoc_mov_soc3 %>% dplyr::group_by(c22_w01) %>% summarise(pred = mean(pred_p1))
elsoc_mov_soc4 %>% dplyr::group_by(c22_w02) %>% summarise(pred = mean(pred_q1))


###############################################################################################################################################################################
#MOVIMIENTOS SOCIALES: NORMAS

elsoc_panel_02 %>% dplyr::filter(is.na(c22_w02)==FALSE & is.na(c22_w01)==FALSE & is.na(c24_w02)==FALSE) -> elsoc_mov_soc5
elsoc_panel_02 %>% dplyr::filter(is.na(c22_w03)==FALSE & is.na(c22_w02)==FALSE & is.na(c24_w03)==FALSE) -> elsoc_mov_soc6

ms5 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador01_w01, data = elsoc_mov_soc5)
ms6 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador01_w01, data = elsoc_mov_soc6)

r0 <- lm(c22_w02 ~     1 + c22_w01 + c24_w02,     data = elsoc_mov_soc5)
r1 <- svyglm(c22_w02 ~ 1 + c22_w01 + c24_w02, design =  ms5, family = gaussian)

s0 <- lm(c22_w03 ~     1 + c22_w02 + c24_w03,     data = elsoc_mov_soc6)
s1 <- svyglm(c22_w03 ~ 1 + c22_w02 + c24_w03, design =  ms6, family = gaussian)


texreg::screenreg(list(r0,r1,s0,s1))

elsoc_mov_soc5$pred_r0 <- predict(r0)
elsoc_mov_soc5$pred_r1 <- predict(r1)

elsoc_mov_soc6$pred_s0 <- predict(s0)
elsoc_mov_soc6$pred_s1 <- predict(s1)

elsoc_mov_soc5 %>% dplyr::group_by(c22_w01) %>% summarise(pred = mean(pred_r1))
elsoc_mov_soc6 %>% dplyr::group_by(c22_w02) %>% summarise(pred = mean(pred_s1))

