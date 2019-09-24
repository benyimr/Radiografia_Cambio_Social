rm(list=ls())

#Cargar paquetes
library(ggplot2)
library(ggthemes)
library(lme4)
library(tidyverse)
library(sjmisc)
library(sjPlot)
library(survey)
library(srvyr)

#Importar bases de datos WIDE
load("/Users/benjaminmunozrojas/Dropbox/6_COES/3_Encuestas_COES/2_ELSOC/5_Bases_de_Datos/11_Combinacion_de_Olas_ELSOC/0C_Bases_de_Datos_Resultantes_2016_2018/ELSOC_Wide_2016_2018_v1.10_R.RData")

#Importar bases de datos LONG
#elsoc_long <- read_stata(path = "ELSOC_Long_2016_2018_Temporal.dta")


###############################################################################################################################################################################
#RECODIFICACION DE VARIABLES

#PHQ-9: Puntaje de Variables Individuales
elsoc_wide_2016_2018$s11_01_w01rec <- car::recode(elsoc_wide_2016_2018$s11_01_w01,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_01_w02rec <- car::recode(elsoc_wide_2016_2018$s11_01_w02,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_01_w03rec <- car::recode(elsoc_wide_2016_2018$s11_01_w03,"1=0;2=1;3=2;c(4,5)=3")

elsoc_wide_2016_2018$s11_02_w01rec <- car::recode(elsoc_wide_2016_2018$s11_02_w01,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_02_w02rec <- car::recode(elsoc_wide_2016_2018$s11_02_w02,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_02_w03rec <- car::recode(elsoc_wide_2016_2018$s11_02_w03,"1=0;2=1;3=2;c(4,5)=3")

elsoc_wide_2016_2018$s11_03_w01rec <- car::recode(elsoc_wide_2016_2018$s11_03_w01,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_03_w02rec <- car::recode(elsoc_wide_2016_2018$s11_03_w02,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_03_w03rec <- car::recode(elsoc_wide_2016_2018$s11_03_w03,"1=0;2=1;3=2;c(4,5)=3")

elsoc_wide_2016_2018$s11_04_w01rec <- car::recode(elsoc_wide_2016_2018$s11_04_w01,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_04_w02rec <- car::recode(elsoc_wide_2016_2018$s11_04_w02,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_04_w03rec <- car::recode(elsoc_wide_2016_2018$s11_04_w03,"1=0;2=1;3=2;c(4,5)=3")

elsoc_wide_2016_2018$s11_05_w01rec <- car::recode(elsoc_wide_2016_2018$s11_05_w01,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_05_w02rec <- car::recode(elsoc_wide_2016_2018$s11_05_w02,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_05_w03rec <- car::recode(elsoc_wide_2016_2018$s11_05_w03,"1=0;2=1;3=2;c(4,5)=3")

elsoc_wide_2016_2018$s11_06_w01rec <- car::recode(elsoc_wide_2016_2018$s11_06_w01,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_06_w02rec <- car::recode(elsoc_wide_2016_2018$s11_06_w02,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_06_w03rec <- car::recode(elsoc_wide_2016_2018$s11_06_w03,"1=0;2=1;3=2;c(4,5)=3")

elsoc_wide_2016_2018$s11_07_w01rec <- car::recode(elsoc_wide_2016_2018$s11_07_w01,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_07_w02rec <- car::recode(elsoc_wide_2016_2018$s11_07_w02,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_07_w03rec <- car::recode(elsoc_wide_2016_2018$s11_07_w03,"1=0;2=1;3=2;c(4,5)=3")

elsoc_wide_2016_2018$s11_08_w01rec <- car::recode(elsoc_wide_2016_2018$s11_08_w01,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_08_w02rec <- car::recode(elsoc_wide_2016_2018$s11_08_w02,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_08_w03rec <- car::recode(elsoc_wide_2016_2018$s11_08_w03,"1=0;2=1;3=2;c(4,5)=3")

elsoc_wide_2016_2018$s11_09_w01rec <- car::recode(elsoc_wide_2016_2018$s11_09_w01,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_09_w02rec <- car::recode(elsoc_wide_2016_2018$s11_09_w02,"1=0;2=1;3=2;c(4,5)=3")
elsoc_wide_2016_2018$s11_09_w03rec <- car::recode(elsoc_wide_2016_2018$s11_09_w03,"1=0;2=1;3=2;c(4,5)=3")


#PHQ-9: Índice Aditivo de Puntajes de Síntomas Depresivos
elsoc_wide_2016_2018$suma_dep_w01 <- elsoc_wide_2016_2018$s11_01_w01rec + elsoc_wide_2016_2018$s11_02_w01rec + elsoc_wide_2016_2018$s11_03_w01rec + 
  elsoc_wide_2016_2018$s11_04_w01rec + elsoc_wide_2016_2018$s11_05_w01rec + elsoc_wide_2016_2018$s11_06_w01rec + 
  elsoc_wide_2016_2018$s11_07_w01rec + elsoc_wide_2016_2018$s11_08_w01rec + elsoc_wide_2016_2018$s11_09_w01rec

elsoc_wide_2016_2018$suma_dep_w02 <- elsoc_wide_2016_2018$s11_01_w02rec + elsoc_wide_2016_2018$s11_02_w02rec + elsoc_wide_2016_2018$s11_03_w02rec + 
  elsoc_wide_2016_2018$s11_04_w02rec + elsoc_wide_2016_2018$s11_05_w02rec + elsoc_wide_2016_2018$s11_06_w02rec + 
  elsoc_wide_2016_2018$s11_07_w02rec + elsoc_wide_2016_2018$s11_08_w02rec + elsoc_wide_2016_2018$s11_09_w02rec

elsoc_wide_2016_2018$suma_dep_w03 <- elsoc_wide_2016_2018$s11_01_w03rec + elsoc_wide_2016_2018$s11_02_w03rec + elsoc_wide_2016_2018$s11_03_w03rec + 
  elsoc_wide_2016_2018$s11_04_w03rec + elsoc_wide_2016_2018$s11_05_w03rec + elsoc_wide_2016_2018$s11_06_w03rec + 
  elsoc_wide_2016_2018$s11_07_w03rec + elsoc_wide_2016_2018$s11_08_w03rec + elsoc_wide_2016_2018$s11_09_w03rec


#PHQ-9: Recodificación de Índice en Categorías (según puntajes de corte)
elsoc_wide_2016_2018$depr_w01 <- car::recode(elsoc_wide_2016_2018$suma_dep_w01,"c(1,2,3,4)='Sin sintomas o Minima';c(5,6,7,8,9)='Depresion Media';
                                       c(10,11,12,13,14)='Depresion Moderada';c(15,16,17,18,19)='Depresion Moderada Severa a Severa';
                                       c(20,21,22,23,24,25,26,27)='Depresion Moderada Severa a Severa'")
elsoc_wide_2016_2018$depr_w01 <- factor(elsoc_wide_2016_2018$depr_w01,c("Sin sintomas o Minima","Depresion Media","Depresion Moderada",
                                                                        "Depresion Moderada Severa a Severa"))  

elsoc_wide_2016_2018$depr_w02 <- car::recode(elsoc_wide_2016_2018$suma_dep_w02,"c(1,2,3,4)='Sin sintomas o Minima';c(5,6,7,8,9)='Depresion Media';
                                       c(10,11,12,13,14)='Depresion Moderada';c(15,16,17,18,19)='Depresion Moderada Severa a Severa';
                                       c(20,21,22,23,24,25,26,27)='Depresion Moderada Severa a Severa'")
elsoc_wide_2016_2018$depr_w02 <- factor(elsoc_wide_2016_2018$depr_w02,c("Sin sintomas o Minima","Depresion Media","Depresion Moderada",
                                                                        "Depresion Moderada Severa a Severa"))  

elsoc_wide_2016_2018$depr_w03 <- car::recode(elsoc_wide_2016_2018$suma_dep_w03,"c(1,2,3,4)='Sin sintomas o Minima';c(5,6,7,8,9)='Depresion Media';
                                       c(10,11,12,13,14)='Depresion Moderada';c(15,16,17,18,19)='Depresion Moderada Severa a Severa';
                                       c(20,21,22,23,24,25,26,27)='Depresion Moderada Severa a Severa'")
elsoc_wide_2016_2018$depr_w03 <- factor(elsoc_wide_2016_2018$depr_w03,c("Sin sintomas o Minima","Depresion Media","Depresion Moderada",
                                                                        "Depresion Moderada Severa a Severa"))  

#Persistencia de Depresión
elsoc_wide_2016_2018$pers_depr <- paste(elsoc_wide_2016_2018$depr_w01,elsoc_wide_2016_2018$depr_w02,elsoc_wide_2016_2018$depr_w03,sep="-")

#Nivel Educativo
elsoc_wide_2016_2018$educ_rec1_w01 <- car::recode(elsoc_wide_2016_2018$m01_w01, "c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")
elsoc_wide_2016_2018$educ_rec1_w02 <- car::recode(elsoc_wide_2016_2018$m01_w02, "c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")
elsoc_wide_2016_2018$educ_rec1_w03 <- car::recode(elsoc_wide_2016_2018$m01_w03, "c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")

elsoc_wide_2016_2018$educ_rec2_w01 <- car::recode(elsoc_wide_2016_2018$m01_w01, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'")
elsoc_wide_2016_2018$educ_rec2_w02 <- car::recode(elsoc_wide_2016_2018$m01_w02, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'")
elsoc_wide_2016_2018$educ_rec2_w03 <- car::recode(elsoc_wide_2016_2018$m01_w03, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'")
elsoc_wide_2016_2018$educ_rec2_w01 <- factor(elsoc_wide_2016_2018$educ_rec2_w01, levels = c("Basica","Media","Tecnica","Universitaria"))
elsoc_wide_2016_2018$educ_rec2_w02 <- factor(elsoc_wide_2016_2018$educ_rec2_w02, levels = c("Basica","Media","Tecnica","Universitaria"))
elsoc_wide_2016_2018$educ_rec2_w03 <- factor(elsoc_wide_2016_2018$educ_rec2_w03, levels = c("Basica","Media","Tecnica","Universitaria"))


#Imputar punto medio de rangos de ingreso
elsoc_wide_2016_2018$m30_w01_temp <- car::recode(elsoc_wide_2016_2018$m30_w01,"1=110000;2=251000;3=305000;4=355000;5=400000;
                                           6=445000;7=490000;8=535000;9=585000;10=640000;11=700000;12=765000;
                                           13=845000;14=935000;15=1040000;16=1180000;17=1375000;18=1670000;
                                           19=2275000;20=2700000;NA=NA")
elsoc_wide_2016_2018$m30_w02_temp <- car::recode(elsoc_wide_2016_2018$m30_w02,"1=110000;2=251000;3=305000;4=355000;5=400000;
                                           6=445000;7=490000;8=535000;9=585000;10=640000;11=700000;12=765000;
                                           13=845000;14=935000;15=1040000;16=1180000;17=1375000;18=1670000;
                                           19=2275000;20=2700000;NA=NA")
elsoc_wide_2016_2018$m30_w03_temp <- car::recode(elsoc_wide_2016_2018$m30_w03,"1=110000;2=251000;3=305000;4=355000;5=400000;
                                           6=445000;7=490000;8=535000;9=585000;10=640000;11=700000;12=765000;
                                           13=845000;14=935000;15=1040000;16=1180000;17=1375000;18=1670000;
                                           19=2275000;20=2700000;NA=NA")

#Combinar ingreso numérico y de tramos en una variable
elsoc_wide_2016_2018$m29_w01_temp <- ifelse(is.na(elsoc_wide_2016_2018$m29_w01)==TRUE,elsoc_wide_2016_2018$m30_w01_temp,elsoc_wide_2016_2018$m29_w01)
elsoc_wide_2016_2018$m29_w02_temp <- ifelse(is.na(elsoc_wide_2016_2018$m29_w02)==TRUE,elsoc_wide_2016_2018$m30_w02_temp,elsoc_wide_2016_2018$m29_w02)
elsoc_wide_2016_2018$m29_w03_temp <- ifelse(is.na(elsoc_wide_2016_2018$m29_w03)==TRUE,elsoc_wide_2016_2018$m30_w03_temp,elsoc_wide_2016_2018$m29_w03)

#Inflactar a precios de Diciembre de 2018
elsoc_wide_2016_2018$m29_w01rec1 <- elsoc_wide_2016_2018$m29_w01_temp*(119.45/113.88)
elsoc_wide_2016_2018$m29_w02rec1 <- elsoc_wide_2016_2018$m29_w02_temp*(119.45/116.46)
elsoc_wide_2016_2018$m29_w03rec1 <- elsoc_wide_2016_2018$m29_w03_temp

#Generar Quintiles del Ingresos del Hogar (con o sin Per Capita) 
elsoc_wide_2016_2018$nhogar_w01 <-elsoc_wide_2016_2018$nhogar1_w01
elsoc_wide_2016_2018$nhogar_w02 <-elsoc_wide_2016_2018$m46_nhogar_w02
elsoc_wide_2016_2018$nhogar_w03 <-elsoc_wide_2016_2018$m54_w03

elsoc_wide_2016_2018$ing_pc_w01 <- elsoc_wide_2016_2018$m29_w01rec1/elsoc_wide_2016_2018$nhogar_w01
elsoc_wide_2016_2018$ing_pc_w02 <- elsoc_wide_2016_2018$m29_w02rec1/elsoc_wide_2016_2018$nhogar_w02
elsoc_wide_2016_2018$ing_pc_w03 <- elsoc_wide_2016_2018$m29_w03rec1/elsoc_wide_2016_2018$nhogar_w03

#Endeudamiento
elsoc_wide_2016_2018$deuda_w01 <- car::recode(elsoc_wide_2016_2018$m43_w01,"c(1,2)=0;c(3,4)=1")
elsoc_wide_2016_2018$deuda_w03 <- car::recode(elsoc_wide_2016_2018$m43_w03,"c(1,2)=0;c(3,4)=1")

#Horas de Trabajo
elsoc_wide_2016_2018$hr_tr_w01 <- car::recode(elsoc_wide_2016_2018$m12_w01,"0:24=0;25:45=1;46:200=2")
elsoc_wide_2016_2018$hr_tr_w03 <- car::recode(elsoc_wide_2016_2018$m12_w03,"0:24=0;25:45=1;46:200=2")

#Horas de Cuidado
elsoc_wide_2016_2018$hr_cd_w01 <- car::recode(elsoc_wide_2016_2018$m17_w01,"0:24=0;25:45=1;46:200=2")
elsoc_wide_2016_2018$hr_cd_w03 <- car::recode(elsoc_wide_2016_2018$m17_w03,"0:24=0;25:45=1;46:200=2")


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

#Bases de Datos por Olas
elsoc_panel_01 %>% dplyr::filter(muestra == 1) %>% dplyr::filter(is.na(segmento_w01)==FALSE) %>% dplyr::filter(tipo_atricion != 5) -> elsoc_original_w01
elsoc_panel_01 %>% dplyr::filter(muestra == 1) %>% dplyr::filter(is.na(segmento_w02)==FALSE) %>% dplyr::filter(tipo_atricion != 5) -> elsoc_original_w02
elsoc_panel_01 %>% dplyr::filter(muestra == 1) %>% dplyr::filter(is.na(segmento_w03)==FALSE) %>% dplyr::filter(tipo_atricion != 5) -> elsoc_original_w03


#Eliminar Valores Perdidos en datos LONG (en diseño datos se eliminaron casos)
#elsoc_long[elsoc_long == -888] <- NA
#elsoc_long[elsoc_long == -999] <- NA

#Ordenar por ID encuesta
#elsoc_long %>% dplyr::arrange(desc(idencuesta)) -> elsoc_long


###############################################################################################################################################################################
#DISEÑO MUESTRAL COMPLEJO

elsoc_panel_02 %>% dplyr::filter(is.na(elsoc_panel_02$depr_w01)==FALSE & is.na(elsoc_panel_02$depr_w02)==FALSE & is.na(elsoc_panel_02$depr_w03)==FALSE) -> elsoc_depr

#Ingreso según Quintiles
elsoc_depr$quintil_01_w01 <- sjmisc::split_var(x = elsoc_depr$m29_w01_temp, n = 5)
elsoc_depr$quintil_01_w02 <- sjmisc::split_var(x = elsoc_depr$m29_w02_temp, n = 5)
elsoc_depr$quintil_01_w03 <- sjmisc::split_var(x = elsoc_depr$m29_w03_temp, n = 5)

elsoc_depr$quintil_02_w01 <- sjmisc::split_var(x = elsoc_depr$ing_pc_w01, n = 5)
elsoc_depr$quintil_02_w02 <- sjmisc::split_var(x = elsoc_depr$ing_pc_w02, n = 5)
elsoc_depr$quintil_02_w03 <- sjmisc::split_var(x = elsoc_depr$ing_pc_w03, n = 5)

#Puntaje Promedio 3 olas
elsoc_depr$score_prom <- (elsoc_depr$suma_dep_w01 + elsoc_depr$suma_dep_w02 + elsoc_depr$suma_dep_w03)/3


#Declaración de Diseño Muestral Complejo
m_orig_des1 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador01_w01, data = elsoc_original_w01)
m_orig_des2 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_original_w01)

m_orig_des3 <- svydesign(ids = ~segmento_w02, strata = ~estrato_w02, weights = ~ponderador01_w02, data = elsoc_original_w02)
m_orig_des4 <- svydesign(ids = ~segmento_w02, strata = ~estrato_w02, weights = ~ponderador02_w02, data = elsoc_original_w02)

m_orig_des5 <- svydesign(ids = ~segmento_w03, strata = ~estrato_w03, weights = ~ponderador01_w03, data = elsoc_original_w03)
m_orig_des6 <- svydesign(ids = ~segmento_w03, strata = ~estrato_w03, weights = ~ponderador02_w03, data = elsoc_original_w03)

m_orig_des7 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador01_w01, data = elsoc_depr)
m_orig_des8 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_depr)


###############################################################################################################################################################################
#DEPRESION (SINTOMATOLOGIA)


depresion <- data.frame(categoria = c("Sin sintomas o Minima","Depresion Media","Depresion Moderada","Depresion Moderada Severa a Severa","Total"))

Nw01d   <- sum(table(elsoc_original_w01$depr_w01))
Nw02d   <- sum(table(elsoc_original_w02$depr_w02))
Nw03d   <- sum(table(elsoc_original_w03$depr_w03))
NwFulld <- sum(table(elsoc_depr$depr_w01))

#Frecuencias originales
depresion$frec_w01 <- c(table(elsoc_original_w01$depr_w01), Nw01d)
depresion$frec_w02 <- c(table(elsoc_original_w02$depr_w02), Nw02d)
depresion$frec_w03 <- c(table(elsoc_original_w03$depr_w03), Nw03d)

#Diseño muestral complejo: Primera Medición y Ponderador Base
depresion$frec_w01a <- c(svytable(formula = ~depr_w01, design = m_orig_des1, Ntotal = Nw01d), Nw01d)
depresion$frec_w02a <- c(svytable(formula = ~depr_w02, design = m_orig_des1, Ntotal = Nw02d), Nw02d)
depresion$frec_w03a <- c(svytable(formula = ~depr_w03, design = m_orig_des1, Ntotal = Nw03d), Nw03d)

#Diseño muestral complejo: Primera Medición y Ponderador Corregido
depresion$frec_w01b <- c(svytable(formula = ~depr_w01, design = m_orig_des2, Ntotal = Nw01d), Nw01d)
depresion$frec_w02b <- c(svytable(formula = ~depr_w02, design = m_orig_des2, Ntotal = Nw02d), Nw02d)
depresion$frec_w03b <- c(svytable(formula = ~depr_w03, design = m_orig_des2, Ntotal = Nw03d), Nw03d)

#Diseño muestral complejo: Medición Correspondiente y Ponderador Base
depresion$frec_w01c <- c(svytable(formula = ~depr_w01, design = m_orig_des1, Ntotal = Nw01d), Nw01d)
depresion$frec_w02c <- c(svytable(formula = ~depr_w02, design = m_orig_des3, Ntotal = Nw02d), Nw02d)
depresion$frec_w03c <- c(svytable(formula = ~depr_w03, design = m_orig_des5, Ntotal = Nw03d), Nw03d)

#Diseño muestral complejo: Medición Correspondiente y Ponderador Corregido
depresion$frec_w01d <- c(svytable(formula = ~depr_w01, design = m_orig_des2, Ntotal = Nw01d), Nw01d)
depresion$frec_w02d <- c(svytable(formula = ~depr_w02, design = m_orig_des4, Ntotal = Nw02d), Nw02d)
depresion$frec_w03d <- c(svytable(formula = ~depr_w03, design = m_orig_des6, Ntotal = Nw03d), Nw03d)

#Frecuencias originales con Submuestra de Siempre Presentes
depresion$frec_full_w01 <- c(table(elsoc_depr$depr_w01), NwFulld)
depresion$frec_full_w02 <- c(table(elsoc_depr$depr_w02), NwFulld)
depresion$frec_full_w03 <- c(table(elsoc_depr$depr_w03), NwFulld)

#Diseño muestral complejo: Primera Medición y Ponderador Base
depresion$frec_full_w01a <- c(svytable(formula = ~depr_w01, design = m_orig_des7, Ntotal = NwFulld), NwFulld)
depresion$frec_full_w02a <- c(svytable(formula = ~depr_w02, design = m_orig_des7, Ntotal = NwFulld), NwFulld)
depresion$frec_full_w03a <- c(svytable(formula = ~depr_w03, design = m_orig_des7, Ntotal = NwFulld), NwFulld)

#Diseño muestral complejo: Primera Medición y Ponderador Corregido
depresion$frec_full_w01b <- c(svytable(formula = ~depr_w01, design = m_orig_des8, Ntotal = NwFulld), NwFulld)
depresion$frec_full_w02b <- c(svytable(formula = ~depr_w02, design = m_orig_des8, Ntotal = NwFulld), NwFulld)
depresion$frec_full_w03b <- c(svytable(formula = ~depr_w03, design = m_orig_des8, Ntotal = NwFulld), NwFulld)

#Exportar objeto como .csv
depresion
write_csv(x = depresion, path = "Depresion_Global.csv")

#Remover objetos innecesarios
rm(Nw01d, Nw02d, Nw03d, NwFulld, m_orig_des1, m_orig_des2, m_orig_des3, m_orig_des4, m_orig_des5, m_orig_des6,
   elsoc_original_w01, elsoc_original_w02, elsoc_original_w03, elsoc_panel_01)


#Generar Patrones de Persistencia de Sintomatologia
persistencia_01 <- data.frame(table(elsoc_depr$pers_depr)) %>% dplyr::rename(pers_depr = Var1)%>% dplyr::arrange(-Freq)
persistencia_02 <- data.frame(svytable(formula = ~pers_depr, design = m_orig_des7, Ntotal = 1370)) %>% dplyr::arrange(-Freq)
persistencia_03 <- data.frame(svytable(formula = ~pers_depr, design = m_orig_des8, Ntotal = 1370)) %>% dplyr::arrange(-Freq)

persistencia_temp <- dplyr::full_join(x = persistencia_01, y = persistencia_02, by = "pers_depr")
persistencia_full <- dplyr::full_join(x = persistencia_temp, y = persistencia_03, by = "pers_depr")
names(persistencia_full) <- c("Patron","Frec_Base","Frec_Pond1","Frec_Pond2")

#Exportar objeto como .csv
write_csv(x = persistencia_full, path = "Depresion_Persistencia.csv")

#Remover objetos innecesarios
rm(persistencia_01,persistencia_02,persistencia_03,persistencia_temp)


###############################################################################################################################################################################
#SEGUN SEXO

s1 <- data.frame(svytable(formula = ~m0_sexo_w01+depr_w01, design = m_orig_des8, Ntotal = 1370))
s2 <- data.frame(svytable(formula = ~m0_sexo_w02+depr_w02, design = m_orig_des8, Ntotal = 1370))
s3 <- data.frame(svytable(formula = ~m0_sexo_w03+depr_w03, design = m_orig_des8, Ntotal = 1370))

names(s1) <- c("sexo","depresion","frecuencia")
names(s2) <- c("sexo","depresion","frecuencia")
names(s3) <- c("sexo","depresion","frecuencia")

s_temp <- dplyr::full_join(x = s1, y = s2, by = c("sexo","depresion"))
s_full <- dplyr::full_join(x = s_temp, y = s3, by = c("sexo","depresion"))

#Exportar objeto como .csv
s_full
write_csv(x = s_full, path = "Depresion_Sexo_01.csv")

#Remover objetos innecesarios
rm(s1,s2,s3,s_temp)


###############################################################################################################################################################################
#SEGUN INGRESO DEL HOGAR

ing1a <- data.frame(svytable(formula = ~quintil_02_w01+depr_w01, design = m_orig_des8, Ntotal = 1120))
ing2a <- data.frame(svytable(formula = ~quintil_02_w02+depr_w02, design = m_orig_des8, Ntotal = 1219))
ing3a <- data.frame(svytable(formula = ~quintil_02_w03+depr_w03, design = m_orig_des8, Ntotal = 1188))

names(ing1a) <- c("quintil","depresion","frecuencia")
names(ing2a) <- c("quintil","depresion","frecuencia")
names(ing3a) <- c("quintil","depresion","frecuencia")

ing_temp <- dplyr::full_join(x = ing1a, y = ing2a, by = c("quintil","depresion"))
ing_full <- dplyr::full_join(x = ing_temp, y = ing3a, by = c("quintil","depresion"))

#Exportar objeto como .csv
ing_full
write_csv(x = ing_full, path = "Depresion_Ingreso_01.csv")

#Remover objetos innecesarios
rm(ing1a,ing2a,ing3a,ing_temp)


###############################################################################################################################################################################
#SEGUN EDUCACIÓN

educ1a <- data.frame(svytable(formula = ~educ_rec2_w01+depr_w01, design = m_orig_des8, Ntotal = 1369))
educ2a <- data.frame(svytable(formula = ~educ_rec2_w02+depr_w02, design = m_orig_des8, Ntotal = 1370))
educ3a <- data.frame(svytable(formula = ~educ_rec2_w03+depr_w03, design = m_orig_des8, Ntotal = 1369))

names(educ1a) <- c("educacion","depresion","frecuencia")
names(educ2a) <- c("educacion","depresion","frecuencia")
names(educ3a) <- c("educacion","depresion","frecuencia")

educ_temp <- dplyr::full_join(x = educ1a,    y = educ2a, by = c("educacion","depresion"))
educ_full <- dplyr::full_join(x = educ_temp, y = educ3a, by = c("educacion","depresion"))

#Exportar objeto como .csv
educ_full
write_csv(x = educ_full, path = "Depresion_Educacion_01.csv")

#Remover objetos innecesarios
rm(educ1a,educ2a,educ3a,educ_temp)

###############################################################################################################################################################################
#PERSISTENCIA DE BRECHAS

brechas <- data.frame(dplyr::bind_rows(svyby(~score_prom, ~m0_sexo_w01, m_orig_des8,svymean),svyby(~score_prom, ~educ_rec2_w01, m_orig_des8,svymean)))

#Exportar objeto como .csv
brechas
write_csv(x = brechas, path = "Depresion_Brechas.csv")

###############################################################################################################################################################################
#ENDEUDAMIENTO

elsoc_panel_02$ds_w01 <- paste0(elsoc_panel_02$m0_sexo_w01,elsoc_panel_02$deuda_w01)
elsoc_panel_02$ds_w03 <- paste0(elsoc_panel_02$m0_sexo_w03,elsoc_panel_02$deuda_w03)

elsoc_panel_02$trs_w01 <- paste0(elsoc_panel_02$m0_sexo_w01,elsoc_panel_02$hr_tr_w01)
elsoc_panel_02$trs_w03 <- paste0(elsoc_panel_02$m0_sexo_w03,elsoc_panel_02$hr_tr_w03)


elsoc_panel_02 %>% dplyr::filter(is.na(deuda_w01)==F & is.na(deuda_w03)==F & is.na(depr_w01)==FALSE & is.na(depr_w03)==FALSE) -> elsoc_deuda
deudas_design <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador01_w01, data = elsoc_deuda)

svytable(formula = ~ds_w01+depr_w01, design = deudas_design, Ntotal = 734)
svytable(formula = ~ds_w03+depr_w03, design = deudas_design, Ntotal = 734)


addmargins(table(elsoc_panel_02$hr_tr_w01, elsoc_panel_02$depr_w01))
addmargins(table(elsoc_panel_02$hr_tr_w03, elsoc_panel_02$depr_w03))

elsoc_panel_02 %>% dplyr::filter(is.na(hr_tr_w01)==F & is.na(hr_tr_w03)==F & is.na(depr_w01)==FALSE & is.na(depr_w03)==FALSE) -> elsoc_tr
jornada_design <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador01_w01, data = elsoc_tr)

svytable(formula = ~trs_w01+depr_w01, design = jornada_design, Ntotal = 787)
svytable(formula = ~trs_w03+depr_w03, design = jornada_design, Ntotal = 787)



###############################################################################################################################################################################
#ANÁLISIS LONGITUDINAL EXPLORATORIO
rm(list=ls())

#Importar bases de datos LONG
elsoc_long <- read_stata(path = "ELSOC_Long_2016_2018_Temporal.dta")

#Renombrar datos Wide
elsoc_panel_01 <- elsoc_wide_2016_2018

#Eliminar Valores Perdidos en datos WIDE
elsoc_panel_01[elsoc_panel_01 == -888] <- NA
elsoc_panel_01[elsoc_panel_01 == -999] <- NA

#Ordenar por ID encuesta
elsoc_panel_01 %>% dplyr::arrange(desc(idencuesta)) -> elsoc_panel_01

#Filtrar datos con 3 mediciones
elsoc_panel_01 %>% dplyr::filter(tipo_atricion == 1 & tipo_caso != 2) -> elsoc_panel_02

#Creación de variables
elsoc_panel_02$horas_t    <- (elsoc_panel_02$m12_w01 + elsoc_panel_02$m12_w03)/2
elsoc_panel_02$horas_c    <- (elsoc_panel_02$m17_w01 + elsoc_panel_02$m17_w03)/2
elsoc_panel_02$endeud01   <-  elsoc_panel_02$m43_w01
elsoc_panel_02$endeud03   <-  elsoc_panel_02$m43_w03
elsoc_panel_02$endeudprom <- round((elsoc_panel_02$m43_w01 + elsoc_panel_02$m43_w03)/2)

elsoc_panel_02 %>% dplyr::select(idencuesta, horas_t, horas_c, endeud01, endeud03, endeudprom) -> temporal

#Eliminar Valores Perdidos en datos LONG (en diseño datos se eliminaron casos)
elsoc_long[elsoc_long == -888] <- NA
elsoc_long[elsoc_long == -999] <- NA


#PHQ-9: Puntaje de Variables Individuales
elsoc_long$s11_01rec <- car::recode(elsoc_long$s11_01,"1=0;2=1;3=2;c(4,5)=3")
elsoc_long$s11_02rec <- car::recode(elsoc_long$s11_02,"1=0;2=1;3=2;c(4,5)=3")
elsoc_long$s11_03rec <- car::recode(elsoc_long$s11_03,"1=0;2=1;3=2;c(4,5)=3")
elsoc_long$s11_04rec <- car::recode(elsoc_long$s11_04,"1=0;2=1;3=2;c(4,5)=3")
elsoc_long$s11_05rec <- car::recode(elsoc_long$s11_05,"1=0;2=1;3=2;c(4,5)=3")
elsoc_long$s11_06rec <- car::recode(elsoc_long$s11_06,"1=0;2=1;3=2;c(4,5)=3")
elsoc_long$s11_07rec <- car::recode(elsoc_long$s11_07,"1=0;2=1;3=2;c(4,5)=3")
elsoc_long$s11_08rec <- car::recode(elsoc_long$s11_08,"1=0;2=1;3=2;c(4,5)=3")
elsoc_long$s11_09rec <- car::recode(elsoc_long$s11_09,"1=0;2=1;3=2;c(4,5)=3")

#PHQ-9: Índice Aditivo de Puntajes de Síntomas Depresivos
elsoc_long$suma_dep <- elsoc_long$s11_01rec + elsoc_long$s11_02rec + elsoc_long$s11_03rec + 
  elsoc_long$s11_04rec + elsoc_long$s11_05rec + elsoc_long$s11_06rec + 
  elsoc_long$s11_07rec + elsoc_long$s11_08rec + elsoc_long$s11_09rec

#PHQ-9: Recodificación de Índice en Categorías (según puntajes de corte)
elsoc_long$depr <- car::recode(elsoc_long$suma_dep,"c(1,2,3,4)='Sin sintomas o Minima';c(5,6,7,8,9)='Depresion Media';
                                       c(10,11,12,13,14)='Depresion Moderada';c(15,16,17,18,19)='Depresion Moderada Severa a Severa';
                                       c(20,21,22,23,24,25,26,27)='Depresion Moderada Severa a Severa'")
elsoc_long$depr <- factor(elsoc_long$depr,c("Sin sintomas o Minima","Depresion Media","Depresion Moderada",
                                            "Depresion Moderada Severa a Severa"))  

elsoc_long$educ <-car::recode(elsoc_long$m01, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'")
elsoc_long$educ <- factor(elsoc_long$educ, levels = c("Basica","Media","Tecnica","Universitaria"))
elsoc_long$wave1 <- elsoc_long$wave - 1
elsoc_long$mujer <- car::recode(elsoc_long$m0_sexo,"1='Hombre';2='Mujer'")
elsoc_long$mujer <- factor(elsoc_long$mujer, levels = c("Hombre","Mujer"))
elsoc_long$insat <- car::recode(elsoc_long$s01, "c(4,5)=0;c(1,2,3)=1")  
elsoc_long$edad <- elsoc_long$m0_edad/10
elsoc_long$religioso <- car::recode(elsoc_long$m38, "1:6=1;7:9=0")

elsoc_long1 <- dplyr::left_join(x= elsoc_long, y = temporal, by = "idencuesta")
elsoc_long1$horas_t1 <- as.factor(car::recode(elsoc_long1$horas_t,"0:24=0;25:45=1;46:200=2;24.5=0;45.5=2"))
elsoc_long1$horas_c1 <- as.factor(car::recode(elsoc_long1$horas_c,"0:24=0;25:45=1;46:200=2"))


library(lme4)
library(texreg)
library(nlme)
library(car)

summary(aov(suma_dep ~ mujer + Error(Participant_ID/Emotion_Condition), data=df))

m0 <- lmer(suma_dep ~ 1 + (1|idencuesta), data = elsoc_long1)
m1 <- lmer(suma_dep ~ 1 + wave1 + (1 + wave1|idencuesta), data = elsoc_long1)
m2 <- lmer(suma_dep ~ 1 + mujer + edad + I(edad^2) + educ + wave1 + (1 + wave1|idencuesta), data = elsoc_long1)
m3 <- lmer(suma_dep ~ 1 + mujer + edad + I(edad^2) + educ + d01_01 + insat + wave1 + (1 + wave1|idencuesta), data = elsoc_long1)
m4 <- lmer(suma_dep ~ 1 + mujer + edad + I(edad^2) + educ + d01_01 + insat + horas_t1 + horas_c1 + horas_c1*mujer + wave1 + (1 + wave1|idencuesta), data = elsoc_long1)
m5 <- lmer(suma_dep ~ 1 + mujer + edad + I(edad^2) + educ + d01_01 + insat + horas_t1 + horas_c1 + wave1*mujer + wave1 + (1 + wave1|idencuesta), data = elsoc_long1)
m6 <- lmer(suma_dep ~ 1 + mujer + edad + I(edad^2) + educ + d01_01 + insat + horas_t1 + horas_c1 + as.factor(wave1)*mujer + as.factor(wave1) + (1 + wave1|idencuesta), data = elsoc_long1)
m7 <- lmer(suma_dep ~ 1 + mujer + edad + I(edad^2) + educ + d01_01 + insat + horas_t1 + horas_c1 + as.factor(wave1)*mujer + as.factor(wave1) + (1 + wave1|idencuesta), data = elsoc_long1)

screenreg(list(m0,m1,m2,m3,m4,m5,m6,m7))


elsoc_long1 %>% dplyr::select(idencuesta,suma_dep, mujer, edad, educ, d01_01, insat, horas_t1, horas_c1,wave1) %>% na.omit()-> elsoc_long2
model = lme(suma_dep ~ mujer + edad + I(edad^2) + educ + d01_01 + insat + horas_t1 + horas_c1 + wave1 +  mujer*wave1,
            random = ~1|idencuesta,
            correlation = corAR1(form = ~ wave1 | idencuesta),
            data=elsoc_long2,
            method="REML")

Anova(model)
