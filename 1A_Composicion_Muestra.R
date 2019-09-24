rm(list=ls())

#Cargar paquetes
library(ggplot2)
library(ggthemes)
library(lme4)
library(polycor)
library(psych)
library(tidyverse)
library(readxl)
library(sjlabelled)
library(sjmisc)
library(sjPlot)
library(survey)
library(xtable)

#Importar bases de datos
load("ELSOC_Wide_2016_2018_v1.10_R.RData")


###############################################################################################################################################################################
#PROCESAMIENTO DE BASE DE DATOS

elsoc_panel_01 <- elsoc_wide_2016_2018

elsoc_panel_01[elsoc_panel_01 == -888] <- NA
elsoc_panel_01[elsoc_panel_01 == -999] <- NA

#Ordenar por ID encuesta
elsoc_panel_01 %>% dplyr::arrange(desc(idencuesta)) -> elsoc_panel_01

#Recodificación de Edad en Tramos
elsoc_panel_01$edad_rec_w01 <- car::recode(elsoc_panel_01$m0_edad_w01, "18:29=1;30:49=2;50:64=3;65:100=4")
elsoc_panel_01$edad_rec_w02 <- car::recode(elsoc_panel_01$m0_edad_w02, "18:29=1;30:49=2;50:64=3;65:100=4")
elsoc_panel_01$edad_rec_w03 <- car::recode(elsoc_panel_01$m0_edad_w03, "18:29=1;30:49=2;50:64=3;65:100=4")

#Recodificación de Educación en Tramos
elsoc_panel_01$educ_rec_w01 <- car::recode(elsoc_panel_01$m01_w01, "c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")
elsoc_panel_01$educ_rec_w02 <- car::recode(elsoc_panel_01$m01_w02, "c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")
elsoc_panel_01$educ_rec_w03 <- car::recode(elsoc_panel_01$m01_w03, "c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")

#Creación de Variable Territorial
elsoc_panel_01$estrato_w01esp <- car::recode(elsoc_panel_01$estrato_w01, "1='Santiago';2='Valparaíso';3='Concepción';4='Ciudades Grandes';5='Ciudades Medianas';6='Ciudades Pequeñas'")
elsoc_panel_01$region_w01esp  <- car::recode(elsoc_panel_01$region_w01, "c(1,2,3,4,15)='Norte';c(5,6,7,13)='Centro';c(8,9,10,11,12,14)='Sur'")
elsoc_panel_01$terr_tempw01   <- paste(elsoc_panel_01$estrato_w01esp, elsoc_panel_01$region_w01esp, sep = " ")
elsoc_panel_01$territ_w01     <- car::recode(elsoc_panel_01$terr_tempw01, "'Concepción Sur'='Concepción';'NA NA'=NA;'Santiago Centro'='Santiago';'Valparaíso Centro'='Valparaíso'")
elsoc_panel_01$territ_w01     <- factor(elsoc_panel_01$territ_w01, levels = c("Santiago","Concepción","Valparaíso","Ciudades Grandes Norte","Ciudades Grandes Centro",
                                                                              "Ciudades Grandes Sur", "Ciudades Medianas Norte", "Ciudades Medianas Centro", 
                                                                              "Ciudades Medianas Sur", "Ciudades Pequeñas Norte", "Ciudades Pequeñas Centro",
                                                                              "Ciudades Pequeñas Sur"))

elsoc_panel_01$estrato_w02esp <- car::recode(elsoc_panel_01$estrato_w02, "1='Santiago';2='Valparaíso';3='Concepción';4='Ciudades Grandes';5='Ciudades Medianas';6='Ciudades Pequeñas'")
elsoc_panel_01$region_w02esp  <- car::recode(elsoc_panel_01$region_w02, "c(1,2,3,4,15)='Norte';c(5,6,7,13)='Centro';c(8,9,10,11,12,14)='Sur'")
elsoc_panel_01$terr_tempw02   <- paste(elsoc_panel_01$estrato_w02esp, elsoc_panel_01$region_w02esp, sep = " ")
elsoc_panel_01$territ_w02     <- car::recode(elsoc_panel_01$terr_tempw02, "'Concepción Sur'='Concepción';'NA NA'=NA;'Santiago Centro'='Santiago';'Valparaíso Centro'='Valparaíso'")
elsoc_panel_01$territ_w02     <- factor(elsoc_panel_01$territ_w02, levels = c("Santiago","Concepción","Valparaíso","Ciudades Grandes Norte","Ciudades Grandes Centro",
                                                                              "Ciudades Grandes Sur", "Ciudades Medianas Norte", "Ciudades Medianas Centro", 
                                                                              "Ciudades Medianas Sur", "Ciudades Pequeñas Norte", "Ciudades Pequeñas Centro",
                                                                              "Ciudades Pequeñas Sur"))

elsoc_panel_01$estrato_w03esp <- car::recode(elsoc_panel_01$estrato_w03, "1='Santiago';2='Valparaíso';3='Concepción';4='Ciudades Grandes';5='Ciudades Medianas';6='Ciudades Pequeñas'")
elsoc_panel_01$region_w03esp  <- car::recode(elsoc_panel_01$region_w03, "c(1,2,3,4,15)='Norte';c(5,6,7,13)='Centro';c(8,9,10,11,12,14)='Sur'")
elsoc_panel_01$terr_tempw03   <- paste(elsoc_panel_01$estrato_w03esp, elsoc_panel_01$region_w03esp, sep = " ")
elsoc_panel_01$territ_w03     <- car::recode(elsoc_panel_01$terr_tempw03, "'Concepción Sur'='Concepción';'NA NA'=NA;'Santiago Centro'='Santiago';'Valparaíso Centro'='Valparaíso'")
elsoc_panel_01$territ_w03     <- factor(elsoc_panel_01$territ_w03, levels = c("Santiago","Concepción","Valparaíso","Ciudades Grandes Norte","Ciudades Grandes Centro",
                                                                              "Ciudades Grandes Sur", "Ciudades Medianas Norte", "Ciudades Medianas Centro", 
                                                                              "Ciudades Medianas Sur", "Ciudades Pequeñas Norte", "Ciudades Pequeñas Centro",
                                                                              "Ciudades Pequeñas Sur"))

#Recodificación de Religión
elsoc_panel_01$relig_w01 <- car::recode(elsoc_panel_01$m38_w01, "1='Católico';c(2,3)='Evangélico';c(4,5,6)='Otro Credo';c(7,8,9)='No Creyente'")
elsoc_panel_01$relig_w01 <- factor(elsoc_panel_01$relig_w01,levels = c("Católico","Evangélico","Otro Credo","No Creyente"))
elsoc_panel_01$relig_w02 <- car::recode(elsoc_panel_01$m38_w02, "1='Católico';c(2,3)='Evangélico';c(4,5,6)='Otro Credo';c(7,8,9)='No Creyente'")
elsoc_panel_01$relig_w02 <- factor(elsoc_panel_01$relig_w02,levels = c("Católico","Evangélico","Otro Credo","No Creyente"))
elsoc_panel_01$relig_w03 <- car::recode(elsoc_panel_01$m38_w03, "1='Católico';c(2,3)='Evangélico';c(4,5,6)='Otro Credo';c(7,8,9)='No Creyente'")
elsoc_panel_01$relig_w03 <- factor(elsoc_panel_01$relig_w03,levels = c("Católico","Evangélico","Otro Credo","No Creyente"))


###############################################################################################################################################################################
#GENERACIÓN DE BASES DE DATOS POR MUESTRA Y DISEÑOS MUESTRALES COMPLEJOS

#Bases de Datos por Olas
elsoc_panel_01 %>% dplyr::filter(muestra == 1) %>% dplyr::filter(is.na(segmento_w01)==FALSE) %>% dplyr::filter(tipo_atricion != 5) -> elsoc_original_w01
elsoc_panel_01 %>% dplyr::filter(muestra == 1) %>% dplyr::filter(is.na(segmento_w02)==FALSE) %>% dplyr::filter(tipo_atricion != 5) -> elsoc_original_w02
elsoc_panel_01 %>% dplyr::filter(muestra == 1) %>% dplyr::filter(is.na(segmento_w03)==FALSE) %>% dplyr::filter(tipo_atricion != 5) -> elsoc_original_w03
elsoc_panel_01 %>% dplyr::filter(muestra == 2) -> elsoc_refresco


#Hay 4 casos en que varía segmento entre 2016 y 2017
#Hay 3 casos en que varía segmento entre 2016 y 2018
#No hay diferencias entre 2017 y 2018 en segmento
#No hay diferencias entre olas en estrato

m_orig_des1 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador01_w01, data = elsoc_original_w01)
m_orig_des2 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_original_w01)

m_orig_des3 <- svydesign(ids = ~segmento_w02, strata = ~estrato_w02, weights = ~ponderador01_w02, data = elsoc_original_w02)
m_orig_des4 <- svydesign(ids = ~segmento_w02, strata = ~estrato_w02, weights = ~ponderador02_w02, data = elsoc_original_w02)

m_orig_des5 <- svydesign(ids = ~segmento_w03, strata = ~estrato_w03, weights = ~ponderador01_w03, data = elsoc_original_w03)
m_orig_des6 <- svydesign(ids = ~segmento_w03, strata = ~estrato_w03, weights = ~ponderador02_w03, data = elsoc_original_w03)

m_refr_des1 <- svydesign(ids = ~segmento_w03, strata = ~estrato_w03, weights = ~ponderador01_w03, data = elsoc_refresco)
m_refr_des2 <- svydesign(ids = ~segmento_w03, strata = ~estrato_w03, weights = ~ponderador02_w03, data = elsoc_refresco)


###############################################################################################################################################################################
#SEXO

sexo <- data.frame(categoria = c("Hombre","Mujer","Total"))

Nw01s <- sum(table(elsoc_original_w01$m0_sexo_w01))
Nw02s <- sum(table(elsoc_original_w02$m0_sexo_w02))
Nw03s <- sum(table(elsoc_original_w03$m0_sexo_w03))
Nrefs <- sum(table(elsoc_refresco$m0_sexo_w03))

#Frecuencias originales
sexo$frec_w01 <- c( table(elsoc_original_w01$m0_sexo_w01), Nw01s)
sexo$frec_w02 <- c( table(elsoc_original_w02$m0_sexo_w02), Nw02s)
sexo$frec_w03 <- c( table(elsoc_original_w03$m0_sexo_w03), Nw03s)
sexo$frec_ref <- c( table(elsoc_refresco$m0_sexo_w03),     Nrefs)

#Diseño muestral complejo: Primera Medición y Ponderador Base
sexo$frec_w01a <- c(svytable(formula = ~m0_sexo_w01, design = m_orig_des1, Ntotal = Nw01s), Nw01s)
sexo$frec_w02a <- c(svytable(formula = ~m0_sexo_w02, design = m_orig_des1, Ntotal = Nw02s), Nw02s)
sexo$frec_w03a <- c(svytable(formula = ~m0_sexo_w03, design = m_orig_des1, Ntotal = Nw03s), Nw03s)
sexo$frec_refa <- c(svytable(formula = ~m0_sexo_w03, design = m_refr_des1, Ntotal = Nrefs), Nrefs)

#Diseño muestral complejo: Primera Medición y Ponderador Corregido
sexo$frec_w01b <- c(svytable(formula = ~m0_sexo_w01, design = m_orig_des2, Ntotal = Nw01s), Nw01s)
sexo$frec_w02b <- c(svytable(formula = ~m0_sexo_w02, design = m_orig_des2, Ntotal = Nw02s), Nw02s)
sexo$frec_w03b <- c(svytable(formula = ~m0_sexo_w03, design = m_orig_des2, Ntotal = Nw03s), Nw03s)
sexo$frec_refb <- c(svytable(formula = ~m0_sexo_w03, design = m_refr_des2, Ntotal = Nrefs), Nrefs)

#Diseño muestral complejo: Medición Correspondiente y Ponderador Base
sexo$frec_w01c <- c(svytable(formula = ~m0_sexo_w01, design = m_orig_des1, Ntotal = Nw01s), Nw01s)
sexo$frec_w02c <- c(svytable(formula = ~m0_sexo_w02, design = m_orig_des3, Ntotal = Nw02s), Nw02s)
sexo$frec_w03c <- c(svytable(formula = ~m0_sexo_w03, design = m_orig_des5, Ntotal = Nw03s), Nw03s)
sexo$frec_refc <- c(svytable(formula = ~m0_sexo_w03, design = m_refr_des1, Ntotal = Nrefs), Nrefs)

#Diseño muestral complejo: Medición Correspondiente y Ponderador Corregido
sexo$frec_w01d <- c(svytable(formula = ~m0_sexo_w01, design = m_orig_des2, Ntotal = Nw01s), Nw01s)
sexo$frec_w02d <- c(svytable(formula = ~m0_sexo_w02, design = m_orig_des4, Ntotal = Nw02s), Nw02s)
sexo$frec_w03d <- c(svytable(formula = ~m0_sexo_w03, design = m_orig_des6, Ntotal = Nw03s), Nw03s)
sexo$frec_refd <- c(svytable(formula = ~m0_sexo_w03, design = m_refr_des2, Ntotal = Nrefs), Nrefs)

#Exportar objeto como .csv
sexo
write_csv(x = sexo, path = "0_Sexo.csv")

#Remover objetos innecesarios
rm(Nw01s, Nw02s, Nw03s, Nrefs)


###############################################################################################################################################################################
#Edad Promedio

edad1 <- data.frame(medicion = c("2016","2017","2018","Refresco"))

#Promedios Originales
edad1$prom <- c( mean(elsoc_original_w01$m0_edad_w01), mean(elsoc_original_w02$m0_edad_w02), mean(elsoc_original_w03$m0_edad_w03), mean(elsoc_refresco$m0_edad_w03,na.rm = TRUE) )

#Diseño muestral complejo: Primera Medición y Ponderador Base
edad1$prom_a <-c( as.data.frame(svymean(x = ~m0_edad_w01, design = m_orig_des1, deff = TRUE))[1], 
                  as.data.frame(svymean(x = ~m0_edad_w02, design = m_orig_des1, deff = TRUE, na.rm = TRUE))[1],
                  as.data.frame(svymean(x = ~m0_edad_w03, design = m_orig_des1, deff = TRUE, na.rm = TRUE))[1],
                  as.data.frame(svymean(x = ~m0_edad_w03, design = m_refr_des1, deff = TRUE, na.rm = TRUE))[1])

edad1$se_a <-c( as.data.frame(svymean(x = ~m0_edad_w01, design = m_orig_des1, deff = TRUE))[2], 
                as.data.frame(svymean(x = ~m0_edad_w02, design = m_orig_des1, deff = TRUE, na.rm = TRUE))[2],
                as.data.frame(svymean(x = ~m0_edad_w03, design = m_orig_des1, deff = TRUE, na.rm = TRUE))[2],
                as.data.frame(svymean(x = ~m0_edad_w03, design = m_refr_des1, deff = TRUE, na.rm = TRUE))[2])

edad1$deff_a <-c( as.data.frame(svymean(x = ~m0_edad_w01, design = m_orig_des1, deff = TRUE))[3], 
                  as.data.frame(svymean(x = ~m0_edad_w02, design = m_orig_des1, deff = TRUE, na.rm = TRUE))[3],
                  as.data.frame(svymean(x = ~m0_edad_w03, design = m_orig_des1, deff = TRUE, na.rm = TRUE))[3],
                  as.data.frame(svymean(x = ~m0_edad_w03, design = m_refr_des1, deff = TRUE, na.rm = TRUE))[3])

#Diseño muestral complejo: Primera Medición y Ponderador Corregido
edad1$prom_b <-c( as.data.frame(svymean(x = ~m0_edad_w01, design = m_orig_des2, deff = TRUE))[1], 
                  as.data.frame(svymean(x = ~m0_edad_w02, design = m_orig_des2, deff = TRUE, na.rm = TRUE))[1],
                  as.data.frame(svymean(x = ~m0_edad_w03, design = m_orig_des2, deff = TRUE, na.rm = TRUE))[1],
                  as.data.frame(svymean(x = ~m0_edad_w03, design = m_refr_des2, deff = TRUE, na.rm = TRUE))[1])

edad1$se_b <-c( as.data.frame(svymean(x = ~m0_edad_w01, design = m_orig_des2, deff = TRUE))[2], 
                as.data.frame(svymean(x = ~m0_edad_w02, design = m_orig_des2, deff = TRUE, na.rm = TRUE))[2],
                as.data.frame(svymean(x = ~m0_edad_w03, design = m_orig_des2, deff = TRUE, na.rm = TRUE))[2],
                as.data.frame(svymean(x = ~m0_edad_w03, design = m_refr_des2, deff = TRUE, na.rm = TRUE))[2])

edad1$deff_b <-c( as.data.frame(svymean(x = ~m0_edad_w01, design = m_orig_des2, deff = TRUE))[3], 
                  as.data.frame(svymean(x = ~m0_edad_w02, design = m_orig_des2, deff = TRUE, na.rm = TRUE))[3],
                  as.data.frame(svymean(x = ~m0_edad_w03, design = m_orig_des2, deff = TRUE, na.rm = TRUE))[3],
                  as.data.frame(svymean(x = ~m0_edad_w03, design = m_refr_des2, deff = TRUE, na.rm = TRUE))[3])

#Diseño muestral complejo: Medición Correspondiente y Ponderador Base
edad1$prom_c <-c( as.data.frame( svymean(x = ~m0_edad_w01, design = m_orig_des1, deff = TRUE) )[1], 
                  as.data.frame( svymean(x = ~m0_edad_w02, design = m_orig_des3, deff = TRUE) )[1],
                  as.data.frame( svymean(x = ~m0_edad_w03, design = m_orig_des5, deff = TRUE) )[1],
                  as.data.frame( svymean(x = ~m0_edad_w03, design = m_refr_des1, deff = TRUE, na.rm = TRUE) )[1])

edad1$se_c <-c( as.data.frame( svymean(x = ~m0_edad_w01, design = m_orig_des1, deff = TRUE) )[2], 
                as.data.frame( svymean(x = ~m0_edad_w02, design = m_orig_des3, deff = TRUE) )[2],
                as.data.frame( svymean(x = ~m0_edad_w03, design = m_orig_des5, deff = TRUE) )[2],
                as.data.frame( svymean(x = ~m0_edad_w03, design = m_refr_des1, deff = TRUE, na.rm = TRUE) )[2])

edad1$deff_c <-c( as.data.frame( svymean(x = ~m0_edad_w01, design = m_orig_des1, deff = TRUE) )[3], 
                  as.data.frame( svymean(x = ~m0_edad_w02, design = m_orig_des3, deff = TRUE) )[3],
                  as.data.frame( svymean(x = ~m0_edad_w03, design = m_orig_des5, deff = TRUE) )[3],
                  as.data.frame( svymean(x = ~m0_edad_w03, design = m_refr_des1, deff = TRUE, na.rm = TRUE) )[3])

#Diseño muestral complejo: Medición Correspondiente y Ponderador Corregido
edad1$prom_d <-c( as.data.frame( svymean(x = ~m0_edad_w01, design = m_orig_des2, deff = TRUE) )[1], 
                  as.data.frame( svymean(x = ~m0_edad_w02, design = m_orig_des4, deff = TRUE) )[1],
                  as.data.frame( svymean(x = ~m0_edad_w03, design = m_orig_des6, deff = TRUE) )[1],
                  as.data.frame( svymean(x = ~m0_edad_w03, design = m_refr_des2, deff = TRUE, na.rm = TRUE) )[1])

edad1$se_d <-c( as.data.frame( svymean(x = ~m0_edad_w01, design = m_orig_des2, deff = TRUE) )[2], 
                as.data.frame( svymean(x = ~m0_edad_w02, design = m_orig_des4, deff = TRUE) )[2],
                as.data.frame( svymean(x = ~m0_edad_w03, design = m_orig_des6, deff = TRUE) )[2],
                as.data.frame( svymean(x = ~m0_edad_w03, design = m_refr_des2, deff = TRUE, na.rm = TRUE) )[2])

edad1$deff_d <-c( as.data.frame( svymean(x = ~m0_edad_w01, design = m_orig_des2, deff = TRUE) )[3], 
                  as.data.frame( svymean(x = ~m0_edad_w02, design = m_orig_des4, deff = TRUE) )[3],
                  as.data.frame( svymean(x = ~m0_edad_w03, design = m_orig_des6, deff = TRUE) )[3],
                  as.data.frame( svymean(x = ~m0_edad_w03, design = m_refr_des2, deff = TRUE, na.rm = TRUE) )[3])


#Transformar a variables numéricas
edad1$prom_a <- as.numeric(edad1$prom_a)
edad1$prom_b <- as.numeric(edad1$prom_b)
edad1$prom_c <- as.numeric(edad1$prom_c)
edad1$prom_d <- as.numeric(edad1$prom_d)

edad1$se_a <- as.numeric(edad1$se_a)
edad1$se_b <- as.numeric(edad1$se_b)
edad1$se_c <- as.numeric(edad1$se_c)
edad1$se_d <- as.numeric(edad1$se_d)

edad1$deff_a <- as.numeric(edad1$deff_a)
edad1$deff_b <- as.numeric(edad1$deff_b)
edad1$deff_c <- as.numeric(edad1$deff_c)
edad1$deff_d <- as.numeric(edad1$deff_d)


#Exportar objeto como .csv
edad1
write_csv(x = edad1, path = "1_Edad_Promedio.csv")


###############################################################################################################################################################################
#Edad en Tramos

edad2 <- data.frame(categoria = c("18 a 29","30 a 49","50 a 64", "65 y más", "Total"))

Nw01e <- sum(table(elsoc_original_w01$edad_rec_w01))
Nw02e <- sum(table(elsoc_original_w02$edad_rec_w02))
Nw03e <- sum(table(elsoc_original_w03$edad_rec_w03))
Nrefe <- sum(table(elsoc_refresco$edad_rec_w03))


#Frecuencias originales
edad2$frec_w01 <- c( table(elsoc_original_w01$edad_rec_w01), Nw01e)
edad2$frec_w02 <- c( table(elsoc_original_w02$edad_rec_w02), Nw02e)
edad2$frec_w03 <- c( table(elsoc_original_w03$edad_rec_w03), Nw03e)
edad2$frec_ref <- c( table(elsoc_refresco$edad_rec_w03),     Nrefe)

#Diseño muestral complejo: Primera Medición y Ponderador Base
edad2$frec_w01a <- c( svytable(formula = ~edad_rec_w01, design = m_orig_des1, Ntotal = Nw01e), Nw01e)
edad2$frec_w02a <- c( svytable(formula = ~edad_rec_w02, design = m_orig_des1, Ntotal = Nw02e), Nw02e)
edad2$frec_w03a <- c( svytable(formula = ~edad_rec_w03, design = m_orig_des1, Ntotal = Nw03e), Nw03e)
edad2$frec_refa <- c( svytable(formula = ~edad_rec_w03, design = m_refr_des1, Ntotal = Nrefe), Nrefe)

#Diseño muestral complejo: Primera Medición y Ponderador Corregido
edad2$frec_w01b <- c( svytable(formula = ~edad_rec_w01, design = m_orig_des2, Ntotal = Nw01e), Nw01e)
edad2$frec_w02b <- c( svytable(formula = ~edad_rec_w02, design = m_orig_des2, Ntotal = Nw02e), Nw02e)
edad2$frec_w03b <- c( svytable(formula = ~edad_rec_w03, design = m_orig_des2, Ntotal = Nw03e), Nw03e)
edad2$frec_refb <- c( svytable(formula = ~edad_rec_w03, design = m_refr_des2, Ntotal = Nrefe), Nrefe)

#Diseño muestral complejo: Medición Correspondiente y Ponderador Base
edad2$frec_w01c <- c( svytable(formula = ~edad_rec_w01, design = m_orig_des1, Ntotal = Nw01e), Nw01e)
edad2$frec_w02c <- c( svytable(formula = ~edad_rec_w02, design = m_orig_des3, Ntotal = Nw02e), Nw02e)
edad2$frec_w03c <- c( svytable(formula = ~edad_rec_w03, design = m_orig_des5, Ntotal = Nw03e), Nw03e)
edad2$frec_refc <- c( svytable(formula = ~edad_rec_w03, design = m_refr_des1, Ntotal = Nrefe), Nrefe)

#Diseño muestral complejo: Medición Correspondiente y Ponderador Corregido
edad2$frec_w01d <- c( svytable(formula = ~edad_rec_w01, design = m_orig_des2, Ntotal = Nw01e), Nw01e)
edad2$frec_w02d <- c( svytable(formula = ~edad_rec_w02, design = m_orig_des4, Ntotal = Nw02e), Nw02e)
edad2$frec_w03d <- c( svytable(formula = ~edad_rec_w03, design = m_orig_des6, Ntotal = Nw03e), Nw03e)
edad2$frec_refd <- c( svytable(formula = ~edad_rec_w03, design = m_refr_des2, Ntotal = Nrefe), Nrefe)


#Exportar objeto como .csv
edad2
write_csv(x = edad2, path = "2_Edad_Tramos.csv")

#Remover objetos innecesarios
rm(Nw01e, Nw02e, Nw03e, Nrefe)


###############################################################################################################################################################################
#EDUCACIÓN 

educacion <- data.frame(categoria = c("Ed. Basica o menos","Ed. Media","Ed. Técnica","Ed. Universitaria", "Total"))

Nw01d <- sum(table(elsoc_original_w01$educ_rec_w01))
Nw02d <- sum(table(elsoc_original_w02$educ_rec_w02))
Nw03d <- sum(table(elsoc_original_w03$educ_rec_w03))
Nrefd <- sum(table(elsoc_refresco$educ_rec_w03))

#Frecuencias originales
educacion$frec_w01 <- c( table(elsoc_original_w01$educ_rec_w01), Nw01d)
educacion$frec_w02 <- c( table(elsoc_original_w02$educ_rec_w02), Nw02d)
educacion$frec_w03 <- c( table(elsoc_original_w03$educ_rec_w03), Nw03d)
educacion$frec_ref <- c( table(elsoc_refresco$educ_rec_w03),     Nrefd)

#Diseño muestral complejo: Primera Medición y Ponderador Base
educacion$frec_w01a <- c( svytable(formula = ~educ_rec_w01, design = m_orig_des1, Ntotal = Nw01d), Nw01d)
educacion$frec_w02a <- c( svytable(formula = ~educ_rec_w02, design = m_orig_des1, Ntotal = Nw02d), Nw02d)
educacion$frec_w03a <- c( svytable(formula = ~educ_rec_w03, design = m_orig_des1, Ntotal = Nw03d), Nw03d)
educacion$frec_refa <- c( svytable(formula = ~educ_rec_w03, design = m_refr_des1, Ntotal = Nrefd), Nrefd)

#Diseño muestral complejo: Primera Medición y Ponderador Corregido
educacion$frec_w01b <- c( svytable(formula = ~educ_rec_w01, design = m_orig_des2, Ntotal = Nw01d), Nw01d)
educacion$frec_w02b <- c( svytable(formula = ~educ_rec_w02, design = m_orig_des2, Ntotal = Nw02d), Nw02d)
educacion$frec_w03b <- c( svytable(formula = ~educ_rec_w03, design = m_orig_des2, Ntotal = Nw03d), Nw03d)
educacion$frec_refb <- c( svytable(formula = ~educ_rec_w03, design = m_refr_des2, Ntotal = Nrefd), Nrefd)

#Diseño muestral complejo: Medición Correspondiente y Ponderador Base
educacion$frec_w01c <- c( svytable(formula = ~educ_rec_w01, design = m_orig_des1, Ntotal = Nw01d), Nw01d)
educacion$frec_w02c <- c( svytable(formula = ~educ_rec_w02, design = m_orig_des3, Ntotal = Nw02d), Nw02d)
educacion$frec_w03c <- c( svytable(formula = ~educ_rec_w03, design = m_orig_des5, Ntotal = Nw03d), Nw03d)
educacion$frec_refc <- c( svytable(formula = ~educ_rec_w03, design = m_refr_des1, Ntotal = Nrefd), Nrefd)

#Diseño muestral complejo: Medición Correspondiente y Ponderador Corregido
educacion$frec_w01d <- c( svytable(formula = ~educ_rec_w01, design = m_orig_des2, Ntotal = Nw01d), Nw01d)
educacion$frec_w02d <- c( svytable(formula = ~educ_rec_w02, design = m_orig_des4, Ntotal = Nw02d), Nw02d)
educacion$frec_w03d <- c( svytable(formula = ~educ_rec_w03, design = m_orig_des6, Ntotal = Nw03d), Nw03d)
educacion$frec_refd <- c( svytable(formula = ~educ_rec_w03, design = m_refr_des2, Ntotal = Nrefd), Nrefd)


#Exportar objeto como .csv
educacion
write_csv(x = educacion, path = "3_Educacion.csv")

#Remover objetos innecesarios
rm(Nw01d, Nw02d, Nw03d, Nrefd)


###############################################################################################################################################################################
#TERRITORIO 

territorio <- data.frame(categoria = c("Santiago","Concepción","Valparaíso","Ciudades Grandes Norte","Ciudades Grandes Centro","Ciudades Grandes Sur",
                                       "Ciudades Medianas Norte","Ciudades Medianas Centro","Ciudades Medianas Sur","Ciudades Pequeñas Norte",
                                       "Ciudades Pequeñas Centro","Ciudades Pequeñas Sur","Total")) 

Nw01t <- sum(table(elsoc_original_w01$territ_w01))
Nw02t <- sum(table(elsoc_original_w02$territ_w02))
Nw03t <- sum(table(elsoc_original_w03$territ_w03))
Nreft <- sum(table(elsoc_refresco$territ_w03))

#Frecuencias originales
territorio$frec_w01 <- c( table(elsoc_original_w01$territ_w01), Nw01t)
territorio$frec_w02 <- c( table(elsoc_original_w02$territ_w02), Nw02t)
territorio$frec_w03 <- c( table(elsoc_original_w03$territ_w03), Nw03t)
territorio$frec_ref <- c( table(elsoc_refresco$territ_w03),     Nreft)

#Diseño muestral complejo: Primera Medición y Ponderador Base
territorio$frec_w01a <- c( svytable(formula = ~territ_w01, design = m_orig_des1, Ntotal = Nw01t), Nw01t)
territorio$frec_w02a <- c( svytable(formula = ~territ_w02, design = m_orig_des1, Ntotal = Nw02t), Nw02t)
territorio$frec_w03a <- c( svytable(formula = ~territ_w03, design = m_orig_des1, Ntotal = Nw03t), Nw03t)
territorio$frec_refa <- c( svytable(formula = ~territ_w03, design = m_refr_des1, Ntotal = Nreft), Nreft)

#Diseño muestral complejo: Primera Medición y Ponderador Corregido
territorio$frec_w01b <- c( svytable(formula = ~territ_w01, design = m_orig_des2, Ntotal = Nw01t), Nw01t)
territorio$frec_w02b <- c( svytable(formula = ~territ_w02, design = m_orig_des2, Ntotal = Nw02t), Nw02t)
territorio$frec_w03b <- c( svytable(formula = ~territ_w03, design = m_orig_des2, Ntotal = Nw03t), Nw03t)
territorio$frec_refb <- c( svytable(formula = ~territ_w03, design = m_refr_des2, Ntotal = Nreft), Nreft)

#Diseño muestral complejo: Medición Correspondiente y Ponderador Base
territorio$frec_w01c <- c( svytable(formula = ~territ_w01, design = m_orig_des1, Ntotal = Nw01t), Nw01t)
territorio$frec_w02c <- c( svytable(formula = ~territ_w02, design = m_orig_des3, Ntotal = Nw02t), Nw02t)
territorio$frec_w03c <- c( svytable(formula = ~territ_w03, design = m_orig_des5, Ntotal = Nw03t), Nw03t)
territorio$frec_refc <- c( svytable(formula = ~territ_w03, design = m_refr_des1, Ntotal = Nreft), Nreft)

#Diseño muestral complejo: Medición Correspondiente y Ponderador Corregido
territorio$frec_w01d <- c( svytable(formula = ~territ_w01, design = m_orig_des2, Ntotal = Nw01t), Nw01t)
territorio$frec_w02d <- c( svytable(formula = ~territ_w02, design = m_orig_des4, Ntotal = Nw02t), Nw02t)
territorio$frec_w03d <- c( svytable(formula = ~territ_w03, design = m_orig_des6, Ntotal = Nw03t), Nw03t)
territorio$frec_refd <- c( svytable(formula = ~territ_w03, design = m_refr_des2, Ntotal = Nreft), Nreft)

#Exportar objeto como .csv
territorio
write_csv(x = territorio, path = "4_Territorio.csv")

#Remover objetos innecesarios
rm(Nw01t, Nw02t, Nw03t, Nreft)


###############################################################################################################################################################################
#RELIGION

religion <- data.frame(categoria = c("Católico","Evangélico","Otro Credo","No Creyente","Total")) 

Nw01r <- sum(table(elsoc_original_w01$relig_w01))
Nw02r <- sum(table(elsoc_original_w02$relig_w02))
Nw03r <- sum(table(elsoc_original_w03$relig_w03))
Nrefr <- sum(table(elsoc_refresco$relig_w03))

#Frecuencias originales
religion$frec_w01 <- c( table(elsoc_original_w01$relig_w01), Nw01r)
religion$frec_w02 <- c( table(elsoc_original_w02$relig_w02), Nw02r)
religion$frec_w03 <- c( table(elsoc_original_w03$relig_w03), Nw03r)
religion$frec_ref <- c( table(elsoc_refresco$relig_w03),     Nrefr)

#Diseño muestral complejo: Primera Medición y Ponderador Base
religion$frec_w01a <- c( svytable(formula = ~relig_w01, design = m_orig_des1, Ntotal = Nw01r), Nw01r)
religion$frec_w02a <- c( svytable(formula = ~relig_w02, design = m_orig_des1, Ntotal = Nw02r), Nw02r)
religion$frec_w03a <- c( svytable(formula = ~relig_w03, design = m_orig_des1, Ntotal = Nw03r), Nw03r)
religion$frec_refa <- c( svytable(formula = ~relig_w03, design = m_orig_des1, Ntotal = Nrefr), Nrefr)

#Diseño muestral complejo: Primera Medición y Ponderador Corregido
religion$frec_w01b <- c( svytable(formula = ~relig_w01, design = m_orig_des2, Ntotal = Nw01r), Nw01r)
religion$frec_w02b <- c( svytable(formula = ~relig_w02, design = m_orig_des2, Ntotal = Nw02r), Nw02r)
religion$frec_w03b <- c( svytable(formula = ~relig_w03, design = m_orig_des2, Ntotal = Nw03r), Nw03r)
religion$frec_refb <- c( svytable(formula = ~relig_w03, design = m_orig_des2, Ntotal = Nrefr), Nrefr)

#Diseño muestral complejo: Medición Correspondiente y Ponderador Base
religion$frec_w01c <- c( svytable(formula = ~relig_w01, design = m_orig_des1, Ntotal = Nw01r), Nw01r)
religion$frec_w02c <- c( svytable(formula = ~relig_w02, design = m_orig_des3, Ntotal = Nw02r), Nw02r)
religion$frec_w03c <- c( svytable(formula = ~relig_w03, design = m_orig_des5, Ntotal = Nw03r), Nw03r)
religion$frec_refc <- c( svytable(formula = ~relig_w03, design = m_orig_des1, Ntotal = Nrefr), Nrefr)

#Diseño muestral complejo: Medición Correspondiente y Ponderador Corregido
religion$frec_w01d <- c( svytable(formula = ~relig_w01, design = m_orig_des2, Ntotal = Nw01r), Nw01r)
religion$frec_w02d <- c( svytable(formula = ~relig_w02, design = m_orig_des4, Ntotal = Nw02r), Nw02r)
religion$frec_w03d <- c( svytable(formula = ~relig_w03, design = m_orig_des6, Ntotal = Nw03r), Nw03r)
religion$frec_refd <- c( svytable(formula = ~relig_w03, design = m_orig_des2, Ntotal = Nrefr), Nrefr)


#Exportar objeto como .csv
religion
write_csv(x = religion, path = "5_Religion.csv")

#Remover objetos innecesarios
rm(Nw01r, Nw02r, Nw03r, Nrefr)
rm(elsoc_original_w01, elsoc_original_w02, elsoc_original_w03, elsoc_refresco,
   sexo, edad1, edad2, educacion, religion, territorio,
   m_orig_des1, m_orig_des2, m_orig_des3, m_orig_des4, m_orig_des5, m_orig_des6, 
   m_refr_des1, m_refr_des2)

###############################################################################################################################################################################
#MUESTRA BAJO ESTUDIO

#Filtrar datos con 3 mediciones
elsoc_panel_01 %>% dplyr::filter(tipo_atricion == 1 & tipo_caso != 2) -> elsoc_panel_02

#Recodificación de edad para chequeo
elsoc_panel_02$edad_temp    <- paste0(elsoc_panel_02$edad_rec_w01,elsoc_panel_02$edad_rec_w02,elsoc_panel_02$edad_rec_w03)
elsoc_panel_02$edad_control <- car::recode(elsoc_panel_02$edad_temp,"c('111','222','333','444')='Estable';
                                           c('112','122','223','233','334','344')='Envejece';
                                           c('211','221','433','442','443')='Rejuvenece';c('224','244')='Envejece mucho';
                                           c('232','323','331','343')='Raro'")

#Diseño muestral complejo
m_orig_des1a <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador01_w01, data = elsoc_panel_02)
m_orig_des2a <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_panel_02)

#Objetos vacíos de almacenamiento
sexo_a       <- data.frame(categoria = c("Hombre","Mujer"))
edad2_a      <- data.frame(categoria = c("18 a 29","30 a 49","50 a 64", "65 y más"))
educacion_a  <- data.frame(categoria = c("Ed. Basica o menos","Ed. Media","Ed. Técnica","Ed. Universitaria"))
territorio_a <- data.frame(categoria = c("Santiago","Concepción","Valparaíso","Ciudades Grandes Norte","Ciudades Grandes Centro","Ciudades Grandes Sur",
                                       "Ciudades Medianas Norte","Ciudades Medianas Centro","Ciudades Medianas Sur","Ciudades Pequeñas Norte",
                                       "Ciudades Pequeñas Centro","Ciudades Pequeñas Sur"))
religion_a   <- data.frame(categoria = c("Católico","Evangélico","Otro Credo","No Creyente"))


#Frecuencias originales
sexo_a$frec        <- table(elsoc_panel_02$m0_sexo_w01)
edad2_a$frec       <- table(elsoc_panel_02$edad_rec_w01)
educacion_a$frec   <- table(elsoc_panel_02$educ_rec_w01)
territorio_a$frec  <- table(elsoc_panel_02$territ_w01)
religion_a$frec    <- table(elsoc_panel_02$relig_w01)

#Frecuencias con Diseño Muestral Complejo
sexo_a$dis1  <- svytable(formula = ~m0_sexo_w01, design = m_orig_des1a, Ntotal = 2096)
sexo_a$dis2  <- svytable(formula = ~m0_sexo_w01, design = m_orig_des2a, Ntotal = 2096)

edad2_a$dis1  <- svytable(formula = ~edad_rec_w01, design = m_orig_des1a, Ntotal = 2096)
edad2_a$dis2  <- svytable(formula = ~edad_rec_w01, design = m_orig_des2a, Ntotal = 2096)

educacion_a$dis1  <- svytable(formula = ~educ_rec_w01, design = m_orig_des1a, Ntotal = 2096)
educacion_a$dis2  <- svytable(formula = ~educ_rec_w01, design = m_orig_des2a, Ntotal = 2096)

territorio_a$dis1  <- svytable(formula = ~territ_w01, design = m_orig_des1a, Ntotal = 2096)
territorio_a$dis2  <- svytable(formula = ~territ_w01, design = m_orig_des2a, Ntotal = 2096)

religion_a$dis1  <- svytable(formula = ~relig_w01, design = m_orig_des1a, Ntotal = 2096)
religion_a$dis2  <- svytable(formula = ~relig_w01, design = m_orig_des2a, Ntotal = 2096)


#Combinar objetos resultados y exportar
total <- bind_rows(sexo_a, edad2_a, educacion_a, territorio_a, religion_a)
total
write_csv(x = total, path = "Total_Casos_Estudio.csv")

#Remover objetos innecesario
rm(sexo_a, edad2_a, educacion_a, religion_a, territorio_a)

