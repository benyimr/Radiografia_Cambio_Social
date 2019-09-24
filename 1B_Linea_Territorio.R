rm(list=ls())

#Cargar paquetes
library(ggplot2)
library(ggthemes)
library(lme4)
library(tidyverse)
library(sjmisc)
library(sjlabelled)
library(sjPlot)
library(survey)
library(srvyr)

#Importar bases de datos WIDE
load("/Users/benjaminmunozrojas/Dropbox/6_COES/3_Encuestas_COES/2_ELSOC/5_Bases_de_Datos/11_Combinacion_de_Olas_ELSOC/0C_Bases_de_Datos_Resultantes_2016_2018/ELSOC_Wide_2016_2018_v1.10_R.RData")

#Importar bases de datos LONG
elsoc_long <- read_stata(path = "ELSOC_Long_2016_2018_Temporal.dta")


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
elsoc_long[elsoc_long == -888] <- NA
elsoc_long[elsoc_long == -999] <- NA

#Ordenar por ID encuesta
elsoc_long %>% dplyr::arrange(desc(idencuesta)) -> elsoc_long


###############################################################################################################################################################################
#RECODIFICACION DE VARIABLES

#Índice de Conflictos Barriales
elsoc_panel_02$ind_confl_w01 <- (elsoc_panel_02$t11_01_w01 + elsoc_panel_02$t11_02_w01 + elsoc_panel_02$t11_03_w01 + elsoc_panel_02$t11_04_w01)/4
elsoc_panel_02$ind_confl_w02 <- (elsoc_panel_02$t11_01_w02 + elsoc_panel_02$t11_02_w02 + elsoc_panel_02$t11_03_w02 + elsoc_panel_02$t11_04_w02)/4
elsoc_panel_02$ind_confl_w03 <- (elsoc_panel_02$t11_01_w03 + elsoc_panel_02$t11_02_w03 + elsoc_panel_02$t11_03_w03 + elsoc_panel_02$t11_04_w03)/4

elsoc_long$score_confl   <- (elsoc_long$t11_01 + elsoc_long$t11_02 + elsoc_long$t11_03 + elsoc_long$t11_04)
elsoc_long$ind_confl     <- elsoc_long$score_confl/4
elsoc_long$ind_confl_rec <- round(elsoc_long$ind_confl)
elsoc_long$ind_confl_rec <- as.factor(elsoc_long$ind_confl_rec)

#Estimgmatización
elsoc_panel_02$estigma_w01 <- car::recode(elsoc_panel_02$t08_w01,"c(1,2)=1;c(3,4,5)=0")
elsoc_panel_02$estigma_w02 <- car::recode(elsoc_panel_02$t08_w02,"c(1,2)=1;c(3,4,5)=0")
elsoc_panel_02$estigma_w03 <- car::recode(elsoc_panel_02$t08_w03,"c(1,2)=1;c(3,4,5)=0")

elsoc_long$estigma <- car::recode(elsoc_long$t08,"c(1,2)=1;c(3,4,5)=0")
elsoc_long$estigma <- as.factor(elsoc_long$estigma)

#Confianza Vecinos
elsoc_panel_02$conf_vec_w01 <- car::recode(elsoc_panel_02$t01_w01,"c(1,2,3)=0;c(4,5)=1")
elsoc_panel_02$conf_vec_w02 <- car::recode(elsoc_panel_02$t01_w02,"c(1,2,3)=0;c(4,5)=1")
elsoc_panel_02$conf_vec_w03 <- car::recode(elsoc_panel_02$t01_w03,"c(1,2,3)=0;c(4,5)=1")

elsoc_long$conf_vec <- car::recode(elsoc_long$t01,"c(1,2,3)=0;c(4,5)=1")

#Inseguridad
elsoc_panel_02$inseg_w01  <- car::recode(elsoc_panel_02$t10_w01,"c(1,2)=1;c(3,4,5)=0")
elsoc_panel_02$inseg_w02  <- car::recode(elsoc_panel_02$t10_w02,"c(1,2)=1;c(3,4,5)=0")
elsoc_panel_02$inseg_w03  <- car::recode(elsoc_panel_02$t10_w03,"c(1,2)=1;c(3,4,5)=0")

elsoc_long$inseg  <- car::recode(elsoc_long$t10,"c(1,2)=1;c(3,4,5)=0")

#Índice de Experiencias Indirectas de Criminalidad
elsoc_panel_02$ind_crim_w01 <- (elsoc_panel_02$t09_01_w01 + elsoc_panel_02$t09_02_w01 + elsoc_panel_02$t09_03_w01)/3
elsoc_panel_02$ind_crim_w02 <- (elsoc_panel_02$t09_01_w02 + elsoc_panel_02$t09_02_w02 + elsoc_panel_02$t09_03_w02)/3
elsoc_panel_02$ind_crim_w03 <- (elsoc_panel_02$t09_01_w03 + elsoc_panel_02$t09_02_w03 + elsoc_panel_02$t09_03_w03)/3

elsoc_long$score_crim   <- (elsoc_long$t09_01 + elsoc_long$t09_02 + elsoc_long$t09_03)
elsoc_long$ind_crim     <- elsoc_long$score_crim/3
elsoc_long$ind_crim_rec <- round(elsoc_long$ind_crim)


#Región del País
elsoc_panel_02$zona1_w01  <- car::recode(elsoc_panel_02$region_w01,"c(1,2,3,4,15)='Norte';c(5,6,7,8)='Centro';c(9,10,11,12,14)='Sur';13='Metropolitana'")
elsoc_panel_02$zona1_w01  <- factor(elsoc_panel_02$zona1_w01,levels=c("Norte","Centro","Sur","Metropolitana"))
elsoc_panel_02$zona1_w02  <- car::recode(elsoc_panel_02$region_w02,"c(1,2,3,4,15)='Norte';c(5,6,7,8)='Centro';c(9,10,11,12,14)='Sur';13='Metropolitana'")
elsoc_panel_02$zona1_w02  <- factor(elsoc_panel_02$zona1_w02,levels=c("Norte","Centro","Sur","Metropolitana"))
elsoc_panel_02$zona1_w03  <- car::recode(elsoc_panel_02$region_w03,"c(1,2,3,4,15)='Norte';c(5,6,7,8)='Centro';c(9,10,11,12,14)='Sur';13='Metropolitana'")
elsoc_panel_02$zona1_w03  <- factor(elsoc_panel_02$zona1_w03,levels=c("Norte","Centro","Sur","Metropolitana"))

elsoc_long$zona1  <- car::recode(elsoc_long$region,"c(1,2,3,4,15)='Norte';c(5,6,7,8)='Centro';c(9,10,11,12,14)='Sur';13='Metropolitana'")
elsoc_long$zona1  <- factor(elsoc_long$zona1,levels=c("Norte","Centro","Sur","Metropolitana"))


elsoc_panel_02$zona2_w01  <- car::recode(elsoc_panel_02$region_w01,"c(1,2,3,4,15)='Norte';c(6,7)='Centro-Sur';c(9,10,11,12,14)='Sur';c(5,8,13)='Otro'")
elsoc_panel_02$zona2_w01  <- factor(elsoc_panel_02$zona2_w01,levels=c("Norte","Centro-Sur","Sur","Otro"))
elsoc_panel_02$zona2_w02  <- car::recode(elsoc_panel_02$region_w02,"c(1,2,3,4,15)='Norte';c(6,7)='Centro-Sur';c(9,10,11,12,14)='Sur';c(5,8,13)='Otro'")
elsoc_panel_02$zona2_w02  <- factor(elsoc_panel_02$zona2_w02,levels=c("Norte","Centro-Sur","Sur","Otro"))
elsoc_panel_02$zona2_w03  <- car::recode(elsoc_panel_02$region_w03,"c(1,2,3,4,15)='Norte';c(6,7)='Centro-Sur';c(9,10,11,12,14)='Sur';c(5,8,13)='Otro'")
elsoc_panel_02$zona2_w03  <- factor(elsoc_panel_02$zona2_w03,levels=c("Norte","Centro-Sur","Sur","Otro"))


#Estratos
elsoc_panel_02$strata_w01 <- car::recode(elsoc_panel_02$estrato_w01,"1='Gran Santiago';2='Gran Valparaíso';3='Gran Concepción';
                                         4='Ciudades Grandes';5='Ciudades Medianas';6='Ciudades Pequeñas'")
elsoc_panel_02$strata_w01 <- factor(elsoc_panel_02$strata_w01,levels = c("Gran Santiago","Gran Valparaíso","Gran Concepción",
                                                                         "Ciudades Grandes","Ciudades Medianas","Ciudades Pequeñas"))
elsoc_panel_02$strata_w02 <- car::recode(elsoc_panel_02$estrato_w02,"1='Gran Santiago';2='Gran Valparaíso';3='Gran Concepción';
                                         4='Ciudades Grandes';5='Ciudades Medianas';6='Ciudades Pequeñas'")
elsoc_panel_02$strata_w02 <- factor(elsoc_panel_02$strata_w02,levels = c("Gran Santiago","Gran Valparaíso","Gran Concepción",
                                                                         "Ciudades Grandes","Ciudades Medianas","Ciudades Pequeñas"))
elsoc_panel_02$strata_w03 <- car::recode(elsoc_panel_02$estrato_w03,"1='Gran Santiago';2='Gran Valparaíso';3='Gran Concepción';
                                         4='Ciudades Grandes';5='Ciudades Medianas';6='Ciudades Pequeñas'")
elsoc_panel_02$strata_w03 <- factor(elsoc_panel_02$strata_w03,levels = c("Gran Santiago","Gran Valparaíso","Gran Concepción",
                                                                         "Ciudades Grandes","Ciudades Medianas","Ciudades Pequeñas"))

#Quintiles de Ingreso

#Imputar punto medio de rangos de ingreso
elsoc_panel_02$m30_w01_temp <- car::recode(elsoc_panel_02$m30_w01,"1=110000;2=251000;3=305000;4=355000;5=400000;
                                           6=445000;7=490000;8=535000;9=585000;10=640000;11=700000;12=765000;
                                           13=845000;14=935000;15=1040000;16=1180000;17=1375000;18=1670000;
                                           19=2275000;20=2700000;NA=NA")
elsoc_panel_02$m30_w02_temp <- car::recode(elsoc_panel_02$m30_w02,"1=110000;2=251000;3=305000;4=355000;5=400000;
                                           6=445000;7=490000;8=535000;9=585000;10=640000;11=700000;12=765000;
                                           13=845000;14=935000;15=1040000;16=1180000;17=1375000;18=1670000;
                                           19=2275000;20=2700000;NA=NA")
elsoc_panel_02$m30_w03_temp <- car::recode(elsoc_panel_02$m30_w03,"1=110000;2=251000;3=305000;4=355000;5=400000;
                                           6=445000;7=490000;8=535000;9=585000;10=640000;11=700000;12=765000;
                                           13=845000;14=935000;15=1040000;16=1180000;17=1375000;18=1670000;
                                           19=2275000;20=2700000;NA=NA")

#Combinar ingreso numérico y de tramos en una variable
elsoc_panel_02$m29_w01_temp <- ifelse(is.na(elsoc_panel_02$m29_w01)==TRUE,elsoc_panel_02$m30_w01_temp,elsoc_panel_02$m29_w01)
elsoc_panel_02$m29_w02_temp <- ifelse(is.na(elsoc_panel_02$m29_w02)==TRUE,elsoc_panel_02$m30_w02_temp,elsoc_panel_02$m29_w02)
elsoc_panel_02$m29_w03_temp <- ifelse(is.na(elsoc_panel_02$m29_w03)==TRUE,elsoc_panel_02$m30_w03_temp,elsoc_panel_02$m29_w03)

#Inflactar a precios de Diciembre de 2018
elsoc_panel_02$m29_w01rec1 <- elsoc_panel_02$m29_w01_temp*(119.45/113.88)
elsoc_panel_02$m29_w02rec1 <- elsoc_panel_02$m29_w02_temp*(119.45/116.46)
elsoc_panel_02$m29_w03rec1 <- elsoc_panel_02$m29_w03_temp

elsoc_panel_02$m29_w01rec2 <- elsoc_panel_02$m29_w01
elsoc_panel_02$m29_w02rec2 <- elsoc_panel_02$m29_w02
elsoc_panel_02$m29_w03rec2 <- elsoc_panel_02$m29_w03

#Generar Ingresos Per Capita del Hogar 
elsoc_panel_02$nhogar_w01 <-elsoc_panel_02$nhogar1_w01
elsoc_panel_02$nhogar_w02 <-elsoc_panel_02$m46_nhogar_w02
elsoc_panel_02$nhogar_w03 <-elsoc_panel_02$m54_w03

elsoc_panel_02$ing_pc_01_w01 <- elsoc_panel_02$m29_w01rec1/elsoc_panel_02$nhogar_w01
elsoc_panel_02$ing_pc_01_w02 <- elsoc_panel_02$m29_w02rec1/elsoc_panel_02$nhogar_w02
elsoc_panel_02$ing_pc_01_w03 <- elsoc_panel_02$m29_w03rec1/elsoc_panel_02$nhogar_w03

elsoc_panel_02$ing_pc_02_w01 <- elsoc_panel_02$m29_w01rec2/elsoc_panel_02$nhogar_w01
elsoc_panel_02$ing_pc_02_w02 <- elsoc_panel_02$m29_w02rec2/elsoc_panel_02$nhogar_w02
elsoc_panel_02$ing_pc_02_w03 <- elsoc_panel_02$m29_w03rec2/elsoc_panel_02$nhogar_w03

elsoc_panel_02$inc_quint_01_w01 <- sjmisc::split_var(x = elsoc_panel_02$ing_pc_01_w01, n = 5)
elsoc_panel_02$inc_quint_01_w02 <- sjmisc::split_var(x = elsoc_panel_02$ing_pc_01_w02, n = 5)
elsoc_panel_02$inc_quint_01_w03 <- sjmisc::split_var(x = elsoc_panel_02$ing_pc_01_w03, n = 5)
elsoc_panel_02$inc_quint_02_w01 <- sjmisc::split_var(x = elsoc_panel_02$ing_pc_02_w01, n = 5)
elsoc_panel_02$inc_quint_02_w02 <- sjmisc::split_var(x = elsoc_panel_02$ing_pc_02_w02, n = 5)
elsoc_panel_02$inc_quint_02_w03 <- sjmisc::split_var(x = elsoc_panel_02$ing_pc_02_w03, n = 5)

#Nivel Educacional
elsoc_panel_02$educ_w01 <- car::recode(elsoc_panel_02$m01_w01,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")
elsoc_panel_02$educ_w02 <- car::recode(elsoc_panel_02$m01_w02,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")
elsoc_panel_02$educ_w03 <- car::recode(elsoc_panel_02$m01_w03,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")

elsoc_long$educ <- car::recode(elsoc_long$m01,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")


###############################################################################################################################################################################
#DISEÑO MUESTRAL COMPLEJO

#Declaración de Diseño Muestral Complejo
m_orig_des1a <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador01_w01, data = elsoc_panel_02)
m_orig_des2a <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_panel_02)


###############################################################################################################################################################################
#CONFLICTOS BARRIALES

elsoc_panel_02 %>% dplyr::filter(is.na(ind_confl_w01)==FALSE & is.na(ind_confl_w02)==FALSE & is.na(ind_confl_w03)==FALSE)  -> elsoc_conflicto
elsoc_conflicto %>% dplyr::filter(is.na(ing_pc_01_w01)==FALSE & is.na(ing_pc_01_w02)==FALSE & is.na(ing_pc_01_w03)==FALSE) -> elsoc_conflicto_ipc


#Promedio Total
svymean(x = ~ind_confl_w01, design = m_orig_des1a, deff = FALSE, na.rm = TRUE)
svymean(x = ~ind_confl_w02, design = m_orig_des1a, deff = FALSE, na.rm = TRUE)
svymean(x = ~ind_confl_w03, design = m_orig_des1a, deff = FALSE, na.rm = TRUE)

svymean(x = ~ind_confl_w01, design = m_orig_des2a, deff = FALSE, na.rm = TRUE)
svymean(x = ~ind_confl_w02, design = m_orig_des2a, deff = FALSE, na.rm = TRUE)
svymean(x = ~ind_confl_w03, design = m_orig_des2a, deff = FALSE, na.rm = TRUE)


#Promedio por Zona Geográfica
svyby( ~ind_confl_w01, ~zona1_w01, m_orig_des1a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w02, ~zona1_w02, m_orig_des1a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w03, ~zona1_w03, m_orig_des1a, svymean, na.rm=TRUE)

svyby( ~ind_confl_w01, ~zona1_w01, m_orig_des2a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w02, ~zona1_w02, m_orig_des2a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w03, ~zona1_w03, m_orig_des2a, svymean, na.rm=TRUE)

svyby( ~ind_confl_w01, ~zona2_w01, m_orig_des2a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w02, ~zona2_w02, m_orig_des2a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w03, ~zona2_w03, m_orig_des2a, svymean, na.rm=TRUE)

#Promedio por Estrato Muestral
svyby( ~ind_confl_w01, ~strata_w01, m_orig_des1a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w02, ~strata_w02, m_orig_des1a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w03, ~strata_w03, m_orig_des1a, svymean, na.rm=TRUE)

svyby( ~ind_confl_w01, ~strata_w01, m_orig_des2a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w02, ~strata_w02, m_orig_des2a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w03, ~strata_w03, m_orig_des2a, svymean, na.rm=TRUE)


#Promedio por Quintiles de Ingreso
svyby( ~ind_confl_w01, ~inc_quint_01_w01, m_orig_des1a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w02, ~inc_quint_01_w02, m_orig_des1a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w03, ~inc_quint_01_w03, m_orig_des1a, svymean, na.rm=TRUE)

svyby( ~ind_confl_w01, ~inc_quint_01_w01, m_orig_des2a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w02, ~inc_quint_01_w02, m_orig_des2a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w03, ~inc_quint_01_w03, m_orig_des2a, svymean, na.rm=TRUE)

svyby( ~ind_confl_w01, ~inc_quint_02_w01, m_orig_des1a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w02, ~inc_quint_02_w02, m_orig_des1a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w03, ~inc_quint_02_w03, m_orig_des1a, svymean, na.rm=TRUE)

svyby( ~ind_confl_w01, ~inc_quint_02_w01, m_orig_des2a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w02, ~inc_quint_02_w02, m_orig_des2a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w03, ~inc_quint_02_w03, m_orig_des2a, svymean, na.rm=TRUE)


#Promedio por Nivel Educativo
svyby( ~ind_confl_w01, ~educ_w01, m_orig_des1a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w02, ~educ_w02, m_orig_des1a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w03, ~educ_w03, m_orig_des1a, svymean, na.rm=TRUE)

svyby( ~ind_confl_w01, ~educ_w01, m_orig_des2a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w02, ~educ_w02, m_orig_des2a, svymean, na.rm=TRUE)
svyby( ~ind_confl_w03, ~educ_w03, m_orig_des2a, svymean, na.rm=TRUE)


###############################################################################################################################################################################
#ESTIGMATIZACIÓN

elsoc_panel_02 %>% dplyr::filter(is.na(estigma_w01)==FALSE & is.na(estigma_w02)==FALSE & is.na(estigma_w03)==FALSE)      -> elsoc_estigma
elsoc_estigma %>% dplyr::filter(is.na(ing_pc_01_w01)==FALSE & is.na(ing_pc_01_w02)==FALSE & is.na(ing_pc_01_w03)==FALSE) -> elsoc_estigma_ipc

design_estigma     <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_estigma)
design_estigma_ipc <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_estigma)

#Porcentaje Total
svytable(formula = ~estigma_w01, design = design_estigma, Ntotal = 2028)
svytable(formula = ~estigma_w02, design = design_estigma, Ntotal = 2028)
svytable(formula = ~estigma_w03, design = design_estigma, Ntotal = 2028)

#Promedio por Zona Geográfica
svytable(formula = ~zona1_w01+estigma_w01, design = design_estigma, Ntotal = 2028)
svytable(formula = ~zona1_w02+estigma_w02, design = design_estigma, Ntotal = 2028)
svytable(formula = ~zona1_w03+estigma_w03, design = design_estigma, Ntotal = 2028)

svytable(formula = ~zona2_w01+estigma_w01, design = design_estigma, Ntotal = 2028)
svytable(formula = ~zona2_w02+estigma_w02, design = design_estigma, Ntotal = 2028)
svytable(formula = ~zona2_w03+estigma_w03, design = design_estigma, Ntotal = 2028)


#Promedio por Estrato Muestral
svytable(formula = ~strata_w01+estigma_w01, design = design_estigma, Ntotal = 2028)
svytable(formula = ~strata_w02+estigma_w02, design = design_estigma, Ntotal = 2028)
svytable(formula = ~strata_w03+estigma_w03, design = design_estigma, Ntotal = 2028)

#Promedio por Quintiles de Ingreso
svytable(formula = ~inc_quint_01_w01+estigma_w01, design = design_estigma_ipc, Ntotal = 1583)
svytable(formula = ~inc_quint_01_w02+estigma_w02, design = design_estigma_ipc, Ntotal = 1583)
svytable(formula = ~inc_quint_01_w03+estigma_w03, design = design_estigma_ipc, Ntotal = 1583)

svytable(formula = ~inc_quint_02_w01+estigma_w01, design = design_estigma_ipc, Ntotal = 1583)
svytable(formula = ~inc_quint_02_w02+estigma_w02, design = design_estigma_ipc, Ntotal = 1583)
svytable(formula = ~inc_quint_02_w03+estigma_w03, design = design_estigma_ipc, Ntotal = 1583)


#Promedio por Nivel Educativo
svytable(formula = ~educ_w01+estigma_w01, design = design_estigma, Ntotal = 2026)
svytable(formula = ~educ_w02+estigma_w02, design = design_estigma, Ntotal = 2026)
svytable(formula = ~educ_w03+estigma_w03, design = design_estigma, Ntotal = 2026)


###############################################################################################################################################################################
#CONFIANZA VECINOS

elsoc_panel_02 %>% dplyr::filter(is.na(conf_vec_w01)==FALSE & is.na(conf_vec_w02)==FALSE & is.na(conf_vec_w03)==FALSE) -> elsoc_conf_vec
design_conf_vec <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_conf_vec)


#Promedio Total
svytable(formula = ~zona1_w01+conf_vec_w01, design = design_conf_vec, Ntotal = 2069)
svytable(formula = ~zona1_w02+conf_vec_w02, design = design_conf_vec, Ntotal = 2069)
svytable(formula = ~zona1_w03+conf_vec_w03, design = design_conf_vec, Ntotal = 2069)

svytable(formula = ~zona2_w01+conf_vec_w01, design = design_conf_vec, Ntotal = 2069)
svytable(formula = ~zona2_w02+conf_vec_w02, design = design_conf_vec, Ntotal = 2069)
svytable(formula = ~zona2_w03+conf_vec_w03, design = design_conf_vec, Ntotal = 2069)

###############################################################################################################################################################################
#INSEGURIDAD


elsoc_panel_02 %>% dplyr::filter(is.na(inseg_w01)==FALSE & is.na(inseg_w02)==FALSE & is.na(inseg_w03)==FALSE) -> elsoc_inseg
design_elsoc_inseg <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_inseg)

#Promedio Total
svytable(formula = ~zona1_w01+inseg_w01, design = design_elsoc_inseg, Ntotal = 2092)
svytable(formula = ~zona1_w02+inseg_w02, design = design_elsoc_inseg, Ntotal = 2092)
svytable(formula = ~zona1_w03+inseg_w03, design = design_elsoc_inseg, Ntotal = 2092)

svytable(formula = ~zona2_w01+inseg_w01, design = design_elsoc_inseg, Ntotal = 2092)
svytable(formula = ~zona2_w02+inseg_w02, design = design_elsoc_inseg, Ntotal = 2092)
svytable(formula = ~zona2_w03+inseg_w03, design = design_elsoc_inseg, Ntotal = 2092)


###############################################################################################################################################################################
#CRIMINALIDAD

elsoc_panel_02 %>% dplyr::filter(is.na(ind_crim_w01)==FALSE & is.na(ind_crim_w02)==FALSE & is.na(ind_crim_w03)==FALSE) -> elsoc_crim

svyby( ~ind_crim_w01, ~zona1_w01, m_orig_des2a, svymean, na.rm=TRUE)
svyby( ~ind_crim_w02, ~zona1_w02, m_orig_des2a, svymean, na.rm=TRUE)
svyby( ~ind_crim_w03, ~zona1_w03, m_orig_des2a, svymean, na.rm=TRUE)

svyby( ~ind_crim_w01, ~zona2_w01, m_orig_des2a, svymean, na.rm=TRUE)
svyby( ~ind_crim_w02, ~zona2_w02, m_orig_des2a, svymean, na.rm=TRUE)
svyby( ~ind_crim_w03, ~zona2_w03, m_orig_des2a, svymean, na.rm=TRUE)


