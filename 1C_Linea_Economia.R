rm(list=ls())

#Cargar paquetes
library(ggplot2)
library(ggthemes)
library(lme4)
library(tidyverse)
library(sjlabelled)
library(sjmisc)
library(sjPlot)
library(survey)
library(srvyr)

#Importar bases de datos WIDE
load("ELSOC_Wide_2016_2018_v1.10_R.RData")

#Tamaño de Redes
redes1 <- read_rds(path = "NSUM_W01.rds")
redes2 <- read_rds(path = "NSUM_W03.rds")


#############################################################################################################################################
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


rm(elsoc_panel_01)

#################################################################################################################################################################
#RECODIFICACION DE VARIABLES

#Imputar punto medio de rangos de ingreso
elsoc_panel_02$m14_w01_temp <- car::recode(elsoc_panel_02$m14_w01,"1=20000;2=62500;3=105000;4=147500;5=190000;
                                           6=220000;7=255000;8=300000;9=340000;10=380000;11=432500;12=502500;
                                           13=602500;14=757500;15=1075000;16=1300000;NA=NA")
elsoc_panel_02$m14_w02_temp <- car::recode(elsoc_panel_02$m14_w02,"1=20000;2=62500;3=105000;4=147500;5=190000;
                                           6=220000;7=255000;8=300000;9=340000;10=380000;11=432500;12=502500;
                                           13=602500;14=757500;15=1075000;16=1300000;NA=NA")
elsoc_panel_02$m14_w03_temp <- car::recode(elsoc_panel_02$m14_w03,"1=20000;2=62500;3=105000;4=147500;5=190000;
                                           6=220000;7=255000;8=300000;9=340000;10=380000;11=432500;12=502500;
                                           13=602500;14=757500;15=1075000;16=1300000;NA=NA")

#Combinar ingreso numérico y de tramos en una variable
elsoc_panel_02$m13_w01_temp <- ifelse(is.na(elsoc_panel_02$m13_w01)==TRUE,elsoc_panel_02$m14_w01_temp,elsoc_panel_02$m13_w01)
elsoc_panel_02$m13_w02_temp <- ifelse(is.na(elsoc_panel_02$m13_w02)==TRUE,elsoc_panel_02$m14_w02_temp,elsoc_panel_02$m13_w02)
elsoc_panel_02$m13_w03_temp <- ifelse(is.na(elsoc_panel_02$m13_w03)==TRUE,elsoc_panel_02$m14_w03_temp,elsoc_panel_02$m13_w03)

#Inflactar a precios de Diciembre de 2018
elsoc_panel_02$m13_w01rec1 <- elsoc_panel_02$m13_w01_temp*(119.45/113.88)
elsoc_panel_02$m13_w02rec1 <- elsoc_panel_02$m13_w02_temp*(119.45/116.46)
elsoc_panel_02$m13_w03rec1 <- elsoc_panel_02$m13_w03_temp

elsoc_panel_02$m13_w01rec2 <- elsoc_panel_02$m13_w01*(119.45/113.88)
elsoc_panel_02$m13_w02rec2 <- elsoc_panel_02$m13_w02*(119.45/116.46)
elsoc_panel_02$m13_w03rec2 <- elsoc_panel_02$m13_w03

#summary(is.na(elsoc_panel_02$m13_w01_rec1))  #58.54%  52.10%
#summary(is.na(elsoc_panel_02$m13_w02_rec1))  #60.88%  56.68%
#summary(is.na(elsoc_panel_02$m13_w03_rec1))  #59.92%  56.06%


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

elsoc_panel_02$m29_w01rec2 <- elsoc_panel_02$m29_w01*(119.45/113.88)
elsoc_panel_02$m29_w02rec2 <- elsoc_panel_02$m29_w02*(119.45/116.46)
elsoc_panel_02$m29_w03rec2 <- elsoc_panel_02$m29_w03

#summary(is.na(elsoc_panel_02$m29_w01rec1))  #95.09%  81.35%
#summary(is.na(elsoc_panel_02$m29_w02rec1))  #97.66%  88.55%
#summary(is.na(elsoc_panel_02$m29_w01rec1))  #96.18%  85.35%


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

#summary(is.na(elsoc_panel_02$ing_pc_w01))  #95.09%
#summary(is.na(elsoc_panel_02$ing_pc_w02))  #84.93%
#summary(is.na(elsoc_panel_02$ing_pc_w03))  #96.04%


#Generar Participación laboral (1 = Trabaja Full Tiempo, Jornada Parcial, Estudiay Trabaja, Desempleado y Busca empleo)
elsoc_panel_02$empleo_w01 <- car::recode(elsoc_panel_02$m02_w01,"c(1,2,3,6)=1;c(4,5,7,8,9)=0")
elsoc_panel_02$empleo_w02 <- car::recode(elsoc_panel_02$m02_w02,"c(1,2,3,6)=1;c(4,5,7,8,9)=0")
elsoc_panel_02$empleo_w03 <- car::recode(elsoc_panel_02$m02_w03,"c(1,2,3,6)=1;c(4,5,7,8,9)=0")


#Generar Participación laboral (1 = Trabaja Full Tiempo, Jornada Parcial, Estudiay Trabaja)
elsoc_panel_02$trabaja_w01 <- car::recode(elsoc_panel_02$m02_w01,"c(1,2,3)=1;c(4,5,6,7,8,9)=0")
elsoc_panel_02$trabaja_w02 <- car::recode(elsoc_panel_02$m02_w02,"c(1,2,3)=1;c(4,5,6,7,8,9)=0")
elsoc_panel_02$trabaja_w03 <- car::recode(elsoc_panel_02$m02_w03,"c(1,2,3)=1;c(4,5,6,7,8,9)=0")


#Generar Línea de Pobreza (1 si la persona está bajo los valores de la línea de pobreza -Diciembre de cada año- 
#                          según Nª  de integrantes del hogar).

elsoc_panel_02$pob_tm1_w01 <- if_else(condition = elsoc_panel_02$m29_w01rec1<=(154709*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==1,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w01rec1<=(251326*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==2,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w01rec1<=(333811*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==3,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w01rec1<=(408280*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==4,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w01rec1<=(477304*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==5,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w01rec1<=(542278*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==6,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w01rec1<=(604066*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==7,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w01rec1<=(663253*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==8,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w01rec1<=(720254*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==9,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w01rec1<=(775382*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==10, true = 1,
                      false = if_else(condition = elsoc_panel_02$nhogar_w01>10, true = 2,
                      false = if_else(condition = is.na(elsoc_panel_02$m29_w01rec1)==TRUE,2,0))))))))))))
elsoc_panel_02$pobreza_01_w01 <- car::recode(elsoc_panel_02$pob_tm1_w01,"1=1;0=0;2=NA")

elsoc_panel_02$pob_tm2_w01 <- if_else(condition = elsoc_panel_02$m29_w01rec2<=(154709*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==1,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w01rec2<=(251326*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==2,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w01rec2<=(333811*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==3,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w01rec2<=(408280*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==4,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w01rec2<=(477304*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==5,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w01rec2<=(542278*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==6,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w01rec2<=(604066*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==7,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w01rec2<=(663253*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==8,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w01rec2<=(720254*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==9,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w01rec2<=(775382*(119.45/113.88)) & elsoc_panel_02$nhogar_w01==10, true = 1,
                      false = if_else(condition = elsoc_panel_02$nhogar_w01>10, true = 2,
                      false = if_else(condition = is.na(elsoc_panel_02$m29_w01rec2)==TRUE,2,0))))))))))))
elsoc_panel_02$pobreza_02_w01 <- car::recode(elsoc_panel_02$pob_tm2_w01,"1=1;0=0;2=NA")

elsoc_panel_02$pob_tm1_w02 <- if_else(condition = elsoc_panel_02$m29_w02rec1<=(158479*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==1,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w02rec1<=(257450*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==2,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w02rec1<=(341945*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==3,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w02rec1<=(418228*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==4,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w02rec1<=(488934*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==5,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w02rec1<=(555491*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==6,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w02rec1<=(618785*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==7,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w02rec1<=(679414*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==8,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w02rec1<=(737804*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==9,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w02rec1<=(794276*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==10, true = 1,
                      false = if_else(condition = elsoc_panel_02$nhogar_w02>10, true = 2,
                      false = if_else(condition = is.na(elsoc_panel_02$m29_w02rec1)==TRUE,2,0))))))))))))
elsoc_panel_02$pobreza_01_w02 <- car::recode(elsoc_panel_02$pob_tm1_w02,"1=1;0=0;2=NA")

elsoc_panel_02$pob_tm2_w02 <- if_else(condition = elsoc_panel_02$m29_w02rec2<=(158479*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==1,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w02rec2<=(257450*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==2,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w02rec2<=(341945*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==3,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w02rec2<=(418228*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==4,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w02rec2<=(488934*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==5,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w02rec2<=(555491*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==6,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w02rec2<=(618785*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==7,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w02rec2<=(679414*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==8,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w02rec2<=(737804*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==9,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w02rec2<=(794276*(119.45/116.46)) & elsoc_panel_02$nhogar_w02==10, true = 1,
                      false = if_else(condition = elsoc_panel_02$nhogar_w02>10, true = 2,
                      false = if_else(condition = is.na(elsoc_panel_02$m29_w02rec2)==TRUE,2,0))))))))))))
elsoc_panel_02$pobreza_02_w02 <- car::recode(elsoc_panel_02$pob_tm2_w02,"1=1;0=0;2=NA")

elsoc_panel_02$pob_tm1_w03 <- if_else(condition = elsoc_panel_02$m29_w03rec1<=(163044) & elsoc_panel_02$nhogar_w03==1,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w03rec1<=(264865) & elsoc_panel_02$nhogar_w03==2,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w03rec1<=(351795) & elsoc_panel_02$nhogar_w03==3,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w03rec1<=(430275) & elsoc_panel_02$nhogar_w03==4,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w03rec1<=(503018) & elsoc_panel_02$nhogar_w03==5,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w03rec1<=(571492) & elsoc_panel_02$nhogar_w03==6,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w03rec1<=(636609) & elsoc_panel_02$nhogar_w03==7,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w03rec1<=(698984) & elsoc_panel_02$nhogar_w03==8,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w03rec1<=(759056) & elsoc_panel_02$nhogar_w03==9,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w03rec1<=(817155) & elsoc_panel_02$nhogar_w03==10, true = 1,
                      false = if_else(condition = elsoc_panel_02$nhogar_w03>10, true = 2,
                      false = if_else(condition = is.na(elsoc_panel_02$m29_w03rec1)==TRUE,2,0))))))))))))
elsoc_panel_02$pobreza_01_w03 <- car::recode(elsoc_panel_02$pob_tm1_w03,"1=1;0=0;2=NA")

elsoc_panel_02$pob_tm2_w03 <- if_else(condition = elsoc_panel_02$m29_w03rec2<=(163044) & elsoc_panel_02$nhogar_w03==1,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w03rec2<=(264865) & elsoc_panel_02$nhogar_w03==2,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w03rec2<=(351795) & elsoc_panel_02$nhogar_w03==3,  true = 1, 
                      false = if_else(condition = elsoc_panel_02$m29_w03rec2<=(430275) & elsoc_panel_02$nhogar_w03==4,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w03rec2<=(503018) & elsoc_panel_02$nhogar_w03==5,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w03rec2<=(571492) & elsoc_panel_02$nhogar_w03==6,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w03rec2<=(636609) & elsoc_panel_02$nhogar_w03==7,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w03rec2<=(698984) & elsoc_panel_02$nhogar_w03==8,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w03rec2<=(759056) & elsoc_panel_02$nhogar_w03==9,  true = 1,
                      false = if_else(condition = elsoc_panel_02$m29_w03rec2<=(817155) & elsoc_panel_02$nhogar_w03==10, true = 1,
                      false = if_else(condition = elsoc_panel_02$nhogar_w03>10, true = 2,
                      false = if_else(condition = is.na(elsoc_panel_02$m29_w03rec2)==TRUE,2,0))))))))))))
elsoc_panel_02$pobreza_02_w03 <- car::recode(elsoc_panel_02$pob_tm2_w03,"1=1;0=0;2=NA")

#Edad
elsoc_panel_02$edad_rec_w01 <- car::recode(elsoc_panel_02$m0_edad_w01, "18:29=1;30:49=2;50:64=3;65:100=4")
elsoc_panel_02$edad_rec_w02 <- car::recode(elsoc_panel_02$m0_edad_w02, "18:29=1;30:49=2;50:64=3;65:100=4")
elsoc_panel_02$edad_rec_w03 <- car::recode(elsoc_panel_02$m0_edad_w03, "18:29=1;30:49=2;50:64=3;65:100=4")

#Educación Entrevistado
elsoc_panel_02$educ_rec2_w01 <- car::recode(elsoc_panel_02$m01_w01, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'")
elsoc_panel_02$educ_rec2_w02 <- car::recode(elsoc_panel_02$m01_w02, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'")
elsoc_panel_02$educ_rec2_w03 <- car::recode(elsoc_panel_02$m01_w03, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'")
elsoc_panel_02$educ_rec2_w01 <- factor(elsoc_panel_02$educ_rec2_w01, levels = c("Basica","Media","Tecnica","Universitaria"))
elsoc_panel_02$educ_rec2_w02 <- factor(elsoc_panel_02$educ_rec2_w02, levels = c("Basica","Media","Tecnica","Universitaria"))
elsoc_panel_02$educ_rec2_w03 <- factor(elsoc_panel_02$educ_rec2_w03, levels = c("Basica","Media","Tecnica","Universitaria"))

#Identificación Indígena
elsoc_panel_02$ind_w02  <- car::recode(elsoc_panel_02$m53_w02,"1:9=1;10=0;11=1")
elsoc_panel_02$ind_w03  <- car::recode(elsoc_panel_02$m53_w03,"1:9=1;10=0;11=1")
elsoc_panel_02$ind_pers <- ifelse(elsoc_panel_02$ind_w02 == 0 & elsoc_panel_02$ind_w03 == 0 ,"No Indígena" ,"Indigena")

#Educación de Padres
elsoc_panel_02$ed_padre1  <- car::recode(elsoc_panel_02$m27_w01, "c(1,2,3)=0;c(4,5)=1;c(6,7)=2;c(8,9,10)=3")
elsoc_panel_02$ed_padre2  <- car::recode(elsoc_panel_02$m27_w01, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'")
elsoc_panel_02$ed_padre2  <- factor(elsoc_panel_02$ed_padre2, levels = c("Basica","Media","Tecnica","Universitaria"))

elsoc_panel_02$ed_madre1  <- car::recode(elsoc_panel_02$m28_w01, "c(1,2,3)=0;c(4,5)=1;c(6,7)=2;c(8,9,10)=3")
elsoc_panel_02$ed_madre2  <- car::recode(elsoc_panel_02$m28_w01, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'")
elsoc_panel_02$ed_madre2  <- factor(elsoc_panel_02$ed_madre2, levels = c("Basica","Media","Tecnica","Universitaria"))

elsoc_panel_02$ed_padres  <- if_else(elsoc_panel_02$ed_padre1 > elsoc_panel_02$ed_madre1, elsoc_panel_02$ed_padre2, elsoc_panel_02$ed_madre2)

#Amigos: 
elsoc_panel_02$amigos <- car::recode(elsoc_panel_02$r15_w02,"c(1,2)='Baja';3='Media';c(4,5)='Alta'")
elsoc_panel_02$amigos <- factor(elsoc_panel_02$amigos,levels = c("Baja","Media","Alta"))

#Organizaciones Voluntarias
elsoc_panel_02$c12_01_w01rec1 <- car::recode(elsoc_panel_02$c12_01_w01,"1=0;c(2,3)=1")
elsoc_panel_02$c12_01_w01rec2 <- car::recode(elsoc_panel_02$c12_01_w01,"1=0;2=1;3=2")
elsoc_panel_02$c12_02_w01rec1 <- car::recode(elsoc_panel_02$c12_02_w01,"1=0;c(2,3)=1")
elsoc_panel_02$c12_02_w01rec2 <- car::recode(elsoc_panel_02$c12_02_w01,"1=0;2=1;3=2")
elsoc_panel_02$c12_03_w01rec1 <- car::recode(elsoc_panel_02$c12_03_w01,"1=0;c(2,3)=1")
elsoc_panel_02$c12_03_w01rec2 <- car::recode(elsoc_panel_02$c12_03_w01,"1=0;2=1;3=2")
elsoc_panel_02$c12_04_w01rec1 <- car::recode(elsoc_panel_02$c12_04_w01,"1=0;c(2,3)=1")
elsoc_panel_02$c12_04_w01rec2 <- car::recode(elsoc_panel_02$c12_04_w01,"1=0;2=1;3=2")
elsoc_panel_02$c12_05_w01rec1 <- car::recode(elsoc_panel_02$c12_05_w01,"1=0;c(2,3)=1")
elsoc_panel_02$c12_05_w01rec2 <- car::recode(elsoc_panel_02$c12_05_w01,"1=0;2=1;3=2")
elsoc_panel_02$c12_06_w01rec1 <- car::recode(elsoc_panel_02$c12_06_w01,"1=0;c(2,3)=1")
elsoc_panel_02$c12_06_w01rec2 <- car::recode(elsoc_panel_02$c12_06_w01,"1=0;2=1;3=2")
elsoc_panel_02$c12_07_w01rec1 <- car::recode(elsoc_panel_02$c12_07_w01,"1=0;c(2,3)=1")
elsoc_panel_02$c12_07_w01rec2 <- car::recode(elsoc_panel_02$c12_07_w01,"1=0;2=1;3=2")
elsoc_panel_02$c12_08_w01rec1 <- car::recode(elsoc_panel_02$c12_08_w01,"1=0;c(2,3)=1")
elsoc_panel_02$c12_08_w01rec2 <- car::recode(elsoc_panel_02$c12_08_w01,"1=0;2=1;3=2")
elsoc_panel_02$c12_09_w01rec1 <- car::recode(elsoc_panel_02$c12_09_w01,"1=0;c(2,3)=1")
elsoc_panel_02$c12_09_w01rec2 <- car::recode(elsoc_panel_02$c12_09_w01,"1=0;2=1;3=2")

elsoc_panel_02$c12_01_w03rec1 <- car::recode(elsoc_panel_02$c12_01_w03,"1=0;c(2,3)=1")
elsoc_panel_02$c12_01_w03rec2 <- car::recode(elsoc_panel_02$c12_01_w03,"1=0;2=1;3=2")
elsoc_panel_02$c12_02_w03rec1 <- car::recode(elsoc_panel_02$c12_02_w03,"1=0;c(2,3)=1")
elsoc_panel_02$c12_02_w03rec2 <- car::recode(elsoc_panel_02$c12_02_w03,"1=0;2=1;3=2")
elsoc_panel_02$c12_03_w03rec1 <- car::recode(elsoc_panel_02$c12_03_w03,"1=0;c(2,3)=1")
elsoc_panel_02$c12_03_w03rec2 <- car::recode(elsoc_panel_02$c12_03_w03,"1=0;2=1;3=2")
elsoc_panel_02$c12_04_w03rec1 <- car::recode(elsoc_panel_02$c12_04_w03,"1=0;c(2,3)=1")
elsoc_panel_02$c12_04_w03rec2 <- car::recode(elsoc_panel_02$c12_04_w03,"1=0;2=1;3=2")
elsoc_panel_02$c12_05_w03rec1 <- car::recode(elsoc_panel_02$c12_05_w03,"1=0;c(2,3)=1")
elsoc_panel_02$c12_05_w03rec2 <- car::recode(elsoc_panel_02$c12_05_w03,"1=0;2=1;3=2")
elsoc_panel_02$c12_06_w03rec1 <- car::recode(elsoc_panel_02$c12_06_w03,"1=0;c(2,3)=1")
elsoc_panel_02$c12_06_w03rec2 <- car::recode(elsoc_panel_02$c12_06_w03,"1=0;2=1;3=2")
elsoc_panel_02$c12_07_w03rec1 <- car::recode(elsoc_panel_02$c12_07_w03,"1=0;c(2,3)=1")
elsoc_panel_02$c12_07_w03rec2 <- car::recode(elsoc_panel_02$c12_07_w03,"1=0;2=1;3=2")
elsoc_panel_02$c12_08_w03rec1 <- car::recode(elsoc_panel_02$c12_08_w03,"1=0;c(2,3)=1")
elsoc_panel_02$c12_08_w03rec2 <- car::recode(elsoc_panel_02$c12_08_w03,"1=0;2=1;3=2")
elsoc_panel_02$c12_09_w03rec1 <- car::recode(elsoc_panel_02$c12_09_w03,"1=0;c(2,3)=1")
elsoc_panel_02$c12_09_w03rec2 <- car::recode(elsoc_panel_02$c12_09_w03,"1=0;2=1;3=2")

elsoc_panel_02$org_vol_01_w01 <- elsoc_panel_02$c12_01_w01rec1 + elsoc_panel_02$c12_02_w01rec1 + elsoc_panel_02$c12_03_w01rec1 + 
                                 elsoc_panel_02$c12_04_w01rec1 + elsoc_panel_02$c12_05_w01rec1 + elsoc_panel_02$c12_06_w01rec1 + 
                                 elsoc_panel_02$c12_07_w01rec1 + elsoc_panel_02$c12_08_w01rec1 + elsoc_panel_02$c12_09_w01rec1

elsoc_panel_02$org_vol_02_w01 <- elsoc_panel_02$c12_01_w01rec2 + elsoc_panel_02$c12_02_w01rec2 + elsoc_panel_02$c12_03_w01rec2 + 
                                 elsoc_panel_02$c12_04_w01rec2 + elsoc_panel_02$c12_05_w01rec2 + elsoc_panel_02$c12_06_w01rec2 + 
                                 elsoc_panel_02$c12_07_w01rec2 + elsoc_panel_02$c12_08_w01rec2 + elsoc_panel_02$c12_09_w01rec2

elsoc_panel_02$org_vol_01_w03 <- elsoc_panel_02$c12_01_w03rec1 + elsoc_panel_02$c12_02_w03rec1 + elsoc_panel_02$c12_03_w03rec1 + 
                                 elsoc_panel_02$c12_04_w03rec1 + elsoc_panel_02$c12_05_w03rec1 + elsoc_panel_02$c12_06_w03rec1 + 
                                 elsoc_panel_02$c12_07_w03rec1 + elsoc_panel_02$c12_08_w03rec1 + elsoc_panel_02$c12_09_w03rec1

elsoc_panel_02$org_vol_02_w03 <- elsoc_panel_02$c12_01_w03rec2 + elsoc_panel_02$c12_02_w03rec2 + elsoc_panel_02$c12_03_w03rec2 + 
                                 elsoc_panel_02$c12_04_w03rec2 + elsoc_panel_02$c12_05_w03rec2 + elsoc_panel_02$c12_06_w03rec2 + 
                                 elsoc_panel_02$c12_07_w03rec2 + elsoc_panel_02$c12_08_w03rec2 + elsoc_panel_02$c12_09_w03rec2


elsoc_panel_02$soc_trust_01_w01 <- car::recode(elsoc_panel_02$c02_w01,"1='Confia';c(2,3)='No confia'")
elsoc_panel_02$soc_trust_01_w01 <- factor(elsoc_panel_02$soc_trust_01_w01,levels = c("No confia","Confia"))
elsoc_panel_02$soc_trust_01_w02 <- car::recode(elsoc_panel_02$c02_w02,"1='Confia';c(2,3)='No confia'")
elsoc_panel_02$soc_trust_01_w02 <- factor(elsoc_panel_02$soc_trust_01_w02,levels = c("No confia","Confia"))
elsoc_panel_02$soc_trust_01_w03 <- car::recode(elsoc_panel_02$c02_w03,"1='Confia';c(2,3)='No confia'")
elsoc_panel_02$soc_trust_01_w03 <- factor(elsoc_panel_02$soc_trust_01_w03,levels = c("No confia","Confia"))

elsoc_panel_02$soc_trust_02_w01 <- car::recode(elsoc_panel_02$c03_w01,"1='Confia';c(2,3)='No confia'")
elsoc_panel_02$soc_trust_02_w01 <- factor(elsoc_panel_02$soc_trust_02_w01,levels = c("No confia","Confia"))
elsoc_panel_02$soc_trust_02_w02 <- car::recode(elsoc_panel_02$c03_w02,"1='Confia';c(2,3)='No confia'")
elsoc_panel_02$soc_trust_02_w02 <- factor(elsoc_panel_02$soc_trust_02_w02,levels = c("No confia","Confia"))
elsoc_panel_02$soc_trust_02_w03 <- car::recode(elsoc_panel_02$c03_w03,"1='Confia';c(2,3)='No confia'")
elsoc_panel_02$soc_trust_02_w03 <- factor(elsoc_panel_02$soc_trust_02_w03,levels = c("No confia","Confia"))

elsoc_panel_02$soc_trust_03_w01 <- car::recode(elsoc_panel_02$c04_w01,"1='Confia';c(2,3)='No confia'")
elsoc_panel_02$soc_trust_03_w01 <- factor(elsoc_panel_02$soc_trust_03_w01,levels = c("No confia","Confia"))
elsoc_panel_02$soc_trust_03_w02 <- car::recode(elsoc_panel_02$c04_w02,"1='Confia';c(2,3)='No confia'")
elsoc_panel_02$soc_trust_03_w02 <- factor(elsoc_panel_02$soc_trust_03_w02,levels = c("No confia","Confia"))
elsoc_panel_02$soc_trust_03_w03 <- car::recode(elsoc_panel_02$c04_w03,"1='Confia';c(2,3)='No confia'")
elsoc_panel_02$soc_trust_03_w03 <- factor(elsoc_panel_02$soc_trust_03_w03,levels = c("No confia","Confia"))


###############################################################################################################################################################################
#PARTICIPACIÓN LABORAL

#Filtrar casos con empleo
elsoc_panel_02 %>% dplyr::filter(is.na(empleo_w01)==FALSE & is.na(empleo_w02)==FALSE & is.na(empleo_w03)==FALSE) -> elsoc_labor

csd_emp_01 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_labor)

svytable(formula = ~empleo_w01, design = csd_emp_01, Ntotal =2085)
svytable(formula = ~empleo_w01, design = csd_emp_01, Ntotal =2085)
svytable(formula = ~empleo_w01, design = csd_emp_01, Ntotal =2085)

svytable(formula = ~m0_sexo_w01+empleo_w01, design = csd_emp_01, Ntotal =2085)
svytable(formula = ~m0_sexo_w02+empleo_w02, design = csd_emp_01, Ntotal =2085)
svytable(formula = ~m0_sexo_w03+empleo_w03, design = csd_emp_01, Ntotal =2085)

svytable(formula = ~edad_rec_w01+empleo_w01, design = csd_emp_01, Ntotal =2085)
svytable(formula = ~edad_rec_w01+empleo_w02, design = csd_emp_01, Ntotal =2085)
svytable(formula = ~edad_rec_w01+empleo_w03, design = csd_emp_01, Ntotal =2085)

svytable(formula = ~educ_rec2_w01+empleo_w01, design = csd_emp_01, Ntotal =2085)
svytable(formula = ~educ_rec2_w01+empleo_w02, design = csd_emp_01, Ntotal =2085)
svytable(formula = ~educ_rec2_w01+empleo_w03, design = csd_emp_01, Ntotal =2085)

svytable(formula = ~ind_pers+empleo_w01, design = csd_emp_01, Ntotal =2045)
svytable(formula = ~ind_pers+empleo_w02, design = csd_emp_01, Ntotal =2045)
svytable(formula = ~ind_pers+empleo_w03, design = csd_emp_01, Ntotal =2045)


svytable(formula = ~trabaja_w01, design = csd_emp_01, Ntotal =2085)
svytable(formula = ~trabaja_w02, design = csd_emp_01, Ntotal =2085)
svytable(formula = ~trabaja_w03, design = csd_emp_01, Ntotal =2085)

svytable(formula = ~m0_sexo_w01+trabaja_w01, design = csd_emp_01, Ntotal =2085)
svytable(formula = ~m0_sexo_w02+trabaja_w02, design = csd_emp_01, Ntotal =2085)
svytable(formula = ~m0_sexo_w03+trabaja_w03, design = csd_emp_01, Ntotal =2085)

svytable(formula = ~edad_rec_w01+trabaja_w01, design = csd_emp_01, Ntotal =2085)
svytable(formula = ~edad_rec_w01+trabaja_w02, design = csd_emp_01, Ntotal =2085)
svytable(formula = ~edad_rec_w01+trabaja_w03, design = csd_emp_01, Ntotal =2085)

svytable(formula = ~educ_rec2_w01+trabaja_w01, design = csd_emp_01, Ntotal =2085)
svytable(formula = ~educ_rec2_w01+trabaja_w02, design = csd_emp_01, Ntotal =2085)
svytable(formula = ~educ_rec2_w01+trabaja_w03, design = csd_emp_01, Ntotal =2085)

svytable(formula = ~ind_pers+trabaja_w01, design = csd_emp_01, Ntotal =2045)
svytable(formula = ~ind_pers+trabaja_w02, design = csd_emp_01, Ntotal =2045)
svytable(formula = ~ind_pers+trabaja_w03, design = csd_emp_01, Ntotal =2045)



###############################################################################################################################################################################
#INGRESOS DEL TRABAJO

#Filtrar casos con ingresos del trabajo
elsoc_panel_02 %>% dplyr::filter(is.na(m13_w01rec1)==FALSE & is.na(m13_w02rec1)==FALSE & is.na(m13_w03rec1)==FALSE) -> elsoc_ing1
#elsoc_panel_02 %>% dplyr::filter(is.na(m13_w01rec2)==FALSE & is.na(m13_w02rec2)==FALSE & is.na(m13_w03rec2)==FALSE) -> elsoc_ing2


#Declaración de Diseño Muestral Complejo
csd_ing_01 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_ing1)
#csd_ing_02 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_ing2)

ingresos <- data.frame(tipo = c("Total","Hombre","Mujer","Básica","Media","Técnica","Universitaria", "Indigena","No Indigena",
                                "Padre Basica","Padre Media","Padre Tecnica","Padre Universitaria"))

ingresos$Media_2016 <- unlist(c(svymean(x = ~m13_w01rec1, design = csd_ing_01, deff = FALSE, na.rm = TRUE)[1],
                                svyby(~m13_w01rec1, ~m0_sexo_w01,   csd_ing_01,svymean)[2],
                                svyby(~m13_w01rec1, ~educ_rec2_w01, csd_ing_01,svymean)[2],
                                svyby(~m13_w01rec1, ~ind_pers, csd_ing_01,svymean)[2],
                                svyby(~m13_w01rec1, ~ed_padres, csd_ing_01,svymean)[2]))

ingresos$SE_2016 <- unlist(c(28197,
                             svyby(~m13_w01rec1, ~m0_sexo_w01,   csd_ing_01,svymean)[3],
                             svyby(~m13_w01rec1, ~educ_rec2_w01, csd_ing_01,svymean)[3],
                             svyby(~m13_w01rec1, ~ind_pers, csd_ing_01,svymean)[3],
                             svyby(~m13_w01rec1, ~ed_padres, csd_ing_01,svymean)[3]))

ingresos$Min_2016 <- unlist(c(confint(svymean(x = ~m13_w01rec1, design = csd_ing_01, deff = FALSE, na.rm = TRUE))[1],
                              confint(svyby(~m13_w01rec1, ~m0_sexo_w01,   csd_ing_01,svymean))[,1],
                              confint(svyby(~m13_w01rec1, ~educ_rec2_w01,   csd_ing_01,svymean))[,1],
                              confint(svyby(~m13_w01rec1, ~ind_pers,   csd_ing_01,svymean))[,1],
                              confint(svyby(~m13_w01rec1, ~ed_padres,   csd_ing_01,svymean))[,1]))

ingresos$Max_2016 <- unlist(c(confint(svymean(x = ~m13_w01rec1, design = csd_ing_01, deff = FALSE, na.rm = TRUE))[2],
                              confint(svyby(~m13_w01rec1, ~m0_sexo_w01,   csd_ing_01,svymean))[,2],
                              confint(svyby(~m13_w01rec1, ~educ_rec2_w01,   csd_ing_01,svymean))[,2],
                              confint(svyby(~m13_w01rec1, ~ind_pers,   csd_ing_01,svymean))[,2],
                              confint(svyby(~m13_w01rec1, ~ed_padres,   csd_ing_01,svymean))[,2]))



ingresos$Media_2017 <- unlist(c(svymean(x = ~m13_w02rec1, design = csd_ing_01, deff = FALSE, na.rm = TRUE)[1],
                                svyby(~m13_w02rec1, ~m0_sexo_w01,   csd_ing_01,svymean)[2],
                                svyby(~m13_w02rec1, ~educ_rec2_w02, csd_ing_01,svymean)[2],
                                svyby(~m13_w02rec1, ~ind_pers, csd_ing_01,svymean)[2],
                                svyby(~m13_w02rec1, ~ed_padres, csd_ing_01,svymean)[2]))

ingresos$SE_2017 <- unlist(c(22522,
                             svyby(~m13_w02rec1, ~m0_sexo_w01,   csd_ing_01,svymean)[3],
                             svyby(~m13_w02rec1, ~educ_rec2_w02, csd_ing_01,svymean)[3],
                             svyby(~m13_w02rec1, ~ind_pers, csd_ing_01,svymean)[3],
                             svyby(~m13_w02rec1, ~ed_padres, csd_ing_01,svymean)[3]))

ingresos$Min_2017 <- unlist(c(confint(svymean(x = ~m13_w02rec1, design = csd_ing_01, deff = FALSE, na.rm = TRUE))[1],
                              confint(svyby(~m13_w02rec1, ~m0_sexo_w01,   csd_ing_01,svymean))[,1],
                              confint(svyby(~m13_w02rec1, ~educ_rec2_w02,   csd_ing_01,svymean))[,1],
                              confint(svyby(~m13_w02rec1, ~ind_pers,   csd_ing_01,svymean))[,1],
                              confint(svyby(~m13_w02rec1, ~ed_padres,   csd_ing_01,svymean))[,1]))

ingresos$Max_2017 <- unlist(c(confint(svymean(x = ~m13_w02rec1, design = csd_ing_01, deff = FALSE, na.rm = TRUE))[2],
                              confint(svyby(~m13_w02rec1, ~m0_sexo_w01,   csd_ing_01,svymean))[,2],
                              confint(svyby(~m13_w02rec1, ~educ_rec2_w02,   csd_ing_01,svymean))[,2],
                              confint(svyby(~m13_w02rec1, ~ind_pers,   csd_ing_01,svymean))[,2],
                              confint(svyby(~m13_w02rec1, ~ed_padres,   csd_ing_01,svymean))[,2]))



ingresos$Media_2018 <- unlist(c(svymean(x = ~m13_w03rec1, design = csd_ing_01, deff = FALSE, na.rm = TRUE)[1],
                                svyby(~m13_w03rec1, ~m0_sexo_w01,   csd_ing_01,svymean)[2],
                                svyby(~m13_w03rec1, ~educ_rec2_w03, csd_ing_01,svymean)[2],
                                svyby(~m13_w03rec1, ~ind_pers, csd_ing_01,svymean)[2],
                                svyby(~m13_w03rec1, ~ed_padres, csd_ing_01,svymean)[2]))

ingresos$SE_2018 <- unlist(c(41558,
                             svyby(~m13_w03rec1, ~m0_sexo_w01,   csd_ing_01,svymean)[3],
                             svyby(~m13_w03rec1, ~educ_rec2_w03, csd_ing_01,svymean)[3],
                             svyby(~m13_w03rec1, ~ind_pers, csd_ing_01,svymean)[3],
                             svyby(~m13_w03rec1, ~ed_padres, csd_ing_01,svymean)[3]))

ingresos$Min_2018 <- unlist(c(confint(svymean(x = ~m13_w03rec1, design = csd_ing_01, deff = FALSE, na.rm = TRUE))[1],
                              confint(svyby(~m13_w03rec1, ~m0_sexo_w01,   csd_ing_01,svymean))[,1],
                              confint(svyby(~m13_w02rec1, ~educ_rec2_w03,   csd_ing_01,svymean))[,1],
                              confint(svyby(~m13_w03rec1, ~ind_pers,   csd_ing_01,svymean))[,1],
                              confint(svyby(~m13_w03rec1, ~ed_padres,   csd_ing_01,svymean))[,1]))

ingresos$Max_2018 <- unlist(c(confint(svymean(x = ~m13_w03rec1, design = csd_ing_01, deff = FALSE, na.rm = TRUE))[2],
                              confint(svyby(~m13_w03rec1, ~m0_sexo_w01,   csd_ing_01,svymean))[,2],
                              confint(svyby(~m13_w03rec1, ~educ_rec2_w03,   csd_ing_01,svymean))[,2],
                              confint(svyby(~m13_w03rec1, ~ind_pers,   csd_ing_01,svymean))[,2],
                              confint(svyby(~m13_w03rec1, ~ed_padres,   csd_ing_01,svymean))[,2]))


ingresos_long <- data.frame(tipo = rep(ingresos$tipo,3), wave = c(rep(2016,13), rep(2017,13), rep(2018,13)),
                            media = c(ingresos$Media_2016, ingresos$Media_2017, ingresos$Media_2018),
                            SE =    c(ingresos$SE_2016, ingresos$SE_2017, ingresos$SE_2018),
                            min =   c(ingresos$Min_2016, ingresos$Min_2017, ingresos$Min_2018),
                            max =   c(ingresos$Max_2016, ingresos$Max_2017, ingresos$Max_2018))


###############################################################################################################################################################################
#POBREZA: 1628 casos

#Filtrar casos con pobreza
elsoc_panel_02 %>% dplyr::filter(is.na(pobreza_01_w01)==FALSE & is.na(pobreza_01_w02)==FALSE & is.na(pobreza_01_w03)==FALSE) -> elsoc_pobreza


#Persistencia Pobreza
elsoc_pobreza$pers_pobr  <- paste0(elsoc_pobreza$pobreza_01_w01,elsoc_pobreza$pobreza_01_w02,elsoc_pobreza$pobreza_01_w03)
elsoc_pobreza$pers_pobr1 <- car::recode(elsoc_pobreza$pers_pobr,"'000'='Nunca Pobre';c('001','010','100')='Una vez Pobre';
                                        c('011','101','110')='Dos veces Pobre';'111'='Siempre Pobre'")
elsoc_pobreza$pers_pobr1 <- factor(elsoc_pobreza$pers_pobr1, levels = c("Nunca Pobre","Una vez Pobre","Dos veces Pobre","Siempre Pobre"))

csd_pobr_01 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_pobreza)

svytable(formula = ~pobreza_01_w01, design = csd_pobr_01, Ntotal = 1628)
svytable(formula = ~pobreza_01_w02, design = csd_pobr_01, Ntotal = 1628)
svytable(formula = ~pobreza_01_w03, design = csd_pobr_01, Ntotal = 1628)

#Persistencia según sexo, educación
svytable(formula = ~pers_pobr, design = csd_pobr_01, Ntotal = 1628)
svytable(formula = ~pers_pobr+m0_sexo_w01, design = csd_pobr_01, Ntotal = 1628)
svytable(formula = ~pers_pobr+educ_rec2_w01, design = csd_pobr_01, Ntotal = 1628)

#Persistencia y redes cercanas
svytable(formula = ~pers_pobr1+amigos, design = csd_pobr_01, Ntotal = 1628)
svychisq( ~pers_pobr1+amigos, design = csd_pobr_01)

#Persistencia y redes lejanas. Primer paso es agregar tamaño de red calculado
elsoc_pobreza1a <- dplyr::left_join(x = elsoc_pobreza,   y = redes1, by ="idencuesta")
elsoc_pobreza2a <- dplyr::left_join(x = elsoc_pobreza1a, y = redes2, by ="idencuesta")

elsoc_pobreza2a %>% dplyr::filter(is.na(scale_up_01.x)==FALSE & is.na(scale_up_01.y)==FALSE) -> elsoc_pobreza_redes
csd_pobr_02 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_pobreza_redes)

#Tamaño de red según perfil de persistencia de pobreza
svyby(~scale_up_01.x, ~pers_pobr1,   csd_pobr_02,svymean)
svyby(~scale_up_01.y, ~pers_pobr1,   csd_pobr_02,svymean)

svyby(~scale_up_02.x, ~pers_pobr1,   csd_pobr_02,svymean)
svyby(~scale_up_02.y, ~pers_pobr1,   csd_pobr_02,svymean)

svyby(~w01_degree_median, ~pers_pobr1,   csd_pobr_02,svymean)
svyby(~w03_degree_median, ~pers_pobr1,   csd_pobr_02,svymean)

#Confianza Social Generalizada
elsoc_pobreza %>% dplyr::filter(is.na(soc_trust_01_w01)==FALSE & is.na(soc_trust_01_w02)==FALSE & is.na(soc_trust_01_w03)==FALSE) -> elsoc_soc_trust_01
elsoc_pobreza %>% dplyr::filter(is.na(soc_trust_02_w01)==FALSE & is.na(soc_trust_02_w02)==FALSE & is.na(soc_trust_02_w03)==FALSE) -> elsoc_soc_trust_02
elsoc_pobreza %>% dplyr::filter(is.na(soc_trust_03_w01)==FALSE & is.na(soc_trust_03_w02)==FALSE & is.na(soc_trust_03_w03)==FALSE) -> elsoc_soc_trust_03

csd_trust_01 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_soc_trust_01)
csd_trust_02 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_soc_trust_02)
csd_trust_03 <- svydesign(ids = ~segmento_w01, strata = ~estrato_w01, weights = ~ponderador02_w01, data = elsoc_soc_trust_03)

svytable(formula = ~pers_pobr1+soc_trust_01_w01, design = csd_pobr_01, Ntotal = 1575)
svytable(formula = ~pers_pobr1+soc_trust_01_w02, design = csd_pobr_01, Ntotal = 1575)
svytable(formula = ~pers_pobr1+soc_trust_01_w03, design = csd_pobr_01, Ntotal = 1575)


###############################################################################################################################################################################
#Modelo Predictivo
library(lme4)
library(nnet)
library(texreg)

#Creación de variables
elsoc_pobreza$temp1   <- paste0(elsoc_pobreza$trabaja_w01, elsoc_pobreza$trabaja_w02, elsoc_pobreza$trabaja_w03)
elsoc_pobreza$trabajo <- car::recode(elsoc_pobreza$temp1, "'000'=0;c('001','010','100')=1;c('011','101','110')=2;'111'=3;else=NA")
elsoc_pobreza$temp2a <- car::recode(elsoc_pobreza$m10_w01,"c(1,2)=1;3=0")
elsoc_pobreza$temp2b <- car::recode(elsoc_pobreza$m10_w03,"c(1,2)=1;3=0")
elsoc_pobreza$temp2c <- paste0(elsoc_pobreza$temp2a, elsoc_pobreza$temp2b)
elsoc_pobreza$contrato <- car::recode(elsoc_pobreza$temp2c,"'11'=2;c('01','10','1NA','NA1')=1;else=0")
elsoc_pobreza$edad2    <- elsoc_pobreza$m0_edad_w01*elsoc_pobreza$m0_edad_w01
elsoc_pobreza$estrato  <- as.factor(elsoc_pobreza$estrato_w01)
elsoc_pobreza$contrato <- as.factor(elsoc_pobreza$contrato)
elsoc_pobreza$ind_pers <- as.factor(elsoc_pobreza$ind_pers)

#Estimación de multinomial logístico
n0 <- multinom(pers_pobr1 ~ 1 + m0_sexo_w01 + m0_edad_w01 + I(m0_edad_w01^2) + educ_rec2_w01 + ed_padres + as.factor(estrato_w01) + ind_pers, data = elsoc_pobreza)
n1 <- multinom(pers_pobr1 ~ 1 + m0_sexo_w01 + m0_edad_w01 + I(m0_edad_w01^2) + educ_rec2_w01 + ed_padres + trabaja_w01 + trabaja_w02 + trabaja_w03 + as.factor(estrato_w01) + ind_pers, data = elsoc_pobreza)
n2 <- multinom(pers_pobr1 ~ 1 + m0_sexo_w01 + m0_edad_w01 + I(m0_edad_w01^2) + educ_rec2_w01 + ed_padres + trabajo + as.factor(estrato_w01) + ind_pers, data = elsoc_pobreza)
n3 <- multinom(pers_pobr1 ~ 1 + m0_sexo_w01 + m0_edad_w01 + I(m0_edad_w01^2) + educ_rec2_w01 + ed_padres + trabajo + as.factor(estrato_w01) + as.factor(contrato) + m54_w03 + ind_pers, data = elsoc_pobreza)
n4 <- multinom(pers_pobr1 ~ 1 + m0_sexo_w01 + m0_edad_w01 + edad2 + educ_rec2_w01 + ed_padres + trabajo + estrato + contrato + m54_w03 + ind_pers, data = elsoc_pobreza)

#Tabla de Resultados
screenreg(list(n0,n1,n2,n3,n4))

#Probabilidades predichas
datos.simulados_01 <- with(elsoc_pobreza, data.frame(m0_sexo_w01=0,m0_edad_w01=46.01,edad2=2116.92,educ_rec2_w01=c("Basica","Media","Tecnica","Universitaria"),
                                                     ed_padres = "Basica",trabajo=3, estrato="1",contrato="0",m54_w03=3.446,ind_pers="No Indígena"))
datos.simulados_02 <- with(elsoc_pobreza, data.frame(m0_sexo_w01=1,m0_edad_w01=46.01,edad2=2116.92,educ_rec2_w01=c("Basica","Media","Tecnica","Universitaria"),
                                                     ed_padres = "Basica",trabajo=3, estrato="1",contrato="0",m54_w03=3.446,ind_pers="No Indígena"))

predict(n4, newdata = datos.simulados_01, "probs")
predict(n4, newdata = datos.simulados_02, "probs")


#Importar bases de datos LONG
elsoc_long <- read_stata(path = "ELSOC_Long_2016_2018_Temporal.dta")

#Eliminar Valores Perdidos en datos LONG (en diseño datos se eliminaron casos)
elsoc_long[elsoc_long == -888] <- NA
elsoc_long[elsoc_long == -999] <- NA

#Ordenar por ID encuesta
elsoc_long %>% dplyr::arrange(desc(idencuesta)) -> elsoc_long

#Imputar punto medio de rangos de ingreso
elsoc_long$m14_temp <- car::recode(elsoc_long$m14,"1=20000;2=62500;3=105000;4=147500;5=190000;
                                           6=220000;7=255000;8=300000;9=340000;10=380000;11=432500;12=502500;
                                           13=602500;14=757500;15=1075000;16=1300000;NA=NA")

#Combinar ingreso numérico y de tramos en una variable
elsoc_long$m13_temp <- ifelse(is.na(elsoc_long$m13)==TRUE,elsoc_long$m14_temp,elsoc_long$m13)

#Inflactar a precios de Diciembre de 2018
elsoc_long$m13_rec1 <- if_else(condition = elsoc_long$wave == 1, true = elsoc_long$m13_temp*(119.45/113.88), 
                               false = if_else(condition = elsoc_long$wave == 2, true = elsoc_long$m13_temp*(119.45/116.46), 
                                               false = if_else(condition = elsoc_long$wave == 3, true = elsoc_long$m13_temp, 
                                                               false = -999999999)))  
elsoc_long$m13_rec2 <- if_else(condition = elsoc_long$wave == 1, true = elsoc_long$m13*(119.45/113.88), 
                               false = if_else(condition = elsoc_long$wave == 2, true = elsoc_long$m13*(119.45/116.46), 
                                               false = if_else(condition = elsoc_long$wave == 3, true = elsoc_long$m13, 
                                                               false = -999999999)))  

#Imputar punto medio de rangos de ingreso
elsoc_long$m30_temp <- car::recode(elsoc_long$m30,"1=110000;2=251000;3=305000;4=355000;5=400000;
                                           6=445000;7=490000;8=535000;9=585000;10=640000;11=700000;12=765000;
                                           13=845000;14=935000;15=1040000;16=1180000;17=1375000;18=1670000;
                                           19=2275000;20=2700000;NA=NA")

#Combinar ingreso numérico y de tramos en una variable
elsoc_long$m29_temp <- ifelse(is.na(elsoc_long$m29)==TRUE, elsoc_long$m30_temp, elsoc_long$m13)

#Inflactar a precios de Diciembre de 2018
elsoc_long$m29_rec1 <- if_else(condition = elsoc_long$wave == 1, true = elsoc_long$m29_temp*(119.45/113.88), 
                               false = if_else(condition = elsoc_long$wave == 2, true = elsoc_long$m29_temp*(119.45/116.46), 
                                               false = if_else(condition = elsoc_long$wave == 3, true = elsoc_long$m29_temp, 
                                                               false = -999999999)))  
elsoc_long$m29_rec2 <- if_else(condition = elsoc_long$wave == 1, true = elsoc_long$m29*(119.45/113.88), 
                               false = if_else(condition = elsoc_long$wave == 2, true = elsoc_long$m29*(119.45/116.46), 
                                               false = if_else(condition = elsoc_long$wave == 3, true = elsoc_long$m29, 
                                                               false = -999999999)))  

#Generar Ingresos Per Capita del Hogar 
elsoc_long$ing_pc_01 <- elsoc_long$m29_rec1/elsoc_long$nhogar
elsoc_long$ing_pc_02 <- elsoc_long$m29_rec2/elsoc_long$nhogar


#Generar Línea de Pobreza (1 si la persona está bajo los valores de la línea de pobreza -Diciembre de cada año- 
#                          según Nª  de integrantes del hogar).
elsoc_long$pob_tm1 <- if_else(elsoc_long$wave==1 & is.na(elsoc_long$m29_rec1)==TRUE,2,
                              if_else(elsoc_long$wave==1 & elsoc_long$nhogar>10, 3,
                                      if_else(elsoc_long$wave==1 & elsoc_long$m29_rec1<=(154709*(119.45/113.88)) & elsoc_long$nhogar==1,  1,
                                              if_else(elsoc_long$wave==1 & elsoc_long$m29_rec1<=(251326*(119.45/113.88)) & elsoc_long$nhogar==2,  1,
                                                      if_else(elsoc_long$wave==1 & elsoc_long$m29_rec1<=(333811*(119.45/113.88)) & elsoc_long$nhogar==3,  1,
                                                              if_else(elsoc_long$wave==1 & elsoc_long$m29_rec1<=(408280*(119.45/113.88)) & elsoc_long$nhogar==4,  1,
                                                                      if_else(elsoc_long$wave==1 & elsoc_long$m29_rec1<=(477304*(119.45/113.88)) & elsoc_long$nhogar==5,  1,
                                                                              if_else(elsoc_long$wave==1 & elsoc_long$m29_rec1<=(542278*(119.45/113.88)) & elsoc_long$nhogar==6,  1,
                                                                                      if_else(elsoc_long$wave==1 & elsoc_long$m29_rec1<=(604066*(119.45/113.88)) & elsoc_long$nhogar==7,  1,
                                                                                              if_else(elsoc_long$wave==1 & elsoc_long$m29_rec1<=(663253*(119.45/113.88)) & elsoc_long$nhogar==8,  1,
                                                                                                      if_else(elsoc_long$wave==1 & elsoc_long$m29_rec1<=(720254*(119.45/113.88)) & elsoc_long$nhogar==9,  1,
                                                                                                              if_else(elsoc_long$wave==1 & elsoc_long$m29_rec1<=(775382*(119.45/113.88)) & elsoc_long$nhogar==10, 1,
                                                                                                                      if_else(elsoc_long$wave==2 & is.na(elsoc_long$m29_rec1)==TRUE,2,        
                                                                                                                              if_else(elsoc_long$wave==2 & elsoc_long$nhogar>10, 3,
                                                                                                                                      if_else(elsoc_long$wave==2 & elsoc_long$m29_rec1<=(158479*(119.45/116.46)) & elsoc_long$nhogar==1,  1,
                                                                                                                                              if_else(elsoc_long$wave==2 & elsoc_long$m29_rec1<=(257450*(119.45/116.46)) & elsoc_long$nhogar==2,  1,
                                                                                                                                                      if_else(elsoc_long$wave==2 & elsoc_long$m29_rec1<=(341945*(119.45/116.46)) & elsoc_long$nhogar==3,  1,
                                                                                                                                                              if_else(elsoc_long$wave==2 & elsoc_long$m29_rec1<=(418228*(119.45/116.46)) & elsoc_long$nhogar==4,  1,
                                                                                                                                                                      if_else(elsoc_long$wave==2 & elsoc_long$m29_rec1<=(488934*(119.45/116.46)) & elsoc_long$nhogar==5,  1,
                                                                                                                                                                              if_else(elsoc_long$wave==2 & elsoc_long$m29_rec1<=(555491*(119.45/116.46)) & elsoc_long$nhogar==6,  1,
                                                                                                                                                                                      if_else(elsoc_long$wave==2 & elsoc_long$m29_rec1<=(618785*(119.45/116.46)) & elsoc_long$nhogar==7,  1,
                                                                                                                                                                                              if_else(elsoc_long$wave==2 & elsoc_long$m29_rec1<=(679414*(119.45/116.46)) & elsoc_long$nhogar==8,  1,
                                                                                                                                                                                                      if_else(elsoc_long$wave==2 & elsoc_long$m29_rec1<=(737804*(119.45/116.46)) & elsoc_long$nhogar==9,  1,
                                                                                                                                                                                                              if_else(elsoc_long$wave==2 & elsoc_long$m29_rec1<=(794276*(119.45/116.46)) & elsoc_long$nhogar==10, 1,
                                                                                                                                                                                                                      if_else(elsoc_long$wave==3 & is.na(elsoc_long$m29_rec1)==TRUE,2,
                                                                                                                                                                                                                              if_else(elsoc_long$wave==3 & elsoc_long$nhogar>10, 3,
                                                                                                                                                                                                                                      if_else(elsoc_long$wave==3 & elsoc_long$m29_rec1<=(163044) & elsoc_long$nhogar==1,   1,
                                                                                                                                                                                                                                              if_else(elsoc_long$wave==3 & elsoc_long$m29_rec1<=(264865) & elsoc_long$nhogar==2,   1,
                                                                                                                                                                                                                                                      if_else(elsoc_long$wave==3 & elsoc_long$m29_rec1<=(351795) & elsoc_long$nhogar==3,   1,
                                                                                                                                                                                                                                                              if_else(elsoc_long$wave==3 & elsoc_long$m29_rec1<=(430275) & elsoc_long$nhogar==4,   1,
                                                                                                                                                                                                                                                                      if_else(elsoc_long$wave==3 & elsoc_long$m29_rec1<=(503018) & elsoc_long$nhogar==5,   1,
                                                                                                                                                                                                                                                                              if_else(elsoc_long$wave==3 & elsoc_long$m29_rec1<=(571492) & elsoc_long$nhogar==6,   1,
                                                                                                                                                                                                                                                                                      if_else(elsoc_long$wave==3 & elsoc_long$m29_rec1<=(636609) & elsoc_long$nhogar==7,   1,
                                                                                                                                                                                                                                                                                              if_else(elsoc_long$wave==3 & elsoc_long$m29_rec1<=(698984) & elsoc_long$nhogar==8,   1,
                                                                                                                                                                                                                                                                                                      if_else(elsoc_long$wave==3 & elsoc_long$m29_rec1<=(759056) & elsoc_long$nhogar==9,   1,
                                                                                                                                                                                                                                                                                                              if_else(elsoc_long$wave==3 & elsoc_long$m29_rec1<=(817155) & elsoc_long$nhogar==10,  1,
                                                                                                                                                                                                                                                                                                                      4))))))))))))))))))))))))))))))))))))

elsoc_long$pobreza_01 <- car::recode(elsoc_long$pob_tm1,"1=1;4=0;3=NA;2=NA")

elsoc_long$pob_tm2 <- if_else(elsoc_long$wave==1 & is.na(elsoc_long$m29_rec2)==TRUE,2,
                              if_else(elsoc_long$wave==1 & elsoc_long$nhogar>10, 3,
                                      if_else(elsoc_long$wave==1 & elsoc_long$m29_rec2<=(154709*(119.45/113.88)) & elsoc_long$nhogar==1,  1,
                                              if_else(elsoc_long$wave==1 & elsoc_long$m29_rec2<=(251326*(119.45/113.88)) & elsoc_long$nhogar==2,  1,
                                                      if_else(elsoc_long$wave==1 & elsoc_long$m29_rec2<=(333811*(119.45/113.88)) & elsoc_long$nhogar==3,  1,
                                                              if_else(elsoc_long$wave==1 & elsoc_long$m29_rec2<=(408280*(119.45/113.88)) & elsoc_long$nhogar==4,  1,
                                                                      if_else(elsoc_long$wave==1 & elsoc_long$m29_rec2<=(477304*(119.45/113.88)) & elsoc_long$nhogar==5,  1,
                                                                              if_else(elsoc_long$wave==1 & elsoc_long$m29_rec2<=(542278*(119.45/113.88)) & elsoc_long$nhogar==6,  1,
                                                                                      if_else(elsoc_long$wave==1 & elsoc_long$m29_rec2<=(604066*(119.45/113.88)) & elsoc_long$nhogar==7,  1,
                                                                                              if_else(elsoc_long$wave==1 & elsoc_long$m29_rec2<=(663253*(119.45/113.88)) & elsoc_long$nhogar==8,  1,
                                                                                                      if_else(elsoc_long$wave==1 & elsoc_long$m29_rec2<=(720254*(119.45/113.88)) & elsoc_long$nhogar==9,  1,
                                                                                                              if_else(elsoc_long$wave==1 & elsoc_long$m29_rec2<=(775382*(119.45/113.88)) & elsoc_long$nhogar==10, 1,
                                                                                                                      if_else(elsoc_long$wave==2 & is.na(elsoc_long$m29_rec2)==TRUE,2,        
                                                                                                                              if_else(elsoc_long$wave==2 & elsoc_long$nhogar>10, 3,
                                                                                                                                      if_else(elsoc_long$wave==2 & elsoc_long$m29_rec2<=(158479*(119.45/116.46)) & elsoc_long$nhogar==1,  1,
                                                                                                                                              if_else(elsoc_long$wave==2 & elsoc_long$m29_rec2<=(257450*(119.45/116.46)) & elsoc_long$nhogar==2,  1,
                                                                                                                                                      if_else(elsoc_long$wave==2 & elsoc_long$m29_rec2<=(341945*(119.45/116.46)) & elsoc_long$nhogar==3,  1,
                                                                                                                                                              if_else(elsoc_long$wave==2 & elsoc_long$m29_rec2<=(418228*(119.45/116.46)) & elsoc_long$nhogar==4,  1,
                                                                                                                                                                      if_else(elsoc_long$wave==2 & elsoc_long$m29_rec2<=(488934*(119.45/116.46)) & elsoc_long$nhogar==5,  1,
                                                                                                                                                                              if_else(elsoc_long$wave==2 & elsoc_long$m29_rec2<=(555491*(119.45/116.46)) & elsoc_long$nhogar==6,  1,
                                                                                                                                                                                      if_else(elsoc_long$wave==2 & elsoc_long$m29_rec2<=(618785*(119.45/116.46)) & elsoc_long$nhogar==7,  1,
                                                                                                                                                                                              if_else(elsoc_long$wave==2 & elsoc_long$m29_rec2<=(679414*(119.45/116.46)) & elsoc_long$nhogar==8,  1,
                                                                                                                                                                                                      if_else(elsoc_long$wave==2 & elsoc_long$m29_rec2<=(737804*(119.45/116.46)) & elsoc_long$nhogar==9,  1,
                                                                                                                                                                                                              if_else(elsoc_long$wave==2 & elsoc_long$m29_rec2<=(794276*(119.45/116.46)) & elsoc_long$nhogar==10, 1,
                                                                                                                                                                                                                      if_else(elsoc_long$wave==3 & is.na(elsoc_long$m29_rec2)==TRUE,2,
                                                                                                                                                                                                                              if_else(elsoc_long$wave==3 & elsoc_long$nhogar>10, 3,
                                                                                                                                                                                                                                      if_else(elsoc_long$wave==3 & elsoc_long$m29_rec2<=(163044) & elsoc_long$nhogar==1,   1,
                                                                                                                                                                                                                                              if_else(elsoc_long$wave==3 & elsoc_long$m29_rec2<=(264865) & elsoc_long$nhogar==2,   1,
                                                                                                                                                                                                                                                      if_else(elsoc_long$wave==3 & elsoc_long$m29_rec2<=(351795) & elsoc_long$nhogar==3,   1,
                                                                                                                                                                                                                                                              if_else(elsoc_long$wave==3 & elsoc_long$m29_rec2<=(430275) & elsoc_long$nhogar==4,   1,
                                                                                                                                                                                                                                                                      if_else(elsoc_long$wave==3 & elsoc_long$m29_rec2<=(503018) & elsoc_long$nhogar==5,   1,
                                                                                                                                                                                                                                                                              if_else(elsoc_long$wave==3 & elsoc_long$m29_rec2<=(571492) & elsoc_long$nhogar==6,   1,
                                                                                                                                                                                                                                                                                      if_else(elsoc_long$wave==3 & elsoc_long$m29_rec2<=(636609) & elsoc_long$nhogar==7,   1,
                                                                                                                                                                                                                                                                                              if_else(elsoc_long$wave==3 & elsoc_long$m29_rec2<=(698984) & elsoc_long$nhogar==8,   1,
                                                                                                                                                                                                                                                                                                      if_else(elsoc_long$wave==3 & elsoc_long$m29_rec2<=(759056) & elsoc_long$nhogar==9,   1,
                                                                                                                                                                                                                                                                                                              if_else(elsoc_long$wave==3 & elsoc_long$m29_rec2<=(817155) & elsoc_long$nhogar==10,  1,
                                                                                                                                                                                                                                                                                                                      4))))))))))))))))))))))))))))))))))))

elsoc_long$pobreza_02 <- car::recode(elsoc_long$pob_tm2,"1=1;4=0;3=NA")

#Creación de otras variables
elsoc_long$educ <-car::recode(elsoc_long$m01, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'")
elsoc_long$educ <- factor(elsoc_long$educ, levels = c("Basica","Media","Tecnica","Universitaria"))
elsoc_long$wave1 <- elsoc_long$wave - 1
elsoc_long$mujer <- car::recode(elsoc_long$m0_sexo,"1='Hombre';2='Mujer'")
elsoc_long$mujer <- factor(elsoc_long$mujer, levels = c("Hombre","Mujer"))
elsoc_long$insat <- car::recode(elsoc_long$s01, "c(4,5)=0;c(1,2,3)=1")  
elsoc_long$edad <- elsoc_long$m0_edad/10
elsoc_long$religioso <- car::recode(elsoc_long$m38, "1:6=1;7:9=0")

#Generar Participación laboral (1 = Trabaja Full Tiempo, Jornada Parcial, Estudiay Trabaja, Desempleado y Busca empleo)
elsoc_long$empleo  <- car::recode(elsoc_long$m02,"c(1,2,3,6)=1;c(4,5,7,8,9)=0")
elsoc_long$trabaja <- car::recode(elsoc_long$m02,"c(1,2,3)=1;c(4,5,6,7,8,9)=0")

#Modelos longitudinales básicos: ingreso
m0  <- lmer(log(m13_rec1+1) ~ 1 + (1|idencuesta), data = elsoc_long)
m1  <- lmer(log(m13_rec1+1) ~ 1 + wave1 + (1 + wave1 |idencuesta), data = elsoc_long)
m2  <- lmer(log(m13_rec1+1) ~ 1 + mujer + edad + I(edad^2) + educ + wave1 + (1 + wave1 |idencuesta), data = elsoc_long)
m3  <- lmer(log(m13_rec1+1) ~ 1 + mujer + edad + I(edad^2) + educ + trabaja + wave1 + (1 + wave1 |idencuesta), data = elsoc_long)
m4  <- lmer(log(m13_rec1+1) ~ 1 + mujer + edad + I(edad^2) + educ + trabaja + wave1 + wave1*mujer   +  (1 + wave1 |idencuesta), data = elsoc_long)
m5  <- lmer(log(m13_rec1+1) ~ 1 + mujer + edad + I(edad^2) + educ + trabaja + wave1 + wave1*educ    +  (1 + wave1 |idencuesta), data = elsoc_long)
m6  <- lmer(log(m13_rec1+1) ~ 1 + mujer + edad + I(edad^2) + educ + trabaja + wave1 + wave1*trabaja +  (1 + wave1 |idencuesta), data = elsoc_long)
m7  <- lmer(log(m13_rec1+1) ~ 1 + mujer + edad + I(edad^2) + educ + trabaja + as.factor(wave1) + (1 + wave1 |idencuesta), data = elsoc_long)
m8  <- lmer(log(m13_rec1+1) ~ 1 + mujer + edad + I(edad^2) + educ + trabaja + as.factor(wave1) + as.factor(wave1)*mujer   +  (1 + wave1 |idencuesta), data = elsoc_long)
m9  <- lmer(log(m13_rec1+1) ~ 1 + mujer + edad + I(edad^2) + educ + trabaja + as.factor(wave1) + as.factor(wave1)*educ    +  (1 + wave1 |idencuesta), data = elsoc_long)
m10 <- lmer(log(m13_rec1+1) ~ 1 + mujer + edad + I(edad^2) + educ + trabaja + as.factor(wave1) + as.factor(wave1)*trabaja +  (1 + wave1 |idencuesta), data = elsoc_long)

elsoc_pobreza %>% dplyr::select(idencuesta, contrato, ed_padres, ind_pers) -> elsoc_pobreza1
elsoc_long1 <- dplyr::left_join(x = elsoc_long, y = elsoc_pobreza1, by = "idencuesta")

screenreg(list(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10))

#Modelos longitudinales básicos: pobreza
m0  <- glmer(pobreza_01 ~ 1 + (1|idencuesta), data = elsoc_long, family = binomial)
m1  <- glmer(pobreza_01 ~ 1 + wave1 + (1 + wave1|idencuesta), data = elsoc_long, family = binomial)
m2  <- glmer(pobreza_01 ~ 1 + wave1 + mujer + edad + I(edad^2) + educ + trabaja + as.factor(estrato) + nhogar + (1 + wave1|idencuesta), data = elsoc_long, family = binomial)
m3  <- glmer(pobreza_01 ~ 1 + wave1 + mujer + edad + I(edad^2) + educ + trabaja + as.factor(estrato) + nhogar + as.factor(contrato) + ed_padres + ind_pers  + (1 + wave1|idencuesta), data = elsoc_long1, family = binomial)

screenreg(list(m0,m1,m2,m3))
