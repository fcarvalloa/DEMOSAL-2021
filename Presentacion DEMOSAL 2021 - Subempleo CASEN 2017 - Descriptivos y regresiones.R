#----PRESENTACIÓN DEMOSAL 2021-----
#----CASEN 2017----
setwd("G:/Mi unidad/Fernando Carvallo/.Trabajo en R/")
library(readxl)
library(dplyr)
library(haven)
library(car)
library(tidyverse)
library(srvyr)


#BASES DE DATOS

# elimina notacion cientifica ----
options(scipen=999, OutDec = ",")

# Carga paquetes ----
pacman::p_load(dplyr, summarytools, ggmosaic, sjPlot, texreg, haven, ca, ggrepel, DescTools)

# Cargar base de datos ----

casen_2017 <- read_spss("ISA 2020/casen 2017.sav")

saveRDS(casen_2017, file = "casen_2017.RDS")

casen_2017 <- readRDS(file = "casen_2017.RDS")

sjlabelled::get_label(casen_2017)


#----Definicion de universo y población----

#actividad, niv educativo, edad, tipo de institucion, ocupación, sexo, quintil, rama, fact expansión, nivel educ padre

c2017u <- select(casen_2017, activ, e6a, edad, e8, oficio1, sexo, qaut, rama1, expr, oficio4, r12b,varstrat)


#Renombrar nombre variables

c2017u <- rename(c2017u, ocupacion=activ, empleo=oficio1, niveleduc=e6a, tipoinst=e8
                 , quintil=qaut, rama=rama1, niveleducpadre=r12b, empleo2=oficio4)

#Definición de la población
c2017u$ocupacion <- as.numeric(c2017u$ocupacion)
c2017u$niveleduc <- as.numeric(c2017u$niveleduc)


#Proporción de población activa con estudios 17

activ17 <- filter(c2017u, ocupacion==1) 
activ17 <-  mutate(activ17, niveleduc = car::recode(activ17$niveleduc, "12:17=1; else = 2"))
#1 = Con estudios ESUP
#2= sin estudios ESUP

#Factor de expansion para base casen 2017
activ17p <- activ17 %>% as_survey_design(ids = 1, weights = expr)

conestudios17<- activ17p%>% 
  group_by(niveleduc) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))
conestudios17

# % graduados: 36,3 (35,8-36,8)


#Filtro por ocupados de nivel técnico incompleto o superior:
c2017f <- filter(c2017u, ocupacion==1 & (niveleduc>=12 & niveleduc<=17))

#Construir variable subempleo por competencias ----

#basarnos en juan bravo, http://www.clapesuc.cl/assets/uploads/2016/12/26-12-16-anlisis-subempleo-por-competencias.pdf

#empleo CIUO 4 digitos
#1. sin calificacipon edusup (CIUO 4000 a 9000) excepto: 4121, 4122 5132 a 5133, 7230 a 7233, 7243
#2. baja calificacion (CIUO 3000 y 4121, 4122, 5132 a 5133, 7230 a 7233, 7243
#3. alta calificaci?n (CIUO 1000 y 2000)

#niveleduc
#1. sin titulo, tecnico superior incompleto 
#2. tecnico superior completo y y universitario incompleto
#3. universitario completo y posgraduados

#Segmentar variables Casen 2017:


califempleo17<- mutate(c2017f, empleo2 = car::recode(c2017f$empleo2, "1000:2999 = 3; 3000:3999= 2; 4121:4122=2; 5132:5133=2; 7230:7233=2;7243=2; 4000:9998=1; else = NA"), 
                       niveleduc = car::recode(c2017f$niveleduc, "1:12 = 1; 13:14= 2; 15:17 = 3; else = NA"))

table(califempleo17$niveleduc)
table(califempleo17$empleo2)
table(is.na(califempleo17$empleo2))

#Sintaxis subempleo por competencia Casen 2017:
base17 <-
  mutate(califempleo17,
         subempleo17 = case_when(empleo2 == 1 & niveleduc == 1 ~ "No",
                                 empleo2 == 1 & niveleduc == 2 ~ "Si",
                                 empleo2 == 1 & niveleduc == 3 ~ "Si",
                                 empleo2 == 2 & niveleduc == 1 ~ "No",
                                 empleo2 == 2 & niveleduc == 2 ~ "No",
                                 empleo2 == 2 & niveleduc == 3 ~ "Si",
                                 empleo2 == 3 & niveleduc == 1 ~ "No",
                                 empleo2 == 3 & niveleduc == 2 ~ "No",
                                 empleo2 == 3 & niveleduc == 3 ~ "No",
                                 ))


table(is.na(base17$subempleo17))

"Recodificación"
base17 <- mutate(base17, subempleorec17 = dplyr::recode(base17$subempleo17, "NO" = "1", "SI" = "2"))

base17$subempleorec17 <- factor(base17$subempleorec17,
                                labels = c("No subempleado", "Subempleado"))
base17$subempleorec17

#Factor de expansion para base casen 2017
base17p <- base17 %>% as_survey_design(ids = 1,  weights = expr)

subempleo17 <- base17p %>% 
  group_by(subempleo17) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
                                      proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
                                      n = unweighted(n()))

subempleo17
#Subempleados 1 digito %: 37,5 (36,6 - 38,4)
#Subempleados 4 digitos %: 35,5% (34,6 - 36,4)


#Subempleo CIUO 1 digito

califempleo17a<- mutate(c2017f, empleo = car::recode(c2017f$empleo, "1:2 = 3; 3= 2; 4:9 = 1; else = NA"), 
                       niveleduc = car::recode(c2017f$niveleduc, "1:12 = 1; 13:14= 2; 15:17 = 3; else = NA"))

#Sintaxis subempleo por competencia Casen 2017:
base17a <-
  mutate(califempleo17a,
         subempleo17 = case_when(empleo == 1 & niveleduc == 1 ~ "NO",
                                 empleo == 1 & niveleduc == 2 ~ "SI",
                                 empleo == 1 & niveleduc == 3 ~ "SI",
                                 empleo == 2 & niveleduc == 1 ~ "NO",
                                 empleo == 2 & niveleduc == 2 ~ "NO",
                                 empleo == 2 & niveleduc == 3 ~ "SI",
                                 empleo == 3 & niveleduc == 1 ~ "NO",
                                 empleo == 3 & niveleduc == 2 ~ "NO",
                                 empleo == 3 & niveleduc == 3 ~ "NO"))

#Recodificar para base casen 2017

base17a <- mutate(base17a, subempleorec17 = dplyr::recode(base17a$subempleo17, "NO" = "1", "SI" = "2"))

base17a$subempleorec17 <- factor(base17a$subempleorec17,
                                labels = c("No subempleado", "Subempleado"))
table(base17a$subempleorec17)

#Factor de expansion para base casen 2017
base17pa <- base17a %>% as_survey_design(ids = 1, weights = expr)

subempleo17a <- base17pa %>% 
  group_by(subempleo17) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))

#_______________________

## Recodificaciones  ----
#Sexo (REF: Hombre)
base17$sexo <- factor(base17$sexo,
                      labels = c("Hombre", "Mujer"))

table(base17$sexo)

#Tipo institución (REF: Estatal CRUCH)
base17<- mutate(base17, tipoinst = car::recode
                (base17$tipoinst, "1 = 2; 2= 3; 3 = 4; 4= 5; 5=1; 7=7; else = NA"))

base17$tipoinst <- factor(base17$tipoinst,
                          labels = c("Estatal CRUCH", "CFT", "IP", "Privada No CRUCH", "Privada CRUCH", "Extranjera"))
#Edad (REF: Adultos)
base17$edad <- as.numeric(base17$edad)
base17 <- mutate(base17, tramo_edad = car::recode(base17$edad, "15:29 = 1;30:59 = 2; 60:hi =3"))
base17$tramo_edad <- factor(base17$tramo_edad, labels= c("Jovenes", "Adultos", "Adulto mayor"))

## Rama (REF: S. Avanzados) 
base17$rama <- as.numeric(base17$rama)
base17 <- mutate(base17, rama = car::recode(base17$rama, "1:3 = 1;4:5 = 2; 6=3;7=4;8:9=5;10:11=6;
                                      12=7;13:15 =8; else=NA"))
base17$rama <- factor(base17$rama, labels= c("Primario", "Industria y suministro", "Construccion","Comercio",
                                             "S. Tradicionales","S. Avanzados", "Administracion Publica", "Educacion, Salud y otros"))
## Quintil (REF: Quintil I)
table(base17$quintil)
base17$quintil <- factor(base17$quintil, labels= c("I", "II", "III","IV", "V"))

## ------- Distribución variables ----
##contamos y eliminamos los NA - suspendido
#sum(is.na(base17)) 
#base17 <-na.omit(base17)

view(dfSummary(base17, headings = FALSE, method = "render"))

#----ANALISIS DESCRIPTIVO UNIVARIADO Y BIVARIADO---- 
#Subempleo en términos absolutos
freq(base17$subempleorec17, weights = base17$expr, report.nas = FALSE)

#SUBEMPLEO POR COMPETENCIAS SEGUN TIPO DE INSTITUCIÓN
tab_xtab(var.row = base17$subempleorec17,base17$tipoinst, var.labels = c("Subempleo", "Tipo institución"),
         show.col.prc = TRUE,show.summary = FALSE, weight.by = base17$expr)
plot_xtab(base17$subempleorec17,base17$tipoinst,bar.pos = "dodge",axis.titles = c("Subempleo", ""),
          show.summary = FALSE, show.n=FALSE,legend.title = "Tipo institución" , weight.by = base17$expr)

#SUBEMPLEO POR COMPETENCIAS SEGÚN SEXO
tab_xtab(var.row = base17$subempleorec17,base17$sexo, var.labels = c("Subempleo", "Sexo"),
         show.col.prc = TRUE,show.summary = FALSE, weight.by = base17$expr)
plot_xtab(base17$subempleorec17,base17$sexo,bar.pos = "dodge",axis.titles = c("Subempleo", ""),
          show.summary = FALSE, show.n=FALSE,legend.title = "Sexo" , weight.by = base17$expr)

#SUBEMPLEO POR COMPETENCIAS SEGÚN TRAMO ETARIO
tab_xtab(var.row = base17$subempleorec17,base17$tramo_edad,var.labels = c("Subempleo", "Grupo edad"),
         show.col.prc = TRUE,show.summary = FALSE, weight.by = base17$expr)
plot_xtab(base17$subempleorec17,base17$tramo_edad,bar.pos = "dodge",axis.titles = c("Subempleo", ""),
          show.summary = FALSE, show.n=FALSE,legend.title = "Tramo etario" , weight.by = base17$expr)

#SUEMPLEO POR COMPETENCIA SEGUN QUINTIL DE INGRESOS AUTONOMOS DEL HOGAR
tab_xtab(var.row = base17$subempleorec17,base17$quintil,var.labels = c("Subempleo", "Quintil"),
         show.col.prc = TRUE,show.summary = FALSE, weight.by = base17$expr)
plot_xtab(base17$subempleorec17,base17$quintil,bar.pos = "dodge",axis.titles = c("Subempleo", ""),
          show.summary = FALSE, show.n=FALSE,legend.title = "Quintil" , weight.by = base17$expr)

#SUBEMPLEO POR COMPETENCIAS SEGÚN RAMA
tab_xtab(var.row = base17$subempleorec17,base17$rama,var.labels = c("Subempleo", "Rama de ocupación"),
         show.col.prc = TRUE,show.summary = FALSE, weight.by = base17$expr)
plot_xtab(base17$subempleorec17,base17$rama,bar.pos = "dodge",axis.titles = c("Subempleo", ""),
          show.summary = FALSE, show.n=FALSE,legend.title = "Rama" , weight.by = base17$expr)

#----ANALISIS DESCRIPTIVO MULTIVARIADO---- 
##Análisis de correspondencias
## para conocer asociaciones descriptivas entre variables

attach(base17)
DF_ENE0<- data.frame(subempleorec17,tipoinst, rama, quintil, sexo,tramo_edad)

mca_t <- mjca(DF_ENE0)
cats_t <- apply(DF_ENE0, 2, function(x) nlevels(as.factor(x)))
mca_dft <- data.frame(mca_t$colcoord, Variable = rep(names(cats_t), cats_t))
rownames(mca_dft) = mca_t$levelnames

a<- ggplot(data = mca_dft, aes(x = X1, y = X2, label = mca_t$levelnames)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text_repel(aes(colour = Variable), size=4.5, hjust = 0.5, nudge_y = 0.2) +
  scale_color_manual(values=c("#01befe", "#e7b222","#ff006d","#ff7d00","#1fd224","#8f00ff"))+
  geom_point(aes(x = X1, y = X2)) +
  theme(axis.text.x=element_text(size=3), axis.text.y=element_text(size=3))+
  labs(x = paste0("Dimensión 1 (", signif((mca_t$inertia.e[1]*100), 3), "%)"),
       y = paste0("Dimensión 2  (", signif((mca_t$inertia.e[2]*100), 1), "%)"),
       col = "",caption="N: 24.169 casos")+
  xlim(-3,3) + ylim(-3,3)+  guides(shape="legend")+ ggtitle("Subempleo")+
  theme_bw()+theme(legend.position = "none") 
a

##---- MODELOS DE REGRESION LOGISTICA ----
# Especificar grupo de control
base17$subempleorec17 <- relevel(base17$subempleorec17, "No subempleado")
base17$tipoinst <- relevel(base17$tipoinst, "Estatal CRUCH")
base17$sexo <- relevel(base17$sexo, "Hombre")
base17$tramo_edad <- relevel(base17$tramo_edad, "Adultos")
base17$rama <- relevel(base17$rama, "S. Avanzados")
base17$quintil <- relevel(base17$quintil, "V")

#Regresiones
m00 <- glm(subempleorec17~1,data = base17,family = "binomial", weights=base17$expr)
m01 <- glm(subempleorec17~sexo,data = base17,family = "binomial", weights=base17$expr)
m02 <- glm(subempleorec17~tipoinst,data = base17,family = "binomial", weights=base17$expr)
m03 <- glm(subempleorec17~quintil,data = base17,family = "binomial", weights=base17$expr)
m04 <- glm(subempleorec17~tramo_edad,data = base17,family = "binomial", weights=base17$expr)
m05 <- glm(subempleorec17~sexo+tipoinst+quintil+rama+tramo_edad,data = base17,family = binomial(link = "logit"), weights=base17$expr)

summary(m00)
summary(m01)
summary(m02)
summary(m03)
summary(m04)
summary(m05)

htmlreg(l = list(m00,m01,m02,m03,m04,m05),
        custom.coef.names=c("Intercepto","Sexo (Mujer ref Hombre)",
                            "CFT","IP", "Privada No CRUCH", "Privada CRUCH", "Extranjera",
                            "Quintil I","Quintil II","Quintil III","Quintil IV",
                            "Jóvenes", "Adulto mayor",
                            "Primario", "Industria y suministro", "Construcción","Comercio",
                            "S. Tradicionales", "Administración Pública", "Educación, Salud y otros"),
        custom.model.names = c("Modelo 0","Modelo 1","Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5"), 
        file = "regresion.doc", stars = c(0.001, 0.01, 0.05, 0.1), caption = "Modelos de regresión logística")

## Ejemplo de grafico de probabilidades predichas

f <- plot_model(m05,type = "pred", 
           terms = c("tipoinst", "sexo"),
           title = "Probabilidades predichas para subempleo según sexo y tipo institución",
           show.intercept=TRUE, legend.title="Sexo", show.values = TRUE) + labs(x = "Tipo institución", y = "Subempleo") + geom_line() +  theme_bw()
f

 ggsave(t, filename = "prob pred.jpg",
        dpi = 600, width = 11, height = 5)
 
 g <- plot_model(m05,type = "pred", 
                 terms = c("tipoinst", "tramo_edad"),
                 title = "Probabilidades predichas para subempleo según tramo etario y tipo institución",
                 show.intercept=TRUE, legend.title="Tramo Edad", show.values = TRUE) + labs(x = "Tipo institución", y = "Subempleo") + geom_line() +  theme_bw()
 g
 
 ggsave(t, filename = "prob pred 2.jpg",
        dpi = 600, width = 11, height = 5)
 

## Ajustes de los modelos ----
# Test de devianza (fijarse en significatividad prueba Chi2)
test01<- anova(m00,m01,test = "Chisq")
test02<- anova(m00,m02,test = "Chisq") #tira error
test03<- anova(m00,m03,test = "Chisq") #tira error
test04<- anova(m00,m04,test = "Chisq")
test05<- anova(m00,m05,test = "Chisq") #tira error

##### DE AQUÍ EN ADELANTE TIRA ERROR

lrt01<- rbind(test01,test02,test03,test04,test05) %>% unique()
row.names(lrt01) <- c("Modelo nulo",
                      "Modelo 1 (Sexo)",
                      "Modelo 2 (Tipo inst)",
                      "Modelo 3 (Edad)",
                      "Modelo 4 (Quintil)",
                      "Modelo 5 (Todos)")
knitr::kable(lrt01,digits = 3, caption = "Test de devianza entre modelos")

# Fijarse en significancia Pr(>Chi)
# Modelo 1 vs nulo: ofrece mejor ajuste que sin predictores con sexo
# Modelo 2 vs nulo: ofrece mejor ajuste que sin predictores con tipo inst
# Modelo 3 vs nulo: ofrece mejor ajuste que sin predictores con edad
# Modelo 4 vs nulo: ofrece mejor ajuste que sin predictores con quintil
# Modelo 5 vs nulo: ofrece mejor ajuste que sin predictores con todos los predictores

# Guardar para agregar en modelo de regresión
test.pvalues1<- test01$`Pr(>Chi)`[2]
test.pvalues2<- test02$`Pr(>Chi)`[2]
test.pvalues3<- test03$`Pr(>Chi)`[2]
test.pvalues4<- test04$`Pr(>Chi)`[2]
test.pvalues5<- test05$`Pr(>Chi)`[2]


#### ACA VUELVE A CORRER BIEN

## Pseudo R2 de McFadden
# Debe aumentar para mostrar mejor ajuste de variables
# Manual
1-(logLik(m01)[1]/ logLik(m00)[1]) # modelo 1 vs modelo nulo
1-(logLik(m02)[1]/ logLik(m00)[1]) # modelo 2 vs modelo nulo
1-(logLik(m03)[1]/ logLik(m00)[1]) # modelo 3 vs modelo nulo
1-(logLik(m04)[1]/ logLik(m00)[1]) # modelo 4 vs modelo nulo
1-(logLik(m05)[1]/ logLik(m00)[1]) # modelo 5 vs modelo nulo

# Mismo procedimiento pero con paquete DescTools
mfr2.00 <- DescTools::PseudoR2(m00)
mfr2.01 <- DescTools::PseudoR2(m01)
mfr2.02 <- DescTools::PseudoR2(m02)
mfr2.03 <- DescTools::PseudoR2(m03)
mfr2.04 <- DescTools::PseudoR2(m04)
mfr2.05 <- DescTools::PseudoR2(m05)

# Guarda como DF
r2<- as.data.frame(cbind(c(mfr2.00,mfr2.01,mfr2.02,mfr2.03,mfr2.04,mfr2.05)))
rownames(r2) <- c("Modelo nulo",
                  "Modelo 1",
                  "Modelo 2",
                  "Modelo 3",
                  "Modelo 4",
                  "Modelo 5")
knitr::kable(r2,digits = 3, col.names = c("McFadden R2"))

## Tranformar estimaciones a Odds Ratios ---- ACA TIRA ERROR por el tema de los test previos, ver como se corrige
or <- texreg::extract(m05)
or@coef <- exp(or@coef)

htmlreg(l = list(m05,or), doctype = F,caption = "Modelos de regresión logística y Odds Ratio",caption.above = T,
        custom.model.names = c("Modelo 5", "Modelo 5 (OR)"),
        custom.coef.names=c("Intercepto","Sexo (Mujer ref Hombre)",
                            "CFT","IP", "Privada No CRUCH", "Privada CRUCH", "Extranjera",
                            "Quintil I","Quintil II","Quintil III","Quintil IV",
                            "Jóvenes", "Adulto mayor",
                            "Primario", "Industria y suministro", "Construcción","Comercio",
                            "S. Tradicionales", "Administración Pública", "Educación, Salud y otros"),
        ci.force = c(TRUE,TRUE),
        override.coef = list(coef(m05),or@coef),
        custom.gof.rows=list("Deviance Test " = c(test.pvalues5,
                                                       test.pvalues5),
                             "Pseudo R2" = c(mfr2.05,mfr2.05)), file = "oddsregresion.doc",
        custom.note = "p < 0.001; p < 0.01; p < 0.05 <br> Errores estándar entre paréntesis. <br> Nota: La significancia estadística de los coeficientes en unidades de Odds ratio está calculada en base a los valores t, <br> los cuales a su vez se calculan en base a log(Odds)/SE")

# Gráficos
plot02<- plot_model(m05,vline.color = "grey", title = "") + theme_bw() #Odds Ratio
plot02
plot01<- plot_model(m05,vline.color = "grey",transform = NULL, title = "")+ theme_bw() #Log-Odds
plot01

plot_grid(list(plot02,plot01),tags = c("Subempleo", ""), #Mapas juntos
          margin = c(0,0,0,0))
 
 






#-----------------Evolución subempleo y graduados en relación a población activa 92-2017-------

########## CASEN 1992 ###########

# Cargar base de datos ----

casen_1992 <- read_spss("ISA 2020/casen 1992.sav")

saveRDS(casen_1992, file = "casen_1992.RDS")

casen_1992 <- readRDS(file = "casen_1992.RDS")

sjlabelled::get_label(casen_1992)

#----Definicion de universo y población----

#actividad, niv educativo, edad, tipo de institucion, ocupación, sexo, quintil, rama, fact expansión

c1992u <- select(casen_1992, o21, e8, oficio, edad, sexo, qaut, rama, expr,o5)

#Renombrar nombre variables

c1992u <- rename(c1992u, ocupacion=o21, empleo=o5, niveleduc=e8,
                 quintil=qaut, rama=rama)

#Definición de la población
c1992u$ocupacion <- as.numeric(c1992u$ocupacion)
c1992u$niveleduc <- as.numeric(c1992u$niveleduc)

#Proporción de población activa con estudios 92

activ1992 <- filter(c1992u, ocupacion==1) #49.382
activ1992 <-  mutate(activ1992, niveleduc = car::recode(activ1992$niveleduc, "6:10=1; else = 2"))
#1 = Con estudios ESUP
#2= sin estudios ESUP

#Factor de expansion para base casen 92
activ1992p <- activ1992 %>% as_survey_design(ids = 1, weights = expr)

conestudios92<- activ1992p%>% 
  group_by(niveleduc) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))

# % graduados: 15,9 (15,5 - 16,3)

#Filtro por ocupados de nivel técnico incompleto o superior:
c1992f <- filter(c1992u, ocupacion==1 & (niveleduc>=6 & niveleduc<=10))

#Construir variable subempleo por competencias ----

#basarnos en juan bravo, http://www.clapesuc.cl/assets/uploads/2016/12/26-12-16-anlisis-subempleo-por-competencias.pdf

#empleo CIUO 3 digitos
#1. sin calificacipon edusup (CIUO 400 a 998) excepto: 412, 723, 724) 
#2. baja calificacion (CIUO 300 a 399 y 412, 723, 724)
#3. alta calificacion (CIUO 100 a 299)

#niveleduc
#1. sin titulo, tecnico superior incompleto 
#2. tecnico superior completo y y universitario incompleto
#3. universitario completo y posgraduados

#Segmentar variables Casen 1992:

califempleo92<- mutate(c1992f, empleo = car::recode(c1992f$empleo, "100:299 = 3; 300:399= 2; 412=2; 723:724=2; 400:998= 1; else = NA"), 
                       niveleduc = car::recode(c1992f$niveleduc, "8=1; 6=2; 9=2; 7=3; 10=3; else = NA"))

#Sintaxis subempleo por competencia Casen 1992:


base92 <-
  mutate(califempleo92,
         subempleo92 = case_when(empleo == 1 & niveleduc == 1 ~ "NO",
                                 empleo == 1 & niveleduc == 2 ~ "SI",
                                 empleo == 1 & niveleduc == 3 ~ "SI",
                                 empleo == 2 & niveleduc == 1 ~ "NO",
                                 empleo == 2 & niveleduc == 2 ~ "NO",
                                 empleo == 2 & niveleduc == 3 ~ "SI",
                                 empleo == 3 & niveleduc == 1 ~ "NO",
                                 empleo == 3 & niveleduc == 2 ~ "NO",
                                 empleo == 3 & niveleduc == 3 ~ "NO"))

#Recodificar para base casen 1992

base92 <- mutate(base92, subempleorec92 = dplyr::recode(base92$subempleo92, "NO" = "1", "SI" = "2"))

base92$subempleorec92 <- factor(base92$subempleorec92,
                                labels = c("No subempleado", "Subempleado"))

#Factor de expansion para base casen 1992
base92p <- base92 %>% as_survey_design(ids = 1, weights = expr)

subempleo92 <- base92p %>% 
  group_by(subempleo92) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))


#Subempleados 1 digito %: 32,5 (31,1 - 33,9)
#Subempleados 3 digitos %: 30,2 (28,9 - 31,6)

subempleo92

######## CASEN 1994 #########

# Cargar base de datos ----

casen_1994 <- read_spss("ISA 2020/casen 1994.sav")

saveRDS(casen_1994, file = "casen_1994.RDS")

casen_1994 <- readRDS(file = "casen_1994.RDS")

sjlabelled::get_label(casen_1994)

#----Definicion de universo y población----

#actividad, niv educativo, edad, tipo de institucion, ocupación, sexo, quintil, rama, fact expansión

c1994u <- select(casen_1994, o21, o5, e9, oficio, edad, sexo, qaut, rama, expr)

#Renombrar nombre variables

c1994u <- rename(c1994u, ocupacion=o21, empleo=o5, niveleduc=e9,
                 quintil=qaut, rama=rama)

#Definición de la población
c1994u$ocupacion <- as.numeric(c1994u$ocupacion)
c1994u$niveleduc <- as.numeric(c1994u$niveleduc)

#Proporción de población activa con estudios 92

activ1994 <- filter(c1994u, ocupacion==1) 
activ1994 <-  mutate(activ1994, niveleduc = car::recode(activ1994$niveleduc, "9:13=1; else = 2"))
#1 = Con estudios ESUP
#2= sin estudios ESUP

#Factor de expansion para base casen 92
activ1994p <- activ1994 %>% as_survey_design(ids = 1, weights = expr)

conestudios94<- activ1994p%>% 
  group_by(niveleduc) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))
conestudios94
# % graduados: 18,3 (17,8 - 18,8)


#Filtro por ocupados de nivel técnico incompleto o superior:
c1994f <- filter(c1994u, ocupacion==1 & (niveleduc>=9 & niveleduc<=13))

#Construir variable subempleo por competencias ----

#basarnos en juan bravo, http://www.clapesuc.cl/assets/uploads/2016/12/26-12-16-anlisis-subempleo-por-competencias.pdf

#empleo CIUO 3 digitos
#1. sin calificacipon edusup (CIUO 400 a 998) excepto: 412, 723, 724) 
#2. baja calificacion (CIUO 300 a 399 y 412, 723, 724)
#3. alta calificacion (CIUO 100 a 299)

#niveleduc
#1. sin titulo, tecnico superior incompleto 
#2. tecnico superior completo y y universitario incompleto
#3. universitario completo y posgraduados

#Segmentar variables Casen 1994:

califempleo94<- mutate(c1994f, empleo = car::recode(c1994f$empleo, "100:299 = 3; 300:399= 2; 412=2; 723:724=2; 400:998= 1; else = NA"), 
                       niveleduc = car::recode(c1994f$niveleduc, "11=1; 12=2; 9=2; 13=3; 10=3; else = NA"))
table(califempleo94$empleo)
table(califempleo94$niveleduc)
table (is.na)
#Sintaxis subempleo por competencia Casen 1994:
base94 <-
  mutate(califempleo94,
         subempleo94 = case_when(empleo == 1 & niveleduc == 1 ~ "NO",
                                 empleo == 1 & niveleduc == 2 ~ "SI",
                                 empleo == 1 & niveleduc == 3 ~ "SI",
                                 empleo == 2 & niveleduc == 1 ~ "NO",
                                 empleo == 2 & niveleduc == 2 ~ "NO",
                                 empleo == 2 & niveleduc == 3 ~ "SI",
                                 empleo == 3 & niveleduc == 1 ~ "NO",
                                 empleo == 3 & niveleduc == 2 ~ "NO",
                                 empleo == 3 & niveleduc == 3 ~ "NO"))

#Recodificar para base casen 1994

base94 <- mutate(base94, subempleorec94 = dplyr::recode(base94$subempleo94, "NO" = "1", "SI" = "2"))

base94$subempleorec94 <- factor(base94$subempleorec94,
                                labels = c("No subempleado", "Subempleado"))

#Factor de expansion para base casen 1994
base94p <- base94 %>% as_survey_design(ids = 1, weights = expr)

subempleo94 <- base94p %>% 
  group_by(subempleo94) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))
subempleo94

#Subempleados 1 digito %: %: 31,6 (30,3-33,0)
#Subempleados 4 digitos%: 29,8 (28,5-31,2)





######## CASEN 1996 #########

# Cargar base de datos ----

casen_1996 <- read_spss("ISA 2020/casen 1996.sav")

saveRDS(casen_1996, file = "casen_1996.RDS")

casen_1996 <- readRDS(file = "casen_1996.RDS")

sjlabelled::get_label(casen_1996)

#----Definicion de universo y población----

#actividad, niv educativo, edad, tipo de institucion, ocupación, sexo, quintil, rama, fact expansión

c1996u <- select(casen_1996, activ, o6, e6, oficio, edad, sexo, qaut, rama, expr)

#Renombrar nombre variables

c1996u <- rename(c1996u, ocupacion=activ, empleo=o6, niveleduc=e6,
                 quintil=qaut, rama=rama)

#Definición de la población
c1996u$ocupacion <- as.numeric(c1996u$ocupacion)
c1996u$niveleduc <- as.numeric(c1996u$niveleduc)

#Proporción de población activa con estudios 96

activ1996 <- filter(c1996u, ocupacion==1) 
activ1996 <-  mutate(activ1996, niveleduc = car::recode(activ1996$niveleduc, "9:15=1; else = 2"))
#1 = Con estudios ESUP
#2= sin estudios ESUP

#Factor de expansion para base casen 96
activ1996p <- activ1996 %>% as_survey_design(ids = 1, weights = expr)

conestudios96<- activ1996p%>% 
  group_by(niveleduc) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))

# % graduados: 20,0 (19,3-20,7)

#Filtro por ocupados de nivel técnico incompleto o superior:
c1996f <- filter(c1996u, ocupacion==1 & (niveleduc>=9 & niveleduc<=15))

#Construir variable subempleo por competencias ----

#basarnos en juan bravo, http://www.clapesuc.cl/assets/uploads/2016/12/26-12-16-anlisis-subempleo-por-competencias.pdf

#empleo CIUO 4 digitos
#1. sin calificacipon edusup (CIUO 4000 a 9000) excepto: 4121, 4122 5132 a 5133, 7230 a 7233, 7243
#2. baja calificacion (CIUO 3000 y 4121, 4122, 5132 a 5133, 7230 a 7233, 7243
#3. alta calificaci?n (CIUO 1000 y 2000)

#niveleduc
#1. sin titulo, tecnico superior incompleto 
#2. tecnico superior completo y y universitario incompleto
#3. universitario completo y posgraduados

#Segmentar variables Casen 1994:

califempleo96<- mutate(c1996f, empleo = car::recode(c1996f$empleo,  "1000:2999 = 3; 3000:3999= 2; 4121:4122=2; 5132:5133=2; 7230:7233=2;7243=2; 4000:9998=1; else = NA"), 
                       niveleduc = car::recode(c1996f$niveleduc, "9=1; 11=1; 10=2; 12=2; 13=2; 14=3; 15=3; else = NA"))
table(califempleo96$empleo)
table(is.na(califempleo96$empleo))

#Sintaxis subempleo por competencia Casen 1996:
base96 <-
  mutate(califempleo96,
         subempleo96 = case_when(empleo == 1 & niveleduc == 1 ~ "NO",
                                 empleo == 1 & niveleduc == 2 ~ "SI",
                                 empleo == 1 & niveleduc == 3 ~ "SI",
                                 empleo == 2 & niveleduc == 1 ~ "NO",
                                 empleo == 2 & niveleduc == 2 ~ "NO",
                                 empleo == 2 & niveleduc == 3 ~ "SI",
                                 empleo == 3 & niveleduc == 1 ~ "NO",
                                 empleo == 3 & niveleduc == 2 ~ "NO",
                                 empleo == 3 & niveleduc == 3 ~ "NO"))

#Recodificar para base casen 1996

base96 <- mutate(base96, subempleorec96 = dplyr::recode(base96$subempleo96, "NO" = "1", "SI" = "2"))

base96$subempleorec96 <- factor(base96$subempleorec96,
                                labels = c("No subempleado", "Subempleado"))

#Factor de expansion para base casen 1996
base96p <- base96 %>% as_survey_design(ids = 1, weights = expr)

subempleo96 <- base96p %>% 
  group_by(subempleo96) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))

subempleo96
#Subempleados 1 digito %: 31,5 (29,6-33,5)
#Subempleados 4 digitos %: 28,8 (26,9-30,7)





######## CASEN 1998 #########

# Cargar base de datos ----

casen_1998 <- read_spss("ISA 2020/casen 1998.sav")

saveRDS(casen_1998, file = "casen_1998.RDS")

casen_1998 <- readRDS(file = "casen_1998.RDS")

sjlabelled::get_label(casen_1998)

#----Definicion de universo y población----

#actividad, niv educativo, edad, tipo de institucion, ocupación, sexo, quintil, rama, fact expansión

c1998u <- select(casen_1998, o21, o6, e6, oficio, edad, sexo, qaut, rama, expr)

#Renombrar nombre variables

c1998u <- rename(c1998u, ocupacion=o21, empleo=o6, niveleduc=e6,
                 quintil=qaut, rama=rama)

#Definición de la población
c1998u$ocupacion <- as.numeric(c1998u$ocupacion)
c1998u$niveleduc <- as.numeric(c1998u$niveleduc)

#Proporción de población activa con estudios 98

activ1998 <- filter(c1998u, ocupacion==1) 
activ1998 <-  mutate(activ1998, niveleduc = car::recode(activ1998$niveleduc, "9:15=1; else = 2"))
#1 = Con estudios ESUP
#2= sin estudios ESUP

#Factor de expansion para base casen 92
activ1998p <- activ1998 %>% as_survey_design(ids = 1, weights = expr)

conestudios98<- activ1998p%>% 
  group_by(niveleduc) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))
conestudios98
# % graduados: 22,2 (21,7-22,8)

#Filtro por ocupados de nivel técnico incompleto o superior:
c1998f <- filter(c1998u, ocupacion==1 & (niveleduc>=9 & niveleduc<=15))

#Construir variable subempleo por competencias ----

#basarnos en juan bravo, http://www.clapesuc.cl/assets/uploads/2016/12/26-12-16-anlisis-subempleo-por-competencias.pdf

#empleo CIUO 4 digitos
#1. sin calificacipon edusup (CIUO 4000 a 9000) excepto: 4121, 4122 5132 a 5133, 7230 a 7233, 7243
#2. baja calificacion (CIUO 3000 y 4121, 4122, 5132 a 5133, 7230 a 7233, 7243
#3. alta calificaci?n (CIUO 1000 y 2000)

#niveleduc
#1. sin titulo, tecnico superior incompleto 
#2. tecnico superior completo y y universitario incompleto
#3. universitario completo y posgraduados

#Segmentar variables Casen 1994:

califempleo98<- mutate(c1998f, empleo = car::recode(c1998f$empleo, "1000:2999 = 3; 3000:3999= 2; 4121:4122=2; 5132:5133=2; 7230:7233=2;7243=2; 4000:9998=1; else = NA"), 
                       niveleduc = car::recode(c1998f$niveleduc, "9=1; 11=1; 10=2; 12=2; 13=2; 14=3; 15=3; else = NA"))

#Sintaxis subempleo por competencia Casen 1996:
base98 <-
  mutate(califempleo98,
         subempleo98 = case_when(empleo == 1 & niveleduc == 1 ~ "NO",
                                 empleo == 1 & niveleduc == 2 ~ "SI",
                                 empleo == 1 & niveleduc == 3 ~ "SI",
                                 empleo == 2 & niveleduc == 1 ~ "NO",
                                 empleo == 2 & niveleduc == 2 ~ "NO",
                                 empleo == 2 & niveleduc == 3 ~ "SI",
                                 empleo == 3 & niveleduc == 1 ~ "NO",
                                 empleo == 3 & niveleduc == 2 ~ "NO",
                                 empleo == 3 & niveleduc == 3 ~ "NO"))

#Recodificar para base casen 1998

base98 <- mutate(base98, subempleorec98 = dplyr::recode(base98$subempleo98, "NO" = "1", "SI" = "2"))

base98$subempleorec98 <- factor(base98$subempleorec98,
                                labels = c("No subempleado", "Subempleado"))

#Factor de expansion para base casen 1998
base98p <- base98 %>% as_survey_design(ids = 1, weights = expr)

subempleo98 <- base98p %>% 
  group_by(subempleo98) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))
subempleo98 

#Subempleados 1 digito %: 33,3 (31,9-34,6)
#Subempleados 4 digitos %: 30,5 (29,2-31,8)


######## CASEN 2000 ######### ERROR EN LA CIUO

# Cargar base de datos ----

casen_2000 <- read_spss("ISA 2020/casen 2000.sav")

saveRDS(casen_2000, file = "casen_2000.RDS")

casen_2000 <- readRDS(file = "casen_2000.RDS")

sjlabelled::get_label(casen_2000)

#----Definicion de universo y población----

#actividad, niv educativo, edad, tipo de institucion, ocupación, sexo, quintil, rama, fact expansión

c2000u <- select(casen_2000, activ, educ, oficio, edad, sexo, qaut, rama, o6, expr)
c2000u$empleo
#Renombrar nombre variables

c2000u <- rename(c2000u, ocupacion=activ, empleo=oficio, niveleduc=educ,
                 quintil=qaut, rama=rama)

#Definición de la población
c2000u$ocupacion <- as.numeric(c2000u$ocupacion)
c2000u$niveleduc <- as.numeric(c2000u$niveleduc)

#Proporción de población activa con estudios 00

activ2000 <- filter(c2000u, ocupacion==1) 
activ2000 <-  mutate(activ2000, niveleduc = car::recode(activ2000$niveleduc, "8:11=1; else = 2"))
#1 = Con estudios ESUP
#2= sin estudios ESUP

#Factor de expansion para base casen 92
activ2000p <- activ2000 %>% as_survey_design(ids = 1, weights = expr)

conestudios00<- activ2000p%>% 
  group_by(niveleduc) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))
conestudios00
activ2000$empleo
# % graduados: 23,7 (23,0-24,4)

#Filtro por ocupados de nivel técnico incompleto o superior:
c2000f <- filter(c2000u, ocupacion==1 & (niveleduc>=8 & niveleduc<=11))

#Construir variable subempleo por competencias ----

#basarnos en juan bravo, http://www.clapesuc.cl/assets/uploads/2016/12/26-12-16-anlisis-subempleo-por-competencias.pdf

#empleo
#1. sin calificacipon edusup (CIUO 4 a 9)
#2. baja calificacion (CIUO 3)
#3. alta calificaci?n (CIUO 1 y 2)


#niveleduc
#1. sin titulo, tecnico superior incompleto 
#2. tecnico superior completo y y universitario incompleto
#3. universitario completo y posgraduados

#Segmentar variables Casen 1994:

califempleo00<- mutate(c2000f, empleo = car::recode(c2000f$empleo, "1:2 = 3; 3= 2; 4:9 = 1; else = NA"), 
                       niveleduc = car::recode(c2000f$niveleduc, "8=1; 9=2; 10=2; 11=3; else = NA"))

table(califempleo00$niveleduc)
table(is.na(califempleo00$niveleduc))

table(califempleo00$empleo)
table(is.na(califempleo00$empleo))


#Sintaxis subempleo por competencia Casen 2000:
base00 <-
  mutate(califempleo00,
         subempleo00 = case_when(empleo == 1 & niveleduc == 1 ~ "NO",
                                 empleo == 1 & niveleduc == 2 ~ "SI",
                                 empleo == 1 & niveleduc == 3 ~ "SI",
                                 empleo == 2 & niveleduc == 1 ~ "NO",
                                 empleo == 2 & niveleduc == 2 ~ "NO",
                                 empleo == 2 & niveleduc == 3 ~ "SI",
                                 empleo == 3 & niveleduc == 1 ~ "NO",
                                 empleo == 3 & niveleduc == 2 ~ "NO",
                                 empleo == 3 & niveleduc == 3 ~ "NO"))

#Recodificar para base casen 2000

base00 <- mutate(base00, subempleorec00 = dplyr::recode(base00$subempleo00, "NO" = "1", "SI" = "2"))

base00$subempleorec00 <- factor(base00$subempleorec00,
                                labels = c("No subempleado", "Subempleado"))

#Factor de expansion para base casen 2017
base00p <- base00 %>% as_survey_design(ids = 1, weights = expr)

subempleo00 <- base00p %>% 
  group_by(subempleo00) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))

subempleo00 
#Subempleados %: 73,0 (71,5- 74,6) #ERROR, EXCLUIDA


######## CASEN 2003 #########

# Cargar base de datos ----

casen_2003 <- read_spss("ISA 2020/casen 2003.sav")

saveRDS(casen_2003, file = "casen_2003.RDS")

casen_2003 <- readRDS(file = "casen_2003.RDS")

sjlabelled::get_label(casen_2003)

#----Definicion de universo y población----

#actividad, niv educativo, edad, tipo de institucion, ocupación, sexo, quintil, rama, fact expansión

c2003u <- select(casen_2003, ACTIV, EDUC, OFICIO, O7, EDAD, SEXO, QAUT, RAMA, EXPR)

#Renombrar nombre variables

c2003u <- rename(c2003u, ocupacion=ACTIV, empleo=O7, niveleduc=EDUC,
                 quintil=QAUT, rama=RAMA)

#Definición de la población
c2003u$ocupacion <- as.numeric(c2003u$ocupacion)
c2003u$niveleduc <- as.numeric(c2003u$niveleduc)

#Proporción de población activa con estudios 03

activ2003 <- filter(c2003u, ocupacion==1) 
activ2003 <-  mutate(activ2003, niveleduc = car::recode(activ2003$niveleduc, "8:11=1; else = 2"))
#1 = Con estudios ESUP
#2= sin estudios ESUP

#Factor de expansion para base casen 03
activ2003p <- activ2003 %>% as_survey_design(ids = 1, weights = EXPR)

conestudios03<- activ2003p%>% 
  group_by(niveleduc) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))

conestudios03
# % graduados: 25,0 (24,4-25,5)

#Filtro por ocupados de nivel técnico incompleto o superior:
c2003f <- filter(c2003u, ocupacion==1 & (niveleduc>=8 & niveleduc<=11))
table(c2003f$empleo)
#Construir variable subempleo por competencias ----

#basarnos en juan bravo, http://www.clapesuc.cl/assets/uploads/2016/12/26-12-16-anlisis-subempleo-por-competencias.pdf


#empleo CIUO 4 digitos
#1. sin calificacipon edusup (CIUO 4000 a 9000) excepto: 4121, 4122 5132 a 5133, 7230 a 7233, 7243
#2. baja calificacion (CIUO 3000 y 4121, 4122, 5132 a 5133, 7230 a 7233, 7243
#3. alta calificaci?n (CIUO 1000 y 2000)

#niveleduc
#1. sin titulo, tecnico superior incompleto 
#2. tecnico superior completo y y universitario incompleto
#3. universitario completo y posgraduados

#Segmentar variables Casen 1994:

califempleo03<- mutate(c2003f, empleo = car::recode(c2003f$empleo, "1000:2999 = 3; 3000:3999= 2; 4121:4122=2; 5132:5133=2; 7230:7233=2;7243=2; 4000:9998=1; else = NA"), 
                       niveleduc = car::recode(c2003f$niveleduc, "8=1; 9=2; 10=2; 11=3;else = NA"))
table(califempleo03$empleo)
table(is.na(califempleo03$empleo))
#Sintaxis subempleo por competencia Casen 2003:
base03 <-
  mutate(califempleo03,
         subempleo03 = case_when(empleo == 1 & niveleduc == 1 ~ "NO",
                                 empleo == 1 & niveleduc == 2 ~ "SI",
                                 empleo == 1 & niveleduc == 3 ~ "SI",
                                 empleo == 2 & niveleduc == 1 ~ "NO",
                                 empleo == 2 & niveleduc == 2 ~ "NO",
                                 empleo == 2 & niveleduc == 3 ~ "SI",
                                 empleo == 3 & niveleduc == 1 ~ "NO",
                                 empleo == 3 & niveleduc == 2 ~ "NO",
                                 empleo == 3 & niveleduc == 3 ~ "NO"))

#Recodificar para base casen 2003

base03 <- mutate(base03, subempleorec03 = dplyr::recode(base03$subempleo03, "NO" = "1", "SI" = "2"))

base03$subempleorec03 <- factor(base03$subempleorec03,
                                labels = c("No subempleado", "Subempleado"))

table(base03$subempleorec03)
#Factor de expansion para base casen 2003
base03p <- base03 %>% as_survey_design(ids = 1, weights = EXPR)

subempleo03 <- base03p %>% 
  group_by(subempleo03) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))

subempleo03

#Subempleados 1 digito%: 31,3 (30,1 - 32,6) 
#Subempleados 4 digitos%: 29,1 (27,9 - 30,3) 





######## CASEN 2006 #########

# Cargar base de datos ----

casen_2006 <- read_spss("ISA 2020/casen 2006.sav")

saveRDS(casen_2006, file = "casen_2006.RDS")

casen_2006 <- readRDS(file = "casen_2006.RDS")

sjlabelled::get_label(casen_2006)

#----Definicion de universo y población----

#actividad, niv educativo, edad, tipo de institucion, ocupación, sexo, quintil, rama, fact expansión

c2006u <- select(casen_2006, ACTIV, E8T, OFICIO,C_O11, EDAD, SEXO, QAUT, RAMA, EXPR)

#Renombrar nombre variables

c2006u <- rename(c2006u, ocupacion=ACTIV, empleo=C_O11, niveleduc=E8T,
                 quintil=QAUT, rama=RAMA)

#Definición de la población
c2006u$ocupacion <- as.numeric(c2006u$ocupacion)
c2006u$niveleduc <- as.numeric(c2006u$niveleduc)

#Proporción de población activa con estudios 92

activ2006 <- filter(c2006u, ocupacion==1) 
activ2006 <-  mutate(activ2006, niveleduc = car::recode(activ2006$niveleduc, "9:15=1; else = 2"))
#1 = Con estudios ESUP
#2= sin estudios ESUP

#Factor de expansion para base casen 92
activ2006p <- activ2006 %>% as_survey_design(ids = 1, weights = EXPR)

conestudios06<- activ2006p%>% 
  group_by(niveleduc) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))

conestudios06

# % graduados: 24,9 (24,4 - 25,4)

#Filtro por ocupados de nivel técnico incompleto o superior:
c2006f <- filter(c2006u, ocupacion==1 & (niveleduc>=9 & niveleduc<=15))

#Construir variable subempleo por competencias ----

#basarnos en juan bravo, http://www.clapesuc.cl/assets/uploads/2016/12/26-12-16-anlisis-subempleo-por-competencias.pdf

#empleo CIUO 4 digitos
#1. sin calificacipon edusup (CIUO 4000 a 9000) excepto: 4121, 4122 5132 a 5133, 7230 a 7233, 7243
#2. baja calificacion (CIUO 3000 y 4121, 4122, 5132 a 5133, 7230 a 7233, 7243
#3. alta calificaci?n (CIUO 1000 y 2000)

#niveleduc
#1. sin titulo, tecnico superior incompleto 
#2. tecnico superior completo y y universitario incompleto
#3. universitario completo y posgraduados

#Segmentar variables Casen 1994:

califempleo06<- mutate(c2006f, empleo = car::recode(c2006f$empleo, "1000:2999 = 3; 3000:3999= 2; 4121:4122=2; 5132:5133=2; 7230:7233=2;7243=2; 4000:9998=1; else = NA"), 
                       niveleduc = car::recode(c2006f$niveleduc, "9=1; 11=1; 10=2; 12=2;13=2; 14=3;15=3;else = NA"))

#Sintaxis subempleo por competencia Casen 1996:
base06 <-
  mutate(califempleo06,
         subempleo06 = case_when(empleo == 1 & niveleduc == 1 ~ "NO",
                                 empleo == 1 & niveleduc == 2 ~ "SI",
                                 empleo == 1 & niveleduc == 3 ~ "SI",
                                 empleo == 2 & niveleduc == 1 ~ "NO",
                                 empleo == 2 & niveleduc == 2 ~ "NO",
                                 empleo == 2 & niveleduc == 3 ~ "SI",
                                 empleo == 3 & niveleduc == 1 ~ "NO",
                                 empleo == 3 & niveleduc == 2 ~ "NO",
                                 empleo == 3 & niveleduc == 3 ~ "NO"))

#Recodificar para base casen 2006

base06 <- mutate(base06, subempleorec06 = dplyr::recode(base06$subempleo06, "NO" = "1", "SI" = "2"))

base06$subempleorec06 <- factor(base06$subempleorec06,
                                labels = c("No subempleado", "Subempleado"))


#Factor de expansion para base casen 2017
base06p <- base06 %>% as_survey_design(ids = 1, weights = EXPR)

subempleo06 <- base06p %>% 
  group_by(subempleo06) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))
subempleo06 

#Subempleados 1 digito %: 34,8 (33,7-36,0)
#Subempleados 4 digitos %: 33,2 (32,0-34,3)





######## CASEN 2009 #########

# Cargar base de datos ----

casen_2009 <- read_spss("ISA 2020/casen 2009.sav")

saveRDS(casen_2009, file = "casen_2009.RDS")

casen_2009 <- readRDS(file = "casen_2009.RDS")

sjlabelled::get_label(casen_2009)

#----Definicion de universo y población----

#actividad, niv educativo, edad, tipo de institucion, ocupación, sexo, quintil, rama, fact expansión

c2009u <- select(casen_2009, ACTIV, E7T, OFICIO,C_O12, EDAD, SEXO, QAUT, RAMA, EXPR)

#Renombrar nombre variables

c2009u <- rename(c2009u, ocupacion=ACTIV, empleo=C_O12, niveleduc=E7T,
                 quintil=QAUT, rama=RAMA)

#Definición de la población
c2009u$ocupacion <- as.numeric(c2009u$ocupacion)
c2009u$niveleduc <- as.numeric(c2009u$niveleduc)

#Proporción de población activa con estudios 09

activ2009 <- filter(c2009u, ocupacion==1) 
activ2009 <-  mutate(activ2009, niveleduc = car::recode(activ2009$niveleduc, "9:15=1; else = 2"))
#1 = Con estudios ESUP
#2= sin estudios ESUP

#Factor de expansion para base casen 09
activ2009p <- activ2009 %>% as_survey_design(ids = 1, weights = EXPR)

conestudios09<- activ2009p%>% 
  group_by(niveleduc) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))

conestudios09

# % graduados: 27,3 (26,7-27,9)

#Filtro por ocupados de nivel técnico incompleto o superior:
c2009f <- filter(c2009u, ocupacion==1 & (niveleduc>=9 & niveleduc<=15))

#Construir variable subempleo por competencias ----

#basarnos en juan bravo, http://www.clapesuc.cl/assets/uploads/2016/12/26-12-16-anlisis-subempleo-por-competencias.pdf

#empleo CIUO 4 digitos
#1. sin calificacipon edusup (CIUO 4000 a 9000) excepto: 4121, 4122 5132 a 5133, 7230 a 7233, 7243
#2. baja calificacion (CIUO 3000 y 4121, 4122, 5132 a 5133, 7230 a 7233, 7243
#3. alta calificaci?n (CIUO 1000 y 2000)

#niveleduc
#1. sin titulo, tecnico superior incompleto 
#2. tecnico superior completo y y universitario incompleto
#3. universitario completo y posgraduados

#Segmentar variables Casen 2009:

califempleo09<- mutate(c2009f, empleo = car::recode(c2009f$empleo, "1000:2999 = 3; 3000:3999= 2; 4121:4122=2; 5132:5133=2; 7230:7233=2;7243=2; 4000:9998=1; else = NA"), 
                       niveleduc = car::recode(c2009f$niveleduc, "9=1; 11=1; 10=2; 12=2;13=2; 14=3;15=3;else = NA"))

#Sintaxis subempleo por competencia Casen 2009:
base09 <-
  mutate(califempleo09,
         subempleo09 = case_when(empleo == 1 & niveleduc == 1 ~ "NO",
                                 empleo == 1 & niveleduc == 2 ~ "SI",
                                 empleo == 1 & niveleduc == 3 ~ "SI",
                                 empleo == 2 & niveleduc == 1 ~ "NO",
                                 empleo == 2 & niveleduc == 2 ~ "NO",
                                 empleo == 2 & niveleduc == 3 ~ "SI",
                                 empleo == 3 & niveleduc == 1 ~ "NO",
                                 empleo == 3 & niveleduc == 2 ~ "NO",
                                 empleo == 3 & niveleduc == 3 ~ "NO"))

#Recodificar para base casen 2009

base09 <- mutate(base09, subempleorec09 = dplyr::recode(base09$subempleo09, "NO" = "1", "SI" = "2"))

base09$subempleorec09 <- factor(base09$subempleorec09,
                                labels = c("No subempleado", "Subempleado"))

#Factor de expansion para base casen 2009
base09p <- base09 %>% as_survey_design(ids = 1, weights = EXPR)

subempleo09 <- base09p %>% 
  group_by(subempleo09) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))
subempleo09

#Subempleados 1 digito %: 31,4 (30,1 - 32,7)  
#Subempleados 4 digitos %: 29,5 (28,2 - 30,7)  







######## CASEN 2011 #########

# Cargar base de datos ----

casen_2011 <- read_stata("ISA 2020/casen 2011.dta")

saveRDS(casen_2011, file = "casen_2011.RDS")

casen_2011 <- readRDS(file = "casen_2011.RDS")

sjlabelled::get_label(casen_2011)

#----Definicion de universo y población----

#actividad, niv educativo, edad, tipo de institucion, ocupación, sexo, quintil, rama, fact expansión

c2011u <- select(casen_2011, activ, e6a, e6b, oficio1, oficio4, edad, sexo, qaut, rama1, expr_full)

c2011u <- rename(c2011u, ocupacion=activ, empleo=oficio4, niveleduc1=e6a, niveleduc2=e6b,
                 quintil=qaut, rama=rama1)
##Crear variable educacion

c2011u <- mutate (c2011u,
                  niveleduc= case_when(niveleduc1 == 11 & niveleduc2 == 2 ~ 9,
                                       niveleduc1 == 11 & niveleduc2 == 1 ~ 10,
                                       niveleduc1 == 12 & niveleduc2 == 2 ~ 11,
                                       niveleduc1 == 12 & niveleduc2 == 1 ~ 12,
                                       niveleduc1 == 13 & niveleduc2 == 2 ~ 13,
                                       niveleduc1 == 13 & niveleduc2 == 1 ~ 14,))



#Definición de la población
c2011u$ocupacion <- as.numeric(c2011u$ocupacion)
c2011u$niveleduc <- as.numeric(c2011u$niveleduc)


#Proporción de población activa con estudios 2011

activ2011 <- filter(c2011u, ocupacion==1) 
activ2011 <-  mutate(activ2011, niveleduc = car::recode(activ2011$niveleduc, "9:14=1; else = 2"))
#1 = Con estudios ESUP
#2= sin estudios ESUP

#Factor de expansion para base casen 11
activ2011p <- activ2011 %>% as_survey_design(ids = 1, weights = expr_full)

conestudios11<- activ2011p%>% 
  group_by(niveleduc) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))
conestudios11
# % graduados: 28,4 (27,8-29,0)

#Filtro por ocupados de nivel técnico incompleto o superior:
c2011f <- filter(c2011u, ocupacion==1 & (niveleduc>=9 & niveleduc1<=14))


#Construir variable subempleo por competencias ----

#basarnos en juan bravo, http://www.clapesuc.cl/assets/uploads/2016/12/26-12-16-anlisis-subempleo-por-competencias.pdf

#empleo CIUO 4 digitos
#1. sin calificacipon edusup (CIUO 4000 a 9000) excepto: 4121, 4122 5132 a 5133, 7230 a 7233, 7243
#2. baja calificacion (CIUO 3000 y 4121, 4122, 5132 a 5133, 7230 a 7233, 7243
#3. alta calificaci?n (CIUO 1000 y 2000)

#niveleduc
#1. sin titulo, tecnico superior incompleto 
#2. tecnico superior completo y y universitario incompleto
#3. universitario completo y posgraduados


#Segmentar variables Casen 2011:

califempleo11<- mutate(c2011f, empleo = car::recode(c2011f$empleo, "1000:2999 = 3; 3000:3999= 2; 4121:4122=2; 5132:5133=2; 7230:7233=2;7243=2; 4000:9998=1; else = NA"), 
                       niveleduc = car::recode(c2011f$niveleduc, "9=1; 10=2; 11=2; 12=3; 13=3; 14 = 3; else = NA"))

#Sintaxis subempleo por competencia Casen 2011:
base11 <-
  mutate(califempleo11,
         subempleo11 = case_when(empleo == 1 & niveleduc == 1 ~ "NO",
                                 empleo == 1 & niveleduc == 2 ~ "SI",
                                 empleo == 1 & niveleduc == 3 ~ "SI",
                                 empleo == 2 & niveleduc == 1 ~ "NO",
                                 empleo == 2 & niveleduc == 2 ~ "NO",
                                 empleo == 2 & niveleduc == 3 ~ "SI",
                                 empleo == 3 & niveleduc == 1 ~ "NO",
                                 empleo == 3 & niveleduc == 2 ~ "NO",
                                 empleo == 3 & niveleduc == 3 ~ "NO"))

#Recodificar para base casen 2011

base11 <- mutate(base11, subempleorec11 = dplyr::recode(base11$subempleo11, "NO" = "1", "SI" = "2"))

base11$subempleorec11 <- factor(base11$subempleorec11,
                                labels = c("No subempleado", "Subempleado"))

#Factor de expansion para base casen 2011
base11p <- base11 %>% as_survey_design(ids = 1, weights = expr_full)

subempleo11 <- base11p %>% 
  group_by(subempleo11) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))
subempleo11

#Subempleados 1 digito %: 32,7 (31,5-33,9) 
#Subempleados 4 digitos %: 30,9 (29,7-32,0) 



######## CASEN 2013 #########

# Cargar base de datos ----

casen_2013 <- read_spss("ISA 2020/casen 2013.sav")

saveRDS(casen_2013, file = "Demosal/casen_2013.RDS")

casen_2013 <- readRDS(file = "Demosal/casen_2013.RDS")

sjlabelled::get_label(casen_2013)


#----Definicion de universo y población----

#actividad, niv educativo, edad, tipo de institucion, ocupación, sexo, quintil, rama, fact expansión

c2013u <- select(casen_2013, activ, educ, oficio1,oficio4, edad, sexo, QAUT_MN, rama1, expr)

#Renombrar nombre variables

c2013u <- rename(c2013u, ocupacion=activ, empleo=oficio4, niveleduc=educ,
                 quintil=QAUT_MN, rama=rama1)
table(c2013u$empleo)

#Definición de la población
c2013u$ocupacion <- as.numeric(c2013u$ocupacion)
c2013u$niveleduc <- as.numeric(c2013u$niveleduc)
c2013u$empleo <- as.numeric(c2013u$empleo) # Tira error


#Proporción de población activa con estudios 13

activ2013 <- filter(c2013u, ocupacion==1) 
activ2013 <-  mutate(activ2013, niveleduc = car::recode(activ2013$niveleduc, "7:12=1; else = 2"))
#1 = Con estudios ESUP
#2= sin estudios ESUP

#Factor de expansion para base casen 92
activ2013p <- activ2013 %>% as_survey_design(ids = 1, weights = expr)

conestudios13<- activ2013p%>% 
  group_by(niveleduc) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))
conestudios13
# % graduados: 31,2 (30,6-31,9)

#Corregir "empleo" que es numero guardado como texto, para que sea numeric
c2013u$empleo<-type.convert(c2013u$empleo) #pasarlo a interger
c2013u$empleo <- as.numeric(c2013u$empleo) #pasarlo a numeric (hacerlo directo tira error)
class(c2013u$empleo)

#Filtro por ocupados de nivel técnico incompleto o superior:
c2013f <- filter(c2013u, ocupacion==1 & (niveleduc>=7 & niveleduc<=12))
table(c2013f$empleo)

#Construir variable subempleo por competencias ----

#basarnos en juan bravo, http://www.clapesuc.cl/assets/uploads/2016/12/26-12-16-anlisis-subempleo-por-competencias.pdf

#empleo CIUO 4 digitos
#1. sin calificacipon edusup (CIUO 4000 a 9000) excepto: 4121, 4122 5132 a 5133, 7230 a 7233, 7243
#2. baja calificacion (CIUO 3000 y 4121, 4122, 5132 a 5133, 7230 a 7233, 7243
#3. alta calificaci?n (CIUO 1000 y 2000)

#niveleduc
#1. sin titulo, tecnico superior incompleto 
#2. tecnico superior completo y y universitario incompleto
#3. universitario completo y posgraduados


#Segmentar variables Casen 2013:


califempleo13<- mutate(c2013f, empleo = car::recode(c2013f$empleo, "1000:2999 = 3; 3000:3999= 2; 4121:4122=2; 5132:5133=2; 7230:7233=2;7243=2; 4000:9998=1; else = NA"), 
                       niveleduc = car::recode(c2013f$niveleduc, "7=1; 8=2; 9=2; 10=3; 11=3; 12 = 3; else = NA"))
table(califempleo13$empleo)
table(is.na(califempleo13$empleo))
table(califempleo13$niveleduc)
table(is.na(califempleo13$niveleduc))

#Sintaxis subempleo por competencia Casen 2013:
base13 <-
  mutate(califempleo13,
         subempleo13 = case_when(empleo == 1 & niveleduc == 1 ~ "NO",
                                 empleo == 1 & niveleduc == 2 ~ "SI",
                                 empleo == 1 & niveleduc == 3 ~ "SI",
                                 empleo == 2 & niveleduc == 1 ~ "NO",
                                 empleo == 2 & niveleduc == 2 ~ "NO",
                                 empleo == 2 & niveleduc == 3 ~ "SI",
                                 empleo == 3 & niveleduc == 1 ~ "NO",
                                 empleo == 3 & niveleduc == 2 ~ "NO",
                                 empleo == 3 & niveleduc == 3 ~ "NO"))

#Recodificar para base casen 2013

base13 <- mutate(base13, subempleorec13 = dplyr::recode(base13$subempleo13, "NO" = "1", "SI" = "2"))

base13$subempleorec13 <- factor(base13$subempleorec13,
                                labels = c("No subempleado", "Subempleado"))

#Factor de expansion para base casen 2017
base13p <- base13 %>% as_survey_design(ids = 1, weights = expr)

subempleo13 <- base13p %>% 
  group_by(subempleo13) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))

subempleo13
#Subempleados 1 digito %: 34,8 (33,5-36,1)
#Subempleados 4 digitos %: 33,0 (31,7-34,3)



######## CASEN 2015 #########

# Cargar base de datos ----

casen_2015 <- read_spss("ISA 2020/casen 2015.sav")

saveRDS(casen_2015, file = "Demosal/casen_2015.RDS")

casen_2015 <- readRDS(file = "Demosal/casen_2015.RDS")

sjlabelled::get_label(casen_2015)

#----Definicion de universo y población----

#actividad, niv educativo, edad, tipo de institucion, ocupación, sexo, quintil, rama, fact expansión

c2015u <- select(casen_2015, activ, educ, oficio1, oficio4, edad, sexo, qaut, rama1, expr)

#Renombrar nombre variables

c2015u <- rename(c2015u, ocupacion=activ, empleo=oficio4, niveleduc=educ,
                 quintil=qaut, rama=rama1)

#Definición de la población
c2015u$ocupacion <- as.numeric(c2015u$ocupacion)
c2015u$niveleduc <- as.numeric(c2015u$niveleduc)



#Proporción de población activa con estudios 92

activ2015 <- filter(c2015u, ocupacion==1) 
activ2015 <-  mutate(activ2015, niveleduc = car::recode(activ2015$niveleduc, "7:12=1; else = 2"))
#1 = Con estudios ESUP
#2= sin estudios ESUP

#Factor de expansion para base casen 92
activ2015p <- activ2015 %>% as_survey_design(ids = 1, weights = expr)

conestudios15<- activ2015p%>% 
  group_by(niveleduc) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))
conestudios15

# % graduados: 33,5 (33,1-34,0)

#Corregir "empleo" que es numero guardado como texto, para que sea numeric
c2015u$empleo<-type.convert(c2015u$empleo) #pasarlo a interger
c2015u$empleo <- as.numeric(c2015u$empleo) #pasarlo a numeric (hacerlo directo tira error)
class(c2015u$empleo)

#Filtro por ocupados de nivel técnico incompleto o superior:
c2015f <- filter(c2015u, ocupacion==1 & (niveleduc>=7 & niveleduc<=12))

#Construir variable subempleo por competencias ----

#basarnos en juan bravo, http://www.clapesuc.cl/assets/uploads/2016/12/26-12-16-anlisis-subempleo-por-competencias.pdf

#empleo CIUO 4 digitos
#1. sin calificacipon edusup (CIUO 4000 a 9000) excepto: 4121, 4122 5132 a 5133, 7230 a 7233, 7243
#2. baja calificacion (CIUO 3000 y 4121, 4122, 5132 a 5133, 7230 a 7233, 7243
#3. alta calificaci?n (CIUO 1000 y 2000)

#niveleduc
#1. sin titulo, tecnico superior incompleto 
#2. tecnico superior completo y y universitario incompleto
#3. universitario completo y posgraduados



#Segmentar variables Casen 2015:


califempleo15<- mutate(c2015f, empleo = car::recode(c2015f$empleo, "1000:2999 = 3; 3000:3999= 2; 4121:4122=2; 5132:5133=2; 7230:7233=2;7243=2; 4000:9998=1; else = NA"), 
                       niveleduc = car::recode(c2015f$niveleduc, "7=1; 8=2; 9=2; 10=3; 11=3; 12 = 3; else = NA"))

#Sintaxis subempleo por competencia Casen 2015:
base15 <-
  mutate(califempleo15,
         subempleo15 = case_when(empleo == 1 & niveleduc == 1 ~ "NO",
                                 empleo == 1 & niveleduc == 2 ~ "SI",
                                 empleo == 1 & niveleduc == 3 ~ "SI",
                                 empleo == 2 & niveleduc == 1 ~ "NO",
                                 empleo == 2 & niveleduc == 2 ~ "NO",
                                 empleo == 2 & niveleduc == 3 ~ "SI",
                                 empleo == 3 & niveleduc == 1 ~ "NO",
                                 empleo == 3 & niveleduc == 2 ~ "NO",
                                 empleo == 3 & niveleduc == 3 ~ "NO"))

#Recodificar para base casen 2015

base15 <- mutate(base15, subempleorec15 = dplyr::recode(base15$subempleo15, "NO" = "1", "SI" = "2"))

base15$subempleorec15 <- factor(base15$subempleorec15,
                                labels = c("No subempleado", "Subempleado"))

#Factor de expansion para base casen 2015
base15p <- base15 %>% as_survey_design(ids = 1, weights = expr)

subempleo15 <- base15p %>% 
  group_by(subempleo15) %>% 
  summarize(frecuencias = survey_total(na.rm = T),
            proporcion = survey_mean(vartype = "ci", level = 0.95, na.rm = TRUE),
            n = unweighted(n()))
subempleo15

#Subempleados 1 digito %: 33,7 (32,9-34,6)
#Subempleados 4 digitos %: 31,9 (31,1-32,7)

