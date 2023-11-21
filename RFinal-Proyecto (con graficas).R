#Modelo escogido LOG IT

rm(list=ls())                  
options(warn=-1)              


#1. Cargar librerias

library(foreign)                 
library(dplyr) 
library(stats) 
library(ggplot2)

#2. Cargar base de datos 
url<-"https://raw.githubusercontent.com/anddie6/Proyecto-Estad-stica/main/Base.csv"
Base<-read.csv(url)
View(Base)

#3. Descripción de las Variables

# Variable DEPENDIENTE (categórica)
#   CONFIABLE = Qué tan confiable perciben los perfiles (Recodificada 1 y 2 = No confiable / 3,4 y 5 = COnfiable)
#           0 = No confiable
#           1 = Confiable

# Covariables_INDEPENDIENTES (categóricas)
#     esc_cve= Clave de la escuela
#         sexo= Sexo
#           1 = Hombre
#           2 = Mujer
#   nivel_esc = nivel educativo
#           1 = Posgrado
#           2 = Licenciatura
#    lenguaje = Idioma del perfil
#           0 = Español  
#           1 = Ingles
#      puesto = Tipo de puesto
#           1 = Alta dirección
#           2 = Responsabilidad Intermedia
#           3 = Nivel inicial
#           4 = Nivel Basico
#sostenimiento= Tipo de sostenimiento
#           0 = Privado
#           1 = Publico
# experiencia = tiene o no
#           0 = Sin experiencia
#           1 = Con experiencia

# NUMÉRICAS: edad,
# (número de habilidades) cognitivas, sociales, tecnicas,
# (número) trabajos_n, palabras_n, validaciones_n, contactos_n, Rimbombante, atractivo


#3.1 Convertir variables al formato correspondiente
Base$confiable=as.factor(Base$confiable)

#Categóricas
Base$nivel_esc=as.factor(Base$nivel_esc)
Base$sexo=as.factor(Base$sexo)
Base$lenguaje=as.factor(Base$lenguaje)
Base$sostenimiento=as.factor(Base$sostenimiento)
Base$experiencia=as.factor(Base$experiencia)
Base$puesto=as.factor(Base$puesto)
Base$esc_cve=as.factor(Base$esc_cve)
#Numérica 
Base$edad=as.numeric(Base$edad)
Base$cognitivas=as.numeric(Base$cognitivas)
Base$sociales=as.numeric(Base$sociales)
Base$tecnicas=as.numeric(Base$tecnicas)
Base$trabajos_n=as.numeric(Base$trabajos_n)
Base$palabras_n=as.numeric(Base$palabras_n)
Base$validaciones_n=as.numeric(Base$validaciones_n)
Base$contactos_n=as.numeric(Base$contactos_n)
Base$rimbo_n=as.numeric(Base$rimbo_n)
Base$atractivo=as.numeric(Base$atractivo)

str(Base)

#Limpiar los missings de la base
BaseL<-na.omit(Base)

#3. Etiquetar variables (metodologia)
BaseL$confiable<-factor(BaseL$confiable, levels = c(0,1), labels = c("No confiable","Confiable"))
BaseL$nivel_esc<-factor(BaseL$nivel_esc, levels = c(1,2), labels = c("Posgrado","Licenciatura"))
BaseL$sexo<-factor(BaseL$sexo, levels = c(1,2), labels = c("Hombre","Mujer"))
BaseL$lenguaje<-factor(BaseL$lenguaje, levels = c(0,1), labels = c("Español","Ingles"))
BaseL$puesto<-factor(BaseL$puesto, levels = c(1,2,3,4), labels = c("Alta dirección","Intermedia","Nivel Inicial","Nivel Basico"))
BaseL$sostenimiento<-factor(BaseL$sostenimiento, levels = c(0,1), labels = c("Privado","Publico"))
BaseL$experiencia<-factor(BaseL$experiencia, levels = c(0,1), labels = c("Sin experiencia","Con experiencia"))

#4.1 Variable dependiente porcentajes

resultados_confiable<-table(BaseL$confiable)
porcentajes_confiable <- prop.table(resultados_confiable) * 100
porcentajes_confiable

#4.2 Covariables porcentajes
#4.2.1 Categoricas

#Nivel de escolaridad
resultados_nivel<-table(BaseL$nivel_esc)
porcentajes_nivel <- prop.table(resultados_nivel) * 100
porcentajes_nivel

#Sexo
resultados_sexo<-table(BaseL$sexo)
porcentajes_sexo <- prop.table(resultados_sexo) * 100
porcentajes_sexo

#Lenguaje
resultados_lenguaje<-table(BaseL$lenguaje)
porcentajes_lenguaje <- prop.table(resultados_lenguaje) * 100
porcentajes_lenguaje

#Puesto
resultados_puesto<-table(BaseL$puesto)
porcentajes_puesto <- prop.table(resultados_puesto) * 100
porcentajes_puesto

#Sostenimiento
resultados_sostenimiento<-table(BaseL$sostenimiento)
porcentajes_sostenimiento <- prop.table(resultados_sostenimiento) * 100
porcentajes_sostenimiento

#Experiencia
resultados_experiencia<-table(BaseL$experiencia)
porcentajes_experiencia <- prop.table(resultados_experiencia) * 100
porcentajes_experiencia

#Clave de Escuela
resultados_clave<-table(BaseL$esc_cve)
porcentajes_clave <- prop.table(resultados_clave) * 100
porcentajes_clave

#4.2.2 Numéricas

#Edad
resultados_edad<-table(BaseL$edad)
porcentajes_edad <- prop.table(resultados_edad) * 100
porcentajes_edad
datosedad <- data.frame(categoria = c("22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "51"),
                        porcentaje = c(0.133, 0.026, 0.026, 0.026, 0.348, 1.607, 2.223, 4.286, 11.626, 19.528, 15.778, 15.965, 11.867, 9.697, 4.955, 0.830, 0.294, 0.187, 0.267, 0.133, 0.026, 0.026, 0.026, 0.026, 0.026, 0.026, 0.026))
ggplot(datos, aes(x = "", y = porcentaje, fill = categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "edades", fill = "edades") +
  theme_void()

#Cognitivas
resultados_cognitivas<-table(BaseL$cognitivas)
porcentajes_cognitivas <- prop.table(resultados_cognitivas) * 100
porcentajes_cognitivas
datos_cog <- data.frame(categoria = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21"),
                        porcentaje = c(42.700, 6.214, 15.188, 11.626, 4.045, 5.518, 3.375, 3.160, 1.768, 2.196, 2.035, 0.776, 0.401, 0.294, 0.214, 0.133, 0.160, 0.053, 0.053, 0.026, 0.026, 0.0267))
ggplot(datos_cog, aes(x = "", y = porcentaje, fill = categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "habilidades cognitivas", fill = "no. habilidades") +
  theme_void()

#Sociales
resultados_sociales<-table(BaseL$sociales)
porcentajes_sociales <- prop.table(resultados_sociales) * 100
porcentajes_sociales
datos_soc <- data.frame(categoria = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21"),
                        porcentaje = c(34.369, 14.090, 16.474, 8.223, 6.723, 6.830, 4.178,  2.169,  1.714, 1.607, 0.964, 0.776, 0.616, 0.321, 0.214, 0.294, 0.160, 0.080, 0.080, 0.053, 0.026, 0.026))
ggplot(datos_soc, aes(x = "", y = porcentaje, fill = categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "habilidades sociales", fill = "no. habilidades") +
  theme_void()

#Técnicas
resultados_tecnicas<-table(BaseL$tecnicas)
porcentajes_tecnicas <- prop.table(resultados_tecnicas) * 100
porcentajes_tecnicas
datos_tec <- data.frame(categoria = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "12", "13", "15"),
                        porcentaje = c(49.852, 24.859, 10.715, 6.455, 4.312, 1.768, 0.910, 0.562, 0.214, 0.133, 0.133, 0.026, 0.267, 0.0267))
ggplot(datos_tec, aes(x = "", y = porcentaje, fill = categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "habilidades tecnicas", fill = "no. habilidades") +
  theme_void()

#Atractivo
resultados_atractivo<-table(BaseL$atractivo)
porcentajes_atractivo <- prop.table(resultados_atractivo) * 100
porcentajes_atractivo
datos_atra <- data.frame(categoria = c("0", "1", "2", "3", "4"),
                         porcentaje = c(8.1703, 29.975, 34.744, 19.957, 7.152))
ggplot(datos_atra, aes(x = "", y = porcentaje, fill = categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "atractivo", fill = "nivel") +
  theme_void()

#Trabajos
resultados_trabajos_n<-table(BaseL$trabajos_n)
porcentajes_trabajos_n <- prop.table(resultados_trabajos_n) * 100
porcentajes_trabajos_n
#Palabras
resultados_palabras_n<-table(BaseL$palabras_n)
porcentajes_palabras_n <- prop.table(resultados_palabras_n) * 100
porcentajes_palabras_n
#Validaciones
resultados_validaciones_n<-table(BaseL$validaciones_n)
porcentajes_validaciones_n <- prop.table(resultados_validaciones_n) * 100
porcentajes_validaciones_n
#Contactos
resultados_contactos_n<-table(BaseL$contactos_n)
porcentajes_contactos_n<- prop.table(resultados_contactos_n) * 100
porcentajes_contactos_n
#Rimbombantes
resultados_rimbo_n<-table(BaseL$rimbo_n)
porcentajes_rimbo_n <- prop.table(resultados_rimbo_n) * 100
porcentajes_rimbo_n


#5. Ajuste del modelo

regresion <- glm(confiable ~nivel_esc + sexo + lenguaje + sostenimiento + experiencia + puesto + edad + cognitivas + sociales + tecnicas + trabajos_n + palabras_n + validaciones_n + contactos_n + rimbo_n + atractivo + esc_cve , 
                 data = BaseL, family = "binomial")
summary(regresion)

#6. Interpretacion (momios)

momios<-exp(coefficients(regresion))%>%round(digits = 4)%>%data.frame()
momios

