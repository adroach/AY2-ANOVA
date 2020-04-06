#Ayudantía 2 ANOVA 2020 - 1
#Modelo de un factor fijo, supuestos, comparación de medias de a pares.
#Modelo de dos factores fijos.
rm(list = ls())

#Pregunta 1: Modelo de un factor fijo
#Estamos interesados en conocer si hay colores más atractivos para los insectos. Para ello se diseñaron 
#trampas con los siguientes colores: amarillo, azul, blanco y verde. Se cuantificó el número de insectos 
#que quedaban atrapados:

insectos <- c(16,11,20,21,14,7,37,32,15,25,39,41,21,12,14,17,13,17,45,59,48,46,38,47)
colores <- as.factor(c(rep(c("azul", "verde", "blanco", "amarillo"), each = 6)))
atractivo <- cbind.data.frame(colores, insectos)

str(atractivo)
#P1
  
  
library(dplyr)

medias_color = atractivo %>%
  group_by(colores)%>%
  summarise(mu_i = mean(insectos))

medias_color

# Pareciera que os colores mas atractivos para los insectos son son el
# amarillo y el verde, en comparacion al azul y el blanco.

#grafico boxplot 1

boxplot(insectos ~ colores, col = c("yellow","blue","white","green"),main ="numero de insectos atrapados por color",ylab = "numero de insectos")


grafico boxplot 2
library(ggplot2)

p = ggplot(atractivo, aes(x = colores, y = insectos)) + geom_boxplot(fill = c("yellow","blue","white","green"),outlier.color = NULL)

p + ylab("colores") + ylab("numero de insectos")+ ggtitle("Numero de insectos atrapados por color")+
  theme_bw()











#gana el amarillo por muy lejos, le sigue el verde y el azul y blanco estan mas abajo y estan mas parecidos
#si quieremos hacer el grafico del boxplot con colores,pintar o rellenar on ggplot, se puede obtener un grafico mejor





#P2 ajustar anova

# Con el factr colores que expica la variable

# Numero de insectos que quedan en la trampa


fm = aov(insectos ~ colores)

# ver supuestos


r = length(unique(colores))# categorias del factor


# vemos si es balanceado


casos_balanceados = atractivo%>%
  group_by(colores)%>%
  summarize(n_i = length(insectos))
ni = casos_balanceados$n_i[1]


nT = sum(casos_balanceados$n_i)

