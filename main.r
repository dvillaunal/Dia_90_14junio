## ---- eval=FALSE, include=TRUE-------------------------------------------------------
## "Protocolo:
## 
##  1. Daniel Felipe Villa Rengifo
## 
##  2. Lenguaje: R
## 
##  3. Tema: ANÁLISIS DISCRIMINANTE LINEAL Y CUADRÁTICO EN R [Parte 2]
## 
##  4. Fuentes:
##     https://www.r-bloggers.com/2018/11/linear-quadratic-and-regularized-discriminant-analysis/"


## ------------------------------------------------------------------------------------
# Creador de los OUTPUTS:
sink(file = "OUTPUTS.txt")

# lecturas de la base de datos:
Auto <- read.csv(file = "Auto.csv")

attach(Auto)

library(dplyr)
library(knitr)

# Subgrupo con las variables seleccionadas para el modelo
Auto2 <- select(Auto, cylinders, displacement, horsepower, weight, mpg01)
Auto2$mpg01 <- as.factor(mpg01)
kable(head(Auto2, 3), align = "c")

print("#Visualizar Los 10 primeros datos de [Auto2]")
print(head(Auto2, 10))

# Clases de las variables de [Auto2]
print("# Clases de las variables de [Auto2]")
print(str(Auto2))

# Estadisticas basicas de [Auto2]
print("# Estadisticas basicas de [Auto2]")
print(summary(Auto2))

# Ahora Graficaremos, algunos histogramas con un loop -> veremos las distribuciones:
#mpg01 = 0 (bajo rendimiento)
print("#mpg01 = 0 (bajo rendimiento)")
#mpg01 = 1 (alto rendimiento)
print("#mpg01 = 1 (alto rendimiento)")

pdf("HistogramDistros.pdf")

for (k in 2:4) {
     v <- names(Auto2)[k]
     for (i in 1:2) {
         l <- levels(Auto2$mpg01)[i]
         x <- Auto2[Auto2$mpg01 == l, v]
         hist(x, proba = T, col = grey(0.8), main = paste("mpg01", l), xlab = v)
         x0 <- seq(min(Auto2[, k]), max(Auto2[, k]), le = 50)
         lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
}
}

dev.off()

# Gráficos Q-Q de cada variable para la clase mpg01 = 0 (bajo rendimiento)
# Gráficos Q-Q de cada variable para la clase mpg01 = 1 (alto rendimiento)

pdf("GraficosQ_Q.pdf")

for (k in 2:4) {
v <- names(Auto2)[k]
for (i in 1:2) {
l <- levels(Auto2$mpg01)[i]
x <- Auto2[Auto2$mpg01 == l, v]
qqnorm(x, main = paste("mpg01", l, v), pch = 19, col = i + 1)
qqline(x)
}
}

dev.off()

library(reshape2)
library(knitr)
library(dplyr)
# Contraste de normalidad Shapiro-Wilk para cada variable en cada rendimiento
Auto2.tidy <- melt(Auto2, value.name = "valor")

# Kable -> es una manera con markdown de presentar datos:
kable(Auto2.tidy %>% group_by(mpg01, variable) %>% summarise(p_value_Shapiro.test = shapiro.test(valor)$p.value), align = "c")

# Conclusión:

print("En conjunto hay evidencias de falta de normalidad univariante en todas las variables empleadas como predictores, menos la variable weight para la clase mpg01 = 0 (rendimiento bajo).")



## ------------------------------------------------------------------------------------
#Además de la normalidad univariante, se requiere evaluar la normalidad multivariante.

# La presencia de valores atípicos puede ser causa de no cumplir esta condición.
#Por ello, es conveniente verificar si los datos tienen outliers multivariantes (valores extremos para combinaciones de variables) antes de comenzar con el análisis multivariante.

#Gracias al paquete "MVN" podemos evaluar la normalidad multivariante con tres de los test comúnmente utilizados, como el de "Mardia", "Royston" y "Henze-Zirkler", así como identificar los outliers multivariantes que puedan influir en el contraste.

#install.packages("MVN")
library(MVN)

#Detección de outliers multivariantes
print("el argumento multivariateOutlierMethod emplea el método de cuantiles basado en la distancia de Mahalanobis y la distancia de Mahalanobis ajustada.")

pdf("multivariateOutlierMethod.pdf")

# Distancia de Mahalanobis

outliers <- mvn(data = Auto2[,2:4], multivariateOutlierMethod = "quan")

# Distancia ajustada de Mahalanobis

outliers.adj <- mvn(data = Auto2[,2:4], multivariateOutlierMethod = "adj")

dev.off()

print("En torno al 40% de las observaciones se consideran outliers multivariantes (excluyendo la variable continua discreta cylinders),un porcentaje muy alto. Hay que tener en cuenta de lo anterior que la normalidad univariante no se cumple.")


#Test MVN de Royston:

print("#Test MVN de Royston:")

png(filename = "Test_MVN_Royston.png")

royston <- mvn(data = Auto2[,-5], mvnTest = "royston", multivariatePlot = "qq")

dev.off()

#Nombre de las variables de la anterior función:
print("#Nombre de las variables de la anterior función:")
names(royston)

#Imprimimos el test:
print(royston)

#Test MVN de Mardia
print("#Test MVN de Mardia")

print("El test de Mardia calcula los coeficientes de asimetría y curtosis y la correspondiente significancia estadística, combinando ambos para dar un resultado global de normalidad multivariante (MVN):")

#organizamos el test eliminando la varible 5:
mardia <- mvn(data = Auto2[,-5], mvnTest = "mardia")

print(mardia$multivariateNormality)

#Test MVN de Henze-Zirkler
print("#Test MVN de Henze-Zirkler")

#organizamos el test eliminando la varible 5:
hz <- mvn(data = Auto2[,-5], mvnTest = "hz")

print(hz$multivariateNormality)

#Conclusión:

print("Todos los test llevados a cabo muestran evidencias significativas (α = 0,05) de que los datos no siguen una distribución normal multivariante (podríamos deducir que el problema se debe a la falta de normalidad univariante), por lo que habrá que tener en cuenta que tendrá implicaciones en la precisión del LDA/QDA.")
