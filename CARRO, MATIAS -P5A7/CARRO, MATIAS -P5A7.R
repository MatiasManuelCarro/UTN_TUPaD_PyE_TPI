
#TPI SEMANA 11
#Alumno: Matias Carro

#Ejercicio 5

#EJ 5.A
#usando pbinom
pbinom(9, size = 16, prob = 0.572, lower.tail = FALSE)

#sumando todos
dbinom(10, 16, 0.572)+dbinom(11, 16, 0.572)+dbinom(12, 16, 0.572)+dbinom(13, 16, 0.572)+dbinom(14, 16, 0.572)+dbinom(15, 16, 0.572)+dbinom(16, 16, 0.572)

#utilizando sum
sum(dbinom(10:16, 16, 0.572))


#EJ 5.B
#satisfechos = 0.304
# P=(4 <= x <= 8)

#con pbinom acumuladas
pbinom(8, 16, prob = 0.304) - pbinom(3, 16, prob = 0.304)

#sumando todos los dbinom
dbinom(4, 16, 0.304)+dbinom(5, 16, 0.304)+dbinom(6, 16, 0.304)+dbinom(7, 16, 0.304)+dbinom(8, 16, 0.304)

#utilizando sum
sum(dbinom(4:8, 16, 0.304))


#Ej 5.c
#insatisfechos = 0.64
#usando pbinom
pbinom(4, 16, 0.64)

#sumando todos los dbinom
dbinom(1, 16, 0.64)+dbinom(2, 16, 0.64)+dbinom(3, 16, 0.64)+dbinom(4, 16, 0.64)

#utilizando sum
sum(dbinom(1:4, 16, 0.64))

#Ej 5.d

#muy insatisfechos = 0.06
dbinom(10, 16, 0.06)
#resultado: 3.34044e-09

#mostrando sin notacion cientifica:
format(dbinom(10, 16, 0.06), scientific = FALSE, digits = 12)
#resultado: 0.00000000334044011088

#---------------------------------
#Ej 6
#Se calcula lambda para el enunciado
#15 consultas = k
#30 minutos = n
#k / n = 15/30 
λ <- 15/30 # = 0.5

#Ej 6.A
#x = 6
#Se calcula lambda
λ <- λ * 20 # = 10
15/30 * 20 # =10

#se aplica poisson
dpois(6,10)
#Resultado: 0.06305546

#EJ 6.B
#x <= 12

#Se calcula lambda
15/30 * 40 # = 20

#se calcula 
ppois(12, 20)
#Resultado:  0.03901199

#Ej 6.c
#x >= 8  x <= 9

#Lambda = 15 (mismo tiempo que el enunciado)

#Poisson
#x >= 8
ppois(8 - 1, 15, lower.tail = FALSE)
#x <= 9
ppois(9, 15)

#Diferencia
ppois(8 - 1, 15, lower.tail = FALSE) - ppois(9, 15)

#Resultado:
#0.9121441

#==============
# == Punto 7 ==
#==============

#Calculando variable continua altura

rm(list = ls()) #Elimina los objetos del entorno para evitar interferencias 

if (!require(readxl)) install.packages("readxl")
library(readxl)
#usamos openxlsx para guardar las tablas al final
if (!require(openxlsx)) install.packages("openxlsx")
library(openxlsx)
# Instalar ggplot2 si no está instalado
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

#Cargar el archivo de excel
datos <- read_excel(file.choose())

variable_continua <- "ESTATURA CM." #nombre de la variable continua a analizar
k <- ceiling(1 + 3.22 * log10(nrow(datos)))
min_val <- floor(min(datos[[variable_continua]], na.rm = TRUE))
max_val <- ceiling(max(datos[[variable_continua]], na.rm = TRUE))
amplitud <- ceiling(max_val - min_val) / k
max_tope <- min_val + amplitud * k 
cortes <- seq(min_val, max_tope, by = amplitud)

datos$clases <- cut(datos[[variable_continua]], breaks = cortes, right = FALSE, include.lowest = TRUE)


#------------------------
#---- Marca de clase ----
#------------------------

marca_clase <- (head(cortes, -1) + tail(cortes, -1)) / 2 #marca de clase de cada intervalo
marca_clase

#tabla de frecuencias

tabla_clases <- table(datos$clases)
f_acum <- cumsum(tabla_clases) #Frecuencia acumulada 
f_rel <- prop.table(tabla_clases) #Frecuencia relativa
f_rel_acum <- cumsum(f_rel) #frecuencia relativa acumulada 

#Mostrar tabla de frecuencias 
tabla_frecuencia <- data.frame(
  Intervalo =names(tabla_clases),
  Marca = as.vector(marca_clase),
  Frec_Abs = as.vector(tabla_clases),
  Frec_Acum = as.vector(f_acum),
  Frec_Rel = round(as.vector(f_rel), 4),
  Frec_Rel_Acum = round(as.vector(f_rel_acum), 4)
)

message("\n Tbla de Frecuencias - VARIABLE CONTINUA (", variable_continua,")")

print(tabla_frecuencia, row.names = FALSE)

#-----------------------------
#---- calculo de la media ----
#-----------------------------

frecuencias <- as.vector(tabla_clases) #vector de frecuencias absolutas
media_continua <- sum(marca_clase * frecuencias) / sum(frecuencias)

#-------------------------
#---- Calculo de moda ----
#-------------------------

i_modal <- which.max(frecuencias) #indice de la clase modal
L_m <- cortes[i_modal] #limite inferior del intervalo modal
f_m <- frecuencias[i_modal] #Frecuencias del intervalo modal

#sintaxis ---- ifelse ----
f_1 <- ifelse(i_modal == 1, 0, frecuencias[i_modal -1]) #frecuencias anterior
f_2 <- ifelse(i_modal == length(frecuencias), 0, frecuencias[i_modal + 1]) 

moda_continua <- L_m + ((f_m - f_1)/((f_m - f_1) + (f_m -f_2))) * amplitud 

#-------------------------------
#---- calculo de la mediana ----
#-------------------------------

n_total <- sum(frecuencias)
n_2 <- n_total / 2

clase_mediana_index <- which(f_acum >= n_2)[1]
L <- cortes[clase_mediana_index] #limite inferior del intervalo de la mediana

F_anterior <- ifelse(clase_mediana_index == 1, 0, f_acum[clase_mediana_index -1])

f_mediana <- frecuencias[clase_mediana_index] #frecuencia de la clase mediana

mediana_continua <- L + ((n_2 - F_anterior) / f_mediana) * amplitud


#-------------------------------
#---- Medidas de dispercion ----
#-------------------------------

varianza_continua <- sum(frecuencias * (marca_clase - media_continua)^2) / (n_total -1) #varianza

desvio_continua <- sqrt(varianza_continua) #desvio estandar agrupado

coef_var_continua <- (desvio_continua / media_continua) * 100 #coeficiente de variacion




#mostrar datos agrupados
message("\n Resultados AGRUPADOS - VARIABLE CONTINUA (", variable_continua,")")
continua_stats <- data.frame(
  Media = round(media_continua, 4),
  Moda = round(moda_continua, 4),  
  Mediana = round(mediana_continua, 4),
  Varianza = round(varianza_continua, 4),
  Desvio_Estandar = round(desvio_continua, 4),
  Coef_Variacion_pct = round(coef_var_continua, 4)
)

print(continua_stats, row.names = FALSE)

#creamos el archivo de excel para almacenar usanddo openxlsx
wb <- createWorkbook()


#agregamos hojas para la tabla

addWorksheet(wb, "Stats_Altura")
writeData(wb, sheet = "Stats_Altura", continua_stats)

#guardamos el archivo usando 
saveWorkbook(wb, file = "D:\\TUP UTN\\TPI_PYE\\TPI alturas.xlsx", overwrite = TRUE)

# Esto nos da los resultados:

# Media:  164,6089

#Moda: 164,0139

#Mediana: 164,5

#Varianza: 41,5928

#Desvio_Estandar: 6,4492

#Coef_Variacion_pct: 3,9179

#--------------
#Ej 7 - A
# Calcular la probabilidad de que un estudiante seleccionado aleatoriamente
#tenga una estatura mayor o igual que 179 cm.

# Definimos los parámetros de la distribución normal
media <- 164.6089 # Media (μ)
desv <- 6.4492 # Desviación estándar (σ)
valor <- 179

x <- seq(media - 4*desv, media + 4*desv, length = 200)
y <- dnorm(x, mean = media, sd = desv)

# -----------------------------------------------
# Probabilidad mayor que
# -----------------------------------------------
# Ejemplo: P(X > valor)
# Cálculo numérico:
P_mayor <- pnorm(valor, mean = media, sd = desv, lower.tail = F)
cat('Probabilidad P(X > valor) =', P_mayor, '\n')
# Gráfico con el área sombreada a la derecha
# type = 'l' define que el gráfico será una línea continua
# lwd = 2 define el grosor de la línea
plot(x, y, type = 'l', lwd = 2, col = 'black', main = paste0('Probabilidad: P(X > ', valor, ') = ', round(P_mayor, 4)), xlab = 'X', ylab = 'Y')

# Para una probabilidad mayor que, los valores de x van desde valor hasta media + 4*desv
# El valor length = 100 es suficiente para lograr un sombreado adecuado.
x_sombreado <- seq(valor, media + 4*desv, length = 100)
y_sombreado <- dnorm(x_sombreado, mean = media, sd = desv)
# A continuación se definen las coordenadas x del polígono:
# Para una probabilidad mayor que los valores de x parten desde valor, continúan con los
#       valores intermedios x sombreado y finalizan en media + 4*desv
polygon(c(valor, x_sombreado, media + 4*desv), c(0, y_sombreado, 0), col = '#FF7F00', border = NA)

#Resultado: 0.0128


#----------------------
#Ejercicio 7 B
#Calcular la probabilidad de que un estudiante seleccionado aleatoriamente
#tenga una estatura comprendida entre 147 cm. y 172 cm.

# Definimos los parámetros de la distribución normal
media <- 164.6089 # Media (μ)
desv <-  6.4492  # Desviación estándar (σ)

valor1 <- 147 
valor2 <- 172 

x <- seq(media - 4*desv, media + 4*desv, length = 200)
y <- dnorm(x, mean = media, sd = desv)

P_entre <- pnorm(valor2, mean = media, sd = desv) - pnorm(valor1, mean = media, sd = desv)
cat('Probabilidad P(valor1 < X < valor2) =', P_entre, '\n')
# Gráfico con el área central sombreada
# type = 'l' define que el gráfico será una línea llena, por defecto será una línea de puntos
# lwd = 2 define el grosor de la línea
plot(x, y, type = 'l', lwd = 2, col = 'black',
     main = paste0('Probabilidad: P(', valor1, ' < X < ', valor2, ') = ', round(P_entre, 4)),
     xlab = 'X', ylab = 'Y')
# Para una probabilidad entre, los valores de x van desde valor1 hasta valor2
# El valor length = 100 es suficiente para lograr un sombreado adecuado.
x_sombreado <- seq(valor1, valor2, length = 100)
y_sombreado <- dnorm(x_sombreado, mean = media, sd = desv)

# A continuación se definen las coordenadas x del gráfico:
# Para una probabilidad entre, los valores de x van desde valor1, continúan con los valores
#     intermedios x sombreado y finalizan en valor2 porque debe calcularse P(valor1 < X < valor2)
polygon(c(valor1, x_sombreado, valor2), c(0, y_sombreado, 0), col = '#b8860b', border = NA)

#Resultado
#0.8709

#--------------------------
#Ejercicio 7.C
#Hallar el valor que excede al 97,5% de las estaturas

media <- 164.6089
desvio <- 6.4492

# Valor que excede al 97.5% (percentil 2.5)

qnorm(0.975, mean = media, sd = desvio)
#Resultado: 177.2491

#Calculamos y graficamos la distribucion normal

# Definimos los parámetros de la distribución normal
media <- 164.6089 # Media (μ)
desv <- 6.4492 # Desviación estándar (σ)
valor <- 177.2491 #valor que excede al 97.5% de las estaturas

x <- seq(media - 4*desv, media + 4*desv, length = 200)
y <- dnorm(x, mean = media, sd = desv)

# -----------------------------------------------
# Probabilidad mayor que
# -----------------------------------------------
# Ejemplo: P(X > valor)
# Cálculo numérico:
P_mayor <- pnorm(valor, mean = media, sd = desv, lower.tail = F)
cat('Probabilidad P(X > valor) =', P_mayor, '\n')
# Gráfico con el área sombreada a la derecha
# type = 'l' define que el gráfico será una línea continua
# lwd = 2 define el grosor de la línea
plot(x, y, type = 'l', lwd = 2, col = 'black', main = paste0('Probabilidad: P(X > ', valor, ') = ', round(P_mayor, 4)), xlab = 'X', ylab = 'Y')

# Para una probabilidad mayor que, los valores de x van desde valor hasta media + 4*desv
# El valor length = 100 es suficiente para lograr un sombreado adecuado.
x_sombreado <- seq(valor, media + 4*desv, length = 100)
y_sombreado <- dnorm(x_sombreado, mean = media, sd = desv)
# A continuación se definen las coordenadas x del polígono:
# Para una probabilidad mayor que los valores de x parten desde valor, continúan con los
#       valores intermedios x sombreado y finalizan en media + 4*desv
polygon(c(valor, x_sombreado, media + 4*desv), c(0, y_sombreado, 0), col = '#FF7F00', border = NA)

#Resultado: 0.025



