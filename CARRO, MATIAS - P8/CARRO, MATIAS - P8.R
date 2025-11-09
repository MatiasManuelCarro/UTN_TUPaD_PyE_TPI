############################################################
# Pregunta 8 – Trabajo Práctico Integrador
# Muestreo aleatorio simple y comparación de medias
# Estadística – UTN (Semana 12-13)
############################################################

rm(list = ls()) #Elimina los objetos del entorno para evitar interferencias 

if (!require(readxl)) install.packages("readxl")
library(readxl)
if (!require(writexl)) install.packages("writexl")
library(writexl)
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
} else {
  library(ggplot2)
}


datos <- read_excel(file.choose())

#Cargamos la poblacion, en este Caso el tiempo semanal dedicado de estudio
poblacion <- datos$TIEMPO_SEMANAL_DEDIC


## 2. Parámetro poblacional (lo que vamos a usar de referencia) ----
media_poblacional <- mean(poblacion)
longitud_poblacion <- length(poblacion)
cat("Tamaño de la población:", longitud_poblacion, "\n")
cat("Media poblacional:", round(media_poblacional, 2), "\n\n")

## 3. Definir cuántas muestras y de qué tamaño ---------------------
set.seed(123) # para que te dé lo mismo cada vez (podés cambiarlo)
cantidad_muestras <- 6
tamanio_muestra <- 20
## 4. Generar las 6 muestras aleatorias simples --------------------
# vamos a guardar todo en una lista
lista_muestras <- list()
medias_muestrales <- numeric(cantidad_muestras)
for (i in 1:cantidad_muestras) {
  # sample() hace el muestreo aleatorio simple SIN reposición
  muestra_i <- sample(poblacion, tamanio_muestra, replace = FALSE)
  lista_muestras[[i]] <- muestra_i
  medias_muestrales[i] <- mean(muestra_i)
  
}

## 5. Armar un cuadro resumen con las 6 medias ---------------------
resultado <- data.frame(
  muestra = paste0("Muestra_", 1:cantidad_muestras),
  media_muestral = round(medias_muestrales, 2)
)


# Agregamos la diferencia respecto de la media poblacional
resultado$diferencia_respecto_poblacion <- round(
  resultado$media_muestral - media_poblacional, 2
)
print("Resultado: ")
print(resultado)
## 6. Interpretación sugerida (para el informe) --------------------
cat("\nComparación con la media poblacional (", round(media_poblacional,2), "):\n", sep = "")
for (i in 1:cantidad_muestras) {
  cat(resultado$muestra[i], "→ media =", resultado$media_muestral[i],
      " | diferencia =", resultado$diferencia_respecto_poblacion[i], "\n")
}

## 7. (Opcional) Visualización rápida ------------------------------
barplot(
  resultado$media_muestral,
  names.arg = resultado$muestra,
  main = "Medias de las 6 muestras vs. parámetro poblacional",
  ylab = "Media muestral"
)
abline(h = media_poblacional, lty = 2) # línea de la media poblacional


## 6. Se calcula el IC de las muestras, para realizar la comparacion con 
#la poblacion
cat("\n\n")
cat("IC de las muestras: ","\n\n")
alpha <- 0.05
gl <- tamanio_muestra - 1
tcrit <- qt(1 - alpha/2, df = gl)

# Calcular medias e IC en un solo paso
ics <- do.call(rbind, lapply(1:cantidad_muestras, function(i) {
  x <- lista_muestras[[i]]
  m <- mean(x)
  s <- sd(x)
  se <- s / sqrt(tamanio_muestra)
  li <- m - tcrit * se
  ls <- m + tcrit * se
  data.frame(
    muestra = paste0("Muestra_", i),
    media = round(m, 2),
    desviacion_estandar = round(s, 2),
    tamanio_muestra = tamanio_muestra,
    error_estandar = round(se, 2),
    li = round(li, 2),
    ls = round(ls, 2)
  )
}))


print(ics)  # ver tabla con medias e IC

# Gráfico para comparar los IC de las muestras con la media poblacional
print(
  ggplot(ics, aes(x = muestra, y = media)) +
    geom_point(size = 3, color = "blue") +
    geom_errorbar(aes(ymin = li, ymax = ls), width = 0.2, color = "darkblue") +
    geom_hline(yintercept = media_poblacional, linetype = 2, color = "red", linewidth = 1) +
    annotate("text", 
             x = 0.5,  # posición del texto sobre eje X
             y = media_poblacional + 0.5,  # posicion sobre eje Y 
             label = paste("Media poblacional =", round(media_poblacional, 2)),
             color = "red", size = 3, hjust = 0) +
    labs(
      title = "Medias muestrales y sus IC95% comparado con media poblacional",
      y = "Horas semanales",
      x = "Muestras"
    ) +
    theme_minimal()
)

#Exportacion de los datos
write_xlsx(
  list(
    "Resumen_muestras" = resultado,
    "IC_muestras" = ics
  ),
  "analisis_muestras.xlsx"
)



