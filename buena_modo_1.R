
#PRUEBA 8:

#####-----1. Cargar y explorar los datos----####

library(tuneR)

# Cargar el archivo de sonido
load("cabritus.Rdata")

# Explorar la estructura del objeto
str(cabritus)

# Extraer los valores numéricos del sonido
y <- cabritus@left
I <- length(y)  # Número total de observaciones

# Reproducir el sonido original (opcional)
tuneR::play(cabritus)

####----2. Construccion de la Matriz de Diseño----####
# Crear la matriz con las funciones trigonométricas
X <- matrix(nrow=I, ncol=10000)
x <- 2 * pi * (1:I) / I

for (i in 1:5000) {
  X[,(i-1)*2+1] <- sin(i*x)
  X[,i*2] <- cos(i*x)
}

####----3. Selección de covariables con alta correlación----####
cors <- apply(X, 2, function(col) cor(col, y))
altas <- which(abs(cors) > 0.015)
length(altas)  # 438 funciones altamente correlacionadas

####----4. Seleccion de funciones adicionales con baja correlación----####
set.seed(1)
otras <- sample((1:10000)[-altas], length(altas))  # Seleccionar 438 funciones adicionales

# Crear matriz de diseño con las variables seleccionadas
freq.amp <- X[, c(altas, otras)]

# Guardar para uso posterior
save(freq.amp, file="freq_amp.Rdata")

# Eliminar la matriz original para ahorrar memoria
rm(list="X")

####----5. Ajuste del modelo lineal----####
# Cargar matriz reducida si no se ha cargado
load("freq_amp.Rdata")

# Ajustar modelo de regresión lineal
modelo <- lm(y ~ freq.amp)

# Obtener predicciones
y_pred <- predict(modelo)

# Guardar los valores predichos
save(y_pred, file="prediccion_cabritus.Rdata")

####----6. Evaluación del Modelo----####
# Comparar predicción con la señal ruidosa
plot(y[1:200], type="l", col="black", main="Comparación de señal ruidosa y predicción",
     ylab="Valores del archivo de sonido", xlab="Index")
lines(y_pred[1:200], col="red")  # Superponer predicción en rojo

####----7. Calculo del Error Cuadrático Medio----####
mse <- mean((y - y_pred)^2)
cat("MSE:", mse)


####----Empezamos a retocar cosas para mejorar el ajuste----####

##----PASO 1: REVISAR LA MATRZI DE DISEÑO----####
# Cargar datos
load("cabritus.Rdata")  # Asegúrate de tener el archivo en el directorio de trabajo

# Construcción de la matriz de Fourier
X <- matrix(nrow = length(cabritus@left), ncol = 10000)
x <- 2 * pi * (1:length(cabritus@left)) / length(cabritus@left)
for(i in 1:5000) {
  X[, (i - 1) * 2 + 1] <- sin(i * x)
  X[, i * 2] <- cos(i * x)
}

# Evaluamos la correlación con la señal
cors <- apply(X, 2, function(col) cor(col, cabritus@left))

# Probamos diferentes umbrales de correlación
umbrales <- c(0.01, 0.015, 0.02)
seleccion <- lapply(umbrales, function(umbral) which(abs(cors) > umbral))

# Cantidad de funciones seleccionadas por cada umbral
sapply(seleccion, length)

# Seleccionamos las columnas de la matriz de Fourier con correlación > 0.015
X_filtrado <- X[, seleccion[[2]], drop = FALSE]  # Usamos el segundo umbral (0.015)

# Ajustamos el modelo lineal
modelo <- lm(cabritus@left ~ X_filtrado)

# Predicción
y_pred <- predict(modelo)

# Evaluación del error cuadrático medio (MSE)
mse <- mean((cabritus@left - y_pred)^2)
cat("MSE:", mse)

# Graficamos la señal original y la predicción
plot(cabritus@left[1:200], type = "l", col = "black", ylab = "Valores del archivo de sonido", xlab = "Index")
lines(y_pred[1:200], col = "red")  # La predicción en rojo



####----Probamos añadiendo funciones:----####
# Probamos con el umbral de 0.01 (más funciones de Fourier)
X_filtrado <- X[, seleccion[[1]], drop = FALSE]  # Selección con umbral 0.01

# Ajustamos el modelo lineal
modelo <- lm(cabritus@left ~ X_filtrado)

# Predicción
y_pred <- predict(modelo)

# Nuevo MSE
mse <- mean((cabritus@left - y_pred)^2)
cat("Nuevo MSE:", mse)

# Graficamos la señal original vs predicción
plot(cabritus@left[1:200], type = "l", col = "black", ylab = "Valores del archivo de sonido", xlab = "Index")
lines(y_pred[1:200], col = "red")  

#¡Hemos logrado reducir el MSE significativamente! 🎉 Pasamos de 35,053,815 a 27,271,349, lo que indica que incluir más funciones de Fourier ha mejorado el ajuste.


#Probamos a seguir reduciendo:

# Probamos con un umbral menor (0.007) para ver si mejora el ajuste
umbral_nuevo <- 0.007
seleccion_nueva <- which(abs(cors) > umbral_nuevo)
X_filtrado_nuevo <- X[, seleccion_nueva, drop = FALSE]

# Ajustamos el modelo lineal con más funciones
modelo_nuevo <- lm(cabritus@left ~ X_filtrado_nuevo)

# Nueva predicción
y_pred_nuevo <- predict(modelo_nuevo)

# Nuevo MSE
mse_nuevo <- mean((cabritus@left - y_pred_nuevo)^2)
cat("MSE con umbral 0.007:", mse_nuevo)

# Graficamos la señal original vs predicción mejorada
plot(cabritus@left[1:200], type = "l", col = "black", ylab = "Valores del archivo de sonido", xlab = "Index")
lines(y_pred_nuevo[1:200], col = "blue")  # Nueva predicción en azul


#Probamos a seguir reduciendo:


# Probamos con un umbral aún menor (0.005)
umbral_mejorado <- 0.005
seleccion_mejorada <- which(abs(cors) > umbral_mejorado)
X_filtrado_mejorado <- X[, seleccion_mejorada, drop = FALSE]

# Ajustamos el modelo con más funciones de Fourier
modelo_mejorado <- lm(cabritus@left ~ X_filtrado_mejorado)

# Nueva predicción
y_pred_mejorado <- predict(modelo_mejorado)

# Nuevo MSE
mse_mejorado <- mean((cabritus@left - y_pred_mejorado)^2)
cat("MSE con umbral 0.005:", mse_mejorado)

# Graficamos la nueva predicción
plot(cabritus@left[1:200], type = "l", col = "black", ylab = "Valores del archivo de sonido", xlab = "Index")
lines(y_pred_mejorado[1:200], col = "green")  # Nueva predicción en verde


#Continuamos bajando el umbral:

# Probamos con un umbral aún menor (0.004)
umbral_nuevo <- 0.004
seleccion_nueva <- which(abs(cors) > umbral_nuevo)
X_filtrado_nuevo <- X[, seleccion_nueva, drop = FALSE]

# Ajustamos el modelo con más funciones de Fourier
modelo_nuevo <- lm(cabritus@left ~ X_filtrado_nuevo)

# Nueva predicción
y_pred_nuevo <- predict(modelo_nuevo)

# Nuevo MSE
mse_nuevo <- mean((cabritus@left - y_pred_nuevo)^2)
cat("MSE con umbral 0.004:", mse_nuevo)

# Graficamos la nueva predicción
plot(cabritus@left[1:200], type = "l", col = "black", ylab = "Valores del archivo de sonido", xlab = "Index")
lines(y_pred_nuevo[1:200], col = "blue")  # Nueva predicción en azul


#Seguimos bajando:

# Probamos con un umbral aún menor (0.003)
umbral_003 <- 0.003
seleccion_003 <- which(abs(cors) > umbral_003)
X_filtrado_003 <- X[, seleccion_003, drop = FALSE]

# Ajustamos el modelo con más funciones de Fourier
modelo_003 <- lm(cabritus@left ~ X_filtrado_003)

# Nueva predicción
y_pred_003 <- predict(modelo_003)

# Nuevo MSE
mse_003 <- mean((cabritus@left - y_pred_003)^2)
cat("MSE con umbral 0.003:", mse_003)

# Graficamos la nueva predicción
plot(cabritus@left[1:200], type = "l", col = "black", ylab = "Valores del archivo de sonido", xlab = "Index")
lines(y_pred_003[1:200], col = "red")  # Nueva predicción en morado


#Seguimos bajando:

# Probamos con un umbral aún menor (0.0025)
umbral_0025 <- 0.0025
seleccion_0025 <- which(abs(cors) > umbral_0025)
X_filtrado_0025 <- X[, seleccion_0025, drop = FALSE]

# Ajustamos el modelo con más funciones de Fourier
modelo_0025 <- lm(cabritus@left ~ X_filtrado_0025)

# Nueva predicción
y_pred_0025 <- predict(modelo_0025)

# Nuevo MSE
mse_0025 <- mean((cabritus@left - y_pred_0025)^2)
cat("MSE con umbral 0.0025:", mse_0025)

# Graficamos la nueva predicción
plot(cabritus@left[1:200], type = "l", col = "black", ylab = "Valores del archivo de sonido", xlab = "Index")
lines(y_pred_0025[1:200], col = "purple")  # Nueva predicción en naranja


#Puede que sea la ultima bajada:

# Probamos con un umbral aún menor (0.002)
umbral_002 <- 0.002
seleccion_002 <- which(abs(cors) > umbral_002)
X_filtrado_002 <- X[, seleccion_002, drop = FALSE]

# Ajustamos el modelo con más funciones de Fourier
modelo_002 <- lm(cabritus@left ~ X_filtrado_002)

# Nueva predicción
y_pred_002 <- predict(modelo_002)

# Nuevo MSE
mse_002 <- mean((cabritus@left - y_pred_002)^2)
cat("MSE con umbral 0.002:", mse_002)

# Graficamos la nueva predicción
plot(cabritus@left[1:200], type = "l", col = "black", ylab = "Valores del archivo de sonido", xlab = "Index")
lines(y_pred_002[1:200], col = "blue")  # Nueva predicción en azul


#Nos quedaríamos con el de 0.0025
# Guardamos la mejor predicción con umbral 0.0025
save(y_pred_0025, file = "cabritus_filtrado00025.Rdata")

# Confirmamos que el archivo se ha guardado
cat("Archivo cabritus_filtrado00025.Rdata guardado con éxito.\n")

