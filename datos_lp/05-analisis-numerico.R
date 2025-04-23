library(tidyselect)
library(janitor)

# Estructura del conjunto de datos
str(datos_limpios)

# Paqueta de medidas resumen 
summary(datos_limpios)
summary(datos_limpios[,c(2,3,4)])

# Funciones para obtener medidas
attach(datos_limpios)

# Posición: tendencia central
mean(porcentaje_de_aumento_de_alquiler, na.rm = TRUE) # Mediana
median(porcentaje_de_aumento_de_alquiler, na.rm = TRUE) # Media aritmética

# Posición: otras
min(porcentaje_de_aumento_de_alquiler, na.rm = TRUE) 
max(porcentaje_de_aumento_de_alquiler, na.rm = TRUE)
quantile(porcentaje_de_aumento_de_alquiler, na.rm = TRUE) # 5 medidas resumen
quantile(porcentaje_de_aumento_de_alquiler, na.rm = TRUE, 0.9) # Otros percentiles

# Dispersión
range(porcentaje_de_aumento_de_alquiler, na.rm = TRUE) # Valores mín y max
max(porcentaje_de_aumento_de_alquiler, na.rm = TRUE) - min(porcentaje_de_aumento_de_alquiler, na.rm = TRUE) # Rango
sd(porcentaje_de_aumento_de_alquiler, na.rm = TRUE) # Desvío estándar
var(porcentaje_de_aumento_de_alquiler, na.rm = TRUE) # Variancia
IQR(porcentaje_de_aumento_de_alquiler, na.rm = TRUE) # Rango intercuartílico
round(sd(porcentaje_de_aumento_de_alquiler, na.rm = TRUE)/mean(porcentaje_de_aumento_de_alquiler, na.rm = TRUE)*100,1) # Coeficiente de variación


# Posición: tendencia central
mean(núm_integrantes, na.rm = TRUE) # Mediana
median(núm_integrantes, na.rm = TRUE) # Media aritmética

# Posición: otras
min(núm_integrantes, na.rm = TRUE) 
max(núm_integrantes, na.rm = TRUE)
quantile(núm_integrantes, na.rm = TRUE) # 5 medidas resumen
quantile(núm_integrantes, na.rm = TRUE, 0.9) # Otros percentiles

# Dispersión
range(núm_integrantes, na.rm = TRUE) # Valores mín y max
max(núm_integrantes, na.rm = TRUE) - min(núm_integrantes, na.rm = TRUE) # Rango
sd(núm_integrantes, na.rm = TRUE) # Desvío estándar
var(núm_integrantes, na.rm = TRUE) # Variancia
IQR(núm_integrantes, na.rm = TRUE) # Rango intercuartílico
round(sd(núm_integrantes, na.rm = TRUE)/mean(núm_integrantes, na.rm = TRUE)*100,1) # Coeficiente de variación

sort(table(relación_con_propiedad), decreasing = TRUE)[1] # Moda

sort(table(hacinamiento), decreasing = TRUE)[1] # Moda

respuesta_multiple_table <- as.data.frame(c(sum(na.omit(plaga_cucaracha)), sum(na.omit(plaga_mosquito)), sum(na.omit(plaga_rata))))
colnames(respuesta_multiple_table) <- c("Freq")
rownames(respuesta_multiple_table) <- c("Cucarachas", "Mosquitos", "Ratas")
rownames(respuesta_multiple_table)[which.max(respuesta_multiple_table$Freq)] # Moda
rm(respuesta_multiple_table)

sort(table(conviven_personas_con_discapacidades), decreasing = TRUE)[1] # Moda
