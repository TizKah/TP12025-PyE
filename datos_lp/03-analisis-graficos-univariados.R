# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)

# Fijo el dataset
attach(datos_limpios)

##############
# Histograma #
##############

ggplot(datos_limpios) +
  aes(x = porcentaje_de_aumento_de_alquiler, y = ..count../sum(..count..)) +
  geom_histogram(fill = "lightgray", col = "black", 
                 breaks = c(0,.25,.5,.75,1,2,6)) +
  scale_x_continuous(labels = scales::percent, breaks = c(0,.25,.5,.75,1,2,6)) +
  scale_y_continuous(breaks = c(0,.2,.4,.6), limits=c(0,.6)) +
  ggtitle("Porcentaje de aumento de alquiler en el último año.\nRelevamiento de Condiciones Habitacionales 2022, La Poderosa.") +
  labs(x = "Porcentaje de aumento de alquiler", y = "Densidad")

#######################
# Gráfico de bastones #
#######################

ggplot(datos) +
  aes(x = núm_integrantes) + 
  geom_bar(width = 0.10) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  ggtitle("Cantidades de viviendas que presentan cierta cantidad de integrantes.\nRelevamiento de Condiciones Habitacionales 2022, La Poderosa.") +
  labs(x = "Cantidad de integrantes de la vivienda", y = "Cantidad de viviendas") +
  theme_classic()

#####################
# Gráfico de barras #
#####################

datos_analisis %>%
  ggplot() + 
  
  #aes(x = hacinamiento) + # Frecuencias absolutas
  aes(x = hacinamiento) +
  #aes(x = hacinamiento, y = ..count.. / sum(..count..)) + # Porcentajes
  # aes(x = reorder(hacinamiento, hacinamiento, function(x) -length(x)), 
  #		y = ..count.. / sum(..count..)) +  # Porcentajes ordenados según frecuencia
  #scale_y_continuous(labels = scales::percent) +    # Eje para porcentajes
  
  geom_bar(width = 0.75,   # Ancho de barras
           fill = '#7ed021',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6) +  # Transparencia
  
  labs(y = "Cantidad de viviendas", x = "Nivel de hacinamiento") + # Nombres de ejes
  
  ggtitle("Niveles de hacinamiento, derivados de la cantidad de integrantes de la vivienda\nrelativo a la cantidad de habitaciones.\nRelevamiento de Condiciones Habitacionales 2022, La Poderosa.") +
  
  coord_flip() + # Barras horizontales o verticales
  
  theme_classic() # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/

#####################
# Gráfico de barras #
#####################

datos_analisis %>%
  ggplot() + 
  
  #aes(x = hacinamiento) + # Frecuencias absolutas
  aes(x = relación_con_propiedad) +
  #aes(x = hacinamiento, y = ..count.. / sum(..count..)) + # Porcentajes
  # aes(x = reorder(hacinamiento, hacinamiento, function(x) -length(x)), 
  #		y = ..count.. / sum(..count..)) +  # Porcentajes ordenados según frecuencia
  #scale_y_continuous(labels = scales::percent) +    # Eje para porcentajes
  
  geom_bar(width = 0.75,   # Ancho de barras
           fill = '#7ed021',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6) +  # Transparencia
  
  labs(y = "Cantidad de hogares", x = "Relación con propiedad") + # Nombres de ejes
  
  ggtitle("Relación del jefe/a de hogar con propiedad.\nRelevamiento de Condiciones Habitacionales 2022, La Poderosa.") +
  
  coord_flip() + # Barras horizontales o verticales
  
  theme_classic() # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/

#####################
# Gráfico de barras #
#####################

respuesta_multiple_table <- as.data.frame(c(sum(na.omit(plaga_cucaracha)), sum(na.omit(plaga_mosquito)), sum(na.omit(plaga_rata))))
colnames(respuesta_multiple_table) <- c("Freq")
rownames(respuesta_multiple_table) <- c("Cucarachas", "Mosquitos", "Ratas")

respuesta_multiple_table %>%
  ggplot() + 
  
  #aes(x = hacinamiento) + # Frecuencias absolutas
  aes(x = rownames(respuesta_multiple_table), y = Freq) +
  #aes(x = hacinamiento, y = ..count.. / sum(..count..)) + # Porcentajes
  # aes(x = reorder(hacinamiento, hacinamiento, function(x) -length(x)), 
  #		y = ..count.. / sum(..count..)) +  # Porcentajes ordenados según frecuencia
  #scale_y_continuous(labels = scales::percent) +    # Eje para porcentajes
  
  geom_bar(width = 0.75,   # Ancho de barras
           fill = '#7ed021',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6, stat="identity") +  # Transparencia
  
  labs(y = "Cantidad de viviendas", x = "Tipo de plaga") + # Nombres de ejes
  
  ggtitle("Plagas por viviendas.\nRelevamiento de Condiciones Habitacionales 2022, La Poderosa.") +
  
  coord_flip() + # Barras horizontales o verticales
  
  theme_classic() # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/

rm(respuesta_multiple_table)

##################################
# Gráfico de sectores circulares #
##################################
table(datos_limpios$hay_plagas) %>% pie(main = "Presencia de plagas")
