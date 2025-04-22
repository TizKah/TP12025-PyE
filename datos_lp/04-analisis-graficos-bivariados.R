# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)

# Fijo el dataset
attach(datos_limpios)

########################
# Boxplot comparativos #
########################

ggplot(datos_limpios) +
  aes(x = relación_con_propiedad, y = edad_jefe_del_hogar) +
  geom_boxplot(show.legend = F, fill = "lightblue") +
  labs(x = "Relación con propiedad", y = "Edad") +
  coord_flip() +
  ggtitle("Edades de jefe/a del hogar relativo a relación con propiedad") +
  theme_light()

####################
# Barras agrupadas #
####################

table(tiene_contrato_de_alquiler, hubo_intento_de_desalojo) %>%
prop.table(margin=1) %>%
as.data.frame() %>%
ggplot() +
  aes(x = tiene_contrato_de_alquiler, y = Freq, fill = hubo_intento_de_desalojo) +
  geom_bar(position="stack", stat="identity")

##########################
# Diagrama de dispersión #
##########################

ggplot(datos_limpios) +
  aes(x = edad_jefe_del_hogar, y = años_de_residencia) +
  geom_point() +
  #labs(x = "Número de integrantes del hogar", y = "Número dormitorios")+
  #ggtitle("Relación entre el número de integrantes del hogar y el número de dormitorios") +
  theme_classic()

##########################
# Plagas y basurales     #
##########################

ggplot(data = table(hay_plagas, hay_basurales_cerca) %>%
         prop.table(margin = 2) %>%
         as.data.frame(), aes(x = hay_basurales_cerca, y = Freq, fill = hay_plagas)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "¿Hay basurales cerca?",
    y = "Proporción",
    fill = "¿Hay plagas?",
    title = "Relación entre plagas y basurales cercanos"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()


##########################
# Hacinamiento por barrio#
##########################

ggplot(datos_limpios, aes(x = hacinamiento, fill = barrio)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribución del Hacinamiento por Barrio",
    x = "Nivel de Hacinamiento",
    y = "Frecuencia",
    fill = "Barrio"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
