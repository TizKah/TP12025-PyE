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
  aes(x = relación_con_propiedad, y = años_de_residencia) +
  geom_boxplot(show.legend = F, fill = "lightblue") +
  labs(x = "Relación con propiedad", y = "Años de residencia") +
  coord_flip() +
  ggtitle("Años de residencia relativo a relación con propiedad.\nRelevamiento de Condiciones Habitacionales 2022, La Poderosa.") +
  theme_light()

####################
# Barras agrupadas #
####################

table(tiene_contrato_de_alquiler, hubo_intento_de_desalojo) %>%
prop.table(margin=1) %>%
as.data.frame() %>%
ggplot() +
  ggtitle("Existencia de un contrato de alquiler relativo a la ocurrencia de un intento de desalojo.\nRelevamiento de Condiciones Habitacionales 2022, La Poderosa.") +
  aes(x = tiene_contrato_de_alquiler, y = Freq, fill = hubo_intento_de_desalojo) +
  labs(x = "Tiene contrato de alquiler", fill = "Hubo un intento de desalojo", y = "Proporción")+
  geom_bar(position="stack", stat="identity")

##########################
# Diagrama de dispersión #
##########################

ggplot(datos_limpios) +
  aes(x = edad_jefe_del_hogar, y = años_de_residencia) +
  geom_point() +
  labs(x = "Edad de jefa/a del hogar", y = "Años de residencia")+
  ggtitle("Relación entre la edad de jefe/a del hogar y años de residencia") +
  geom_smooth(method = "lm") +
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
    title = "Relación entre plagas y basurales cercanos.\nRelevamiento de Condiciones Habitacionales 2022, La Poderosa."
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()


##########################
# Hacinamiento por barrio#
##########################

ggplot(datos_limpios, aes(x = barrio, fill = hacinamiento)) +
  geom_bar(position = "fill") +
  labs(
    title = "Distribución del hacinamiento por barrio.\nRelevamiento de Condiciones Habitacionales 2022, La Poderosa.",
    x = "Barrio",
    y = "Porcentaje",
    fill = "Nivel de hacinamiento"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####################################################
# Suministro agua y conexión eláctrica             #
####################################################

ggplot(datos_limpios, aes(x = conexión_red_eléctrica, fill = suministro_de_agua)) +
  geom_bar(position = "fill") +
  labs(
    title = "Distribución del suministro de agua por tipo de conexión eléctrica.\nRelevamiento de Condiciones Habitacionales 2022, La Poderosa.",
    x = "Tipo de suministro de agua",
    y = "Frecuencia",
    fill = "Conexión eléctrica"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Tipo de baño y presencia de plagas

table(posee_baño, hay_plagas) %>%
  prop.table(margin=1) %>%
  as.data.frame() %>%
ggplot(aes(x = posee_baño, y = Freq, fill = hay_plagas)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(
    title = "Relación entre tipo de baño y presencia de plagas.\nRelevamiento de Condiciones Habitacionales 2022, La Poderosa.",
    x = "Tipo de Baño",
    y = "Frecuencia relativa",
    fill = "Presencia de plagas"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
