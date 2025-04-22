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
