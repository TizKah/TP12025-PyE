# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("janitor")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(janitor)

# Fijo el dataset
attach(datos_limpios)

###########################
# TABLAS PARA EL ANÁLISIS #
###########################

### 1. ANÁLISIS DE TENENCIA Y DESALOJOS ###

# Frecuencia de tipos de tenencia
tabla_tenencia <- datos_limpios %>% 
  tabyl(relación_con_propiedad) %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits = 1) %>%
  rename(
    "Tipo de tenencia" = relación_con_propiedad,
    "Cant. hogares" = n,
    "% hogares" = percent
  )

# Tabla cruzada: Tenencia vs Intentos de desalojo (CORRECCIÓN aplicada)
tabla_desalojos <- datos_limpios %>%
  tabyl(relación_con_propiedad, hubo_intento_de_desalojo) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_title(placement = "top", row_name = "Tipo de tenencia", col_name = "Intento de desalojo")

### 2. ANÁLISIS DE HACINAMIENTO ###

# Distribución de niveles de hacinamiento
tabla_hacinamiento <- datos_limpios %>%
  tabyl(hacinamiento) %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits = 1) %>%
  rename(
    "Nivel hacinamiento" = hacinamiento,
    "Cant. hogares" = n,
    "% hogares" = percent
  )

# Hacinamiento por material de piso
tabla_hacinamiento_material <- datos_limpios %>%
  tabyl(material_piso, hacinamiento) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_title(placement = "top", row_name = "Material de piso", col_name = "Nivel de hacinamiento")

### 3. ANÁLISIS DE SERVICIOS BÁSICOS ###

# Frecuencia de tipos de suministro de agua
tabla_agua <- datos_limpios %>%
  tabyl(suministro_de_agua) %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits = 1) %>%
  rename(
    "Suministro de agua" = suministro_de_agua,
    "Cant. hogares" = n,
    "% hogares" = percent
  )

# Tabla cruzada: Agua vs Electricidad
tabla_servicios <- datos_limpios %>%
  tabyl(suministro_de_agua, conexión_red_eléctrica) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_title(placement = "top", row_name = "Suministro de agua", col_name = "Conexión eléctrica")

### 4. ANÁLISIS DE PLAGAS Y AMBIENTE ###

# Tabla de plagas (respuesta múltiple) - Versión corregida
tabla_plagas <- datos_limpios %>%
  summarize(
    Cucarachas = sum(plaga_cucaracha, na.rm = TRUE),
    Mosquitos = sum(plaga_mosquito, na.rm = TRUE),
    Ratas = sum(plaga_rata, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Plaga", values_to = "Cantidad") %>%
  mutate(
    Porcentaje = paste0(round(Cantidad/nrow(datos_limpios)*100, 1), "%")
  ) %>%
  arrange(desc(Cantidad))

# Relación entre plagas y basurales - Versión corregida
tabla_plagas_basurales <- datos_limpios %>%
  tabyl(hay_basurales_cerca, hay_plagas) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_title(placement = "top", row_name = "Presencia de basurales", col_name = "Reporte de plagas")

### 5. ANÁLISIS ECONÓMICO (ALQUILERES) ###

# Resumen estadístico de costos de alquiler
resumen_alquileres <- datos_limpios %>%
  filter(relación_con_propiedad == "Inquilino" & !is.na(costo_de_alquiler)) %>%
  summarize(
    Media = round(mean(costo_de_alquiler), 2),
    Mediana = round(median(costo_de_alquiler), 2),
    Desviación = round(sd(costo_de_alquiler), 2),
    Mínimo = min(costo_de_alquiler),
    Máximo = max(costo_de_alquiler),
    Casos = n()
  )

# Aumentos de alquiler por provincia 
tabla_aumentos <- datos_limpios %>%
  filter(!is.na(porcentaje_de_aumento_de_alquiler) & 
           relación_con_propiedad == "Inquilino") %>%
  group_by(provincia) %>%
  summarize(
    Aumento_promedio = paste0(round(mean(porcentaje_de_aumento_de_alquiler)*100, 1), "%"),
    Casos = n()
  ) %>%
  arrange(desc(Aumento_promedio))

### 6. TABLAS PARA MODELADO ###

# Tabla de correlaciones entre variables clave 
tabla_correlaciones <- datos_limpios %>%
  select(
    personas_por_dormitorio,
    costo_de_alquiler,
    porcentaje_de_aumento_de_alquiler,
    núm_integrantes
  ) %>%
  cor(use = "complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  mutate(across(where(is.numeric), ~round(., 3)))

### FUNCIÓN PARA EXPORTAR TABLAS ###
# install.packages("openxlsx")
library(openxlsx)

# Función para guardar tablas en Excel
guardar_tabla_excel <- function(tabla, nombre_archivo) {
  # Crear carpeta si no existe
  if (!dir.exists("tablas")) {
    dir.create("tablas")
  }
  
  # Ruta completa del archivo
  ruta_archivo <- paste0("tablas/", nombre_archivo, ".xlsx")
  
  # Crear un nuevo libro de Excel
  wb <- createWorkbook()
  addWorksheet(wb, "Datos")
  
  # Escribir datos en la hoja
  writeData(wb, sheet = "Datos", x = tabla, rowNames = FALSE)
  
  # Guardar el archivo Excel
  saveWorkbook(wb, ruta_archivo, overwrite = TRUE)
  
  message(paste("Tabla guardada en Excel como:", ruta_archivo))
  return(tabla)
}

tablas <- list(
  "Tenencia" = tabla_tenencia,
  "Tenencia_vs_Desalojos" = tabla_desalojos,
  "Hacinamiento" = tabla_hacinamiento,
  "Hacinamiento_Material" = tabla_hacinamiento_material,
  "Suministro_Agua" = tabla_agua,
  "Servicios_Basicos" = tabla_servicios,
  "Plagas" = tabla_plagas,
  "Plagas_Basurales" = tabla_plagas_basurales,
  "Resumen_Alquileres" = resumen_alquileres,
  "Aumentos_Alquiler" = tabla_aumentos,
  "Correlaciones" = tabla_correlaciones
)

# "For" para cada elemento de tablas
walk2(tablas, names(tablas), ~guardar_tabla_excel(.x, .y))
