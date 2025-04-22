# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")

# Cargo los paquetes que voy a usar
library(tidyverse)

# Fijo el dataset
attach(datos)

######################
# Renombrar columnas #
######################
colnames(datos) <- c(
  "orden_inicial",
  "provincia",
  "barrio",
  
  # Sección 1: Características generales de la composición del hogar
  "edad_jefe_del_hogar",
  "años_de_residencia",
  "núm_integrantes",
  "núm_familias",
  "núm_varones",
  "núm_mujeres",
  "núm_género_disidente",
  "núm_menores",
  "conviven_personas_con_discapacidades",
  
  # No se presentan datos para las preguntas de la sección 2...
  
  # Sección 3: Hacinamiento
  "núm_dormitorios",
  "máx_personas_por_dormitorio",
  
  # Sección 4: Propiedad
  "posee_renabap",
  "hubo_intento_de_desalojo",
  "núm_intentos_de_desalojo",
  "tiempo_desde_último_intento_de_desalojo",
  "relación_con_propiedad",
  "tiene_contrato_de_alquiler",
  "costo_de_alquiler",
  "aumento_de_alquiler_en_último_año",
  "porcentaje_de_aumento_de_alquiler",
  
  # Sección 5: Agua y saneamiento
  "suministro_de_agua",
  "compra_agua_embotellada",
  "presión_del_agua",
  "tiene_tanque_de_agua",
  "litros_tanque",
  "posee_baño",
  "lugar_de_higienización",
  "baños_compartidos_con_otras_familias",
  "baño_posee_descarga_de_agua",
  "tipo_de_desagüe",
  "posee_agua_en_cocina",
  "tipo_de_agua_caliente_en_cocina",
  "posee_agua_en_baño",
  "tipo_de_agua_caliente_en_baño",
  
  # Sección 6: Cocina y calefacción
  "gas_red_cocina",
  "garrafa_cocina",
  "electricidad_cocina",
  "leña_o_carbón_cocina",
  "no_tiene_cocina",
  # No aparece una columna para "no necesita cocina"
  "gas_red_calefacción",
  "garrafa_calefacción",
  "electricidad_calefacción",
  "leña_o_carbón_calefacción",
  "no_tiene_calefacción",
  "no_necesita_calefacción",
  "posee_ventilación_calefacción",
  
  # Sección 7: Electricidad
  "conexión_red_eléctrica",
  "tendido_eléctrico_en_vivienda",
  "pérdida_de_electrodomésticos",
  "incendios_por_instalación_eléctrica",
  "cortes_frecuentes_electricidad_verano",
  "cortes_frecuentes_electricidad_invierno",
  
  # Sección 8: Conectividad
  "posee_banda_ancha",
  "posee_celular_con_datos_móviles",
  "núm_abonos_datos_móviles",
  "núm_computadoras",
  "núm_teléfonos_tablets",
  
  # Sección 9: Condiciones materiales de la vivienda
  "tiene_contrapiso",
  "material_piso",
  "material_techo",
  "techo_aislamiento_térmico",
  # Hay un serio problema con el contenido de la planilla acá,
  # los datos se corresponden al material del piso...
  "puerta_exterior_material1",
  "puerta_exterior_material2",
  "puerta_exterior_material3",
  "puerta_exterior_material4",
  "material_paredes_exteriores",
  "paredes_exteriores_tienen_terminación_exterior",
  "tipo_terminación_exterior",
  "terminación_de_pintura",
  "filtraciones_dormitorios",
  "filtraciones_cocina",
  "filtraciones_baño",
  "filtraciones_living",
  "sin_filtraciones",
  "filtraciones_otro",
  "problema_estructural_dormitorios",
  "problema_estructural_cocina",
  "problema_estructural_baño",
  "problema_estructural_living",
  "sin_problemas_estructurales",
  "problema_estructural_otro",
  "trabaja_en_vivienda",
  "tipo_de_trabajo_en_vivienda",
  
  # Sección 10: Servicions barriales
  "calle_asfaltada",
  "tiene_salida_a_calle",
  "hay_veredas",
  "hay_alumbrado_público",
  "calificación_arbolado",
  "hay_plagas",
  "plaga_cucaracha",
  "plaga_mosquito",
  "plaga_rata",
  
  # Sección 11: Ambiente
  "polideportivo_municipal_a_500m",
  "natatorio_municipal_a_500m",
  "playón_multiuso_a_500m",
  "cancha_fútbol_a_500m",
  "posta_de_ejercicio_a_500m",
  "skatepark_a_500m",
  "balneario_a_500m",
  "sin_espacio_práctica_corporal_a_500m",
  "otro_espacio_práctica_corporal_a_500m",
  "frecuencia_uso_espacio_práctica_corporal",
  "placita_a_500m",
  "plaza_a_500m",
  "parque_urbano_a_500m",
  "sin_espacio_verde_a_500m",
  "frecuencia_uso_espacio_verde",
  "frecuencia_tránsito",
  "frecuencia_tránsito_dispar_entre_día_y_noche",
  "acceso_a_bicicleta_pública",
  "hay_basurales_cerca",
  "hay_cesto_comunitario_en_cuadra",
  "forma_eliminación_residuos",
  "frecuencia_recolección_residuos_municipio",
  "riesgo_inundación")

###################
# Modificar datos #
###################

datos_limpios <- datos %>%
  mutate(
    # Variables de hacinamiento
    personas_por_dormitorio = ifelse(núm_dormitorios > 0, 
                                     núm_integrantes/núm_dormitorios, 
                                     NA_real_),
    
    hacinamiento = factor(
      case_when(
        personas_por_dormitorio > 3 ~ "Crítico",
        personas_por_dormitorio > 2 ~ "Alto",
        personas_por_dormitorio > 1 ~ "Moderado",
        personas_por_dormitorio > 0 ~ "Bajo",
        TRUE ~ "Sin datos"
      ),
      levels = c("Sin datos", "Bajo", "Moderado", "Alto", "Crítico"),
      ordered = TRUE
    ),
    
    # Tenencia
    relación_con_propiedad = factor(
      case_when(
        relación_con_propiedad %in% c("Propio con algún comprobante de tenencia", "Propio sin títulos") ~ "Propietario",
        relación_con_propiedad == "Alquilado" ~ "Inquilino",
        relación_con_propiedad %in% c("Prestado", "Ocupado/Tomado") ~ "Ocupante/Prestado",
        TRUE ~ "Otro"
      ),
      levels = c("Propietario", "Inquilino", "Ocupante/Prestado", "Otro")
    ),
    
    tiene_contrato_de_alquiler = ifelse(tiene_contrato_de_alquiler == "NA", NA, tiene_contrato_de_alquiler),
    
    # Servicios básicos
    suministro_de_agua = factor(
      case_when(
        str_detect(suministro_de_agua, "conexión con medidor") ~ "Red formal",
        str_detect(suministro_de_agua, "sin medidor|informal") ~ "Red informal",
        str_detect(suministro_de_agua, "cisterna|pozo|comunitario") ~ "Fuente alternativa",
        suministro_de_agua == "No poseo agua dentro de la vivienda" ~ "Sin acceso",
        TRUE ~ "No sabe"
      ),
      levels = c("Red formal", "Red informal", "Fuente alternativa", "Sin acceso", "No sabe")
    ),
    
    conexión_red_eléctrica = factor(
      case_when(
        conexión_red_eléctrica == "Conexión a través de un medidor a la red eléctrica" ~ "Formal",
        conexión_red_eléctrica == "Conexión a través de un medidor comunitario a la red eléctrica" ~ "Comunitario",
        str_detect(conexión_red_eléctrica, "sin medidor|informal") ~ "Informal",
        TRUE ~ "Sin conexión"
      ),
      levels = c("Formal", "Comunitario", "Informal", "Sin conexión")
    ),
    
    # Infraestructura sanitaria 
    posee_baño = factor(
      recode(posee_baño, 
             "Si, dentro de la vivienda" = "Dentro", 
             "Si, fuera de la vivienda" = "Fuera"),
      levels = c("Dentro", "Fuera", "No")
    ),
    
    baño_posee_descarga_de_agua = factor(
      case_when(
        baño_posee_descarga_de_agua == "Si, posee cadena/botón de descarga" ~ "Descarga automática",
        baño_posee_descarga_de_agua == "Descargamos manualmente con baldes" ~ "Descarga manual",
        TRUE ~ "Sin descarga"
      ),
      levels = c("Descarga automática", "Descarga manual", "Sin descarga")
    ),
    
    
    # Variables ambientales 
    calificación_arbolado = factor(
      calificación_arbolado,
      levels = c("Inexistente", "Escaso", "Suficiente"),
      ordered = TRUE
    ),
    
    hay_plagas = ifelse(hay_plagas == "NA", NA, hay_plagas),
    
    # Tratamiento de variables binarias 
    across(c(plaga_cucaracha, plaga_mosquito, plaga_rata),
           ~ifelse(.x == "Cucarachas" | .x == "Mosquitos" | .x == "Ratas", 1, 0)),
    
    # Variables de materiales 
    material_piso = factor(
      case_when(
        material_piso %in% c("Carpeta de cemento", "Cerámico") ~ "Sólido",
        material_piso == "Madera" ~ "Madera",
        TRUE ~ "Tierra"
      ),
      levels = c("Sólido", "Madera", "Tierra")
    ),
    
    # Variables de servicios urbanos 
    calle_asfaltada = factor(calle_asfaltada, levels = c("Sí", "No")),
    hay_alumbrado_público = factor(
      case_when(
        hay_alumbrado_público == "No" ~ "No",
        str_detect(hay_alumbrado_público, "Estado") ~ "Estatal",
        TRUE ~ "Vecinal"
      ),
      levels = c("Estatal", "Vecinal", "No")
    ),
    
    # Composición del hogar
    conviven_personas_con_discapacidades = ifelse(conviven_personas_con_discapacidades == 1, "Sí", "No")
  ) %>%
  # Convertir porcentajes a numéricos
  mutate(porcentaje_de_aumento_de_alquiler = as.numeric(porcentaje_de_aumento_de_alquiler))

##########################################
# Selección de columnas clave            #
##########################################

datos_analisis <- datos_limpios %>%
  select(
    provincia, barrio,
    # Variables demográficas
    edad_jefe_del_hogar, núm_integrantes, conviven_personas_con_discapacidades,
    # Vivienda
    relación_con_propiedad, tiene_contrato_de_alquiler, costo_de_alquiler, porcentaje_de_aumento_de_alquiler,
    # Hacinamiento
    núm_dormitorios, máx_personas_por_dormitorio, hacinamiento,
    # Servicios básicos
    suministro_de_agua, conexión_red_eléctrica, posee_baño, baño_posee_descarga_de_agua,
    # Problemáticas
    hubo_intento_de_desalojo, hay_plagas, plaga_cucaracha, plaga_mosquito, plaga_rata,
    # Entorno
    calificación_arbolado, hay_basurales_cerca,
    # Materiales
    material_piso
  )

# Verificar niveles de variables clave
list(
  hacinamiento = levels(datos_limpios$hacinamiento),
  relación_propiedad = levels(datos_limpios$relación_con_propiedad),
  agua = levels(datos_limpios$suministro_de_agua),
  electricidad = levels(datos_limpios$conexión_red_eléctrica)
)

##########################################
# SUBCONJUNTOS PARA ANÁLISIS ESPECÍFICOS #
##########################################

# Análisis de tenencia y desalojos
datos_tenencia <- datos_limpios %>%
  select(
    provincia, barrio,
    relación_con_propiedad, tiene_contrato_de_alquiler,
    hubo_intento_de_desalojo, núm_intentos_de_desalojo,
    tiempo_desde_último_intento_de_desalojo,
    costo_de_alquiler, aumento_de_alquiler_en_último_año
  ) %>%
  filter(!is.na(hubo_intento_de_desalojo))

# Análisis de hacinamiento
datos_hacinamiento_critico <- datos_limpios %>%
  select(
    barrio, provincia,
    núm_integrantes, núm_dormitorios, personas_por_dormitorio, máx_personas_por_dormitorio, hacinamiento,
    material_piso, filtraciones_dormitorios
  ) %>%
  filter(hacinamiento %in% c("Alto", "Crítico"))

# Análisis de servicios básicos deficitarios
datos_servicios_deficit <- datos_limpios %>%
  select(
    barrio, provincia,
    suministro_de_agua, conexión_red_eléctrica,
    posee_baño, baño_posee_descarga_de_agua,
    tiene_contrapiso, material_piso
  ) %>%
  filter(
    suministro_de_agua == "No poseo agua dentro de la vivienda y/o tengo que acarrear desde fuera del terreno en que se ubica mi vivienda" |
      conexión_red_eléctrica == "No posee conexión a la red eléctrica en la vivienda" |
      posee_baño == "No"
  )

# Análisis de condiciones ambientales
datos_ambiente <- datos_limpios %>%
  select(
    barrio, provincia,
    calificación_arbolado, hay_basurales_cerca,
    hay_plagas, plaga_cucaracha, plaga_mosquito, plaga_rata,
    riesgo_inundación
  ) %>%
  filter(
    calificación_arbolado == "Inexistente" |
      hay_basurales_cerca != "No" |
      hay_plagas == "Sí"
  )

# Análisis económico (alquileres)
datos_economicos <- datos_limpios %>%
  select(
    barrio, provincia,
    relación_con_propiedad, tiene_contrato_de_alquiler,
    costo_de_alquiler, aumento_de_alquiler_en_último_año,
    porcentaje_de_aumento_de_alquiler
  ) %>%
  filter(
    relación_con_propiedad == "Inquilino",
    !is.na(costo_de_alquiler)
  )

# 6. Muestra estratificada para análisis cualitativo
# set.seed(123)
# datos_cualitativo <- datos_limpios %>%
#   group_by(provincia, hacinamiento) %>%
#   sample_n(size = min(15, n()), replace = FALSE) %>%
#   ungroup() %>%
#   select(
#     barrio, provincia,
#     relación_con_propiedad, hacinamiento,
#     suministro_de_agua, conexión_red_eléctrica,
#     hay_plagas, calificación_arbolado
#   )

# Datos para modelos estadísticos (sin NAs)
datos_modelado <- datos_limpios %>%
  select(
    hacinamiento,
    relación_con_propiedad,
    suministro_de_agua,
    conexión_red_eléctrica,
    hay_plagas,
    calificación_arbolado,
    hay_basurales_cerca,
    núm_integrantes,
    núm_dormitorios
  ) %>%
  drop_na() # Ver como manejar mejor NAS

# Análisis espacial (geo-referenciable)
datos_espacial <- datos_limpios %>%
  select(
    barrio, provincia,
    hacinamiento,
    suministro_de_agua,
    conexión_red_eléctrica,
    hay_basurales_cerca,
    calificación_arbolado,
    riesgo_inundación
  ) %>%
  group_by(barrio, provincia) %>%
  summarise(
    tasa_hacinamiento_critico = mean(hacinamiento %in% c("Alto", "Crítico"),
                                     sin_agua = mean(suministro_de_agua == "No poseo agua dentro de la vivienda y/o tengo que acarrear desde fuera del terreno en que se ubica mi vivienda"),
                                     sin_luz = mean(conexión_red_eléctrica == "No posee conexión a la red eléctrica en la vivienda"),
                                     con_basurales = mean(hay_basurales_cerca != "No"),
                                     .groups = 'drop'
    ))
    