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
datos_limpios <- datos %>% # Los pipelines permiten encadenar acciones
	
	mutate(   # Para crear nuevas variables y editar las ya existentes
		
		# Veo valores min y max de la variable para elegir una
		# particion en intervalos apropiada
		# min(altura)
		# max(altura)
		# sqrt(nrow(datos))
		
		# Creo una variable nueva, con la partición en intervalos de altura
		altura_int = cut(altura,
										 breaks = seq(from=0, to=50, by = 5),
										 right = F),
		
		# Modifico las columnas de la variable de respuesta múltiple
		# para dejarlas como indicadoras con valores 1 (en caso de presentar
		# el atributo) y 0 (en caso de no presentarlo)
		atracnosis = ifelse( atracnosis == "atracnosis", 1, 0 ),
		roya = ifelse( roya == "roya", 1, 0 ),
		manchas = ifelse( manchas == "manchas", 1, 0 ),
		ampollas = ifelse( ampollas == "ampollas", 1, 0),
		# Notar que los NA no entran dentro de la categoría "no presentar 
		# el atributo", por lo que requieren un tratamiento particular:
		
		atracnosis = ifelse(is.na(atracnosis), 0, 1),
		roya = ifelse(is.na(roya), 0, 1),
		manchas = ifelse(is.na(manchas), 0, 1),
		ampollas = ifelse(is.na(ampollas), 0, 1),
		# Esto solo es correcto porque teníamos dos valores posibles en estas
		# columnas: presencia de atributo (nombre de la plaga) y ausencia (NA).
		# En los casos en los que se presenten ambas categorías además del NA
		# correspondería trabajarlos como tres valores distintos (presencia,
		# ausencia y faltante) y su tratamiento dependerá de lo que se desee hacer
		
		# Para condiciones ifelse múltiples puedo usar la función case_when
		inclinacion_cate = case_when(inclinacion == 0 ~ "Sin inclinación",
																 inclinacion < 15 ~ "Inclinación leve",
																 inclinacion < 30 ~ "Inclinación moderada",
																 TRUE ~ "Inclinación alta"),
		
		# Recodifico las etiquetas de una variable categórica
		especie = recode(especie, "ala" = "Álamo",
										 "casu" = "Casuarina",
										 "euca" = "Eucalipto",
										 "jaca" = "Jacarandá",
										 "palo"  = "Palo borracho"),
		
		# Especifico ordinalidad a las categorías de una variable
		tiempo = factor(tiempo,
										levels = 1:5,
										labels = c("Menos de 2 años", "Entre 2 y 5 años",
																				 "Entre 5 y 10 años", "Entre 10 y 20 años",
																				 "20 años o más"))

	)

##########################################
# Seleccionar un subconjunto de columnas #
##########################################

# Opcion 1
datos_chico1 <- datos_limpios %>%
	select(   # Seleccionar las columnas que quiero conservar
		id, altura, edad, follaje, inclinacion_cate
	)

# Opcion 2
datos_chico2 <- datos_limpios %>%
	select(   # Eliminar las columnas que no quiero conservar
		-altura, -edad, -follaje, -inclinacion_cate
	)

# Opcion 3
datos_orden <- datos_limpios %>%
	select(   # Reordeno columnas
		id, especie, tiempo, everything()
	)


###########################################
# Seleccionar un subconjunto de registros #
###########################################

# Opción 1: por criterio
datos_reducido1 <-datos_orden %>%
	filter((brotes > 4 & origen == "Nativo/Autóctono") | tiempo == "20 años o más")

# Opción 2: por indexación
datos_reducido2 <-datos_orden %>%
	slice(1:500)
