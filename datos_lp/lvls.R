# Script para extraer niveles exactos de todas las variables
library(tidyverse)

# Función para obtener niveles únicos (incluye NA)
get_niveles <- function(columna) {
  unique(columna) %>% 
    sort(na.last = TRUE) %>% 
    paste(collapse = " | ")
}

# Extraer nombres de todas las columnas
vars <- names(datos)

# Crear tabla con niveles exactos
niveles_variables <- map_dfr(vars, ~{
  data.frame(
    variable = .x,
    niveles = get_niveles(datos[[.x]]),
    n_niveles = length(unique(datos[[.x]])),
    stringsAsFactors = FALSE
  )
})

# Exportar resultados a CSV
write.csv(niveles_variables, "niveles_variables.csv", row.names = FALSE)