# =============================================================================
# Script de Verificación Mejorado
#
# OBJETIVO:
# Inspeccionar una muestra de cada uno de los 4 archivos de datos brutos
# para asegurar que son accesibles y tienen una estructura consistente antes
# de proceder con el procesamiento completo.
# =============================================================================

# Cargar librerías necesarias
library(here)
library(haven)
library(dplyr)
library(purrr) # Usaremos purrr para iterar de forma limpia

# 1. Definir los nombres de las carpetas y archivos a verificar
dataset_info <- list(
  list(folder = "DS0001", file_prefix = "36862-0001"),
  list(folder = "DS0002", file_prefix = "36862-0002"),
  list(folder = "DS0003", file_prefix = "36862-0003"),
  list(folder = "DS0004", file_prefix = "36862-0004")
)

# 2. Función para verificar un solo archivo
verify_dataset <- function(info) {
  folder_name <- info$folder
  file_name <- paste0(info$file_prefix, "-Data.dta")
  
  cat(paste0("\n\n================ VERIFICANDO: ", folder_name, " ================\n"))
  
  # Construir la ruta
  raw_data_path <- here::here("data", "raw", folder_name, file_name)
  cat("Ruta construida:", raw_data_path, "\n")
  
  # Verificar existencia
  if (!file.exists(raw_data_path)) {
    cat("-> ERROR: Archivo no encontrado. Verifique la estructura de carpetas.\n")
    return(invisible(NULL))
  }
  cat("-> Éxito: Archivo encontrado.\n")
  
  # Intentar leer una muestra
  cat("Intentando leer una muestra de 10 filas...\n")
  sample_chunk <- tryCatch({
    haven::read_dta(raw_data_path, n_max = 10)
  }, error = function(e) {
    cat("-> ERROR al leer el archivo .dta:", e$message, "\n")
    return(NULL)
  })
  
  # Inspeccionar la muestra
  if (!is.null(sample_chunk)) {
    cat("-> Éxito: Muestra leída correctamente.\n\n")
    cat("Estructura de la muestra (str):\n")
    str(sample_chunk)
    cat("\nPrimeras 6 columnas de la muestra (head):\n")
    # Limpiamos nombres para consistencia en la visualización
    print(head(janitor::clean_names(as.data.frame(sample_chunk)) %>% select(1:6)))
  } else {
    cat("-> La lectura de la muestra falló.\n")
  }
  
  return(invisible(NULL))
}

# 3. Ejecutar la verificación para todos los datasets
walk(dataset_info, verify_dataset)

cat("\n\n================ VERIFICACIÓN COMPLETA ================\n")
cat("Revise los resultados anteriores para cada dataset.\n")