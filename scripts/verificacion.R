# =============================================================================
# Script de Verificación (TSV)
#
# OBJETIVO:
# Inspeccionar una muestra del archivo .tsv para asegurar que es accesible
# y se está leyendo correctamente con el delimitador de tabulación.
# =============================================================================

# Cargar librerías necesarias
library(here)
library(readr) # Usamos readr para una lectura de tsv simple y rápida
library(dplyr)

# 1. Definir la ruta al nuevo archivo .tsv
raw_data_path <- here::here("data", "raw", "DS0001", "36862-0001-Data.tsv")

# 2. Verificar que el archivo existe
cat("Verificando la ruta del archivo:\n")
cat("Ruta construida:", raw_data_path, "\n")
if (!file.exists(raw_data_path)) {
  stop("-> ERROR: Archivo TSV no encontrado. Verifique el nombre y la ubicación.")
}
cat("-> Éxito: Archivo encontrado.\n")

# 3. Leer las primeras 10 filas del archivo TSV
cat("\nIntentando leer una muestra de 10 filas del archivo TSV...\n")
sample_chunk <- tryCatch({
  # Usamos n_max para leer solo las primeras filas y show_col_types = FALSE para un output más limpio
  readr::read_tsv(raw_data_path, n_max = 10, show_col_types = FALSE)
}, error = function(e) {
  cat("-> ERROR al leer el archivo .tsv:", e$message, "\n")
  return(NULL)
})

# 4. Inspeccionar la muestra si la lectura fue exitosa
if (!is.null(sample_chunk)) {
  cat("-> Éxito: Muestra leída correctamente.\n\n")
  cat("Estructura de la muestra (str):\n")
  # Usamos janitor para limpiar los nombres para la inspección
  sample_chunk <- janitor::clean_names(sample_chunk)
  str(sample_chunk)
  cat("\nPrimeras filas y columnas de la muestra (head):\n")
  print(head(select(sample_chunk, 1:6)))
} else {
  cat("-> La lectura de la muestra falló. Revise el error anterior.\n")
}