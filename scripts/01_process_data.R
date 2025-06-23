# =============================================================================
# 01_process_data.R (Versión Refactorizada)
#
# OBJETIVO:
# Este script procesa el dataset NCRP Term Records (DS0001), que es el
# único con un identificador de recluso consistente para el análisis de reincidencia.
# 1. Lee el archivo .dta en trozos (chunks) para manejar la memoria eficientemente.
# 2. Guarda los trozos procesados como archivos Parquet temporales.
# 3. Consolida los archivos temporales.
# 4. Aplica la lógica de limpieza y creación de variables de análisis (JRI, recidivismo).
# 5. Guarda el dataset final y limpio en formato Parquet.
#
# =============================================================================

# 1. CONFIGURACIÓN INICIAL ----
# =============================================================================
library(here)
library(arrow)
library(haven)
library(dplyr)
library(janitor)
library(parallel)

options(future.globals.maxSize = 8000 * 1024^2) # Aumentar límite de memoria para procesamiento
arrow::set_cpu_count(parallel::detectCores() - 1) # Usar casi todos los cores

cat("Iniciando el script 01_process_data.R...\n")


# 2. DEFINICIÓN DE RUTAS Y PARÁMETROS ----
# =============================================================================
raw_data_path <- here::here("data", "raw", "DS0001", "36862-0001-Data.dta")
temp_dir <- here::here("data", "temp", "ds1_processed")
processed_dir <- here::here("data", "processed")
output_file <- file.path(processed_dir, "ncrp_jri_clean.parquet")

dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(processed_dir, showWarnings = FALSE, recursive = TRUE)

# Mapeos y definiciones
state_mapping <- data.frame(
  state_num = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
)

jri_states <- data.frame(
  state_name = c("South Carolina", "New Hampshire", "Kentucky", "Ohio", "Pennsylvania", "Arkansas", "Georgia", "Louisiana", "North Carolina", "Hawaii", "Delaware", "Missouri", "Oklahoma", "West Virginia", "Kansas", "Oregon", "South Dakota"),
  jri_year = c(2010, 2010, 2011, 2011, 2012, 2011, 2012, 2011, 2011, 2012, 2012, 2012, 2012, 2013, 2013, 2013, 2013)
)

# 3. PROCESAMIENTO EN CHUNKS ----
# =============================================================================
cat("Paso 3: Procesando archivo DTA en chunks...\n")
chunk_size <- 100000
chunk_number <- 1
temp_files <- c()

repeat {
  temp_parquet_file <- file.path(temp_dir, sprintf("chunk_%d.parquet", chunk_number))
  
  if (file.exists(temp_parquet_file)) {
    cat(sprintf("  - Chunk %d ya procesado, saltando.\n", chunk_number))
    temp_files <- c(temp_files, temp_parquet_file)
    chunk_number <- chunk_number + 1
    next
  }
  
  cat(sprintf("  - Leyendo chunk %d...\n", chunk_number))
  chunk <- tryCatch({
    haven::read_dta(
      raw_data_path,
      skip = (chunk_number - 1) * chunk_size,
      n_max = chunk_size
    )
  }, error = function(e) NULL)
  
  if (is.null(chunk) || nrow(chunk) == 0) break
  
  processed_chunk <- chunk %>%
    clean_names() %>%
    mutate(across(where(is.labelled), as_factor))
  
  write_parquet(processed_chunk, sink = temp_parquet_file)
  temp_files <- c(temp_files, temp_parquet_file)
  cat(sprintf("  - Chunk %d procesado y guardado (%d filas).\n", chunk_number, nrow(processed_chunk)))
  
  rm(chunk, processed_chunk); gc()
  chunk_number <- chunk_number + 1
}

# 4. CONSOLIDACIÓN Y TRANSFORMACIÓN FINAL ----
# =============================================================================
cat("Paso 4: Consolidando chunks y aplicando transformaciones finales...\n")

# Abrir todos los chunks como un solo dataset de Arrow
full_dataset <- open_dataset(temp_files, format = "parquet")

# Aplicar las transformaciones
data_clean <- full_dataset %>%
  filter(!is.na(releaseryr) & !is.na(state)) %>%
  mutate(state_num = as.numeric(as.character(state))) %>%
  left_join(state_mapping, by = "state_num") %>%
  left_join(jri_states, by = "state_name") %>%
  mutate(
    treatment = if_else(!is.na(jri_year), 1L, 0L),
    post = if_else(!is.na(jri_year) & releaseryr >= jri_year, 1L, 0L),
    post = if_else(is.na(post), 0L, post)
  ) %>%
  collect() # Traer a memoria para el cálculo de reincidencia

# Calcular recidivismo
recidivism_calculation <- data_clean %>%
  group_by(abt_inmate_id) %>%
  summarise(recidivism = if_else(n() > 1, 1, 0), .groups = "drop")

# Unir y limpiar
data_final <- data_clean %>%
  left_join(recidivism_calculation, by = "abt_inmate_id") %>%
  mutate(
    releaseryr = if_else(releaseryr == "Missing", NA_character_, as.character(releaseryr)),
    releaseryr = as.integer(releaseryr),
    ageadmit_cat = case_when(
      ageadmit == "18-24 years" ~ "18-24",
      ageadmit == "25-34 years" ~ "25-34",
      ageadmit == "35-44 years" ~ "35-44",
      ageadmit == "45-54 years" ~ "45-54",
      ageadmit == "55 years and over" ~ "55+",
      TRUE ~ "Unknown/Missing"
    ),
    sentlgth_cat = case_when(
      sentlgth == "< 1 year" ~ "< 1 year",
      sentlgth == "1-1.9 years" ~ "1-2 years",
      sentlgth == "2-4.9 years" ~ "2-5 years",
      sentlgth == "5-9.9 years" ~ "5-10 years",
      sentlgth == "10-24.9 years" ~ "10-25 years",
      sentlgth == "25 years or more" ~ ">=25 years",
      sentlgth == "Life/death" ~ "Life/Death",
      TRUE ~ "Unknown/Missing"
    )
  ) %>%
  select(
    abt_inmate_id, state, state_name, releaseryr, jri_year,
    treatment, post, recidivism,
    sex, race, ageadmit_cat, offgeneral, sentlgth, sentlgth_cat
  )

cat("-> Transformaciones finales completadas.\n")

# 5. GUARDADO DEL DATASET FINAL ----
# =============================================================================
cat("Paso 5: Guardando el dataset procesado final...\n")

write_parquet(data_final, sink = output_file, compression = "snappy")
unlink(temp_dir, recursive = TRUE) # Opcional: eliminar archivos temporales

cat("-> Éxito: Dataset limpio guardado en:", output_file, "\n")
cat("-> Total de filas en el dataset final:", nrow(data_final), "\n")
cat("-> Total de columnas en el dataset final:", ncol(data_final), "\n")
cat("Script 01_process_data.R finalizado.\n")