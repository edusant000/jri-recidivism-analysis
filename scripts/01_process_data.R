# =============================================================================
# 01_process_data.R (Versión 14 - Implementando correcciones del usuario)
#
# OBJETIVO:
# Incorpora correcciones clave para asegurar la robustez del pipeline:
# 1. Corrige el error de tipeo en 'releaseyr'.
# 2. Añade una comprobación defensiva para columnas esenciales.
# 3. Asegura la conversión de tipos de dato antes de las operaciones.
# =============================================================================

# 1. CONFIGURACIÓN INICIAL ----
library(here)
library(readr)
library(dplyr)
library(janitor)
library(arrow)

cat("Iniciando el script 01_process_data.R (versión con correcciones)...\n")

# 2. DEFINICIÓN DE RUTAS Y PARÁMETROS ----
raw_data_path <- here::here("data", "raw", "DS0001", "36862-0001-Data.tsv") 
processed_dir <- here::here("data", "processed")
output_file <- file.path(processed_dir, "ncrp_jri_clean.parquet")
dir.create(processed_dir, showWarnings = FALSE, recursive = TRUE)

state_mapping <- data.frame(
  state_num = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
)
jri_states <- data.frame(
  state_name = c("South Carolina", "New Hampshire", "Kentucky", "Ohio", "Pennsylvania", "Arkansas", "Georgia", "Louisiana", "North Carolina", "Hawaii", "Delaware", "Missouri", "Oklahoma", "West Virginia", "Kansas", "Oregon", "South Dakota"),
  jri_year = c(2010, 2010, 2011, 2011, 2012, 2011, 2012, 2011, 2011, 2012, 2012, 2012, 2012, 2013, 2013, 2013, 2013)
)

# 3. LECTURA Y TRANSFORMACIÓN DE DATOS ----
cat("Paso 3: Leyendo y procesando el archivo TSV con readr...\n")
if (!file.exists(raw_data_path)) {
  stop("¡ERROR! Archivo TSV no encontrado en: ", raw_data_path)
}

raw_data <- readr::read_tsv(raw_data_path, show_col_types = FALSE) %>%
  clean_names()

# --- INICIO DE LA SECCIÓN CORREGIDA (según sus sugerencias) ---

# Chequeo defensivo para asegurar que las columnas clave existen
cols_needed <- c("releaseyr", "state")
missing_cols <- setdiff(cols_needed, names(raw_data))
if (length(missing_cols) > 0) {
  stop("Columnas clave faltantes en los datos leídos: ", paste(missing_cols, collapse = ", "))
}
cat("-> Comprobación defensiva de columnas superada.\n")

# Transformación de datos con correcciones
data_clean <- raw_data %>%
  # Corrección 1: Nombre de columna corregido a 'releaseyr'
  filter(!is.na(releaseyr) & !is.na(state)) %>%
  # Corrección 2: Asegurar que 'state' es numérico antes del join
  mutate(state = as.integer(state)) %>%
  left_join(state_mapping, by = c("state" = "state_num")) %>%
  left_join(jri_states, by = "state_name") %>%
  mutate(
    treatment = if_else(!is.na(jri_year), 1L, 0L),
    post = if_else(treatment == 1 & releaseyr >= jri_year, 1L, 0L)
  )
# --- FIN DE LA SECCIÓN CORREGIDA ---

# Calcular recidivismo
recidivism_calculation <- data_clean %>%
  filter(!is.na(abt_inmate_id)) %>%
  group_by(abt_inmate_id) %>%
  summarise(recidivism = if_else(n() > 1, 1, 0), .groups = "drop")

# Unir y recodificar
data_final <- data_clean %>%
  left_join(recidivism_calculation, by = "abt_inmate_id") %>%
  mutate(
    recidivism = if_else(is.na(recidivism), 0, recidivism),
    releaseyr = if_else(releaseyr == 9999, NA_integer_, as.integer(releaseyr)),
    # Corrección 3: Forzar a numérico antes de 'case_when' para mayor robustez
    sex = case_when(as.numeric(sex) == 1 ~ "Male", as.numeric(sex) == 2 ~ "Female", TRUE ~ "Unknown"),
    race = case_when(
      as.numeric(race) == 1 ~ "White", as.numeric(race) == 2 ~ "Black", as.numeric(race) == 3 ~ "Hispanic",
      as.numeric(race) == 4 ~ "Asian", TRUE ~ "Other/Unknown"
    ),
    offgeneral = case_when(
      as.numeric(offgeneral) == 1 ~ "Violent", as.numeric(offgeneral) == 2 ~ "Property", as.numeric(offgeneral) == 3 ~ "Drug",
      as.numeric(offgeneral) == 4 ~ "Public Order", TRUE ~ "Other/Unknown"
    ),
    ageadmit_cat = case_when(
      as.numeric(ageadmit) == 1 ~ "18-24", as.numeric(ageadmit) == 2 ~ "25-34", as.numeric(ageadmit) == 3 ~ "35-44",
      as.numeric(ageadmit) == 4 ~ "45-54", as.numeric(ageadmit) == 5 ~ "55+", TRUE ~ "Unknown/Missing"
    ),
    sentlgth_cat = case_when(
      as.numeric(sentlgth) == 0 ~ "< 1 year", as.numeric(sentlgth) == 1 ~ "1-2 years", as.numeric(sentlgth) == 2 ~ "2-5 years",
      as.numeric(sentlgth) == 3 ~ "5-10 years", as.numeric(sentlgth) == 4 ~ "10-25 years", as.numeric(sentlgth) == 5 ~ ">=25 years",
      as.numeric(sentlgth) == 6 ~ "Life/Death", TRUE ~ "Unknown/Missing"
    )
  ) %>%
  # Corrección 4: Convertir a character antes de guardar para evitar problemas con Parquet
  mutate(across(where(is.factor), as.character)) %>%
  select(
    abt_inmate_id, state, state_name, releaseyr, jri_year,
    treatment, post, recidivism,
    sex, race, ageadmit_cat, offgeneral, sentlgth_cat
  )

cat("-> Transformaciones finales completadas.\n")

# 4. GUARDADO DEL DATASET FINAL ----
cat("Paso 4: Guardando el dataset procesado final...\n")
write_parquet(data_final, sink = output_file, compression = "snappy")
cat("-> Éxito: Dataset limpio guardado en:", output_file, "\n")
cat("-> Total de filas en el dataset final:", nrow(data_final), "\n")
cat("-> Total de columnas en el dataset final:", ncol(data_final), "\n")
cat("Script 01_process_data.R finalizado.\n")