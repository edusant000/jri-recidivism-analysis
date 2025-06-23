# =============================================================================
# 03_causal_analysis.R
#
# OBJETIVO:
# Implementar un análisis de inferencia causal robusto para medir el
# impacto de la JRI en las tasas de reincidencia.
#
# FASE 1: Control Sintético (SCM) para un estudio de caso (South Carolina).
# FASE 2: Diferencias en Diferencias con Adopción Escalonada (Callaway & Sant'Anna)
#          para estimar el efecto promedio en todos los estados.
#
# NOTA: Este script incorpora correcciones para manejar NAs mediante imputación,
# asegurar la inclusión de la unidad de tratamiento y rellenar metadatos.
# =============================================================================

# 1. CONFIGURACIÓN INICIAL ----
library(here)
library(arrow)
library(dplyr)
library(ggplot2)
library(tidyr)     # Añadido explícitamente para fill() y complete()
library(tidysynth) # Para Control Sintético
library(did)       # Para Callaway & Sant'Anna
library(zoo)       # Para imputación lineal (na.approx)
library(purrr)      # ← necesario para map_lgl

cat("Iniciando el script 03_causal_analysis.R...\n")

# Crear directorios de salida si no existen
dir.create(here::here("output", "plots"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("output", "tables"), showWarnings = FALSE, recursive = TRUE)

# 2. CARGA Y PREPARACIÓN DE DATOS ----
cat("Paso 2: Cargando y preparando datos para el análisis causal...\n")
clean_data_path <- here::here("data", "processed", "ncrp_jri_clean.parquet")
df <- read_parquet(clean_data_path)

# --- Agregación de datos a nivel Estado-Año ---
state_level_data <- df %>%
  group_by(state_name, releaseyr) %>%
  summarise(
    avg_recidivism = mean(recidivism, na.rm = TRUE),
    pct_male = mean(sex == "Male", na.rm = TRUE),
    pct_white = mean(race == "White", na.rm = TRUE),
    pct_black = mean(race == "Black", na.rm = TRUE),
    pct_violent_offense = mean(offgeneral == "Violent", na.rm = TRUE),
    pct_property_offense = mean(offgeneral == "Property", na.rm = TRUE),
    jri_year = first(jri_year),
    treatment = first(treatment),
    .groups = "drop"
  )

# --- [BLOQUE CORREGIDO] Validación y Limpieza de Datos para SCM ---
cat("-> Validando datos para SCM: balanceando, rellenando metadatos e imputando...\n")

# Definir la ventana de pre-tratamiento y los predictores clave
PRE_TREATMENT_WINDOW <- 2000:2009
PREDICTOR_VARS <- c("avg_recidivism", "pct_male", "pct_white", "pct_black", "pct_violent_offense")

# 1. Balancear el panel: crear filas explícitas para años faltantes
balanced_data <- state_level_data %>%
  tidyr::complete(state_name, releaseyr)

# 2. [NUEVO] Rellenar metadatos de tratamiento (jri_year, treatment)
#    Se rellenan los NAs creados por complete() usando los valores existentes del mismo estado.
imputed_metadata <- balanced_data %>%
  group_by(state_name) %>%
  tidyr::fill(treatment, jri_year, .direction = "downup") %>%
  ungroup()

# 3. Imputar predictores faltantes usando interpolación lineal
imputed_data <- imputed_metadata %>%
  group_by(state_name) %>%
  arrange(releaseyr) %>%
  mutate(across(
    all_of(PREDICTOR_VARS),
    ~ zoo::na.approx(.x, na.rm = FALSE, rule = 2)
  )) %>%
  ungroup()

# 4. Identificar estados con un mínimo de datos válidos (80%) en la ventana pre-tratamiento
states_with_enough_data <- imputed_data %>%
  filter(releaseyr %in% PRE_TREATMENT_WINDOW) %>%
  group_by(state_name) %>%
  summarise(prop_valid = mean(complete.cases(across(all_of(PREDICTOR_VARS))))) %>%
  filter(prop_valid >= 0.80) %>%
  pull(state_name)

# 5. Salvaguarda: Asegurarse de que la unidad de tratamiento ("South Carolina") siempre esté incluida
case_study_state <- "Pennsylvania"
if (!case_study_state %in% states_with_enough_data) {
  states_with_enough_data <- c(states_with_enough_data, case_study_state)
  cat("-> Advertencia: 'South Carolina' no cumplía el umbral del 80% y fue añadido manualmente.\n")
}

# 6. Crear el conjunto de datos final y limpio para el análisis SCM
scm_data_clean <- imputed_data %>%
  filter(state_name %in% states_with_enough_data)

cat("-> Preparación de datos para SCM finalizada. Unidades a procesar:",
    length(unique(scm_data_clean$state_name)), "\n")

cat("-> Preparación de datos para SCM finalizada. Unidades a procesar:", length(states_with_enough_data), "\n")

# =============================================================================
# FASE 1: CONTROL SINTÉTICO (SCM) PARA SOUTH CAROLINA
# =============================================================================
cat("\nFASE 1: Ejecutando análisis de Control Sintético para South Carolina...\n")

treatment_year <- 2012

sc_model <- scm_data_clean %>%
  synthetic_control(
    outcome = avg_recidivism,
    unit = state_name,
    time = releaseyr,
    i_unit = case_study_state,
    i_time = treatment_year,
    generate_placebos = TRUE
  ) %>%
  generate_predictor(
    time_window = PRE_TREATMENT_WINDOW,
    pct_male = mean(pct_male, na.rm = TRUE),
    pct_white = mean(pct_white, na.rm = TRUE),
    pct_black = mean(pct_black, na.rm = TRUE),
    pct_violent_offense = mean(pct_violent_offense, na.rm = TRUE)
  ) %>%
  generate_weights(
    optimization_window = PRE_TREATMENT_WINDOW,
    margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6
  ) %>%
  generate_control()

# --- Visualización de Resultados SCM ---
cat("-> Generando gráficos del SCM...\n")
scm_trends_plot <- plot_trends(sc_model) +
  labs(
    title = "Impacto del JRI en South Carolina (Control Sintético)",
    subtitle = "Comparación de la Tasa de Reincidencia Real vs. Contrafactual Sintético (con imputación)",
    y = "Tasa de Reincidencia Promedio",
    x = "Año de Liberación"
  ) + theme_minimal(base_size = 14)
print(scm_trends_plot)
ggsave(here::here("output", "plots", "02_scm_trends_plot.png"), scm_trends_plot, width = 12, height = 8, bg = "white")

scm_weights_plot <- plot_weights(sc_model) +
  labs(title = "Ponderaciones del Grupo de Control para 'Synthetic South Carolina'")
print(scm_weights_plot)
ggsave(here::here("output", "plots", "03_scm_weights_plot.png"), scm_weights_plot, width = 10, height = 7, bg = "white")

# --- Placebos: descartar unidades con trt_time faltante ----------------------
# --- Placebos: descartar unidades con trt_time faltante ----------------------
sc_model_clean <- sc_model %>%                          # <-- SUSTITUIR
  filter(
    purrr::map_lgl(
      .meta,
      ~ !is.null(.x$treatment_time) &&                  # nombre correcto
        !is.na(.x$treatment_time)                       # siempre devuelve TRUE/FALSE
    )
  )


scm_data_clean <- scm_data_clean %>% mutate(releaseyr = as.integer(releaseyr))

# Definir ventana de placebos desde el inicio del pre-tratamiento hasta el máximo año disponible
max_year <- max(scm_data_clean$releaseyr, na.rm = TRUE)        
PLACEBO_WINDOW <- c(min(PRE_TREATMENT_WINDOW), max_year)

# (opcional) Verificar que incluya el año de tratamiento
print(PLACEBO_WINDOW) 

scm_placebo_plot <- plot_placebos(
    sc_model_clean,
    time_window = PLACEBO_WINDOW
) +
  labs(title = "Prueba de Placebo: Efecto en South Carolina vs. Efectos Falsos")

print(scm_placebo_plot)
ggsave(
  here::here("output", "plots", "04_scm_placebo_plot.png"),
  scm_placebo_plot, width = 12, height = 8, bg = "white"
)
cat("-> Análisis SCM finalizado. Gráficos guardados en output/plots.\n")

# =============================================================================
# FASE 2: CALLAWAY & SANT'ANNA (DiD con Adopción Escalonada)
# =============================================================================
cat("\nFASE 2: Ejecutando modelo de Callaway & Sant'Anna...\n")

pre_period <- min(cs_data$jri_year_imputed[cs_data$jri_year_imputed > 0], na.rm = TRUE) - 1

candidate_covars <- c("pct_male", "pct_white", "pct_black",
                      "pct_violent_offense", "pct_property_offense")

vars_to_drop <- cs_data %>%
  filter(releaseyr <= pre_period) %>%
  summarise(across(all_of(candidate_covars), ~ var(.x, na.rm = TRUE))) %>%
  pivot_longer(everything()) %>%
  filter(is.na(value) | value == 0) %>%
  pull(name)

dynamic_covars <- setdiff(candidate_covars, vars_to_drop)

covariate_formula <- ~ 1 

cs_data <- imputed_data %>%
  mutate(
    jri_year_imputed = if_else(is.na(jri_year), 0L, as.integer(jri_year)),
    state_id         = as.numeric(factor(state_name)),
    releaseyr        = as.integer(releaseyr)
  )

cs_model <- att_gt(
  yname   = "avg_recidivism",
  tname   = "releaseyr",
  idname  = "state_id",
  gname   = "jri_year_imputed",
  xformla = covariate_formula,          # usa solo intercepto
  data    = cs_data,
  control_group = "notyettreated",
  est_method    = "reg",                # <-- SUSTITUIR (antes “dr”)
  allow_unbalanced_panel = TRUE
)


# --- Análisis de Resultados C&S ---
cat("-> Modelo estimado. Generando resultados agregados y gráficos...\n")

# 1. Calcular el efecto agregado GENERAL para el resumen en tabla (sin cambios)
overall_att <- aggte(cs_model, type = "simple")
cat("\n--- Estimación Agregada del Efecto del Tratamiento (ATT) ---\n")
summary(overall_att)

# 2. [NUEVO] Calcular los efectos DINÁMICOS (por event-time) para el gráfico
dynamic_att <- aggte(cs_model, type = "dynamic")

# 3. [MODIFICADO] Llamar a ggdid() con el objeto de efectos dinámicos
cs_dynamic_plot <- ggdid(dynamic_att) +
  labs(
    title = "Efecto Dinámico del JRI en la Reincidencia (Callaway & Sant'Anna)",
    subtitle = "Efecto promedio en todos los estados tratados, relativo al año de la reforma",
    y = "Cambio en la Tasa de Reincidencia (ATT)",
    x = "Años relativos a la implementación del JRI (0 = año de la reforma)"
  ) + theme_minimal(base_size = 14)

print(cs_dynamic_plot)
ggsave(here::here("output", "plots", "05_cs_dynamic_plot.png"), cs_dynamic_plot, width = 12, height = 8, bg = "white")

cat("-> Análisis de Callaway & Sant'Anna finalizado. Gráfico guardado en output/plots.\n")
cat("\nScript 03_causal_analysis.R finalizado.\n")

cat("-> Generando gráfico de efectos por cohorte...\n")

# Determinar los límites del eje X para hacer el código más robusto
min_year <- min(cs_data$releaseyr, na.rm = TRUE)
max_year <- max(cs_data$releaseyr, na.rm = TRUE)

# ggdid puede graficar directamente el objeto att_gt, mostrando cada grupo
cohort_att_plot <- ggdid(cs_model) +
  labs(
    title = "Efecto del JRI por Cohorte de Adopción",
    subtitle = "Comparación de los efectos del tratamiento para grupos que adoptaron en diferentes años",
    y = "Cambio en la Tasa de Reincidencia (ATT)",
    x = "Año del Calendario"
  ) +
  # --- LÍNEAS AÑADIDAS PARA CORREGIR LA VISUALIZACIÓN ---
  scale_x_continuous(breaks = seq(from = min_year, to = max_year, by = 5)) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


print(cohort_att_plot)
ggsave(here::here("output", "plots", "06_cs_cohort_plot.png"),
       cohort_att_plot, width = 12, height = 9, bg = "white") # Aumenté un poco la altura

cat("-> Gráfico de efectos por cohorte guardado en output/plots.\n")