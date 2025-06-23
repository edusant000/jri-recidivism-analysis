# =============================================================================
# 02_exploratory_analysis.R
#
# OBJETIVO:
# Realizar un análisis exploratorio de la base de datos limpia.
# 1. Generar estadísticas descriptivas de la composición de la muestra.
# 2. Analizar el balance de características entre grupos pre-tratamiento.
# 3. Crear el gráfico de tendencias paralelas para validar el supuesto clave
#    del modelo de Diferencias en Diferencias.
# =============================================================================

# 1. CONFIGURACIÓN INICIAL ----
library(here)
library(arrow)
library(dplyr)
library(ggplot2)
library(knitr) # Para crear tablas limpias en la consola
library(scales) # Para formatear ejes en los gráficos

# Crear directorios de salida si no existen
dir.create(here::here("output", "plots"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("output", "tables"), showWarnings = FALSE, recursive = TRUE)

# 2. CARGA DE DATOS ----
cat("Paso 2: Cargando el dataset procesado...\n")
clean_data_path <- here::here("data", "processed", "ncrp_jri_clean.parquet")
if (!file.exists(clean_data_path)) {
  stop("El archivo de datos procesados no se encontró. Por favor, ejecute el script 01 primero.")
}
df <- read_parquet(clean_data_path)
cat("-> Datos cargados. Total de filas:", nrow(df), "\n")


# 3. ANÁLISIS DESCRIPTIVO DE LA MUESTRA ----
cat("\nPaso 3: Realizando análisis descriptivo...\n")

# Resumen de composición de grupos
composition_summary <- df %>%
  group_by(treatment = factor(treatment, labels = c("Control", "Tratamiento"))) %>%
  summarise(
    n_observaciones = n(),
    n_estados = n_distinct(state_name)
  ) %>%
  mutate(pct_observaciones = n() / sum(n()) * 100)

cat("\n--- Composición de Grupos de Tratamiento y Control ---\n")
print(kable(composition_summary, caption = "Composición de la Muestra por Grupo"))


# 4. ANÁLISIS DE BALANCE PRE-TRATAMIENTO ----
# Comparamos características de ambos grupos antes de la primera intervención (2010)
cat("\nPaso 4: Analizando balance de covariables en el período pre-tratamiento (antes de 2010)...\n")
pre_treatment_balance <- df %>%
  filter(releaseyr < 2010) %>%
  group_by(treatment = factor(treatment, labels = c("Control", "Tratamiento"))) %>%
  summarise(
    tasa_reincidencia_promedio = mean(recidivism, na.rm = TRUE),
    pct_delitos_violentos = mean(offgeneral == "Violent", na.rm = TRUE) * 100,
    pct_delitos_propiedad = mean(offgeneral == "Property", na.rm = TRUE) * 100,
    pct_delitos_drogas = mean(offgeneral == "Drug", na.rm = TRUE) * 100,
    pct_hombres = mean(sex == "Male", na.rm = TRUE) * 100
  ) %>%
  t() # Transponer para mejor legibilidad

cat("\n--- Balance de Características Pre-Tratamiento (< 2010) ---\n")
print(kable(pre_treatment_balance, caption = "Comparación de Grupos Antes de la Intervención"))


# 5. VERIFICACIÓN DE TENDENCIAS PARALELAS ----
cat("\nPaso 5: Generando el gráfico de tendencias paralelas...\n")

# Calcular tasas de reincidencia anuales por grupo
recidivism_trends <- df %>%
  group_by(releaseyr, treatment) %>%
  summarise(
    avg_recidivism = mean(recidivism, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(treatment = factor(treatment, labels = c("Control", "Tratamiento")))

# Crear el gráfico
parallel_trends_plot <- ggplot(recidivism_trends, aes(x = releaseyr, y = avg_recidivism, color = treatment, group = treatment)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  # Línea vertical indicando el inicio de las primeras políticas JRI
  geom_vline(xintercept = 2010, linetype = "dashed", color = "gray40", linewidth = 1) +
  # Anotación para la línea vertical
  annotate("text", x = 2010.5, y = max(recidivism_trends$avg_recidivism) * 0.95, 
           label = "Inicio JRI", hjust = 0, angle = 90, color = "gray20") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(min(df$releaseyr, na.rm=T), max(df$releaseyr, na.rm=T), by = 2)) +
  labs(
    title = "Verificación de Tendencias Paralelas en Tasas de Reincidencia",
    subtitle = "Comparación entre estados con y sin políticas JRI",
    x = "Año de Liberación",
    y = "Tasa de Reincidencia Promedio",
    color = "Grupo"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Guardar el gráfico
plot_path <- here::here("output", "plots", "01_parallel_trends_plot.png")
ggsave(plot_path, parallel_trends_plot, width = 12, height = 8, bg = "white")
cat("-> Gráfico de tendencias paralelas guardado en:", plot_path, "\n")
cat("\nScript 02_exploratory_analysis.R finalizado.\n")

# Mostrar el gráfico en la ventana de RStudio
print(parallel_trends_plot)