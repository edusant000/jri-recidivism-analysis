# Análisis Causal del Impacto de la Justice Reinvestment Initiative (JRI) en la Reincidencia

## 1\. Descripción del Proyecto

Este repositorio contiene el código y los resultados de un análisis de inferencia causal que evalúa el impacto de la **Justice Reinvestment Initiative (JRI)** en las tasas de reincidencia en Estados Unidos. [cite\_start]La JRI es un programa de reforma del sistema de justicia penal que busca reducir costos correccionales y disminuir la reincidencia a través de políticas basadas en datos[cite: 3, 4].

El objetivo de este proyecto es ir más allá de las correlaciones y aplicar métodos econométricos modernos para estimar el efecto causal de la JRI, explorando la heterogeneidad de su impacto entre los diferentes estados y cohortes de adopción.

## 2\. Hallazgos Principales

El análisis revela una historia compleja y matizada sobre la efectividad de la JRI:

  * **Éxito Aislado en el Caso de Estudio:** Un análisis profundo de South Carolina, uno de los estados pioneros, mediante el Método de Control Sintético sugiere que la JRI tuvo un **efecto exitoso, logrando reducir la tasa de reincidencia** en comparación con un contrafactual creíble.
  * **Efecto Agregado Nulo o Adverso:** Sin embargo, al analizar el efecto promedio en todos los estados tratados mediante un modelo de Diferencias en Diferencias, el resultado es distinto. El análisis por cohortes de adopción muestra que el efecto promedio es, en el mejor de los casos, nulo, e incluso **se asocia con un aumento en la reincidencia para la cohorte que adoptó la política en 2011**.
  * **Conclusión Central:** La efectividad de la JRI no es universal. Su éxito parece depender fuertemente del contexto local, el momento de la adopción y la fidelidad de la implementación.

## 3\. Metodología

Se utilizaron dos enfoques de inferencia causal complementarios para garantizar la robustez de los hallazgos:

1.  **Método de Control Sintético (SCM):** Utilizado para realizar un estudio de caso profundo y transparente sobre South Carolina, comparando su trayectoria real con un contrafactual construido a partir de un promedio ponderado de estados no tratados.
2.  **Diferencias en Diferencias con Múltiples Períodos de Tiempo (Callaway & Sant'Anna, 2021):** Empleado para estimar el efecto promedio del tratamiento en todas las cohortes de estados que adoptaron la JRI en diferentes años, controlando por tendencias previas y covariables.

## 4\. Estructura del Repositorio

```
jri-recidivism-analysis/
├── data/
│   ├── processed/
│   │   └── ncrp_jri_clean.parquet
│   └── raw/
│       └── DS0001/
│           └── 36862-0001-Data.tsv
├── output/
│   ├── plots/
│   │   ├── 01_parallel_trends_plot.png
│   │   ├── 02_scm_trends_plot.png
│   │   ├── 03_scm_weights_plot.png
│   │   ├── 04_scm_placebo_plot.png
│   │   └── 05_cs_dynamic_plot.png
│   └── tables/
├── renv/
├── scripts/
│   ├── 01_process_data.R
│   ├── 02_exploratory_analysis.R
│   └── 03_causal_analysis.R
├── renv.lock
└── README.md
```

## 5\. Configuración y Ejecución

### Prerrequisitos

  * R (versión 4.0 o superior)
  * RStudio (recomendado)

### Instrucciones de Instalación

1.  **Clonar el Repositorio:**

    ```sh
    git clone <URL-del-repositorio>
    cd jri-recidivism-analysis
    ```

2.  **Adquisición de Datos:**

      * [cite\_start]Descargar el set de datos `National Corrections Reporting Program, 1991-2015: Selected Variables (ICPSR 36862)` desde el archivo de ICPSR[cite: 73].
      * [cite\_start]Colocar el archivo de datos `36862-0001-Data.tsv` dentro del directorio `data/raw/DS0001/`[cite: 74]. La estructura de carpetas debe coincidir con la mostrada arriba.

3.  **Instalación de Dependencias:**

      * Este proyecto utiliza `renv` para la gestión de paquetes, asegurando la reproducibilidad.
      * Abra el proyecto en RStudio. La consola de R debería detectar el archivo `renv.lock`.
      * Ejecute el siguiente comando en la consola de R para instalar todas las dependencias exactas:
        ```r
        renv::restore()
        ```

### Cómo Ejecutar el Análisis

Los scripts están numerados y deben ejecutarse en orden secuencial.

1.  **Procesar los Datos Crudos:**

    ```r
    source("scripts/01_process_data.R")
    ```

2.  **Realizar el Análisis Exploratorio:**

    ```r
    source("scripts/02_exploratory_analysis.R")
    ```

3.  **Ejecutar los Modelos de Inferencia Causal:**

    ```r
    source("scripts/03_causal_analysis.R")
    ```

## 6\. Descripción de los Scripts

  * `scripts/01_process_data.R`: Carga el archivo `.tsv` crudo, limpia los nombres de las columnas, realiza el mapeo de estados, crea las variables de tratamiento (`treatment`, `post`, `jri_year`) y recodifica las variables categóricas. El resultado es un archivo `parquet` limpio en `data/processed/`.
  * `scripts/02_exploratory_analysis.R`: Carga los datos procesados, genera estadísticas descriptivas, realiza un análisis de balance de covariables en el período pre-tratamiento y crea el gráfico inicial de tendencias paralelas.
  * `scripts/03_causal_analysis.R`: Contiene el núcleo del análisis.
      * **Fase 1:** Implementa el Método de Control Sintético para el estudio de caso de South Carolina.
      * **Fase 2:** Implementa el modelo de Callaway & Sant'Anna para estimar los efectos promedio y por cohorte.

## 7\. Salidas (Outputs)

  * **Datos Procesados:** El dataset limpio y listo para el análisis se guarda en `data/processed/ncrp_jri_clean.parquet`.
  * **Gráficos:** Todos los gráficos generados durante el análisis (tendencias paralelas, resultados de SCM, estudio de eventos de C\&S, etc.) se guardan en el directorio `output/plots/`.

## 8\. Fuente de Datos

  * [cite\_start]**Título:** National Corrections Reporting Program, 1991-2015: Selected Variables[cite: 73].
  * **Productor Principal:** United States Department of Justice. Office of Justice Programs. [cite\_start]Bureau of Justice Statistics[cite: 73].
  * [cite\_start]**Distribuidor:** Inter-university Consortium for Political and Social Research (ICPSR), Ann Arbor, MI[cite: 73].
  * **DOI:** [https://doi.org/10.3886/ICPSR36862.v1](https://www.google.com/search?q=https://doi.org/10.3886/ICPSR36862.v1)