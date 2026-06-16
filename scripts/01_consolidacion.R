library(tidyverse)
library(janitor)



# -----------------------------------------------------------------------------
# ARCHIVOS
# -----------------------------------------------------------------------------

# Tabla de control: cada CSV con su periodo y las filas de preambulo a saltear (skip).
# El skip varia porque cada encuesta trae arriba un bloque de licencia de largo distinto.
# Dos generaciones de archivos:
#   - 2019-2023 (raw): headers en español -> clean_names() los pasa a snake_case
#   - 2024-2026 (CLEAN): headers ya en snake_case; 2024/2025 traen una columna indice
#     extra sin nombre que se ignora sola al no mapearse en coalesce_cols

archivos <- tribble(
  ~archivo, ~anio, ~semestre, ~skip,
  
  "2019.1 - Encuesta de remuneración salarial - Argentina.csv", 2019, 1, 3,
  "2019.2 - sysarmy - Encuesta de remuneración salarial Argentina - Argentina.csv", 2019, 2, 8,
  
  "2020.1 - sysarmy - Encuesta de remuneración salarial Argentina - Argentina.csv", 2020, 1, 9,
  "2020.2 - sysarmy - Encuesta de remuneración salarial Argentina - Argentina.csv", 2020, 2, 9,
  
  "2021.1 - sysarmy - Encuesta de remuneración salarial Argentina - Argentina.csv", 2021, 1, 10,
  "2021.2 - sysarmy - Encuesta de remuneración salarial Argentina - Argentina.csv", 2021, 2, 9,
  "2022.1 - sysarmy - Encuesta de remuneración salarial Argentina - Argentina.csv", 2022, 1, 10,
  "2022.2 - Sysarmy - Encuesta de remuneración salarial Argentina - Argentina.csv", 2022, 2, 9,
  
  "2023.1 - Sysarmy - Encuesta de remuneración salarial Argentina - Argentina.csv", 2023, 1, 8,
  "2023.2 - Sysarmy - Encuesta de remuneración salarial Argentina - Dataset raw.csv", 2023, 2, 7,
  
  "2024.1 - Sysarmy - Encuesta de remuneración salarial Argentina - Sysarmy 2024.01_CLEAN.csv", 2024, 1, 8,
  
  "2025.1 - Sysarmy - Encuesta de remuneración salarial Argentina - Sysarmy - sueldos - 2025.01CLEAN.csv", 2025, 1, 9,
  "2025.2 - Sysarmy - Encuesta de remuneración salarial Argentina - Sysarmy - sueldos - 2025.02CLEAN.csv", 2025, 2, 9,
  
  "2026.1 - Sysarmy - Encuesta de remuneración salarial Argentina - Sysarmy - sueldos - 2026.01CLEAN.csv", 2026, 1, 9
)


# -----------------------------------------------------------------------------
# FUNCION AUXILIAR
# -----------------------------------------------------------------------------

coalesce_cols <- function(data, cols) {
  
  cols_existentes <- intersect(cols, names(data))
 
  if (length(cols_existentes) == 0) {
    return(rep(NA, nrow(data)))
  }
  
  coalesce(!!!data[cols_existentes])
}


# -----------------------------------------------------------------------------
# FUNCION PRINCIPAL DE CONSOLIDACION
# -----------------------------------------------------------------------------

leer_sysarmy <- function(archivo, anio, semestre, skip) {
  
  ruta <- file.path("data","raw", archivo)
  data <- read_csv(
    ruta,
    skip = skip,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  ) %>%
    clean_names()   # funcion de la libreria JANITOR, ayuda a normalizar nombres de las columnas
  
  tibble(
    anio = anio,
    semestre = semestre,
    periodo = paste0(anio, ".", semestre),
    
    provincia = coalesce_cols(data, c(
      "donde_estas_trabajando"
    )),
    
    pais = coalesce_cols(data, c(
      "estoy_trabajando_en"
    )),
    
    genero = coalesce_cols(data, c(
      "me_identifico",
      "me_identifico_genero",
      "genero"
    )),
    
    edad = coalesce_cols(data, c(
      "tengo",
      "tengo_edad"
    )),
    
    dedicacion = coalesce_cols(data, c(
      "dedicacion"
    )),
    
    tipo_contrato = coalesce_cols(data, c(
      "tipo_de_contrato"
    )),
    
    salario_bruto = coalesce_cols(data, c(
      "salario_mensual_bruto_en_tu_moneda_local",
      "salario_mensual_o_retiro_bruto_en_tu_moneda_local",
      "ultimo_salario_mensual_o_retiro_bruto_en_tu_moneda_local",
      "ultimo_salario_mensual_o_retiro_bruto_en_pesos_argentinos"
    )),
    
    salario_neto = coalesce_cols(data, c(
      "salario_mensual_neto_en_tu_moneda_local",
      "salario_mensual_o_retiro_neto_en_tu_moneda_local",
      "ultimo_salario_mensual_o_retiro_neto_en_tu_moneda_local",
      "ultimo_salario_mensual_o_retiro_neto_en_pesos_argentinos"
    )),
    
    pagos_en_dolares = coalesce_cols(data, c(
      "pagos_en_dolares"
    )),
    
    valor_dolar = coalesce_cols(data, c(
      "cual_fue_el_ultimo_valor_de_dolar_que_tomaron",
      "si_tu_sueldo_esta_dolarizado_cual_fue_el_ultimo_valor_del_dolar_que_tomaron"
    )),
    
    trabajo_de = coalesce_cols(data, c(
      "trabajo_de"
    )),
    
    experiencia = coalesce_cols(data, c(
      "anos_de_experiencia"
    )),
    
    antiguedad_empresa = coalesce_cols(data, c(
      "anos_en_la_empresa_actual",
      "antiguedad_en_la_empresa_actual"
    )),
    
    antiguedad_puesto = coalesce_cols(data, c(
      "anos_en_el_puesto_actual",
      "tiempo_en_el_puesto_actual"
    )),
    
    gente_a_cargo = coalesce_cols(data, c(
      "gente_a_cargo",
      "cuantas_personas_a_cargo_tenes",
      "cuantas_personas_tenes_a_cargo"
    )),
    
    nivel_estudios = coalesce_cols(data, c(
      "nivel_de_estudios_alcanzado",
      "maximo_nivel_de_estudios"
    )),
    
    estado_estudios = coalesce_cols(data, c(
      "estado"
    )),
    
    carrera = coalesce_cols(data, c(
      "carrera"
    )),
    
    modalidad_trabajo = coalesce_cols(data, c(
      "modalidad_de_trabajo"
    )),
    
    seniority = coalesce_cols(data, c(
      "seniority"
    )),
    
    cantidad_personas_organizacion = coalesce_cols(data, c(
      "cantidad_de_personas_en_tu_organizacion"
    )),
    
    uso_ia = coalesce_cols(data, c(
      "que_tanto_estas_usando_copilotchatgpt_u_otras_herramientas_de_ia_para_tu_trabajo"
    )),
    
    sueldo_dolarizado = coalesce_cols(data, c(
      "sueldo_dolarizado",
      "tu_sueldo_esta_dolarizado",
      "pagos_en_dolares"
    ))
    
    
  )
}

# -----------------------------------------------------------------------------
# CONCATENAR TODOS LOS DATASETS
# -----------------------------------------------------------------------------

df_sysarmy <- pmap_dfr(
  archivos,
  leer_sysarmy
)

write_csv(df_sysarmy, "data/intermediate/sysarmy_consolidado.csv")