library(tidyverse)


# -----------------------------------------------------------------------------
# 1. CARGA DEL DATASET CONSOLIDADO
# -----------------------------------------------------------------------------

# INPUT: Sysarmy_consolidado.csv — 75.649 observaciones, 26 variables
df_sysarmy <- read_csv(
  "data/intermediate/sysarmy_consolidado.csv",
  show_col_types = FALSE
)

# Verificacion de inputs
dim(df_sysarmy) # 75649



# -----------------------------------------------------------------------------
# 2. LIMPIEZA DE TIPOS
# -----------------------------------------------------------------------------

# Las numericas llegan como character por formatos inconsistentes entre encuestas.
# parse_number extrae el numero ignorando simbolos y separadores.

# NA antes de convertir, para medir cuantos valores eran texto no convertible
na_antes <- colSums(is.na(df_sysarmy[c("salario_bruto", "salario_neto",
                                       "edad", "experiencia",
                                       "antiguedad_empresa", "antiguedad_puesto",
                                       "gente_a_cargo")]))
na_antes # salario_neto 33 , edad 33, salario_bruto 1


df_sysarmy <- df_sysarmy %>%
  mutate(
    salario_bruto = parse_number(as.character(salario_bruto)),
    salario_neto = parse_number(as.character(salario_neto)),
    edad = parse_number(as.character(edad)),
    experiencia = parse_number(as.character(experiencia)),
    antiguedad_empresa = parse_number(as.character(antiguedad_empresa)),
    antiguedad_puesto = parse_number(as.character(antiguedad_puesto)),
    gente_a_cargo = parse_number(as.character(gente_a_cargo))
  )

# NA después de la conversión
na_despues <- colSums(is.na(df_sysarmy[c("salario_bruto", "salario_neto",
                                         "edad", "experiencia",
                                         "antiguedad_empresa", "antiguedad_puesto",
                                         "gente_a_cargo")]))
na_despues

# NA nuevos generados por parse_number (valores que existian pero no eran numericos)
na_despues - na_antes
# salario_bruto: 47 NA nuevos — texto cargado en lugar de numeros (ej. "BRUTO")
# salario_neto:   9 NA nuevos — mismo problema, menor cantidad
# edad: algunos NA nuevos por texto libre — el resto convierte sin perdidas
# Los NA de sal_bruto se eliminan en el paso 6


# -----------------------------------------------------------------------------
# VERIFICACIÓN Y ELIMINACIÓN DE DUPLICADOS
# -----------------------------------------------------------------------------


filas_totales <- nrow(df_sysarmy)
filas_unicas  <- nrow(distinct(df_sysarmy))
duplicados    <- filas_totales - filas_unicas

cat("Filas totales:", filas_totales, "\n")
cat("Filas únicas:", filas_unicas, "\n")
cat("Duplicados:", duplicados, "\n")
# 290 duplicados (0.38%) — formularios enviados mas de una vez. Se eliminan con distinct()


df_sysarmy <- df_sysarmy %>%
  distinct()

cat("Cantidad de registros:", nrow(df_sysarmy), "\n") # 75.359


# -----------------------------------------------------------------------------
# 3. DEFINICIÓN DE ROLES A INCLUIR
# -----------------------------------------------------------------------------

roles_datos <- c(
  "Data Scientist",
  "Data Engineer",
  "BI Analyst / Data Analyst",
  "AI Engineer",
  "AI / Prompt / Chatbots",
  "Data Scientist / Data Engineer",   
  "Machine Learning Engineer",
  "MLOps",
  "NLP",
  "Analytics Engineer",              
  "Data Visualization Engineer",
  "Data Visualization",
  "Data Governance / GRC",
  "DataOps",
  "Data Architect",
  "Business Analyst"
)


roles_infraestructura <- c(
  "SysAdmin / DevOps / SRE",
  "DBA",
  "DBA (Database Administrator)",
  "Infraestructura",
  "Infrasctruture Engineer",          
  "Architect",
  "Cloud Engineer",
  "DevOps Engineer",
  "DevOps",
  "SRE (Site Reliability Engineer)",
  "SysAdmin",
  "Storage / Backup",
  "Networking",
  "Middleware"
)



roles_ciberseguridad <- c(
  "Infosec",
  "IT Security",
  "Analista de Cyberseguridad",
  "Analista de seguridad",
  "Business Continuity Analyst",
  "Cybersecurity",
  "Ciberseguridad",
  "Security",
  "Security Analyst",
  "Security Engineer",
  "SOC Analyst",
  "Pentester"
)

roles_desarrollo <- c(
  "Developer",
  "QA / Tester",
  "Consultant",
  "Software Engineer",
  "Analista Funcional",
  "Analista funcional"
)


roles_liderazgo <- c(
  "Technical Leader",
  "Manager / Director",
  "VP / C-Level"
)

# Cada vector agrupa los roles de un area tematica. Los strings deben coincidir
# EXACTACTAMENTE con trabajo_de. 
# El agrupamiento final se hace en el paso 5 (grupo_rol).
todos_los_roles <- c(
  roles_datos,
  roles_infraestructura,
  roles_ciberseguridad,
  roles_desarrollo,
  roles_liderazgo
)



# -----------------------------------------------------------------------------
# 4. FILTRADO POR ROL
# -----------------------------------------------------------------------------


# Conservamos solo los roles del sector data y tecnologia
# El dataset pasa de 75.359 a 62.757 observaciones
df_clean <- df_sysarmy %>%
  filter(trabajo_de %in% todos_los_roles)


dim(df_clean) # 62757 registros, 26 columnsa

# Roles excluidos con mayor volumen (control)
df_sysarmy %>%
  filter(!trabajo_de %in% todos_los_roles) %>%
  count(trabajo_de, sort = TRUE) %>%
  slice_head(n = 20) %>%
  print(n = Inf)


# Distribucion por rol — identificamos roles con muy pocas observaciones
df_clean %>%
  count(trabajo_de, sort = TRUE)

# Roles con menos de 10 obs no son representativos — se eliminan
# Saca 73 filas repartidas en 21 micro-roles (ej. Pentester, MLOps, DataOps)
roles_minimos <- df_clean %>%
  count(trabajo_de) %>%
  filter(n >= 10)

df_clean <- df_clean %>%
  filter(trabajo_de %in% roles_minimos$trabajo_de)

# Verificacion final
dim(df_clean) # 62.684
df_clean %>%
  count(trabajo_de, sort = TRUE)

cat("Cantidad de registros:", nrow(df_clean), "\n") # 62.684

# -----------------------------------------------------------------------------
# 5. CREACIÓN DE VARIABLES PARA EL ANÁLISIS
# -----------------------------------------------------------------------------


# Asigna cada rol a uno de los cinco grupos definidos en el paso 3.
# No necesita TRUE ~ "Otro": el paso 4 garantiza que toda fila pertenece a un grupo.
# El factor con levels fija el orden en tablas y graficos (sin esto, R ordena alfabetico).
df_clean <- df_clean %>%
  mutate(
    grupo_rol = case_when(
      trabajo_de %in% roles_datos          ~ "Datos / AI",
      trabajo_de %in% roles_infraestructura ~ "Infraestructura",
      trabajo_de %in% roles_ciberseguridad  ~ "Ciberseguridad",
      trabajo_de %in% roles_desarrollo      ~ "Desarrollo / QA",
      trabajo_de %in% roles_liderazgo       ~ "Roles de gestión"
    ),
    
    grupo_rol = factor(
      grupo_rol,
      levels = c(
        "Datos / AI",
        "Infraestructura",
        "Ciberseguridad",
        "Desarrollo / QA",
        "Roles de gestión"
      )
    ),
    
    
    
    # Factor ORDENADO: Junior < Semi-Senior < Senior (ordered = TRUE habilita la jerarquia).
    # 71.7% NA estructurales: la pregunta recien aparece en 2024.1 (2019-2023 no la relevaron).
    # Esto define el alcance temporal del modelo: drop_na(seniority) deja solo 2024+.
    seniority = factor(
      seniority,
      levels = c("Junior", "Semi-Senior", "Senior"),
      ordered = TRUE
    ),
    
   
    # Discretiza gente_a_cargo (muy sesgada: 74% en 0) en cuatro tramos.
    # Agrupar evita que outliers de carga (cientos a cargo) distorsionen el analisis.
    # TRUE ~ NA_character_ captura los NA originales de gente_a_cargo.
    gente_a_cargo_grupo = case_when(
      gente_a_cargo == 0 ~ "Sin equipo",
      gente_a_cargo >= 1 & gente_a_cargo <= 4 ~ "Equipo pequeño",
      gente_a_cargo >= 5 & gente_a_cargo <= 10 ~ "Equipo mediano",
      gente_a_cargo > 10 ~ "Equipo grande",
      TRUE ~ NA_character_
    ),
    
    
    # Factor ORDENADO de menor a mayor tamaño de equipo
    gente_a_cargo_grupo = factor(
      gente_a_cargo_grupo,
      levels = c(
        "Sin equipo",
        "Equipo pequeño",
        "Equipo mediano",
        "Equipo grande"
      ),
      ordered = TRUE
    ),
    
    
    
    # VERIFICACION PREVIA (conteos crudos en df_clean post-rol, antes del mutate):
    # - "Full-Time" (25.989)  -> "Staff": mismo tipo, nombre viejo en encuestas antiguas
    # - "Remoto (empresa de otro pais)" (2.875) -> Contractor (relacion con el exterior)
    # - "Part-Time" (1.588) y "Participacion societaria..." (254) -> Otro
    contrato = case_when(
      tipo_contrato == "Staff (planta permanente)"                         ~ "Staff",
      tipo_contrato == "Full-Time"                                         ~ "Staff",
      tipo_contrato == "Contractor"                                        ~ "Contractor",
      tipo_contrato == "Remoto (empresa de otro país)"                     ~ "Contractor",
      tipo_contrato == "Tercerizado (trabajo a través de consultora o agencia)" ~ "Tercerizado",
      tipo_contrato == "Freelance"                                         ~ "Freelance",
      tipo_contrato == "Part-Time"                                         ~ "Otro",
      tipo_contrato == "Participación societaria en una cooperativa"       ~ "Otro",
      TRUE ~ "Otro"
    ),
    
    contrato = factor(
      contrato,
      levels = c("Staff", "Contractor", "Tercerizado", "Freelance", "Otro")
    ),
    
    
    # Estandariza modalidad_trabajo en tres categorias; TRUE ~ NA_character_ para el resto.
    # 49% NA estructural: la pregunta recien aparece en 2022.2 (2019-2022.1 no la relevaron).
    modalidad = case_when(
      modalidad_trabajo == "100% remoto" ~ "Remoto",
      modalidad_trabajo == "Híbrido (presencial y remoto)" ~ "Hibrido",
      modalidad_trabajo == "100% presencial" ~ "Presencial",
      TRUE ~ NA_character_
    ),
    
    modalidad = factor(
      modalidad,
      levels = c("Remoto", "Hibrido", "Presencial")
    ),
    
    
   
    # Estandariza cantidad_personas_organizacion en 11 tramos ordenados.
    # ordered = TRUE + levels: sin esto R ordena alfabetico ("+10000" quedaria primero).
    # OJO: este case_when solo mapea el formato largo ("De 11 a 50 personas").
    # 2021 y 2022.1 usan formato abreviado ("11-50") que NO matchea -> quedan NA.
    # No es que falte el dato: es una limitacion de mapeo (tam_empresa no se usa en el modelo).
    tam_empresa = case_when(
      cantidad_personas_organizacion == "1 (solamente yo)" ~ "1",
      cantidad_personas_organizacion == "De 2 a 10 personas" ~ "2-10",
      cantidad_personas_organizacion == "De 11  a 50  personas" ~ "11-50",
      cantidad_personas_organizacion == "De 51 a 100 personas" ~ "51-100",
      cantidad_personas_organizacion == "De 101 a 200 personas" ~ "101-200",
      cantidad_personas_organizacion == "De 201 a 500 personas" ~ "201-500",
      cantidad_personas_organizacion == "De 501 a 1000 personas" ~ "501-1000",
      cantidad_personas_organizacion == "De 1001 a 2000 personas" ~ "1001-2000",
      cantidad_personas_organizacion == "De 2001a 5000 personas" ~ "2001-5000",
      cantidad_personas_organizacion == "De 5001 a 10000 personas" ~ "5001-10000",
      cantidad_personas_organizacion == "Más de 10000 personas" ~ "+10000",
      TRUE ~ NA_character_
    ),
    
    tam_empresa = factor(
      tam_empresa,
      levels = c(
        "1", "2-10", "11-50", "51-100", "101-200",
        "201-500", "501-1000", "1001-2000", "2001-5000",
        "5001-10000", "+10000"
      ),
      ordered = TRUE
    ),
    
    
    
    # Colapsa el texto libre de genero en tres categorias (con/sin tilde, may/min, sufijo Cis).
    # Catch-all = "Otro/No binarie", NO NA: no son datos faltantes sino identidades distintas.
    # Sin ordered = TRUE: las categorias de genero no tienen jerarquia.
    genero_simple = case_when(
      genero %in% c("Hombre Cis", "Varón Cis", "Hombre", "Varón", "Varon",
                    "Masculino", "masculino", "hombre", "varón") ~ "Hombre",
      genero %in% c("Mujer Cis", "Mujer", "mujer") ~ "Mujer",
      TRUE ~ "Otro/No binarie"
    ),
    
    genero_simple = factor(
      genero_simple,
      levels = c("Hombre", "Mujer", "Otro/No binarie")
    ),
    
    
    # VERIFICACION PREVIA (conteos crudos en df_clean post-rol):
    # - "Posgrado" (1.138) = "Posgrado/Especialización": mismo nivel, distinto nombre entre ediciones
    # - "Primario" (10) -> NA: muy pocas obs, probable error de carga
    # - ~40% NA: la pregunta paso a ser OPCIONAL desde 2021 (no es error de carga)
    nivel_estudios = case_when(
      nivel_estudios == "Posgrado" ~ "Posgrado/Especialización",
      nivel_estudios == "Primario" ~ NA_character_,  
      TRUE ~ nivel_estudios  
    ),
    
    nivel_estudios = factor(
      nivel_estudios,
      levels = c(
        "Secundario", "Terciario", "Universitario",
        "Posgrado/Especialización", "Maestría", "Doctorado", "Posdoctorado"
      ),
      ordered = TRUE # jerarquia natural de menor a mayor formacion academica
    ),
    
    
    # Colapsa provincia en tres regiones. El is.na() va ANTES del catch-all:
    # sin esa linea, los NA de provincia caerian en "Interior" por el TRUE.
    # Verificado: todo lo que cae en "Interior" son provincias argentinas (sin ubicaciones del exterior).
    # Sin ordered = TRUE: las regiones no tienen jerarquia.
    region = case_when(
      provincia == "Ciudad Autónoma de Buenos Aires" ~ "CABA",
      provincia %in% c("Buenos Aires", 
                       "Provincia de Buenos Aires",
                       "GBA") ~ "GBA / Prov. BA",
      is.na(provincia) ~ NA_character_,
      TRUE ~ "Interior"
    ),
    
    region = factor(
      region,
      levels = c("CABA", "GBA / Prov. BA", "Interior")
    ),
    
    # Conversion a numerico. Escala 0-5 de uso de herramientas de IA (0 = nada, 5 = maximo).
    # Disponible solo desde 2024.1 — el resto son NA estructurales (mismo origen que seniority).
    uso_ia = parse_number(as.character(uso_ia)),
    
    
    # Normaliza a TRUE/FALSE. str_to_lower() unifica may/min antes de detectar.
    # OJO origen mixto:
    #   - 2024+: pregunta directa con TRUE/FALSE reales
    #   - 2021-2023: PROXY de pagos_en_dolares (seleccion multiple). Solo hay afirmativos:
    #     quien no cobra en USD quedaba en blanco -> NA (no FALSE). El grupo dolarizado
    #     pre-2024 esta sesgado a TRUE. NO usar 2021-2023 para comparar dolarizado.
    # "Cobro parte de mi sueldo en otro pais" -> NA (ambiguo, no implica dolares)
    sueldo_dolarizado = case_when(
      str_detect(str_to_lower(as.character(sueldo_dolarizado)), 
                 "si|sí|yes|true|dolarizado|dólares|dolares") ~ TRUE,
      str_detect(str_to_lower(as.character(sueldo_dolarizado)), 
                 "no|false") ~ FALSE,
      TRUE ~ NA
    ),
    
    # Copia corta para uso analitico
    sal_bruto = salario_bruto,
    # log natural — normaliza la distribucion asimetrica del salario en pesos
    log_sal = log(sal_bruto)
  )

df_clean %>% count(contrato, sort = TRUE)

# -----------------------------------------------------------------------------
# VERIFICACIONES FINALES PASO 5
# -----------------------------------------------------------------------------

# Estructura general — tipos de cada variable
glimpse(df_clean)

# Variables categóricas — distribución y NA
df_clean %>% count(grupo_rol)
df_clean %>% count(seniority)
df_clean %>% count(gente_a_cargo_grupo)
df_clean %>% count(contrato)
df_clean %>% count(modalidad)
df_clean %>% count(tam_empresa)
df_clean %>% count(genero_simple)
df_clean %>% count(nivel_estudios)
df_clean %>% count(region)
df_clean %>% count(sueldo_dolarizado)

# Variables numéricas — resumen estadístico
summary(df_clean$sal_bruto)
summary(df_clean$log_sal)
summary(df_clean$uso_ia)

# -----------------------------------------------------------------------------
# OBSERVACIONES POST-VERIFICACIÓN PASO 5
# -----------------------------------------------------------------------------

# sal_bruto: minimo 0 y maximo absurdo (~123 millones — error de carga)
# - Los 0 (33 registros) son encuestados que no revelaron su sueldo o errores de carga
# - El maximo es claramente un error — nadie gana esos montos mensuales
# - Se resuelven en paso 6 (filtro sal_bruto > 0) y paso 8 (filtro outliers p0.1-p99.9)

# log_sal: minimo -Inf y media -Inf
# - Consecuencia directa de los sal_bruto = 0 — log(0) = -Infinito matematicamente
# - Un solo -Inf contamina la media
# - Se resuelve en paso 6 con el filtro sal_bruto > 0 (is.na NO captura -Inf; ver paso 6)

# tam_empresa: 29.777 NA (49% del dataset)
# - 2019, 2020, 2021: 100% NA | 2022: 52% NA | 2023 en adelante: 0% NA
# - OJO: la pregunta SI existe desde 2021, pero el case_when solo mapea el formato largo
#   ("De 11 a 50 personas"); 2021 y 2022.1 usan formato abreviado ("11-50") -> quedan NA.
#   No es un NA estructural puro, es una limitacion de mapeo.
# - Por el alto % de NA, tam_empresa no se incluye en el modelo de regresion




# -----------------------------------------------------------------------------
# 6. FILTROS BASICOS DE CALIDAD
# -----------------------------------------------------------------------------


df_clean <- df_clean %>%
  filter(
    !is.na(sal_bruto),
    sal_bruto > 0,
    !is.na(log_sal)
  )



cat("Observaciones después del filtro de calidad:", nrow(df_clean), "\n")
# Resultado: ~62.610 (verificar al correr) — se eliminan 74 registros (41 NA + 33 ceros)


# -----------------------------------------------------------------------------
# 7. JOIN CON BLUE 
# -----------------------------------------------------------------------------


# Los salarios nominales en pesos no son comparables entre 2019 y 2026 por la inflacion.
# Se deflacta por el dolar blue (no el oficial: durante el cepo el oficial era artificial)
# para obtener una escala de poder adquisitivo comparable en el tiempo.

# Serie historica del blue desde la API publica de Bluelytics
dolar_blue <- read.csv(
  "https://api.bluelytics.com.ar/v2/evolution.csv"
)


# valor_blue = precio medio (promedio compra/venta). Se asigna cada dia a su periodo
# (anio.semestre) y se promedia -> un blue_promedio por edicion semestral de la encuesta
dolar_blue_s<-dolar_blue %>%
  filter(type == "Blue", day >= as.Date("2019-01-01")) %>%
  mutate(
    day        = as.Date(day),
    valor_blue = (value_sell + value_buy) / 2,
    anio       = year(day),
    semestre   = if_else(month(day) <= 6, 1, 2),
    periodo = as.character(paste(anio, semestre, sep = "."))
    ) %>%
  group_by(periodo) %>%
  summarise(blue_promedio = mean(valor_blue))

# periodo a character en ambos lados para que el join matchee (verificado: 0 NA tras el join)
df_clean <- df_clean %>%
  mutate(periodo = as.character(periodo))

# sal_usd_blue = salario deflactado | log_sal_usd = variable objetivo del modelo
df_clean <- df_clean %>%
  left_join(dolar_blue_s, by = "periodo") %>%
  mutate(
    sal_usd_blue = sal_bruto / blue_promedio,
    log_sal_usd  = log(sal_usd_blue),
    valor_blue = blue_promedio
  )


# Piso de calidad: descarta salarios menores a 100 USD/mes (errores de carga / part-times marginales)
df_clean <- df_clean%>%
  filter(sal_usd_blue >100)


# -----------------------------------------------------------------------------
# 8. FILTRO DE OUTLIERS EN SALARIO Y EDAD
# -----------------------------------------------------------------------------

# Filtramos sobre sal_usd_blue (dolares constantes), no sobre sal_bruto nominal:
# asi el criterio de outlier es comparable entre periodos (la inflacion no lo distorsiona).
# Cortes conservadores p0.1 / p99.9: solo el 0.1% mas extremo de cada cola.
q1  <- quantile(df_clean$sal_usd_blue, 0.001, na.rm = TRUE)
q99 <- quantile(df_clean$sal_usd_blue, 0.999, na.rm = TRUE)

cat("Percentil 0.1 (USD):", q1, "\n")
cat("Percentil 99.9 (USD):", q99, "\n")


# is.na(edad) | (...) : conserva filas con edad/experiencia faltante,
# solo descarta las que tienen un valor presente e implausible.
df_clean <- df_clean %>%
  filter(
    sal_usd_blue >= q1,
    sal_usd_blue <= q99,
    is.na(edad)        | (edad >= 16        & edad <= 70),
    is.na(experiencia) | (experiencia >= 0  & experiencia <= 45)  
  )

cat("Observaciones después del filtro de outliers:", nrow(df_clean), "\n")


# -----------------------------------------------------------------------------
# 9. SELECCION FINAL DE COLUMNAS
# -----------------------------------------------------------------------------


# Seleccionamos las 25 variables analiticas finales.
# Se descartan las columnas crudas ya reemplazadas por versiones estandarizadas
# (salario_bruto -> sal_bruto, genero -> genero_simple, provincia -> region,
#  tipo_contrato -> contrato, modalidad_trabajo -> modalidad,
#  cantidad_personas_organizacion -> tam_empresa) y las auxiliares (blue_promedio, etc.)
df_clean <- df_clean %>%
  select(
    anio,
    semestre,
    periodo,
    
    trabajo_de,
    grupo_rol,
    dedicacion,
    
    sal_bruto,
    log_sal,
    sueldo_dolarizado,
    log_sal_usd,
    sal_usd_blue,
    
    
    seniority,
    experiencia,
    antiguedad_empresa,
    antiguedad_puesto,
    gente_a_cargo,
    gente_a_cargo_grupo,
    
    contrato,
    modalidad,
    tam_empresa,
    
    genero_simple,
    edad,
    nivel_estudios,
    region,
    uso_ia
  )


glimpse(df_clean) # 60.356 registros, 25 columnas



# -----------------------------------------------------------------------------
# 10. VERIFICACIONES FINALES
# -----------------------------------------------------------------------------

# Estructura general
glimpse(df_clean)

# Distribucion por periodo y categoricas clave
df_clean %>% count(periodo)
df_clean %>% count(grupo_rol)
df_clean %>% count(seniority)
df_clean %>% count(genero_simple)
df_clean %>% count(contrato)
df_clean %>% count(region)
df_clean %>% count(sueldo_dolarizado)

# Resumen del salario — confirmamos que no hay valores problematicos
df_clean %>%
  summarise(
    filas                  = n(),
    salario_min            = min(sal_bruto, na.rm = TRUE),
    salario_max            = max(sal_bruto, na.rm = TRUE),
    salario_mediana        = median(sal_bruto, na.rm = TRUE),
    salario_promedio       = mean(sal_bruto, na.rm = TRUE),
    faltantes_salario      = sum(is.na(sal_bruto)),
    log_sal_min            = min(log_sal, na.rm = TRUE),
    log_sal_infinitos      = sum(is.infinite(log_sal)),
    usd_min                = min(sal_usd_blue, na.rm = TRUE),
    usd_infinitos          = sum(is.infinite(log_sal_usd))
  )


# CHECK (solo lectura): confirmamos que no hay log_sal_usd negativos.
# Devuelve 0 filas porque el piso sal_usd_blue > 100 del Paso 7 ya lo garantiza.
df_clean %>%
  filter(log_sal_usd < 0) %>%
  summarise(n = n(), min_usd = min(sal_usd_blue), max_usd = max(sal_usd_blue))

# (El re-filtro que estaba aca era redundante: las 4 condiciones ya se aplicaron
#  en el Paso 8 y eliminaba 0 filas. Se elimina para no mutar datos en una seccion
#  de verificacion. No cambia el resultado.)


# -----------------------------------------------------------------------------
# 11. GUARDADO DEL DATASET FINAL
# -----------------------------------------------------------------------------

write_csv(
  df_clean,
  "data/processed/df_sysarmy.csv"
)
