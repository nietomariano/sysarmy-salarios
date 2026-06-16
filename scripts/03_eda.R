# -----------------------------------------------------------------------------
# 03_EDA.R
# -----------------------------------------------------------------------------

library(tidyverse)

#INDICE

# 1. CARGA DEL DATASET 
# 2. ESTRUCTURA GENERAL 
# 3. CALIDAD DE DATOS 
# 4. DISTRIBUCIONES UNIVARIADAS 
# 5. DISTRIBUCIÓN DE LA VARIABLE OBJETIVO 
# 6. VARIABLES NUMÉRICAS 
# 7. VARIABLES CATEGÓRICAS 
# 8. VARIABLES POTENCIALMENTE CONFUNDIDORAS 
# 9. REGIÓN Y ROLES 
# 10. CONCLUSIONES DEL EDA

# -----------------------------------------------------------------------------
# 1. CARGA DEL DATASET LIMPIO
# -----------------------------------------------------------------------------

df <- read_csv(
  "data/processed/df_sysarmy.csv")


# 2. ESTRUCTURA GENERAL
# -----------------------------------------------------------------------------

glimpse(df) # 60.356. 25
colnames(df)

# -----------------------------------------------------------------------------
# 3. CALIDAD DE DATOS
# -----------------------------------------------------------------------------


colSums(is.na(df))
sort(colSums(is.na(df)), decreasing = TRUE)



df %>%
  group_by(anio,semestre) %>%
  summarise(
    registros = n(),
    seniority_na = sum(is.na(seniority)),
    dolarizado_na = sum(is.na(sueldo_dolarizado)),
    modalidad_na = sum(is.na(modalidad)),
    uso_ia_na = sum(is.na(uso_ia))
  )


# CONCLUSION:
# Las variables PRINCIPALES del modelo (log_sal_usd, sal_usd_blue, experiencia,
# grupo_rol, genero_simple) no presentan NA. region y edad tienen NA minimos (204 y 12).
#
# Los faltantes grandes son ESTRUCTURALES (cambios de cuestionario), no aleatorios:
#   - seniority y uso_ia: 71.7% NA — disponibles desde 2024.1
#   - sueldo_dolarizado: 60.4% NA — disponible desde 2021 via proxy pagos_en_dolares
#     (menos NA que seniority porque arrastra esos años extra)
#   - modalidad: 49% NA — desde 2022.2 | tam_empresa: 49% NA — desde 2023
#
# El analisis por anio/semestre confirma que los NA coinciden con los periodos
# en que cada pregunta no fue relevada, no con errores de carga.



# -----------------------------------------------------------------------------
# 4. DISTRIBUCIONES UNIVARIADAS
# -----------------------------------------------------------------------------

### VARIABLES CATEGORICAS

# grupo_rol — cinco grupos. Desarrollo/QA domina con ~54% del dataset,
# seguido por Infraestructura (19%), Roles de gestion (14%), Datos/AI (11%) y Ciberseguridad (2%)
df %>%
  count(grupo_rol)%>%
  mutate(
    porcentaje = round(n/sum(n)*100,1)
  )

# seniority — ~72% NA (periodos 2019-2023). Entre disponibles: Senior 53%, Semi 31%, Junior 16%
df %>%
  count(seniority)%>%
  mutate(
    porcentaje = round(n/sum(n)*100,1)
  )

# genero_simple — ~82% Hombres, ~14% Mujeres, ~4% Otro/No binarie
df %>%
  count(genero_simple)%>%
  mutate(
    porcentaje = round(n/sum(n)*100,1)
  )

# modalidad — ~49% NA (pre-2022.2). Entre los disponibles: ~57% remoto (29% del total)
df %>%
  count(modalidad)%>%
  mutate(
    porcentaje = round(n/sum(n)*100,1)
  )

# gente_a_cargo: mediana 0, media 2.2 — ~74% sin equipo, distribucion muy sesgada (max 2500)
df %>%
  count(gente_a_cargo_grupo)%>%
  mutate(
    porcentaje = round(n/sum(n)*100,1)
  )



# VARIABLES NUMERICAS

# sal_bruto: distribucion asimetrica en pesos — no comparable entre periodos por inflacion
summary(df$sal_bruto)

# sal_usd_blue: salario calculado por dolar blue — comparable entre 2019 y 2026
summary(df$sal_usd_blue)

# log_sal_usd: media y mediana mas cercanas — confirma uso de log como variable objetivo
summary(df$log_sal_usd)

# experiencia: mediana 6 anios, maximo 45
summary(df$experiencia)

# edad: rango 16-70, mediana 32
summary(df$edad)

# gente_a_cargo: mediana 0, media 0  — ~74% sin equipo, distribucion muy sesgada
summary(df$gente_a_cargo)



# -----------------------------------------------------------------------------
# 5. DISTRIBUCION DE LA VARIABLE OBJETIVO
# -----------------------------------------------------------------------------


# sal_usd_blue: distribucion asimetrica — se confirma necesidad de transformacion log
ggplot(df, aes(x = sal_usd_blue)) +
  geom_histogram(
    bins = 40,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribución del salario en dólares blue",
    x = "Salario (USD)",
    y = "Frecuencia"
  ) +
  theme_minimal()



# log_sal_usd: distribucion mas simetrica — confirma uso de log como variable objetivo
ggplot(df, aes(x = log_sal_usd)) +
  geom_histogram(
    bins = 40,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribución del logaritmo del salario (USD)",
    x = "Logaritmo del salario (USD)",
    y = "Frecuencia"
  ) +
  theme_minimal()



# Histograma con densidad — grafico principal de esta seccion
# Distribucion unimodal aproximadamente normal — confirma que log_sal_usd
# es una buena variable objetivo para el modelo de regresion lineal
# La cola izquierda corresponde a casos con salarios bajos que sobrevivieron el filtro
# Contraste con log_sal en pesos: ahi se observaba bimodalidad por la inflacion
ggplot(df, aes(x = log_sal_usd)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40, fill = "steelblue", color = "white"
  ) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Distribución de log_sal_usd con curva de densidad",
    x = "Logaritmo del salario (USD)",
    y = "Densidad"
  ) +
  theme_minimal()



# Boxplot log_sal_usd — mediana ~7, IQR entre 6.5 y 7.5
ggplot(df, aes(y = log_sal_usd)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Boxplot del logaritmo del salario (USD)",
    y = "Logaritmo del salario (USD)",
    x = ""
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


# -----------------------------------------------------------------------------
# 6. VARIABLES NUMERICAS
# -----------------------------------------------------------------------------


# EXPERIENCIA vs SALARIO
# Relacion positiva moderada (r = 0.37) pero con alta dispersion vertical:
# la experiencia sola no determina el salario. Mayoria entre 0 y 20 años.
ggplot(df, aes(x = experiencia, y = log_sal_usd)) +
  geom_jitter(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Relación entre experiencia y salario",
    x = "Años de experiencia",
    y = "Logaritmo del salario (USD)",
    caption = "Línea roja: ajuste lineal"
  ) +
  theme_minimal()



# EDAD vs SALARIO
# Tendencia positiva (r = 0.30), similar a experiencia.
# OJO: edad y experiencia estan fuertemente correlacionadas (r = 0.76) -> multicolinealidad.
# Por eso el modelo incluye solo experiencia, no ambas.
ggplot(df, aes(x = edad, y = log_sal_usd)) +
  geom_jitter(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Relación entre edad y salario",
    x = "Edad (años)",
    y = "Logaritmo del salario (USD)",
    caption = "Línea roja: ajuste lineal"
  ) +
  theme_minimal()


# PERSONAS A CARGO vs SALARIO (filtrando outliers de carga > 200)
# 74% de las observaciones en x = 0. La correlacion lineal cruda es debil (r = 0.09)
# porque la variable es muy sesgada; el grupo discretizado captura mejor la señal.
ggplot(df %>% filter(gente_a_cargo <= 200), aes(x = gente_a_cargo, y = log_sal_usd)) +
  geom_jitter(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm") +
  labs(
    title = "Relación entre personas a cargo y salario",
    x = "Cantidad de personas a cargo",
    y = "Logaritmo del salario (USD)",
    caption = "Línea roja: ajuste lineal | Zona gris: margen de incertidumbre"
  ) +
  theme_minimal()

# DISTRIBUCION SALARIAL POR TAMAÑO DE EQUIPO
# Mediana sube progresivamente: Sin equipo 6.89 < pequeño 7.10 < mediano 7.45 < grande 7.70.
# Es la variable numerica con la señal mas limpia del EDA.
df %>%
  filter(!is.na(gente_a_cargo_grupo)) %>%
  ggplot(aes(x = factor(gente_a_cargo_grupo,
                          levels = c("Sin equipo","Equipo pequeño",
                                     "Equipo mediano","Equipo grande")),
               y = log_sal_usd)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Distribución salarial según tamaño del equipo a cargo",
    x = "Tamaño del equipo a cargo",
    y = "Logaritmo del salario (USD)",
    caption = "Sin equipo: 0 | Equipo pequeño: 1–4 | Equipo mediano: 5–10 | Equipo grande: más de 10"
  ) +
  theme_minimal()



# CONCLUSION SECCION 6:
# Las tres variables numericas muestran relacion positiva con el salario
# pero con alta dispersion — ninguna explica el salario de forma aislada.
# Edad y experiencia probablemente correlacionadas — evaluar multicolinealidad.




# -----------------------------------------------------------------------------
# 7. VARIABLES CATEGORICAS
# -----------------------------------------------------------------------------


# SENIORITY
# Variable con separación clara entre los tres niveles
# Medianas crecen progresivamente: Junior < Semi-Senior < Senior
# Senior tiene la caja con mayor dispersión
ggplot(df %>% filter(!is.na(seniority)), 
       aes(x = seniority, y = log_sal_usd, fill = seniority)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según seniority",
    x = "Seniority",
    y = "Logaritmo del salario (USD)",
    caption = "Solo períodos 2024-2026"
  ) +
  theme_minimal()


# MODALIDAD
# Remoto e Híbrido muestran medianas similares y superiores a Presencial
# Presencial queda claramente por debajo — diferencia más marcada que antes
ggplot(df %>% filter(!is.na(modalidad)),
       aes(x = modalidad, y = log_sal_usd, fill = modalidad)) +
  geom_boxplot() +
  scale_x_discrete(limits = c("Remoto", "Hibrido", "Presencial")) +
  labs(
    title = "Distribución salarial según modalidad de trabajo",
    x = "Modalidad de trabajo",
    y = "Logaritmo del salario (USD)"
  ) +
  theme_minimal()


# SUELDOS DOLARIZADOS VS NO
# Dolarizados: mediana y Q3 superiores (7.60 vs 7.49 en log). Mayor dispersion (IQR 1.16 vs 0.77).
# La brecha persiste al deflactar — cobrar en dolares tiene efecto real.
# OJO: el boxplot SUBESTIMA el efecto (el grupo dolarizado incluye 2021-2023 via proxy).
# Comparacion limpia 2024+: dolarizados ganan ~42% mas (mediana 2537 vs 1790 USD).
ggplot(df %>% filter(!is.na(sueldo_dolarizado)),
       aes(x = factor(sueldo_dolarizado, 
                      labels = c("No dolarizado", "Dolarizado")),
           y = log_sal_usd,
           fill = factor(sueldo_dolarizado))) +
  scale_fill_discrete(name = "Tipo de sueldo") +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según dolarización del sueldo",
    x = "Tipo de sueldo",
    y = "Logaritmo del salario (USD)",
    caption = "Solo períodos 2024-2026"
  ) +
  theme_minimal()

# TABLA DE APOYO
df %>%
  filter(!is.na(sueldo_dolarizado)) %>%
  group_by(sueldo_dolarizado) %>%
  summarise(
    mediana_usd = median(sal_usd_blue, na.rm = TRUE),
    n = n()
  )


# RELACION EXPERIENCIA × SALARIO SEGUN DOLARIZACION
# Ambas rectas arrancan a un nivel similar (~7.1 a los 0 años), pero la pendiente
# de los dolarizados es mas empinada (0.049 vs 0.029): la brecha se ABRE con la experiencia.
# Es la evidencia visual de la interaccion experiencia:dolarizado que captura el modelo.
# (Caption "Solo 2024-2026" es inexacto: el filtro incluye 2021+ via proxy.)
ggplot(df %>% filter(!is.na(sueldo_dolarizado)),
       aes(x = experiencia, 
           y = log_sal_usd,
           color = factor(sueldo_dolarizado, 
                          labels = c("No dolarizado", "Dolarizado")))) +
  geom_point(alpha = 0.4, size = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relación entre experiencia y salario según dolarización",
    x = "Años de experiencia",
    y = "Logaritmo del salario (USD)",
    color = "Tipo de sueldo",
    caption = "Solo períodos 2024-2026 | Línea: ajuste lineal | Zona gris: intervalo de confianza"
  ) +
  theme_minimal()




# TAMAÑO DE LA EMPRESA
# Tendencia positiva clara: la mediana sube de ~6.85 (1-10 personas) a ~7.41 (+10000),
# ~65% en USD de punta a punta. Las empresas grandes pagan mas de forma consistente.
# No se incluye en el modelo por el alto % de NA, no por falta de señal.
ggplot(df %>% filter(!is.na(tam_empresa)),
       aes(x = factor(tam_empresa, 
                      levels = c("1", "2-10", "11-50", "51-100", 
                                 "101-200", "201-500", "501-1000", 
                                 "1001-2000", "2001-5000", 
                                 "5001-10000", "+10000")), 
           y = log_sal_usd)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Distribución salarial según tamaño de la organización",
    x = "Cantidad de personas en la organización",
    y = "Logaritmo del salario (USD)"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  theme_minimal()



# CONCLUSION SECCION 7:
# - Seniority: la mejor categorica. Mediana USD: Junior ~1048, Semi ~1719, Senior ~2627.
# - Modalidad: Remoto/Hibrido similares (~7.3); Presencial claramente por debajo (~6.8).
# - Tamaño de empresa: tendencia positiva clara (6.85 -> 7.41). No entra al modelo por NA, no por falta de señal.
# - Dolarizacion: el boxplot subestima (grupo TRUE arrastra 2021-2023). Comparacion limpia
#   2024+: ~42% mas en USD. El scatter muestra que ademas la brecha crece con la experiencia
#   (interaccion). NO es un efecto inflacionario que desaparece.

# Tablas de apoyo
df %>%
  group_by(seniority) %>%
  summarise(
    mediana_usd = median(sal_usd_blue, na.rm = TRUE),
    cantidad = n()
  )

df %>%
  group_by(modalidad) %>%
  summarise(
    mediana_usd = median(sal_usd_blue, na.rm = TRUE),
    cantidad = n()
  )

df %>%
  group_by(sueldo_dolarizado) %>%
  summarise(
    mediana_usd = median(sal_usd_blue, na.rm = TRUE),
    cantidad = n()
  )

# -----------------------------------------------------------------------------
# 8. VARIABLES POTENCIALMENTE CONFUNDIDORAS
# -----------------------------------------------------------------------------



# GENERO
# Sin controlar por seniority — hombres muestran mediana visiblemente superior
# Podría sugerir una brecha salarial de género, pero es necesario controlar
# por otras variables antes de concluir
ggplot(df,
       aes(x = genero_simple, y = log_sal_usd, fill = genero_simple)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según género",
    x = "Género",
    y = "Logaritmo del salario (USD)"
  ) +
  theme_minimal()


# Al controlar por seniority la brecha se reduce pero NO desaparece:
# persiste en ~11-12% dentro de Semi-Senior y Senior (Junior ~5%).
# Seniority explica la parte de composicion, pero queda una brecha real dentro de cada nivel.
ggplot(df %>% filter(!is.na(seniority), 
                     genero_simple %in% c("Hombre", "Mujer")),

       aes(x = genero_simple, y = log_sal_usd, fill = seniority)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según género y seniority",
    x = "Género",
    y = "Logaritmo del salario (USD)",
    fill = "Seniority",
    caption = "Solo períodos 2024-2026"
  ) +
  theme_minimal()



# Hombres: 56.6% Senior vs Mujeres: 39.3% Senior — los hombres estan mas concentrados en Senior.
# Esto explica la parte de COMPOSICION de la brecha cruda (+21%), pero no toda:
# dentro de cada nivel persiste una brecha de ~11-12% (ver grafico anterior).
ggplot(df %>% filter(!is.na(seniority),
                     genero_simple %in% c("Hombre", "Mujer")),
       
       aes(x = genero_simple, fill = seniority)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proporción de seniority según género",
    x = "Género",
    y = "Proporción",
    fill = "Seniority",
    caption = "Solo períodos 2024-2026"
  ) +
  theme_minimal()


# Tabla de Apoyo - Mediana de salario por grupo_rol
df %>%
  group_by(grupo_rol) %>%
  summarise(
    mediana_usd = median(sal_usd_blue, na.rm = TRUE),
    cantidad = n()
  ) %>%
  arrange(desc(mediana_usd))
  



# CONCLUSION SECCION 8:
# El genero muestra brecha a favor de hombres INCLUSO controlando por seniority,
# especialmente en Semi-Senior y Senior (~11-12% dentro de cada nivel).
# La brecha cruda (+21%) combina dos efectos: composicion (hombres 56.6% Senior
# vs mujeres 39.3%) + brecha dentro de nivel. El modelo aisla el segundo: ~10% neto.
# Por eso el genero se incluye en el modelo: tiene efecto propio mas alla del seniority.


# -----------------------------------------------------------------------------
# 9. REGIONES Y ROLES
# -----------------------------------------------------------------------------

# SALARIO SEGUN REGION
# CABA y GBA/Prov.BA con medianas parecidas (GBA -5%), pero Interior claramente por debajo (-13%).
# El mercado es homogeneo entre CABA y GBA; Interior es la region que se separa.
# Observaciones: CABA = 32.492, Interior = 15.998, GBA = 11.662
ggplot(df %>% filter(!is.na(region)),
       aes(x = region, y = log_sal_usd, fill = region)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según región",
    x = "Región",
    y = "Logaritmo del salario (USD)"
  ) +
  theme_minimal()


# Tabla de apoyo
df %>% count(region)



# SALARIO SEGUN GRUPO DE ROL
# Roles de gestión lidera con mediana claramente superior al resto
# Ciberseguridad segunda, seguida de cerca por Infraestructura y Datos / AI
# Desarrollo / QA tiene la mediana más baja de todos los grupos
# Las diferencias entre los grupos del medio son pequeñas
ggplot(df,
       aes(x = reorder(grupo_rol, log_sal_usd, median),
           y = log_sal_usd,
           fill = grupo_rol)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según grupo de rol",
    x = "Grupo de rol",
    y = "Logaritmo del salario (USD)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

# TOP 10 PUESTOS
# Top 10 roles por cantidad de observaciones en el dataset
top_roles <- df %>%
  count(trabajo_de, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(trabajo_de)

# Boxplot de salario por rol — ordenado por mediana, coloreado por grupo
# Los grupos de roles mejor pagos son de gestion e infraestructura: (Manager/Director y Architect)
# Los roles de Datos/AI (Business Analyst, BI Analyst, Data analyst) se ubican en la mitad baja
# QA/Tester tiene la mediana mas baja — Desarrollo/QA concentra los roles peor pagos
# Developer tiene alta dispersion hacia la derecha — outliers de salarios muy altos
ggplot(df %>% filter(trabajo_de %in% top_roles),
       aes(x = reorder(trabajo_de, log_sal_usd, median),
           y = log_sal_usd,
           fill = grupo_rol)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Distribución salarial por rol - Top 10",
    x = "Rol",
    y = "Logaritmo del salario (USD)",
    fill = "Grupo de rol"
  ) +
  theme_minimal()


# Roles de gestion: intercepto mas alto (7.32) pero pendiente mas plana (0.015) — arranca arriba.
# Desarrollo/QA: pendiente mas pronunciada (0.052) — la experiencia premia mas.
# Entre los tecnicos, Infraestructura es el de pendiente mas baja (0.025).
# Ciberseguridad y Datos/AI: pendientes similares (0.031 / 0.034).
df %>%
  filter(!is.na(experiencia), !is.na(log_sal_usd)) %>%
  ggplot(aes(x = experiencia, y = log_sal_usd, color = grupo_rol)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Salario vs Experiencia por grupo de rol",
    x     = "Años de experiencia",
    y     = "Logaritmo del salario (USD)",
    color = "Grupo de rol"
  ) +
  theme_minimal()


# (excluyendo Roles de gestion, para ver solo los grupos tecnicos)
# Ciberseguridad y Datos/AI: pendientes e interceptos similares, arrancan parecido.
# Infraestructura: la pendiente mas baja (0.025) — crece mas lento.
# Desarrollo/QA: el intercepto MAS BAJO (6.58) pero la pendiente MAS ALTA (0.052) —
# arranca peor pagado pero la experiencia lo premia mas que a cualquier otro grupo.
# Un senior de Desarrollo/QA termina igualando o superando a los demas.
df %>%
  filter(!is.na(experiencia), !is.na(log_sal_usd),
         grupo_rol != "Roles de gestión") %>%
  ggplot(aes(x = experiencia, y = log_sal_usd, color = grupo_rol)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Salario vs Experiencia por grupo de rol",
    x     = "Años de experiencia",
    y     = "Logaritmo del salario (USD)",
    color = "Grupo de rol"
  ) +
  theme_minimal()



# CONCLUSION SECCION 9:
# Region: CABA y GBA parecidas, Interior ~13% por debajo (no es homogeneo del todo).
# Grupo de rol: Roles de gestion lidera; Desarrollo/QA en el extremo inferior.
# A nivel individual: Manager/Director y Architect los mejor pagos; QA/Tester el peor.
# La experiencia premia mas en Desarrollo/QA (pendiente 0.052); Infraestructura es el
# tecnico que crece mas lento. Region y grupo de rol entran al modelo como controles.


# -----------------------------------------------------------------------------
# 9B. USO DE IA — ADOPCION Y PERFIL
# -----------------------------------------------------------------------------


roles_amenazados <- c("BI Analyst / Data Analyst", "Business Analyst")
roles_nativos    <- c("Data Scientist", "AI Engineer", "AI / Prompt / Chatbots")
roles_tecnico    <- c("Data Engineer", "SysAdmin / DevOps / SRE", "DBA","DBA (Database Administrator)")

df_ia_rol <- df %>%
  filter(
    !is.na(uso_ia),
    trabajo_de %in% c(roles_amenazados, roles_nativos, roles_tecnico)
  ) %>%
  mutate(
    categoria_rol = case_when(
      trabajo_de %in% roles_amenazados ~ "Amenazados por IA",
      trabajo_de %in% roles_nativos    ~ "Nativos de IA",
      trabajo_de %in% roles_tecnico    ~ "Técnicos / Infraestructura"
    ),
    categoria_rol = factor(
      categoria_rol,
      levels = c("Nativos de IA", "Amenazados por IA", "Técnicos / Infraestructura")
    )
  )


df_ia_rol %>%
  group_by(anio, categoria_rol) %>%
  summarise(uso_ia_medio = mean(uso_ia, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = anio, y = uso_ia_medio, color = categoria_rol, group = categoria_rol)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "Nativos de IA"              = "steelblue",
    "Amenazados por IA"          = "tomato",
    "Técnicos / Infraestructura" = "gray50"
  )) +
  labs(
    title = "¿Los roles amenazados adoptaron IA más rápido?",
    subtitle = "Evolución del uso promedio de IA por categoría de rol",
    x = "Año",
    y = "Uso promedio de IA (escala)",
    color = NULL
  ) +
  theme_minimal()


df_ia_rol %>%
  group_by(trabajo_de) %>%
  mutate(mediana_uso = median(uso_ia, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(
    x    = reorder(trabajo_de, mediana_uso),
    y    = uso_ia,
    fill = categoria_rol
  )) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Nativos de IA"              = "steelblue",
    "Amenazados por IA"          = "tomato",
    "Técnicos / Infraestructura" = "gray70"
  )) +
  labs(
    title = "Uso de IA por rol individual",
    subtitle = "Ordenado por mediana — color según exposición a la automatización",
    x = NULL,
    y = "Uso de IA (escala)",
    fill = NULL
  ) +
  theme_minimal()

# ADOPCION DE IA POR GRUPO DE ROL (2024-2026)
# Todos los grupos parten de 25-37% de uso alto en 2024.1 y convergen hacia 60-70% en 2026.1
# Datos / AI lidera desde el inicio — Desarrollo / QA arranca bajo pero termina liderando
# Ciberseguridad es el grupo que crece más lento y se queda rezagado al final
# La convergencia sugiere que la IA se está volviendo transversal independientemente del rol
df %>%
  filter(!is.na(uso_ia), !is.na(grupo_rol)) %>%
  group_by(grupo_rol, periodo) %>%
  summarise(
    pct_alto_uso = mean(uso_ia >= 4, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(periodo = factor(periodo)) %>%  
  ggplot(aes(x = periodo, y = pct_alto_uso, color = grupo_rol, group = grupo_rol)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  labs(
    title = "¿Qué grupos adoptaron IA más rápido?",
    subtitle = "% de encuestados con uso alto de IA (≥ 4) por período",
    x = "Período",
    y = "% con uso alto de IA",
    color = "Grupo de rol"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
# HALLAZGO CONTRAINTUITIVO: los juniors y Semi-Seniopr usan mas IA
# Junior y Semi-Senior tienen mediana 4 — Senior tiene mediana 3
# Los juniors entraron al mercado laboral con IA ya disponible
# Esto sugiere que la IA está siendo incorporada de abajo hacia arriba en las organizaciones  
df %>%
    filter(!is.na(uso_ia), !is.na(seniority)) %>%
    ggplot(aes(x = seniority, y = uso_ia, fill = seniority)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.15, fill = "white", outlier.alpha = 0.2) +
    labs(
      title = "¿Los seniors usan más IA?",
      subtitle = "Distribución del uso de IA por nivel de seniority — 2024 a 2026",
      x = "Seniority",
      y = "Uso de IA (escala)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  

# ¿Los que mas usan IA ganan mas?
# Relacion positiva LEVE y monotona: Bajo(0-2) 7.50 < Medio(3) 7.54 < Alto(4-5) 7.67.
# El uso alto se asocia a ~18% mas de salario, pero la diferencia es chica.
# INTERPRETACION: probablemente mediado por rol y seniority (los roles mejor pagos
# y con mas experiencia adoptan mas IA), no una relacion causal directa uso->salario.
  df %>%
    filter(!is.na(uso_ia)) %>%
    mutate(uso_ia_grupo = case_when(
      uso_ia <= 2 ~ "Bajo (1-2)",
      uso_ia == 3 ~ "Medio (3)",
      uso_ia >= 4 ~ "Alto (4-5)"
    ),
    uso_ia_grupo = factor(uso_ia_grupo, 
                          levels = c("Bajo (1-2)", "Medio (3)", "Alto (4-5)"))) %>%
    ggplot(aes(x = uso_ia_grupo, y = log_sal_usd, fill = uso_ia_grupo)) +
    geom_boxplot() +
    labs(
      title = "¿Los que más usan IA ganan más?",
      subtitle = "Distribución salarial según nivel de uso de IA — 2024 a 2026",
      x = "Nivel de uso de IA",
      y = "Logaritmo del salario (USD)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  

  # -----------------------------------------------------------------------------
  # 10. CONCLUSIONES DEL EDA
  # -----------------------------------------------------------------------------
  
  # VARIABLES CON MAYOR PODER EXPLICATIVO:
  #
  # - Seniority: mayor señal del EDA — separacion clara y progresiva entre los tres niveles.
  #   Junior mediana ~7.0, Semi-Senior ~7.5, Senior ~7.8 (log_sal_usd).
  #   Confirmado por el modelo: ser Senior implica ~64% mas que Junior (modp3).
  #
  # - Gente a cargo: tendencia progresiva y consistente en su forma discretizada —
  #   la mediana sube de ~6.9 (sin equipo) a ~7.7 (equipo grande). En su forma continua
  #   la correlacion es debil (r = 0.09) porque el 74% no tiene equipo a cargo.
  #
  # - Grupo de rol: Roles de gestion lidera con mediana claramente superior (1893 USD).
  #   Los cuatro grupos tecnicos quedan comprimidos entre si (Ciberseguridad 1194,
  #   Infraestructura 1100, Datos/AI 1056, Desarrollo/QA 937). A nivel de rol individual,
  #   Manager/Director y Architect son los mejor pagos; QA/Tester el peor del top 10.
  #
  # - Experiencia: relacion positiva moderada (r = 0.37) pero con alta dispersion —
  #   no determina el salario de forma aislada. Hallazgo destacado: la pendiente es mas
  #   pronunciada en Desarrollo/QA (0.052) — ese grupo arranca con el intercepto mas bajo
  #   pero es el que mas se beneficia de cada año adicional de experiencia (ver Rplot20).
  #   OJO: edad y experiencia estan fuertemente correlacionadas (r = 0.76) — multicolinealidad,
  #   por eso el modelo incluye solo experiencia.
  #
  # - Genero: brecha a favor de hombres sin controlar (+21% — Rplot14).
  #   Al controlar por seniority la brecha se reduce pero NO desaparece:
  #   persiste en ~11-12% dentro de Semi-Senior y Senior (Junior ~5%).
  #   La brecha cruda combina composicion (hombres 56.6% Senior vs mujeres 39.3%)
  #   + brecha dentro de nivel. Confirmado por el modelo: ~10% menos (coef. -0.103, ***).
  #
  # - Dolarizacion: su efecto esta tanto en el NIVEL como en la PENDIENTE.
  #   El boxplot exploratorio SUBESTIMA el nivel porque el grupo dolarizado arrastra
  #   2021-2023 (proxy) mientras que el no-dolarizado es solo 2024+. En la comparacion
  #   limpia 2024+, los dolarizados ganan ~42% mas en USD (mediana 2537 vs 1790).
  #   El efecto principal aparece en la interaccion con experiencia: la pendiente salarial
  #   de los dolarizados es mas pronunciada (la brecha crece con los años — Rplot12).
  #   Confirmado en el modelo: la interaccion experiencia:sueldo_dolarizado es altamente
  #   significativa (***) y sube el R² de 0.2881 a 0.3373.
  #
  #
  #
  #
  # VARIABLES CON MENOR PODER EXPLICATIVO:
  #
  # - Modalidad: Remoto e Hibrido muestran medianas similares (~7.3 en log_sal_usd).
  #   Presencial queda claramente por debajo (~6.8) — diferencia equivalente a ~50% en USD.
  #   La distincion relevante es Presencial vs el resto, no entre Remoto e Hibrido.
  #
  # - Region: CABA y GBA/Prov. BA muestran medianas parecidas (GBA -5%).
  #   Interior es la que queda por debajo de forma consistente (-13% respecto a CABA).
  #   El modelo confirma (modp3): GBA = -0.082, Interior = -0.136.
  #   El mercado IT es homogeneo entre CABA y GBA; Interior muestra una diferencia real.
  #
  # - Tamaño de empresa: tendencia positiva CLARA y monotona — la mediana sube de
  #   ~6.85 (1-10 personas) a ~7.41 (+10000), ~65% en USD de punta a punta (Rplot13).
  #   No se incluye en el modelo por el alto % de NA, NO por falta de señal.
  #
  # HALLAZGOS SOBRE USO DE IA (2024-2026):
  # - La adopcion crecio en todos los grupos: de ~25-37% a ~57-70% de uso alto en dos años.
  # - Ciberseguridad es el grupo que adopta mas lento y queda rezagado al final (57%).
  # - Datos/AI lidera desde el inicio (37%); Desarrollo/QA arranca a la par y termina liderando (70%).
  # - Contraintuitivo: Junior y Semi-Senior usan IA mas intensamente que Senior (mediana 4 vs 3).
  #   Interpretacion: los juniors ingresaron al mercado con IA ya disponible —
  #   la adopcion se da de abajo hacia arriba en las organizaciones.
  # - El uso de IA tiene una relacion positiva LEVE con el salario (~18%, Bajo 7.50 < Medio 7.54
  #   < Alto 7.67), probablemente mediada por rol y seniority — no una relacion causal directa.
  #
  # DECISION DE MODELADO:
  # - Variable objetivo: log_sal_usd — salario deflactado por dolar blue (2019-2026)
  # - Los tres modelos trabajan con datos desde 2024, unico periodo con seniority
  #   disponible (n = 17.079 tras drop_na)
  #
  # - Modelo 1: log_sal_usd ~ experiencia + grupo_rol + seniority + genero_simple + region
  #   R² = 0.2881 | RSE = 0.552
  #
  # - Modelo 2: log_sal_usd ~ experiencia * sueldo_dolarizado + grupo_rol + seniority
  #             + genero_simple + region
  #   R² = 0.3373 | RSE = 0.533
  #   La interaccion experiencia:sueldo_dolarizado es altamente significativa (***)
  #
  # - Modelo 3: log_sal_usd ~ poly(experiencia, 2) * sueldo_dolarizado + grupo_rol + seniority
  #             + genero_simple + region
  #   R² = 0.3454 | RSE = 0.530
  #   Permite capturar la curvatura de la experiencia (rendimientos decrecientes)
  #
  # - Los tres modelos son comparados con ANOVA — cada uno agrega explicacion significativa (***)
  #
  # - Seniority es el predictor con mayor impacto individual y Roles de gestion
  #   el grupo con mayor intercepto relativo (confirmado en el modelo).