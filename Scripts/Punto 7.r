# ------------------------------------------------------
# Punto 7
# ------------------------------------------------------

# Cargar librerías
library(readxl)
library(tidyverse)
library(gt)

# Definir ruta del archivo
ruta <- "C:/Users/LENOVO/OneDrive/Documentos/GitHub/Caso-2-ANALITICA/Datos/Hollywood.xls"

# Importar hoja 
Datos <- read_excel(ruta, sheet = "Exhibit 1")

# Renombrar columnas para evitar espacios
names(Datos) <- make.names(names(Datos))

# ------------------------------------------------------
# 7a) Regresión simple: Total U.S. Gross ~ Opening Gross
# ------------------------------------------------------
model7a <- lm(Total.U.S..Gross ~ Opening.Gross, data = Datos)
summary(model7a)

# Crear tabla de resultados regresión simple
resultados7a <- summary(model7a)$coefficients %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  rename(
    Estimate = Estimate,
    StdError = `Std. Error`,
    t_value  = `t value`,
    p_value  = `Pr(>|t|)`
  )

tabla7a <- resultados7a %>%
  gt() %>%
  tab_header(
    title = "Tabla 7.1: Regresión simple",
    subtitle = "Total U.S. Gross ~ Opening Gross"
  ) %>%
  fmt_number(
    columns = c(Estimate, StdError, t_value, p_value),
    decimals = 3
  )

# Mostrar tabla
tabla7a


# ------------------------------------------------------
# 7b-7c) Test de hipótesis H0: beta = 4 (Regla del 25%)
# ------------------------------------------------------
b_hat <- coef(model7a)["Opening.Gross"]
se_b <- summary(model7a)$coefficients["Opening.Gross","Std. Error"]
df_resid <- df.residual(model7a)

t_stat <- (b_hat - 4) / se_b
p_value <- 2 * pt(-abs(t_stat), df = df_resid)

# Crear tabla con resultados del test
tabla_test <- data.frame(
  Beta_Estimado = b_hat,
  Error_Estandar = se_b,
  Estadistico_t = t_stat,
  p_valor = p_value
)

tabla7b <- tabla_test %>%
  gt() %>%
  tab_header(
    title = "Tabla 7.2: Prueba de hipótesis",
    subtitle = "H0: β = 4 (Regla del 25%)"
  ) %>%
  fmt_number(
    columns = everything(),
    decimals = 3
  )

# Mostrar tabla
tabla7b


# ------------------------------------------------------
# 7e) Modelo ampliado (regresión múltiple más sólida)
# ------------------------------------------------------
model7e_lin <- lm(Total.U.S..Gross ~ Opening.Gross + Budget + Sequel +
                    Opening.Theatres + Critics..Opinion + Known.Story,
                  data = Datos)
summary(model7e_lin)

# Crear tabla de resultados modelo ampliado
resultados7e <- summary(model7e_lin)$coefficients %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  rename(
    Estimate = Estimate,
    StdError = `Std. Error`,
    t_value  = `t value`,
    p_value  = `Pr(>|t|)`
  )

tabla7e <- resultados7e %>%
  gt() %>%
  tab_header(
    title = "Tabla 7.3: Regresión múltiple",
    subtitle = "Modelo ampliado con controles"
  ) %>%
  fmt_number(
    columns = c(Estimate, StdError, t_value, p_value),
    decimals = 3
  )

# Mostrar tabla
tabla7e


# ------------------------------------------------------
# 7g) R²: proporción explicada
# ------------------------------------------------------

# R² simple
r2_simple <- summary(model7a)$r.squared

# R² con controles
model_controls <- lm(Total.U.S..Gross ~ Budget + Sequel + Opening.Theatres +
                       Critics..Opinion + Known.Story, data = Datos)
r2_controls <- summary(model_controls)$r.squared

# R² total del modelo ampliado
r2_full <- summary(model7e_lin)$r.squared

# Contribución incremental de Opening.Gross
r2_incremental <- r2_full - r2_controls

# Crear tabla con R²
tabla_r2 <- data.frame(
  R2_Simple = r2_simple,
  R2_Controles = r2_controls,
  R2_Full = r2_full,
  R2_Incremental = r2_incremental
)

tabla7g <- tabla_r2 %>%
  gt() %>%
  tab_header(
    title = "Tabla 7.4: Proporción explicada",
    subtitle = "Comparación de R² entre modelos"
  ) %>%
  fmt_number(
    columns = everything(),
    decimals = 3
  )

# Mostrar tabla
tabla7g
