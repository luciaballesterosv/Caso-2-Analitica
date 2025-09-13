# ------------------------------------------------------
# Punto 9
# ------------------------------------------------------

# Cargar librerías
library(readxl)
library(tidyverse)
library(gt)

# Definir ruta del archivo
ruta <- "C:/Users/LENOVO/OneDrive/Documentos/GitHub/Caso-2-ANALITICA/Datos/Hollywood.xls"

# Importar hoja
Datos <- read_excel(ruta, sheet = "Exhibit 1")

# Renombrar columnas
names(Datos) <- make.names(names(Datos))

# Crear variable dummy para comedia
Datos$Comedy <- ifelse(Datos$Genre == "Comedy", 1, 0)

# Modelo con interacción
model9 <- lm(Total.U.S..Gross ~ Budget + Sequel + Known.Story + MPAA_D +
               Opening.Gross + Opening.Theatres + Summer + Holiday + Christmas +
               Critics..Opinion * Comedy,
             data = Datos)

summary(model9)

# Extraer coeficiente de la interacción
coef_interaccion <- summary(model9)$coefficients["Critics..Opinion:Comedy",]

coef_interaccion

resultados9 <- summary(model9)$coefficients %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  rename(
    Estimate = Estimate,
    StdError = `Std. Error`,
    t_value  = `t value`,
    p_value  = `Pr(>|t|)`
  )

tabla9 <- resultados9 %>%
  gt() %>%
  tab_header(
    title = "Regresión con interacción",
    subtitle = "Efecto de la opinión de los críticos en comedias vs. otros géneros"
  ) %>%
  fmt_number(
    columns = c(Estimate, StdError, t_value, p_value),
    decimals = 3
  )

# Mostrar tabla
tabla9

