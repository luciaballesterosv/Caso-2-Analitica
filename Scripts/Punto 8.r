# ------------------------------------------------------
# Punto 8
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

# --------- PUNTO A -----------------------------------------------------
# Modelo inicial con factores pre, estreno y crítica
model8a <- lm(Total.U.S..Gross ~ Budget + Sequel + Known.Story + MPAA_D +
                Opening.Gross + Opening.Theatres + Summer + Holiday + Christmas +
                Critics..Opinion,
              data = Datos)

summary(model8a)

# Tabla de coeficientes (modelo inicial)
resultados8a <- summary(model8a)$coefficients %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  rename(
    Estimate = Estimate,
    StdError = `Std. Error`,
    t_value  = `t value`,
    p_value  = `Pr(>|t|)`
  )

tabla8a <- resultados8a %>%
  gt() %>%
  tab_header(
    title = "Regresión inicial",
    subtitle = "Modelo con todas las variables consideradas"
  ) %>%
  fmt_number(columns = c(Estimate, StdError, t_value, p_value),
             decimals = 3)

tabla8a

# --------- PUNTO B -----------------------------------------------------
# Backward stepwise
model8b <- step(model8a, direction = "backward")
summary(model8b)

# Tabla de coeficientes (modelo final)
resultados8b <- summary(model8b)$coefficients %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  rename(
    Estimate = Estimate,
    StdError = `Std. Error`,
    t_value  = `t value`,
    p_value  = `Pr(>|t|)`
  )

tabla8b <- resultados8b %>%
  gt() %>%
  tab_header(
    title = "Regresión final",
    subtitle = "Predicción de la taquilla total en EE. UU."
  ) %>%
  fmt_number(columns = c(Estimate, StdError, t_value, p_value),
             decimals = 3)

tabla8b

# --------- PUNTO C -----------------------------------------------------
# Datos de "Flags of Our Fathers"
banderas <- Datos %>% filter(Movie == "Flags of Our Fathers")

# Estimación puntual e intervalo de predicción al 95%
prediccion_banderas <- predict(model8b, newdata = banderas,
                               interval = "prediction", level = 0.95)

prediccion_banderas

# --------- PUNTO D -----------------------------------------------------
# Efecto de aumentar 10 puntos en Critics..Opinion
crit_effect <- coef(model8b)["Critics..Opinion"] * 10
crit_effect
