# ------------------------------------------------------
# Punto 6
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

#---------PUNTO A Y B--------------------------------------------------------

# Modelo inicial (preproducción + estreno)
model1 <- lm(Opening.Gross ~ Budget + Known.Story + Sequel + MPAA_D +
               Opening.Theatres + Summer + Holiday + Christmas,
             data = Datos)

summary(model1)

# Eliminar variables no significativas (p > 0.10)
model_final <- step(model1, direction = "backward")
summary(model_final)

#---------INTERPRETACIÓN COEFICIENTES----------------------------------------

coef(model_final)

#---------PUNTO C------------------------------------------------------------

# Efecto de +100 cines
effect_100 <- coef(model_final)["Opening.Theatres"] * 100
effect_100

# Intervalo de confianza del 95%
ci_100 <- confint(model_final, "Opening.Theatres", level = 0.95) * 100

# Crear data frame con resultados
tabla_effect <- data.frame(
  Estimacion_Puntual = effect_100,
  IC_95_Lower = ci_100[1],
  IC_95_Upper = ci_100[2]
)

# Convertir en tabla con gt
tabla_effect_gt <- tabla_effect %>%
  gt() %>%
  tab_header(
    title = "Efecto de incrementar 100 salas",
    subtitle = "Estimación puntual e intervalo de confianza al 95%"
  ) %>%
  fmt_number(
    columns = everything(),
    decimals = 0,
    sep_mark = ","
  )

# Mostrar tabla
tabla_effect_gt

#---------TABLA DE RESULTADOS REGRESIÓN--------------------------------------

resultados <- summary(model_final)$coefficients

tabla <- resultados %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  rename(
    Estimate = Estimate,
    StdError = `Std. Error`,
    t_value  = `t value`,
    p_value  = `Pr(>|t|)`
  ) %>%
  gt() %>%
  tab_header(
    title = "Resultados de la regresión final",
    subtitle = "Predicción del Opening Gross"
  ) %>%
  fmt_number(
    columns = c(Estimate, StdError, t_value, p_value),
    decimals = 3
  )

# Mostrar tabla
tabla
