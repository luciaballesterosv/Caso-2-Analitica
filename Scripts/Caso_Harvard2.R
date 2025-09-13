setwd("/Users/Lucia/Documents/Analítica de datos")
getwd()

install.packages("tidyverse")
library(tidyverse)  

install.packages("dplyr")
library(dplyr)

install.packages("readxl")
library(readxl)

caso <- read_excel("Hollywood.xls", sheet = "Exhibit 1")
glimpse(caso)
head(caso)  

colnames(caso)

# 1. Para obtener una visión inicial de los datos, calcula los valores mínimo, promedio y máximo de las variables: 
# ingresos de estreno, ingresos totales en EE. UU., ingresos totales fuera de EE. UU. y número de salas de estreno. 
# ¿Cuántas de las películas en el conjunto de datos son comedias y cuántas son clasificadas como R?

caso %>%
  summarise(min_estreno = min(`Opening Gross`),
            prom_estreno = mean(`Opening Gross`),
            max_estreno = max(`Opening Gross`)
  )

caso %>%
        summarise(
            min_eeuu = min(`Total U.S. Gross`),
            prom_eeuu = mean(`Total U.S. Gross`),
            max_eeuu = max(`Total U.S. Gross`),
        )

caso %>%
        summarise(
            min_fuera_eeuu = min(`Total Non-U.S. Gross`),
            prom_fuera_eeuu = mean(`Total Non-U.S. Gross`),
            max_fuera_eeuu = max(`Total Non-U.S. Gross`),
        )

caso %>%
        summarise(
            min_salas = min(`Opening Theatres`),
            prom_salas = mean(`Opening Theatres`),
            max_salas = max(`Opening Theatres`)
        )



caso %>%
  filter(Genre == "Comedy") %>%
  summarise(num_comedias = n()) 

caso %>%
  filter(MPAA == "R") %>%
  summarise(num_R = n())

# 2. Michael London (of Sideways fame) declared in The Hollywood Reporter, “The studio business 
# historically returns around 12 percent a year.” Griffith knew any investor would want justification for such a statement.

# a. Calculate the U.S. return on investment (ROI) (simply defined as the difference of total 
# U.S. box-office gross and budget divided by budget, ignoring any form of discounting) for each movie in the data set.

caso <- caso %>%
  mutate(ROI_US = (`Total U.S. Gross` - Budget) / Budget) 

caso$ROI_US  

# b. Provide a 95 percent confidence interval for the mean U.S. ROI of movies.

t.test(caso$ROI_US, conf.level = 0.95)

# c. Show that the mean U.S. ROI is significantly larger than the 12 percent London cited.

caso %>%
  summarise(prom_ROI_US = mean(ROI_US))

t.test(caso$ROI_US, mu = 0.12, alternative = "greater", conf.level = 0.95)

# 3. While any genre can produce a blockbuster, Griffith suspected that some categories are more likely to do so than others. 
# If he could stack the deck in his favor through storyline selection, he did not want to pass up the opportunity.

#. a. Compare the total U.S. box-office gross of movies from the comedy genre with movies from other genres. 
# Is there a statistically significant difference between the total U.S. gross of comedies and non-comedy movies?\

caso %>%
  group_by(Genre) %>%
  summarise(prom_eeuu = sum(`Total U.S. Gross`)) %>%
  arrange(desc(prom_eeuu))

caso %>%
  filter(Genre == "Comedy") %>%
  summarise(prom_eeuu = sum(`Total U.S. Gross`))

t.test(`Total U.S. Gross` ~ (Genre == "Comedy"), data = caso, var.equal = TRUE)

# b. Griffith was not so sure about the results, because they were contrary to his gut feelings. 
# Maybe higher revenue accompanied higher investments? Calculate additionally the difference of U.S. 
# ROIs from movies of the comedy genre and of other movie genres. Is there a statistically significant 
# difference between the U.S. ROIs?

t.test(ROI_US ~ (Genre == "Comedy"), data = caso, var.equal = TRUE)

# 4. La sabiduría popular sostenía que las películas clasificadas como R tenían un mejor desempeño que las demás.
# a. ¿Existe una diferencia estadísticamente significativa entre la recaudación total en EE. UU. de las películas 
# clasificadas como R y las películas con otras clasificaciones?

caso %>%
  group_by(MPAA) %>%
  summarise(prom_eeuu = sum(`Total U.S. Gross`)) %>%
  arrange(desc(prom_eeuu))

t.test(`Total U.S. Gross` ~ (MPAA == "R"), data = caso, var.equal = TRUE)

# 5. Believed to be among the preproduction factors driving success were budget (which expresses both the cost of 
# the film and the quality of the actors as expressed by their fee), genre (comedy vs. non-comedy), MPAA rating 
# (R-rated vs. other rating), and audiences’ familiarity with the story (whether the film is a sequel or an adaptation 
# of a known story).

# a. Based on the described beliefs, determine a sound regression model predicting total U.S. box-office gross of movies 
# prior to production.

modelo1 <- lm(`Total U.S. Gross` ~ Budget + Genre + MPAA + `Known Story`, data = caso)
summary(modelo1)  

print(modelo1)

# b. Drop all variables from the regression that are not significant at a 10 percent level of significance. 
# Report the final regression.

modelo2 <- lm(`Total U.S. Gross` ~ Budget + `Known Story`, data = caso)
summary(modelo2)

print(modelo2)
