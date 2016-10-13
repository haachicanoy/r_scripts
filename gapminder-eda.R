# Functional programming using Gapminder data
# H. Achicanoy taken from: https://youtu.be/rz3_FDVt9eg
# CIAT, 2016

library(gapminder)
library(dplyr)
library(purrr)
library(tidyr)
library(broom)
library(ggplot2)

gapminder

gapminder <- gapminder %>% mutate(year1950 = year - 1950)

# Nested data ----------------------------------------------------------------------------------

# Save in a nested way different data frames; this is like a data frame of data frames
by_country <- gapminder %>%
  group_by(continent, country) %>%
  nest()

# Symbol "%>%" is called pipe
# same as: nest(group_by(gapminder, continent, country))

by_country
str(by_country)
by_country$data[[1]]

# Fit models -----------------------------------------------------------------------------------

# create a function to estimate a linear regression model using variables: lifeExp and year1950 from each data frame nested
country_model <- function(df){
  lm(lifeExp ~ year1950, data=df)
}

# run the linear model to each data frame nested and saving continent, country and linear model
models <- by_country %>%
  mutate(
    model = data %>% map(country_model)
  )

models
models %>% filter(continent == "Africa")

# Broom ---------------------------------------------------------------------------------------

models <- models %>%
  mutate(
    glance  = model %>% map(broom::glance),
    rsq     = glance %>% map_dbl("r.squared"),
    tidy    = model %>% map(broom::tidy),
    augment = model %>% map(broom::augment)
  )
models

models %>% arrange(desc(rsq))
models %>% filter(continent == "Africa")

models %>%
  ggplot(aes(rsq, reorder(country, rsq))) +
  geom_point(aes(colour = continent))

# Unnest ---------------------------------------------------------------------------------------

# nested to original data frame format
unnest(models, data) # back to where we started
unnest(models, glance, .drop = TRUE) %>% View() # statistical resume of each model per country (model summary)
unnest(models, tidy) # intercept and slope coefficients per each country

models %>%
  unnest(tidy) %>%
  select(continent, country, term, estimate, rsq) %>%
  spread(term, estimate) %>%
  ggplot(aes(`(Intercept)`, year1950)) +
  geom_point(aes(colour = continent, size = rsq)) +
  geom_smooth(se = FALSE) +
  xlab("Life Expectancy (1950)") +
  ylab("Yearly improvement") +
  scale_size_area()

# returns the original data frame plus fitted values, residuals per row and country
unnest(models, augment)

models %>%
  unnest(augment) %>%
  ggplot(aes(year1950, .resid)) +
  geom_line(aes(group = country), alpha = 1/3) +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 0, colour = "white") +
  facet_wrap(~continent)
