rm(list = ls())
setwd("/home/creambbq/facu/desarrollo_económico/tp1/practice")
library(tidyverse)
library(ggthemes)
library(mFilter)

col <- read.table("clear_col.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
eu <- read.table("clear_eu.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
alpha <- 0.3

#------------------------------------------------------------------------------#

col <- col %>% mutate(A_hat = (Y)/((K^(alpha))*(L^(1-alpha))))
col <- col %>% mutate(y = Y/N)
eu <- eu %>% mutate(A_hat = (Y)/((K^(alpha))*(L^(1-alpha))))
eu <- eu %>% mutate(y = Y/N)
col <- col %>% mutate(prod_hat = A_hat/eu['A_hat'])
col$year <- unlist(col$year)
col$prod_hat <- unlist(col$prod_hat)
col$prod <- unlist(col$prod)
col <- col %>% mutate(diff = prod - prod_hat)
eu$y_trend <- hpfilter(eu$y,freq=1600,type = "lambda",drift = FALSE)$trend
col$y_trend <- hpfilter(col$y,freq=160,type = "lambda",drift = FALSE)$trend

#------------------------------------------------------------------------------#

ggplot(col, aes(x=year, y=A_hat)) +
  geom_line(color = "#004c69") +
  theme_economist() + 
  labs(
    title='Productividad estimada por el modelo de Solow para Colombia',
    subtitle = '\n(1950-2019)',
    x = '\nAño',
    y = 'USD PPP 2017\n',
    caption = "Fuente: Elaboración propia. Datos de 'The Penn World Table'") +
  scale_x_continuous(breaks = seq(1950, 2020, 10))
ggsave("DE5.png", units="in", width=10, height=6, dpi=300)

#------------------------------------------------------------------------------#

df <- col %>%
  select(year, prod, prod_hat) %>%
  rename(TPWT = prod, Solow = prod_hat) %>% 
  gather(key = "variable", value = "value", -year)
  
ggplot(df, aes(x = year, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("#004c69", "#72737e")) +
  theme_economist() +
  labs(
       title='TPWT vs. Modelo de Solow: Comparación de productividad para Colombia',
       subtitle = '\n(1950-2019)',
       x = '\nAño',
       y = 'Coeficiente de productividad COL/USA\n',
       caption = "Fuente: Elaboración propia. Datos de 'The Penn World Table'") +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  theme(legend.title = element_blank()) 
ggsave("DE6.png", units="in", width=10, height=6, dpi=300)

mean(col$diff, na.rm = TRUE)
var(col$diff, na.rm = TRUE)
sd(col$diff, na.rm = TRUE)

#------------------------------------------------------------------------------#

df2 <- eu %>%
  select(year, y_trend, y) %>%
  rename(Total = y, Tendencia = y_trend) %>% 
  gather(key = "variable", value = "value", -year)

ggplot(df2, aes(x = year, y = value)) + 
  geom_line(aes(color = variable)) + 
  scale_color_manual(values = c("#72737e","#004c69")) +
  labs(title ='PBI per cápita EU: total y tendencia', 
       subtitle = '\n(1950-2019)',
       x = '\nAño', 
       y = 'USD PPP 2017\n', 
       caption = "Fuente: Elaboración propia. Datos de 'The Penn World Table'") +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  theme_economist() +
  theme(legend.title = element_blank()) 
ggsave("DE7.png", units="in", width=10, height=6, dpi=300)

#------------------------------------------------------------------------------#

df3 <- NULL
df3 <- data.frame(seq(from = 1950, to = 2019, by = 1), eu$y_trend, col$y)
df3 <- df3 %>% rename(year = seq.from...1950..to...2019..by...1.)
df3$rela <- df3$col.y/df3$eu.y_trend

ggplot(df3, aes(x=year, y=rela)) +
  geom_line(color = "#004c69") +
  theme_economist() + 
  labs(
    title='Ingreso per cápita colombiano relativo a EU',
    subtitle = '\n(1950-2019)',
    x = '\nAño',
    y = '% Del ingreso per capita de EU\n',
    caption = "Fuente: Elaboración propia. Datos de 'The Penn World Table'") +
  geom_hline(yintercept=1, linetype="dashed", color = "red")
  scale_x_continuous(breaks = seq(1950, 2020, 10))
  ggsave("DE8.png", units="in", width=10, height=6, dpi=300)
  
#------------------------------------------------------------------------------#

ggplot(df3, aes(x=year, y=rela)) +
  geom_line(color = "#004c69") +
  theme_economist() + 
  labs(
      title='Ingreso per cápita colombiano relativo a EU',
      subtitle = '\n(1950-2019)',
      x = '\nAño',
      y = '% Del ingreso per capita de EU\n',
      caption = "Fuente: Elaboración propia. Datos de 'The Penn World Table'") +
  scale_x_continuous(breaks = seq(1950, 2020, 10))
ggsave("DE9.png", units="in", width=10, height=6, dpi=300)

#------------------------------------------------------------------------------#
