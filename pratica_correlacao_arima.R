# Pacotes ----

library(tidyverse)

library(forecast)

# Dados ----

## Importando ----

ecs <- ggplot2::economics

## Visualizando ----

ecs

ecs |> dplyr::glimpse()

# Correlação Desemprego x Taxa de Poupança Mensal ----

## Normalidade das variáveis ----

### Desemprego ----

ecs$unemploy |> shapiro.test()

ecs |>
  ggplot(aes(unemploy)) +
  geom_histogram(color = "black") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom",
        panel.background = element_rect(color = "black", linewidth = 1))

### Taxa de Poupança Mensal ----

ecs$psavert |> shapiro.test()

ecs |>
  ggplot(aes(psavert)) +
  geom_histogram(color = "black") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom",
        panel.background = element_rect(color = "black", linewidth = 1))

## Correlção ----

cor.test(ecs$psavert, ecs$unemploy, method = "spearman")

## Gráfico ----

ecs |>
  tidyr::pivot_longer(cols = c(4, 6),
                      values_to = "Valor da Variável",
                      names_to = "Variável") |>
  ggplot(aes(date, `Valor da Variável`, color = Variável)) +
  geom_line(linewidth = 1) +
  facet_wrap( ~Variável, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("orange", "royalblue")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom",
        panel.background = element_rect(color = "black", linewidth = 1))

# Modelo ARIMA ----

## Criando o objeto time series ----

ecs_ts <- ts(ecs$psavert,
             start = c(ecs$date |> min() |> lubridate::year(),
                       ecs$date |> min() |> lubridate::month()),
             frequency = 12)

ecs_ts

## Calcular ARIMA ----

### Criando o modelo -----

arima <- ecs_ts |> forecast::auto.arima()

### Diagnóstico do modelo ----

arima |> forecast::checkresiduals()

### Estatísticas do modelo ----

arima

## Gráfico ----

ecs |>
  ggplot(aes(date, psavert, color = unemploy)) +
  geom_line(linewidth = 1) +
  labs(x = "Data",
       y = "Taxa de Poupança Mensal",
       color = "Desemprego") +
  scale_color_viridis_c() +
  guides(color = guide_colorbar(title.position = "top",
                                title.hjust = 0.5,
                                barwidth = 20,
                                frame.colour = "black",
                                frame.linewidth = 1,
                                ticks.colour = "black",
                                ticks.linewidth = 1)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom",
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "arima.png", height = 10, width = 12)
