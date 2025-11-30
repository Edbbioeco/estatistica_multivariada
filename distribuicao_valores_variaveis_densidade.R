# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(ggview)

# Dados ----

## Importando ----

amb <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/mestrado/matriz_ambientais.xlsx")

## Visualizando ----

amb

amb |> dplyr::glimpse()

# Densidade ----

## Dados tratados ----

amb_trat <- amb |>
  tidyr::pivot_longer(cols = c(2, 3, 5, 7 , 9),
                      values_to = "Valor",
                      names_to = "Variável") |>
  dplyr::mutate(Variável = dplyr::case_when(Variável |>
                                              stringr::str_detect("Área") ~ paste0(Variável, " (m²)"),
                                            Variável |>
                                              stringr::str_detect("Altura") ~ paste0(Variável, " (cm)"),
                                            Variável |>
                                              stringr::str_detect("híd") ~ paste0(Variável, " (m)"),
                                            Variável |>
                                              stringr::str_detect("Alt") ~ paste0(Variável, " (m)"),
                                            .default = Variável)) |>
  dplyr::select(5:6)

amb_trat

## Valores médios e Desvio Padrão ----

medias <- amb |>
  tidyr::pivot_longer(cols = c(2, 3, 5, 7 , 9),
                      values_to = "Valor",
                      names_to = "Variável") |>
  dplyr::mutate(Variável = dplyr::case_when(Variável |>
                                              stringr::str_detect("Área") ~ paste0(Variável, " (m²)"),
                                            Variável |>
                                              stringr::str_detect("Altura") ~ paste0(Variável, " (cm)"),
                                            Variável |>
                                              stringr::str_detect("híd") ~ paste0(Variável, " (m)"),
                                            Variável |>
                                              stringr::str_detect("Alt") ~ paste0(Variável, " (m)"),
                                            .default = Variável)) |>
  dplyr::summarise(Média = Valor |> mean(),
                   `Desvio Padrão` = Valor |> sd(),
                   .by = Variável)

medias

## Unindo os dados ----

amb_trat %<>%
  dplyr::left_join(medias,
                   by = "Variável")

amb_trat

## Gráfico ----

amb_trat |>
  ggplot(aes(Valor)) +
  geom_density(linewidth = 1) +
  geom_vline(aes(xintercept = Média, color = "Média"),
             linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = Média - `Desvio Padrão`, color = "Desvio Padrão"),
             linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = Média + `Desvio Padrão`, color = "Desvio Padrão"),
             linewidth = 1, linetype = "dashed") +
  facet_wrap(~Variável, scales = "free") +
  scale_color_manual(values = c("Média" = "black",
                                "Desvio Padrão" = "red")) +
  labs(color = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 25),
        axis.title = element_text(color = "black", size = 25),
        legend.text = element_text(color = "black", size = 25),
        legend.title = element_text(color = "black", size = 25),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.background = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 20)) +
  ggview::canvas(height = 10, width = 16)

ggsave(filename = "densidade_variaveis_amb.png", height = 10, width = 16)
