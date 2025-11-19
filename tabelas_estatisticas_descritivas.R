# Pacotes ----

library(readxl)

library(tidyverse)

library(flextable)

library(ggview)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/mestrado/matriz_ambientais.xlsx")

## Visualizando ----

dados

dados |> dplyr::glimpse()

# Estatísticas descritivas ----

## Tabelas das estatísticas ----

des <- dados |>
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      values_to = "Valor",
                      names_to = "Variável") |>
  dplyr::filter(!Variável |> stringr::str_detect("Número")) |>
  dplyr::mutate(Variável = dplyr::case_when(Variável |>
                                              stringr::str_detect("Área") ~ paste0(Variável, " (m²)"),
                                            Variável |>
                                              stringr::str_detect("Altura") ~ paste0(Variável, " (cm)"),
                                            Variável |>
                                              stringr::str_detect("Temp") ~ paste0(Variável, " (°C)"),
                                            Variável |>
                                              stringr::str_detect("híd") ~ paste0(Variável, " (m)"),
                                            Variável |>
                                              stringr::str_detect("Bor") ~ paste0(Variável, " (m)"),
                                            Variável |>
                                              stringr::str_detect("Alt") ~ paste0(Variável, " (m)"),
                                            .default = Variável)) |>
  dplyr::summarise(`Média ± Desvio Padrão` = paste0(Valor |> mean() |> round(2),
                                                    " ± ",
                                                    Valor |> sd() |> round(2)),
                   .by = Variável) |>
  dplyr::mutate(n = 33) |>
  dplyr::relocate(n, .before = Variável)

des

## Tabela flextable ----

des_flex <- des |>
  flextable::flextable() |>
  flextable::width(width = 2.5, j = 2:3) |>
  flextable::align(align = "center", part = "all") |>
  flextable::bg(bg = "white", part = "all")

des_flex

## Exportando a tabela ----

des_flex |>
  flextable::save_as_image(path = "tabela_descritivas.png")

# Histogramas ----

dados |>
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      values_to = "Valor",
                      names_to = "Variável") |>
  dplyr::filter(!Variável |> stringr::str_detect("Número")) |>
  dplyr::mutate(Variável = dplyr::case_when(Variável |>
                                              stringr::str_detect("Área") ~ paste0(Variável, " (m²)"),
                                            Variável |>
                                              stringr::str_detect("Altura") ~ paste0(Variável, " (cm)"),
                                            Variável |>
                                              stringr::str_detect("Temp") ~ paste0(Variável, " (°C)"),
                                            Variável |>
                                              stringr::str_detect("híd") ~ paste0(Variável, " (m)"),
                                            Variável |>
                                              stringr::str_detect("Bor") ~ paste0(Variável, " (m)"),
                                            Variável |>
                                              stringr::str_detect("Alt") ~ paste0(Variável, " (m)"),
                                            .default = Variável)) |>
  ggplot(aes(Valor)) +
  geom_histogram(color = "black") +
  facet_wrap(~Variável, scales = "free_x") +
  labs(x = "Valor da Variável",
       y = "Contagem") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "histograma_variaveis.png", height = 10, width = 12)
