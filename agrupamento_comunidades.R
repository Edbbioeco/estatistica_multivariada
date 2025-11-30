# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(ggdendro)

library(ggview)

# Dados ----

## Composição ----

## Importando ----

com <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/mestrado/matriz_composicao.xlsx")

## Visualizando ----

com

com |> dplyr::glimpse()

# Agrupamento ----

## Matriz de distância de Bray-Curtis ----

matriz_bray <- com |>
  tibble::column_to_rownames(var = "Unidade Amostral") |>
  vegan::vegdist()

matriz_bray

## Calculando o UPGMA ----

upgma_bray <- matriz_bray |>
  hclust(method = "average")

upgma_bray

upgma_bray |> plot()

# Dendograma ----

## Criando os dados do dendograma ----

dendro_data <- upgma_bray |>
  as.dendrogram() |>
  ggdendro::dendro_data()

dendro_data

ramos <- dendro_data$segments

ramos

uni_amo <- dendro_data$labels

uni_amo

## Gráfico ----

ggplot() +
  geom_segment(data = ramos, aes(x = x, y = y,
                                               xend = xend, yend = yend),
               linewidth = 1) +
  labs(x = NULL,
       y = "Bray-Curtis Distance") +
  scale_x_continuous(breaks = 1:11,
                     labels = uni_amo$label) +
  scale_y_continuous(expand = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                   color = "black", size = 25),
        axis.text = element_text(color = "black", size = 25),
        axis.title = element_text(color = "black", size = 25),
        panel.grid.major.y = element_line(linewidth = 1, color = "gray80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "dendograma_comunidades.png", height = 10, width = 12)
