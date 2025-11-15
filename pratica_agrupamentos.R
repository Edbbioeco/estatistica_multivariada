# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(ggdendro)

library(factoextra)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/mestrado/matriz_composicao.xlsx")

## Visualizando ----

dados

dados |> dplyr::glimpse()

# Agrupamento hierárquico ----

## Calculando uma matriz de distância ----

dist_bray <- dados |>
  dplyr::select_if(is.numeric) |>
  vegan::vegdist()

dist_bray

## Calculando o UPGMA ----

upgma <- dist_bray |>
  hclust(method = "average")

upgma

upgma |> plot()

## Visualizando ----

### Criando um dendro data ----

upgma_data <- upgma |>
  as.dendrogram() |>
  ggdendro::dendro_data()

upgma_data

### Tratando ----

upgma_data$labels$label <- dados$`Unidade Amostral`

upgma_data$labels

### Gráfico ----

ggplot() +
  geom_segment(data = upgma_data$segments, aes(x = x, y = y,
                                               xend = xend, yend = yend),
               linewidth = 1) +
  geom_text(data = upgma_data$labels,
            aes(x = x, y = y, label = label),
            angle = 270,
            hjust = -0.05,
            fontface = "bold",
            size =  5) +
  labs(x = NULL,
       y = "Bray-Curtis Distance") +
  scale_y_continuous(limits = c(-0.1, 0.475), breaks = seq(0, 1, 0.1)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        panel.grid.major.y = element_line(linewidth = 1, color = "gray80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Agrupamento não-hierárquico ----

## Número ideal de grupos ----

dados |>
  dplyr::select_if(is.numeric) |>
  vegan::cascadeKM(inf.gr = 2,
                   sup.gr = 10) |>
  plot()

## Calculando k-means ----

set.seed(123); dados |>
  dplyr::select_if(is.numeric) |>
  kmeans(centers = 2, nstart = 100) -> kmeans

kmeans

## Visualizando ----

kmeans |>
  factoextra::fviz_cluster(geom = "point",
                           data = dados |> dplyr::select_if(is.numeric)) +
  scale_fill_manual(values = c("orange", "royalblue")) +
  scale_color_manual(values = c("orange", "royalblue")) +
  theme_minimal() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15))

