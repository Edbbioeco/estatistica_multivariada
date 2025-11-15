# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(FactoMineR)

# Dados ----

## Ambientais ----

### Importando ----

amb <-  readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/mestrado/matriz_ambientais.xlsx")

### Visualizando ----

amb

amb |> dplyr::glimpse()

## Composição ----

## Importando ----

com <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/mestrado/matriz_composicao.xlsx")

## Visualizando ----

com

com |> dplyr::glimpse()

# Importando os resultados do KMeans ----

source("pratica_agrupamentos.R")

# PCA ----

## Calculando a PCA ----

pca <- amb |>
  dplyr::select_if(is.numeric) |>
  vegan::decostand(method = "standardize") |>
  FactoMineR::PCA()

## Variáveis mais relevantes ----

pca$var

## Contribuição dos PCs ----

pca$eig[1:2, 2]

## Visualizando ----

### Scores ----

pca_scores <- pca$ind$coord |>
  tibble::as_tibble() |>
  dplyr::select(1:2) |>
  dplyr::rename_with(~ paste0("PC", 1:2, " (", pca$eig[1:2, 2] |> round(2), "%)")) |>
  dplyr::mutate(`Unidade Amostral` = amb$`Unidade Amostral`,
                Cluster = kmeans$cluster |> as.character())

pca_scores

### Gráfico ----

pca_scores |>
  ggplot(aes(`PC1 (43.85%)`, `PC2 (24.17%)`, fill = Cluster)) +
  geom_point(shape = 21, stroke = 1, size = 5) +
  scale_fill_manual(values = c("orange", "royalblue")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom")

# PERMANOVA ----

## Matriz de distância de Bray-Curtis ----

com_dist <- com |>
  dplyr::select_if(is.numeric) |>
  vegan::vegdist()

com_dist

## Adiconando os dados de cluster ----

com_trat <- com |>
  dplyr::mutate(Cluster = kmeans$cluster |> as.character())

com_trat

## Calculando PERMANOVA -----

vegan::adonis2(com_dist ~ Cluster,
               data = com_trat,
               permutations = 1000,
               ny = "terms")

## Visualizando ----

### Calculando o NMDS ----

nmds <- com_dist |>
  vegan::metaMDS(k = 2)

nmds

### Scores ----

nmds_scores <- nmds |>
  vegan::scores() |>
  tibble::as_tibble() |>
  dplyr::mutate(Cluster = kmeans$cluster |> as.character())

nmds_scores

### Gráfico ----

nmds_scores |>
  ggplot(aes(NMDS1, NMDS2, fill = Cluster)) +
  geom_point(shape = 21, stroke = 1, size = 5) +
  scale_fill_manual(values = c("orange", "royalblue")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom")
