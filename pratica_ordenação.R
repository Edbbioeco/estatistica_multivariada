# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(FactoMineR)

library(ggtext)

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

permanova <- vegan::adonis2(com_dist ~ Cluster,
                            data = com_trat,
                            permutations = 1000,
                            by = "terms")

permanova

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

### Estaísticas ----

perm_stats <- tibble::tibble(NMDS1 = -0.25,
                             NMDS2 = 0.35,
                             label = paste0("<b>F<sub>(",
                                            permanova$Df[1],
                                            ", ",
                                            permanova$Df[2],
                                            ")</sub> = ",
                                            permanova$F[1] |> round(2),
                                            ", p ",
                                            ifelse(permanova$`Pr(>F)`[1] < 0.01,
                                                   "< 0.01",
                                                   paste0("= ",
                                                          permanova$`Pr(>F)`[1])),
                             ", R² = ",
                             permanova$R2[1] |> round(2),
                             "</b>"),
                             Cluster = NA)

perm_stats

### Gráfico ----

nmds_scores |>
  ggplot(aes(NMDS1, NMDS2, fill = Cluster)) +
  geom_point(shape = 21, stroke = 1, size = 5) +
  ggtext::geom_richtext(data = perm_stats,
                        aes(NMDS1, NMDS2, label = label),
                        label.colour = NA,
                        fill = NA,
                        size = 5) +
  scale_fill_manual(values = c("orange", "royalblue")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom")
