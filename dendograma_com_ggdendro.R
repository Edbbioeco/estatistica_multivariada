# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(ggdendro)

library(factoextra)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/mestrado/matriz_ambientais.xlsx")

## Visualizando ----

dados

dados |> dplyr::glimpse()

# Agrupamento hierárquico ----

## Calculando uma matriz de distância ----

dist_euclid <- dados |>
  tibble::column_to_rownames(var = "Unidade Amostral") |>
  vegan::decostand(method = "standardize") |>
  vegan::vegdist(method = "euclidean")

dist_euclid

## Calculando o UPGMA ----

upgma <- dist_euclid |>
  hclust(method = "average")

upgma

upgma |> plot()

## Visualizando ----

upgma |> ggdendro::ggdendrogram()
