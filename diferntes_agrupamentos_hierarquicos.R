# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(ggdendro)

library(factoextra)

library(ggview)

library(magrittr)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/mestrado/matriz_ambientais.xlsx")

## Visualizando ----

dados

dados |> dplyr::glimpse()

# Agrupamento hierárquico ----

## Calculando a matriz de distância ----

dist_euclid <- dados |>
  tibble::column_to_rownames(var = "Unidade Amostral") |>
  vegan::decostand(method = "standardize") |>
  vegan::vegdist(method = "euclidean")

dist_euclid

## Calculando multiplos agrupamentos ----

clacular_agrup <- function(x){

  agrup_hie <- dist_euclid |>
    hclust(method = x)

  print(agrup_hie)

  plot(agrup_hie, main = x)

  assign(paste0("agrupamento_", x),
         agrup_hie,
         envir = globalenv())

}

metodos <- c("ward.D",
             "ward.D2",
             "single",
             "complete",
             "average",
             "mcquitty",
             "median",
             "centroid")

purrr::walk(metodos, clacular_agrup)

## Visualizando ----

### Criando um dendro data ----

mult_dendro_data <- function(x, y){

  agrup_data <- x |>
    as.dendrogram() |>
    ggdendro::dendro_data()

  print(agrup_data)

  assign(paste0("dendrodata_", y),
         agrup_data,
         envir = globalenv())

}

mult_agrup <- ls(pattern = "agrupamento_") |>
  mget(envir = globalenv())

mult_agrup

purrr::walk2(mult_agrup,
             metodos,
             mult_dendro_data)

### Gráfico ----

gg_dendrodata <- function(agrup_hie, metodos){

  dendro_plot <- ggplot() +
    geom_segment(data = agrup_hie$segments, aes(x = x, y = y,
                                                xend = xend, yend = yend),
                 linewidth = 1) +
    labs(x = NULL,
         y = "Euclidean Distance",
         title = paste0("Método = ", metodos)) +
    scale_x_continuous(breaks = 1:11,
                       labels = agrup_hie$labels$label) +
    scale_y_continuous(expand = FALSE) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                     color = "black", size = 25),
          axis.text = element_text(color = "black", size = 25),
          axis.title = element_text(color = "black", size = 25),
          title = element_text(color = "black", size = 25),
          panel.grid.major.y = element_line(linewidth = 1, color = "gray80"),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    ggview::canvas(height = 10, width = 12)

  print(dendro_plot)

  ggsave(filename = paste0("agrupamento_hierarquico_",
                           metodos,
                           ".png"),
         height = 10, width = 12)


}

agrup_hie <- ls(pattern = "dendrodata_") |>
  mget(envir = globalenv())

agrup_hie

purrr::walk2(agrup_hie,
             metodos,
             gg_dendrodata)
