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

dados <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/matriz_ambientais.xlsx")

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

metodos <- c("ward.D",
             "ward.D2",
             "single",
             "complete",
             "average",
             "mcquitty",
             "median",
             "centroid")

mult_agrup <- purrr::map(metodos,
           purrr::in_parallel(

             ~dist_euclid |>
               hclust(method = .x)

           ),
           .progress = TRUE) |>
  setNames(metodos)

mult_agrup

## Visualizando ----

### Criando um dendro data ----

agrup_hie <- purrr::map(mult_agrup,
                        purrr::in_parallel(

                           ~ .x |>
                             as.dendrogram() |>
                             ggdendro::dendro_data()

                           ),
                        .progress = TRUE) |>
  setNames(paste0("agrupaento_", metodos))

agrup_hie

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
