# Pacotes
# install.packages(c("ggplot2","dplyr","sf","geobr","scales"))
library(ggplot2)
library(dplyr)
library(sf)
library(geobr)
library(scales)
library(ggrepel)
library(patchwork)

# --- 1) Tabela com a "necessidade" (%) do gráfico (valores em vermelho) ---
# Obs.: valores foram lidos do gráfico compartilhado. Ajuste se necessário.
necessidade_uf <- tibble::tribble(
  ~uf,                    ~necessidade_pct,
  "RS",    0.13,
  "AP",                0.20,
  "MS",   0.16,
  "RJ",       0.15,
  "SC",       0.12,  # mínimo observado
  "MT",          0.18,
  "SP",            0.17,
  "RO",             0.25,
  "PR",               0.20,
  "MG",         0.18,
  "PB",              0.19,
  "PE",           0.26,
  "ES",       0.22,
  "AL",              0.26,
  "GO",                0.21,
  "SE",              0.24,
  "PA",                 0.24,
  "TO",            0.23,
  "RN",  0.25,
  "PI",                0.32,
  "AM",             0.32,
  "DF",     0.22,
  "BA",                0.28,
  "AC",                 0.34,
  "MA",             0.43,
  "CE",                0.38,
  "RR",              0.33
)

# --- 2) Geometrias dos estados do Brasil (geobr) ---
br_estados <- geobr::read_state(code_state = "all", year = 2020) |>
  st_transform(4674)  # SIRGAS / Brasil (pode manter 4674 ou 4326; 4674 dá legenda métrica estável)

# Conferir nomes e juntar (geobr usa 'name_state' com acentos corretos)
dados_mapa <- br_estados |>
  left_join(necessidade_uf, 
            by = c("abbrev_state" = "uf"))

# --- 3) Paleta e quebras (legenda 10% a 45%) ---
lim_inf <- 0.10
lim_sup <- 0.45
brks <- seq(lim_inf, lim_sup, by = 0.05)

paleta_vermelha <- c("#FFE5E5", "#FFB3B3", 
                     
                     "#FF6666", "#E62E2E", "#B30000")

# --- 4) Mapa coroplético ---
# 1. Crie o gráfico e salve-o em uma variável
mapa_necessidade <- ggplot(dados_mapa) +
  geom_sf(aes(fill = necessidade_pct), color = "white", size = 0.2, linewidth = 0.2) +
  scale_fill_gradientn(
    colours = paleta_vermelha,
    limits = c(lim_inf, lim_sup),
    values = scales::rescale(c(lim_inf, 0.20, 0.30, 0.38, lim_sup)),
    breaks  = brks,
    labels  = label_percent(accuracy = 1),
    name    = "TCP's por \nnecessidade (%)"
  ) +
  coord_sf() +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(color = "grey85", linewidth = 0.2),
    axis.title = element_blank()
  ) +
  geom_label_repel(
    aes(
      label = paste0(percent(necessidade_pct, accuracy = 1)," (", dados_mapa$abbrev_state,")"),
      geometry = dados_mapa$geom
    ),
    stat = "sf_coordinates",
    color = "black",
    fill = "white",
    size = 3,
    fontface = "bold",
    label.padding = unit(0.2, "lines"),
    label.r = unit(0.15, "lines"),
    min.segment.length = 0
  )

mapa_necessidade

# 2. Salve o objeto 'mapa_final'
ggsave(
  filename = "meu_mapa_necessidade.png", 
  plot = mapa_necessidade, # Especifica qual gráfico salvar
  width = 8,         # Ajuste a largura conforme necessário
  height = 9,        # Ajuste a altura (mapas do BR costumam ser mais altos)
  units = "in",      # Unidade de medida (inches, cm, mm, px)
  dpi = 700          # Resolução (300 é padrão para impressão)
)

persistente_uf <- tibble::tribble(
  ~uf, ~persistente_pct,
  "RS", 0.71, "AP", 0.67, "MS", 0.61, "RJ", 0.61, "SC", 0.61,
  "MT", 0.59, "SP", 0.59, "RO", 0.58, "PR", 0.58, "MG", 0.57,
  "PB", 0.57, "PE", 0.56, "ES", 0.55, "AL", 0.55, "GO", 0.54,
  "SE", 0.53, "PA", 0.53, "TO", 0.49, "RN", 0.48, "PI", 0.47,
  "AM", 0.47, "DF", 0.47, "BA", 0.46, "AC", 0.44, "MA", 0.42,
  "CE", 0.41, "RR", 0.33
)

dados_persistente <- br_estados |>
  left_join(persistente_uf, by = c("abbrev_state" = "uf"))

paleta_verde <- c("#E5F5E0", "#A1D99B", 
                  "#74C476", "#31A354", "#006D2C")


mapa_persistente <- ggplot(dados_persistente) +
  geom_sf(aes(fill = persistente_pct), color = "white", size = 0.2) +
  scale_fill_gradientn(
    colours = paleta_verde,
    limits = c(0.30, 0.75),
    breaks = seq(0.30, 0.75, by = 0.05),
    labels = label_percent(accuracy = 1),
    name = "TCP's \nPersistentes (%)"
  ) +
  geom_label_repel(
    aes(
      label = paste0(percent(persistente_pct, accuracy = 1), " (", abbrev_state, ")"),
      geometry = geom
    ),
    stat = "sf_coordinates",
    fill = "white",
    size = 3,
    fontface = "bold",
    label.padding = unit(0.2, "lines")
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_line(color = "grey85"))

mapa_persistente

ggsave(
  filename = "meu_mapa_persistentes.png", 
  plot = mapa_persistente, # Especifica qual gráfico salvar
  width = 8,         # Ajuste a largura conforme necessário
  height = 9,        # Ajuste a altura (mapas do BR costumam ser mais altos)
  units = "in",      # Unidade de medida (inches, cm, mm, px)
  dpi = 700          # Resolução (300 é padrão para impressão)
)

intermitente_uf <- tibble::tribble(
  ~uf, ~intermitente_pct,
  "RS", 0.17, "AP", 0.13, "MS", 0.23, "RJ", 0.24, "SC", 0.27,
  "MT", 0.24, "SP", 0.24, "RO", 0.17, "PR", 0.22, "MG", 0.25,
  "PB", 0.24, "PE", 0.18, "ES", 0.23, "AL", 0.19, "GO", 0.25,
  "SE", 0.23, "PA", 0.23, "TO", 0.28, "RN", 0.27, "PI", 0.21,
  "AM", 0.21, "DF", 0.31, "BA", 0.26, "AC", 0.21, "MA", 0.14,
  "CE", 0.21, "RR", 0.34
)

dados_intermitente <- br_estados |>
  left_join(intermitente_uf, by = c("abbrev_state" = "uf"))

paleta_amarelo <- c("#FFF7CD", "#FFEB99",
                    "#FFD94D", "#F4C430", "#C49A00")

mapa_intermitente <- ggplot(dados_intermitente) +
  geom_sf(aes(fill = intermitente_pct), color = "white", size = 0.2) +
  scale_fill_gradientn(
    colours = paleta_amarelo,
    limits = c(0.10, 0.40),
    breaks = seq(0.10, 0.40, by = 0.05),
    labels = label_percent(accuracy = 1),
    name = "TCP's \nTransitórios (%)"
  ) +
  geom_label_repel(
    aes(
      label = paste0(percent(intermitente_pct, accuracy = 1), " (", abbrev_state, ")"),
      geometry = geom
    ),
    stat = "sf_coordinates",
    fill = "white",
    size = 3,
    fontface = "bold",
    label.padding = unit(0.2, "lines")
  ) +
  theme_minimal(base_size = 12)

mapa_intermitente

ggsave(
  filename = "meu_mapa_transitorios.png", 
  plot = mapa_intermitente, # Especifica qual gráfico salvar
  width = 8,         # Ajuste a largura conforme necessário
  height = 9,        # Ajuste a altura (mapas do BR costumam ser mais altos)
  units = "in",      # Unidade de medida (inches, cm, mm, px)
  dpi = 700          # Resolução (300 é padrão para impressão)
)


painel_horizontal <- mapa_persistente | mapa_necessidade | mapa_intermitente

painel_horizontal

ggsave(
  "painel_horizontal_tcp.svg",
  painel_horizontal,
  width = 18,
  height = 20,
  dpi = 500
)


painel_vertical <- mapa_persistente / mapa_necessidade / mapa_intermitente

ggsave(
  "painel_vertical_tcp.png",
  painel_vertical,
  width = 7,
  height = 18,
  dpi = 500
)
