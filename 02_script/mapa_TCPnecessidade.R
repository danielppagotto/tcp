# Pacotes
# install.packages(c("ggplot2","dplyr","sf","geobr","scales"))
library(ggplot2)
library(dplyr)
library(sf)
library(geobr)
library(scales)

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
mapa_final <- ggplot(dados_mapa) +
  geom_sf(aes(fill = necessidade_pct), color = "white", size = 0.2, linewidth = 0.2) +
  scale_fill_gradientn(
    colours = paleta_vermelha,
    limits = c(lim_inf, lim_sup),
    values = scales::rescale(c(lim_inf, 0.20, 0.30, 0.38, lim_sup)),
    breaks  = brks,
    labels  = label_percent(accuracy = 1),
    name    = "TCP's por necessidade (%)"
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

# 2. Salve o objeto 'mapa_final'
ggsave(
  filename = "meu_mapa_necessidade.png", 
  plot = mapa_final, # Especifica qual gráfico salvar
  width = 8,         # Ajuste a largura conforme necessário
  height = 9,        # Ajuste a altura (mapas do BR costumam ser mais altos)
  units = "in",      # Unidade de medida (inches, cm, mm, px)
  dpi = 500          # Resolução (300 é padrão para impressão)
)