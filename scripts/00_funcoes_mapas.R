# =============================================================================
# 00_funcoes_mapas.R
# Elementos comuns aos mapas das Figuras 2, 3 e S3-S5: malha de 143 unidades,
# contorno do estado, ampliacao da Regiao Metropolitana de Belem (RMB), escala,
# seta de norte, classes de cor e exportacao. Carregado pelos scripts 09 e 10.
# =============================================================================
carrega(c("sf", "ggplot2", "patchwork", "ragg", "scales", "ggrepel"))
sf_use_s2(FALSE)
exige(ARQ_MALHA, "06_analise_espacial.R")

# ---- Rotulos por idioma ---------------------------------------------------------
TXT <- list(
  pt = list(suf = "pt", dec = ",", mil = ".", sem = "Sem correção", com = "Com correção",
            ambos = "Ambos os sexos", masc = "Masculino", fem = "Feminino",
            hl = "Linfoma de Hodgkin", lnh = "Linfomas não Hodgkin", mm = "Mieloma", leu = "Leucemias",
            ano = "Ano do óbito", obitos = "óbitos",
            ytaxa = "Taxa padronizada por idade\n(por 100.000 habitantes)",
            ylog = "Taxa padronizada por idade\n(por 100.000; escala log)",
            rmp_leg = "Razão de mortalidade padronizada (suavizada)",
            mal_leg = "Óbitos por causa mal definida (%)",
            lisa_leg = "Aglomerado local (LISA, p ajustado < 0,05)",
            lisa_cl = c("Alto-Alto", "Baixo-Baixo", "Não significativo"),
            rmp_cl = c("< 0,67", "0,67-0,89", "0,90-1,10", "1,11-1,49", "≥ 1,50"),
            mal_cl = c("< 5", "5-9,9", "10-19,9", "20-29,9", "≥ 30"),
            rr_leg = "Risco relativo do aglomerado", dur_leg = "Duração do aglomerado",
            dur_cl = c("Todo o período (2000-2024)", "Parte do período"),
            painelC = "Aglomerados detectados (p < 0,05)", periodo_aglom = "Período do aglomerado",
            obs_esp = "Obs. / Esp.", mais = "e mais", mun1 = "município", mun = "municípios",
            faixas = c("0 a 19 anos", "20 a 44 anos", "45 a 64 anos", "65 anos ou mais"),
            ymal = "Óbitos por causa mal definida (%)", loc = "Localização no Brasil",
            rmb = "RMB"),
  en = list(suf = "en", dec = ".", mil = ",", sem = "Uncorrected", com = "Corrected",
            ambos = "Both sexes", masc = "Men", fem = "Women",
            hl = "Hodgkin lymphoma", lnh = "Non-Hodgkin lymphomas", mm = "Multiple myeloma", leu = "Leukemias",
            ano = "Year of death", obitos = "deaths",
            ytaxa = "Age-standardized rate\n(per 100,000)",
            ylog = "Age-standardized rate\n(per 100,000; log scale)",
            rmp_leg = "Standardized mortality ratio (smoothed)",
            mal_leg = "Deaths from ill-defined causes (%)",
            lisa_leg = "Local cluster (LISA, adjusted p < 0.05)",
            lisa_cl = c("High-High", "Low-Low", "Not significant"),
            rmp_cl = c("< 0.67", "0.67-0.89", "0.90-1.10", "1.11-1.49", "≥ 1.50"),
            mal_cl = c("< 5", "5-9.9", "10-19.9", "20-29.9", "≥ 30"),
            rr_leg = "Relative risk of the cluster", dur_leg = "Cluster duration",
            dur_cl = c("Whole period (2000-2024)", "Part of the period"),
            painelC = "Detected clusters (p < 0.05)", periodo_aglom = "Cluster period",
            obs_esp = "Obs. / Exp.", mais = "and", mun1 = "other municipality", mun = "other municipalities",
            faixas = c("0-19 years", "20-44 years", "45-64 years", "65 years or older"),
            ymal = "Deaths from ill-defined causes (%)", loc = "Location in Brazil",
            rmb = "BMA"))
fmt_num <- function(x, L, d = 2) formatC(x, format = "f", digits = d, decimal.mark = L$dec, big.mark = L$mil)

# ---- Cores e classes ------------------------------------------------------------
COR_SEM <- "#D55E00"; COR_COM <- "#0072B2"      # Okabe-Ito
PT_SEM  <- "#F2A97F"; PT_COM  <- "#8FC1E3"
CORTES_RMP <- c(-Inf, 0.67, 0.90, 1.11, 1.50, Inf)   # simetricas em escala log em torno de 1
COR_RMP    <- c("#2166ac", "#86b9da", "#ececec", "#f0a07f", "#b2182b")
CORTES_MAL <- c(-Inf, 5, 10, 20, 30, Inf)
COR_MAL    <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#756bb1", "#4a1486")
COR_LISA   <- c("#b2182b", "#2166ac", "#ececec")
LW_MUN <- 0.08; LW_UF <- 0.25
classe_rmp <- function(v, L) factor(cut(v, CORTES_RMP, labels = L$rmp_cl, right = FALSE), levels = L$rmp_cl)
classe_mal <- function(v, L) factor(cut(v, CORTES_MAL, labels = L$mal_cl, right = FALSE), levels = L$mal_cl)

# ---- Malha e geometria da ampliacao da RMB ----------------------------------------
malha <- st_read(ARQ_MALHA, quiet = TRUE) %>% st_make_valid() %>%
  mutate(cod = as.numeric(code_muni) %/% 10, cod = ifelse(cod == MOJUI, SANTAREM, cod)) %>%
  group_by(cod) %>% summarise(.groups = "drop") %>% st_transform(CRS_PROJ)
stopifnot(nrow(malha) == 143)
nomes_mun <- st_read(ARQ_MALHA, quiet = TRUE) %>% st_drop_geometry() %>%
  transmute(cod = as.numeric(code_muni) %/% 10, nome = name_muni) %>% filter(cod != MOJUI)
contorno <- malha %>% st_buffer(50) %>% st_union() %>% st_buffer(-50) %>% st_cast("POLYGON")
estado <- st_sf(geometry = st_sfc(lapply(contorno, function(p) st_polygon(list(p[[1]]))), crs = CRS_PROJ))

bb_pa <- st_bbox(malha)
larg_pa <- unname(bb_pa["xmax"] - bb_pa["xmin"]); alt_pa <- unname(bb_pa["ymax"] - bb_pa["ymin"])
bb_rmb <- st_bbox(st_buffer(st_union(malha %>% filter(cod %in% RMB)), 8000))
lado_rmb <- unname(max(bb_rmb["xmax"] - bb_rmb["xmin"], bb_rmb["ymax"] - bb_rmb["ymin"]))
c_rmb <- unname(c((bb_rmb["xmin"] + bb_rmb["xmax"]) / 2, (bb_rmb["ymin"] + bb_rmb["ymax"]) / 2))
lado_ins <- 0.25 * larg_pa
escala_ins <- lado_ins / lado_rmb
FOLGA_X <- 0.09 * larg_pa                          # area extra a direita do estado para a ampliacao
c_ins <- unname(c(bb_pa["xmax"] + FOLGA_X - lado_ins / 2, bb_pa["ymin"] + lado_ins / 2))
caixa <- function(cx, cy, l) st_sfc(st_polygon(list(matrix(
  c(cx - l/2, cy - l/2, cx + l/2, cy - l/2, cx + l/2, cy + l/2, cx - l/2, cy + l/2, cx - l/2, cy - l/2),
  ncol = 2, byrow = TRUE))), crs = CRS_PROJ)
moldura_rmb <- caixa(c_rmb[1], c_rmb[2], lado_rmb)
moldura_ins <- caixa(c_ins[1], c_ins[2], lado_ins)
amplia <- function(g) {                             # leva a geometria da RMB para a caixa ampliada
  geo <- (st_geometry(g) - c_rmb) * escala_ins + c_ins
  st_set_geometry(g, st_set_crs(geo, CRS_PROJ))
}
amplia_xy <- function(x, y) cbind((x - c_rmb[1]) * escala_ins + c_ins[1], (y - c_rmb[2]) * escala_ins + c_ins[2])
dentro_rmb <- function(x, y) abs(x - c_rmb[1]) <= lado_rmb / 2 & abs(y - c_rmb[2]) <= lado_rmb / 2
chamada <- st_sfc(
  st_linestring(rbind(c(c_rmb[1] + lado_rmb/2, c_rmb[2] - lado_rmb/2), c(c_ins[1] - lado_ins/2, c_ins[2] + lado_ins/2))),
  st_linestring(rbind(c(c_rmb[1] + lado_rmb/2, c_rmb[2] + lado_rmb/2), c(c_ins[1] + lado_ins/2, c_ins[2] + lado_ins/2))),
  crs = CRS_PROJ)
lim_x <- unname(c(bb_pa["xmin"], bb_pa["xmax"] + FOLGA_X)); lim_y <- unname(c(bb_pa["ymin"], bb_pa["ymax"]))

uf <- tryCatch({
  if (!file.exists(ARQ_UF)) { carrega("geobr"); st_write(geobr::read_state(year = 2020, showProgress = FALSE), ARQ_UF, quiet = TRUE) }
  st_read(ARQ_UF, quiet = TRUE) %>% st_make_valid() %>% st_transform(CRS_PROJ) %>% st_simplify(dTolerance = 4000)
}, error = function(e) { message("Limites dos estados indisponiveis; o painel de localizacao sera omitido."); NULL })

# ---- Tema, escala e seta ------------------------------------------------------------
tema_mapa <- function() theme_void(base_family = FONTE, base_size = TAM) +
  theme(plot.title = element_text(size = TAM + 0.5, face = "bold", hjust = 0, margin = margin(0, 0, 1, 0, "mm")),
        plot.margin = margin(1, 1, 1, 1, "mm"))
tema_legenda <- theme(legend.position = "bottom",
                      legend.title = element_text(size = TAM, family = FONTE),
                      legend.text = element_text(size = TAM - 0.5, family = FONTE),
                      legend.key.width = unit(6, "mm"), legend.key.height = unit(2.6, "mm"),
                      legend.margin = margin(0.5, 0, 0, 0, "mm"))
guia <- function() guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5,
                                override.aes = list(colour = "grey55", linewidth = 0.15))

escala_norte <- function() {
  km <- 150e3
  x0 <- bb_pa[["xmin"]] + 0.012 * larg_pa; y0 <- bb_pa[["ymin"]] + 0.045 * alt_pa; h <- 0.014 * alt_pa
  barra <- data.frame(xmin = x0 + c(0, km/2), xmax = x0 + c(km/2, km), ymin = rep(y0, 2), ymax = rep(y0 + h, 2))
  xn <- x0 + 0.02 * larg_pa; yn <- y0 + 0.07 * alt_pa; hn <- 0.065 * alt_pa
  seta <- data.frame(x = c(xn - hn * 0.28, xn, xn + hn * 0.28, xn), y = c(yn, yn + hn, yn, yn + hn * 0.32))
  list(geom_rect(data = barra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = c("black", "white"),
                 colour = "black", linewidth = 0.15, inherit.aes = FALSE),
       annotate("text", x = x0 + km / 2, y = y0 - 0.006 * alt_pa, label = "150 km", vjust = 1,
                size = (TAM - 1) / .pt, family = FONTE),
       geom_polygon(data = seta, aes(x, y), fill = "black", inherit.aes = FALSE),
       annotate("text", x = xn, y = yn + hn + 0.028 * alt_pa, label = "N", size = TAM / .pt,
                family = FONTE, fontface = "bold"))
}

# Camadas fixas: contorno do estado, moldura e ampliacao da RMB
camadas_rmb <- function(L) list(
  geom_sf(data = estado, fill = NA, colour = "grey25", linewidth = LW_UF),
  geom_sf(data = moldura_rmb, fill = NA, colour = "black", linewidth = 0.2),
  geom_sf(data = chamada, colour = "grey45", linewidth = 0.15),
  geom_sf(data = moldura_ins, fill = "white", colour = "black", linewidth = 0.2))
rotulo_rmb <- function(L) annotate("text", x = c_ins[1] - lado_ins/2 + 0.012 * larg_pa,
                                   y = c_ins[2] + lado_ins/2 - 0.012 * alt_pa, label = L$rmb,
                                   hjust = 0, vjust = 1, size = (TAM - 1) / .pt, family = FONTE)

# Mapa coropletico padrao (dados = malha com a coluna 'classe')
painel_mapa <- function(dados, cores, rotulos, titulo, nome_leg, L, extras = FALSE) {
  names(cores) <- rotulos
  camadas <- camadas_rmb(L)
  ggplot() +
    geom_sf(data = dados, aes(fill = classe), colour = "white", linewidth = LW_MUN) +
    camadas[[1]] + camadas[[2]] + camadas[[3]] + camadas[[4]] +
    geom_sf(data = amplia(dados %>% filter(cod %in% RMB)), aes(fill = classe), colour = "white", linewidth = LW_MUN * 2) +
    rotulo_rmb(L) +
    (if (extras) escala_norte() else NULL) +
    scale_fill_manual(values = cores, drop = FALSE, name = nome_leg, guide = guia()) +
    coord_sf(xlim = lim_x, ylim = lim_y, expand = FALSE, datum = NA) +
    labs(title = titulo) + tema_mapa() + tema_legenda
}

painel_localizacao <- function(titulo) {
  if (is.null(uf)) return(plot_spacer())
  rot <- st_coordinates(suppressWarnings(st_point_on_surface(st_union(estado))))
  ggplot() +
    geom_sf(data = uf, fill = "grey88", colour = "white", linewidth = 0.15) +
    geom_sf(data = estado, fill = "grey30", colour = NA) +
    annotate("text", x = rot[1], y = rot[2], label = "Pará", colour = "white",
             size = (TAM - 0.5) / .pt, family = FONTE, fontface = "bold") +
    coord_sf(expand = FALSE, datum = NA) + labs(title = titulo) + tema_mapa()
}

# ---- Exportacao: PNG e TIFF 600 dpi, PDF e EPS vetoriais ---------------------------
salva_figura <- function(fig, nome, alt_mm, eps = TRUE) {
  base <- file.path(DIR_FIGURAS, nome)
  ggsave(paste0(base, ".png"), fig, width = LARG_MM, height = alt_mm, units = "mm", dpi = 600,
         device = ragg::agg_png, bg = "white")
  ggsave(paste0(base, ".tif"), fig, width = LARG_MM, height = alt_mm, units = "mm", dpi = 600,
         device = ragg::agg_tiff, compression = "lzw", bg = "white")
  ggsave(paste0(base, ".pdf"), fig, width = LARG_MM, height = alt_mm, units = "mm", device = cairo_pdf, bg = "white")
  if (eps) ggsave(paste0(base, ".eps"), fig, width = LARG_MM, height = alt_mm, units = "mm", device = cairo_ps,
                  bg = "white", fallback_resolution = 600)
  message("Salvo: ", base)
}
