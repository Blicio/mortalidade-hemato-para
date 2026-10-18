# =============================================================================
# 10_figuras_suplementares.R
# Figuras S1 a S5 do material suplementar (Online Resource), em portugues e ingles.
#
# Figura S1: % de obitos por causa mal definida, por faixa etaria (A-D)
# Figura S2: taxas anuais padronizadas com IC95% (Dobson), sem e com correcao, por sexo (A-C)
# Figura S3: aglomerados LISA por quinquenio e no periodo completo (A-F)
# Figura S4: RMP suavizada (bayesiano empirico local) por quinquenio (A-E) e localizacao (F)
# Figura S5: % de obitos por causa mal definida por municipio e quinquenio (A-E) e localizacao (F)
#
# Entrada : qualidade_resultados.xlsx (05), resultados_taxas.xlsx (02),
#           espacial_resultados.xlsx (06), malha e uf_BR_2020.gpkg
# Saida   : DIR_FIGURAS/FigS1_mal_definidas_faixa_*, FigS2_taxas_anuais_*,
#           FigS3_LISA_*, FigS4_RMP_quinquenios_*, FigS5_mal_definidas_municipios_*
#           (.tif, .png, .pdf; .eps so nos graficos de linhas)
# =============================================================================
# Pasta de trabalho = pasta dos scripts (no RStudio, e ajustada sozinha)
if (!file.exists("00_config.R")) { .d <- Sys.getenv("DIR_SCRIPTS"); if (!nzchar(.d) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) .d <- dirname(rstudioapi::getSourceEditorContext()$path); if (nzchar(.d)) setwd(.d) }
source("00_config.R"); source("00_funcoes.R")
for (a in list(c(ARQ_QUALIDADE, "05"), c(ARQ_TAXAS, "02"), c(ARQ_ESPACIAL, "06"))) exige(a[1], paste0(a[2], "_*.R"))
source("00_funcoes_mapas.R")
message("\n===== 10. Figuras suplementares =====")

tema_linhas <- function() theme_minimal(base_family = FONTE, base_size = TAM) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.25),
        axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
        axis.ticks.x = element_line(colour = "grey35", linewidth = 0.3), axis.ticks.length = unit(0.8, "mm"),
        axis.text = element_text(size = TAM - 0.5, colour = "grey20"), axis.title = element_text(size = TAM, colour = "grey10"),
        plot.title = element_text(size = TAM + 0.5, face = "bold", hjust = 0, margin = margin(0, 0, 1.2, 0, "mm")),
        plot.title.position = "plot", plot.margin = margin(1, 2, 1, 1, "mm"))
rot_x <- function(L) ggplot() + annotate("text", x = 0, y = 0, label = L$ano, size = TAM / .pt, family = FONTE) + theme_void()

# =============================================================================
# FIGURA S1: % de causas mal definidas por faixa etaria
# =============================================================================
mal <- read_excel(ARQ_QUALIDADE, sheet = "mal_definidas_faixa")
COR_DEST <- "#5E3C99"
figuraS1 <- function(L) {
  amplas <- c("0-19", "20-44", "45-64", "65+")
  paineis <- lapply(seq_along(amplas), function(i) {
    f <- amplas[i]; d <- mal %>% filter(faixa_ampla == f); ext <- d %>% filter(ano %in% c(2000, 2024))
    ggplot() +
      geom_line(data = mal %>% filter(faixa_ampla != f), aes(ano, pct, group = faixa_ampla), colour = "grey80", linewidth = 0.4) +
      geom_line(data = d, aes(ano, pct), colour = COR_DEST, linewidth = 0.8) +
      geom_point(data = ext, aes(ano, pct), colour = COR_DEST, size = 1.3) +
      annotate("text", x = 2001.5, y = ext$pct[ext$ano == 2000] + 1.5, label = paste0(fmt_num(ext$pct[ext$ano == 2000], L, 1), "%"),
               hjust = 0, vjust = 0, size = (TAM - 0.5) / .pt, family = FONTE) +
      annotate("text", x = 2022.5, y = ext$pct[ext$ano == 2024] - 1.5, label = paste0(fmt_num(ext$pct[ext$ano == 2024], L, 1), "%"),
               hjust = 1, vjust = 1, size = (TAM - 0.5) / .pt, family = FONTE) +
      scale_x_continuous(breaks = c(2000, 2012, 2024), limits = c(1999.3, 2024.7), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10), expand = c(0, 0)) +
      labs(title = paste0(LETTERS[i], "   ", L$faixas[i]), x = NULL, y = if (i == 1) L$ymal else NULL) +
      tema_linhas() + theme(axis.text.y = if (i == 1) element_text(size = TAM - 0.5, colour = "grey20") else element_blank())
  })
  (wrap_plots(paineis, nrow = 1) / rot_x(L)) + plot_layout(heights = c(1, 0.06))
}

# =============================================================================
# FIGURA S2: taxas anuais com IC95%
# =============================================================================
tx <- read_excel(ARQ_TAXAS, sheet = "taxas_padronizadas") %>%
  filter(grupo == P, faixa_ampla == "Todas") %>%
  mutate(serie = ifelse(correcao == "com redistribuicao", "com", "sem"))
figuraS2 <- function(L) {
  rot <- c(Ambos = L$ambos, Masculino = L$masc, Feminino = L$fem)
  paineis <- lapply(seq_along(SEXOS), function(i) {
    d <- tx %>% filter(sexo == SEXOS[i])
    ggplot(d, aes(ano, taxa_padr, colour = serie, fill = serie)) +
      geom_ribbon(aes(ymin = ic95_inf, ymax = ic95_sup), colour = NA, alpha = 0.2) +
      geom_line(linewidth = 0.6) +
      geom_point(aes(shape = serie), size = 1.3, stroke = 0.4, fill = "white") +
      scale_colour_manual(values = c(sem = COR_SEM, com = COR_COM), labels = c(sem = L$sem, com = L$com), breaks = c("sem", "com"), name = NULL) +
      scale_fill_manual(values = c(sem = COR_SEM, com = COR_COM), labels = c(sem = L$sem, com = L$com), breaks = c("sem", "com"), name = NULL) +
      scale_shape_manual(values = c(sem = 21, com = 24), labels = c(sem = L$sem, com = L$com), breaks = c("sem", "com"), name = NULL) +
      scale_x_continuous(breaks = seq(2000, 2024, 6), limits = c(1999.3, 2024.7), expand = c(0, 0)) +
      scale_y_continuous(limits = c(1, 7), breaks = 1:7, expand = c(0, 0), labels = function(x) fmt_num(x, L, 0)) +
      labs(title = paste0(LETTERS[i], "   ", rot[[SEXOS[i]]]), x = NULL, y = if (i == 1) L$ytaxa else NULL) +
      tema_linhas() + theme(axis.text.y = if (i == 1) element_text(size = TAM - 0.5, colour = "grey20") else element_blank())
  })
  (wrap_plots(paineis, nrow = 1) / rot_x(L)) + plot_layout(heights = c(1, 0.06), guides = "collect") &
    theme(legend.position = "bottom", legend.text = element_text(size = TAM, family = FONTE), legend.key.width = unit(7, "mm"))
}

# =============================================================================
# FIGURAS S3-S5: mapas por periodo
# =============================================================================
res <- read_excel(ARQ_ESPACIAL, sheet = "rmp_municipios") %>% mutate(cod = as.numeric(cod))
base_periodo <- function(p) malha %>% left_join(res %>% filter(periodo == p), by = "cod")
QUINQ <- names(QUINQUENIOS)

figuraS3 <- function(L) {
  per <- c(QUINQ, "2000-2024")
  traduz <- setNames(L$lisa_cl, c("Alto-Alto", "Baixo-Baixo", "Nao significativo"))
  paineis <- lapply(seq_along(per), function(i) {
    d <- base_periodo(per[i]) %>% mutate(classe = factor(traduz[lisa_cluster], levels = L$lisa_cl))
    painel_mapa(d, COR_LISA, L$lisa_cl, paste0(LETTERS[i], "   ", per[i]), L$lisa_leg, L, extras = i == 1)
  })
  wrap_plots(paineis, ncol = 3) + plot_layout(guides = "collect") & tema_legenda
}
figuraS4 <- function(L) {
  paineis <- lapply(seq_along(QUINQ), function(i) {
    d <- base_periodo(QUINQ[i]) %>% mutate(classe = classe_rmp(rmp_eb_local, L))
    painel_mapa(d, COR_RMP, L$rmp_cl, paste0(LETTERS[i], "   ", QUINQ[i]), L$rmp_leg, L, extras = i == 1)
  })
  wrap_plots(c(paineis, list(painel_localizacao(paste0("F   ", L$loc)))), ncol = 3) + plot_layout(guides = "collect") & tema_legenda
}
figuraS5 <- function(L) {
  paineis <- lapply(seq_along(QUINQ), function(i) {
    d <- base_periodo(QUINQ[i]) %>% mutate(classe = classe_mal(pct_mal, L))
    painel_mapa(d, COR_MAL, L$mal_cl, paste0(LETTERS[i], "   ", QUINQ[i]), L$mal_leg, L, extras = i == 1)
  })
  wrap_plots(c(paineis, list(painel_localizacao(paste0("F   ", L$loc)))), ncol = 3) + plot_layout(guides = "collect") & tema_legenda
}

# =============================================================================
for (id in IDIOMAS) {
  L <- TXT[[id]]
  salva_figura(figuraS1(L), paste0("FigS1_mal_definidas_faixa_", L$suf), 62)
  salva_figura(figuraS2(L), paste0("FigS2_taxas_anuais_", L$suf), 75)
  salva_figura(figuraS3(L), paste0("FigS3_LISA_", L$suf), 130, eps = FALSE)
  salva_figura(figuraS4(L), paste0("FigS4_RMP_quinquenios_", L$suf), 130, eps = FALSE)
  salva_figura(figuraS5(L), paste0("FigS5_mal_definidas_municipios_", L$suf), 130, eps = FALSE)
}
message("Pronto. Figuras em: ", DIR_FIGURAS)
