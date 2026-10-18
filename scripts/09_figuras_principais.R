# =============================================================================
# 09_figuras_principais.R
# Figuras 1, 2 e 3 do manuscrito, em portugues e ingles, no padrao da
# Cancer Causes & Control (174 mm, fonte sem serifa 7 pt, TIFF 600 dpi LZW,
# PNG 600 dpi, PDF e EPS vetoriais).
#
# Figura 1: taxas observadas e modelo joinpoint, sem e com correcao, por sexo
#           (A-C) e grupo diagnostico (D-G)
# Figura 2: RMP 2000-2024 sem e com correcao (A-B) e % de obitos por causa mal
#           definida em 2000-2004 e 2020-2024 (C-D)
# Figura 3: aglomerados espaco-temporais (SaTScan) sem e com correcao (A-B) e
#           linha do tempo dos aglomerados (C)
#
# Entrada : joinpoint_resultados.xlsx (03), resultados_taxas.xlsx (02),
#           espacial_resultados.xlsx (06), satscan_resultados.xlsx (08), malha
# Saida   : DIR_FIGURAS/Fig1_tendencias_pt|en, Fig2_mapas_pt|en, Fig3_aglomerados_pt|en
#           (.tif, .png, .pdf, .eps)
# Nenhuma analise e refeita aqui.
# =============================================================================
# Pasta de trabalho = pasta dos scripts (no RStudio, e ajustada sozinha)
if (!file.exists("00_config.R")) { .d <- Sys.getenv("DIR_SCRIPTS"); if (!nzchar(.d) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) .d <- dirname(rstudioapi::getSourceEditorContext()$path); if (nzchar(.d)) setwd(.d) }
source("00_config.R"); source("00_funcoes.R")
for (a in list(c(ARQ_JOINPOINT, "03"), c(ARQ_TAXAS, "02"), c(ARQ_ESPACIAL, "06"), c(ARQ_SATSCAN, "08")))
  exige(a[1], paste0(a[2], "_*.R"))
source("00_funcoes_mapas.R")
message("\n===== 09. Figuras principais =====")

# =============================================================================
# FIGURA 1
# =============================================================================
aj  <- read_excel(ARQ_JOINPOINT, sheet = "ajustados") %>% filter(cenario == "2000-2024", faixa_ampla == "Todas")
mod <- read_excel(ARQ_JOINPOINT, sheet = "modelos")   %>% filter(cenario == "2000-2024", faixa_ampla == "Todas")
apc <- read_excel(ARQ_JOINPOINT, sheet = "aapc")      %>% filter(cenario == "2000-2024", faixa_ampla == "Todas")
tx  <- read_excel(ARQ_TAXAS, sheet = "taxas_padronizadas") %>%
  filter(faixa_ampla == "Todas", sexo == "Ambos", correcao == "sem redistribuicao")
GR4 <- c("Linfoma de Hodgkin", "Linfomas nao Hodgkin", "Mieloma e plasmocitarias", "Leucemias")
n_obitos <- tx %>% group_by(grupo) %>% summarise(n = sum(obitos), .groups = "drop")

juncoes <- function(g, s, cr) {
  j <- mod %>% filter(grupo == g, sexo == s, correcao == cr) %>% pull(juncoes)
  if (length(j) == 0 || is.na(j) || j == "") return(numeric(0))
  as.numeric(strsplit(as.character(j), ";\\s*")[[1]])
}
# Segmentos ajustados como linhas separadas; tracejado nos segmentos de ate 4 anos
linhas_ajuste <- function(d, g, s, cr) {
  lim <- c(min(d$ano), juncoes(g, s, cr), max(d$ano))
  bind_rows(lapply(seq_len(length(lim) - 1), function(i)
    d %>% filter(ano >= lim[i], ano <= lim[i + 1]) %>% mutate(seg = i, instavel = (lim[i + 1] - lim[i]) <= 3)))
}
serie <- function(g, s) {
  d <- aj %>% filter(grupo == g, sexo == s)
  rot <- function(cr) ifelse(cr == "com redistribuicao", "com", "sem")
  list(pts = d %>% mutate(serie = rot(correcao)),
       lin = bind_rows(lapply(CORRECOES, function(cr) linhas_ajuste(d %>% filter(correcao == cr), g, s, cr) %>% mutate(serie = rot(cr)))),
       jp  = bind_rows(lapply(CORRECOES, function(cr) { j <- juncoes(g, s, cr); if (!length(j)) return(NULL)
         d %>% filter(correcao == cr, ano %in% j) %>% mutate(serie = rot(cr)) })))
}
tema_graf <- function() theme_minimal(base_family = FONTE, base_size = TAM) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.25),
        axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
        axis.ticks.x = element_line(colour = "grey35", linewidth = 0.3), axis.ticks.length = unit(0.8, "mm"),
        axis.text = element_text(size = TAM - 0.5, colour = "grey20"), axis.title = element_text(size = TAM, colour = "grey10"),
        plot.title = element_text(size = TAM + 0.5, face = "bold", hjust = 0, margin = margin(0, 0, 1.2, 0, "mm")),
        plot.title.position = "plot", legend.position = "bottom", legend.text = element_text(size = TAM),
        legend.key.width = unit(7, "mm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(1, 2, 1, 1, "mm"))
escala_cores <- function(L) list(
  scale_colour_manual(values = c(sem = COR_SEM, com = COR_COM), labels = c(sem = L$sem, com = L$com), breaks = c("sem", "com"), name = NULL),
  scale_fill_manual(values = c(sem = PT_SEM, com = PT_COM), labels = c(sem = L$sem, com = L$com), breaks = c("sem", "com"), name = NULL),
  scale_shape_manual(values = c(sem = 21, com = 24), labels = c(sem = L$sem, com = L$com), breaks = c("sem", "com"), name = NULL))

painel_sexo <- function(s, rotulo, letra, L, mostra_y) {
  S <- serie(P, s); jp_anos <- sort(unique(S$jp$ano)); a <- apc %>% filter(grupo == P, sexo == s)
  txt_aapc <- function(cr, lab) { r <- a %>% filter(correcao == cr)
    paste0(lab, ": AAPC ", fmt_num(r$aapc, L), "%", ifelse(r$p < 0.05, "*", "")) }
  ggplot() +
    (if (length(jp_anos)) geom_vline(xintercept = jp_anos, linetype = "22", colour = "grey45", linewidth = 0.3) else NULL) +
    (if (length(jp_anos)) annotate("text", x = jp_anos, y = 6.55, label = jp_anos, size = (TAM - 0.5) / .pt, colour = "grey30", family = FONTE) else NULL) +
    geom_point(data = S$pts, aes(ano, taxa, fill = serie, colour = serie, shape = serie), size = 1.25, stroke = 0.25) +
    geom_line(data = S$lin, aes(ano, ajustada, colour = serie, group = interaction(serie, seg), linetype = instavel), linewidth = 0.65) +
    (if (nrow(S$jp)) geom_point(data = S$jp, aes(ano, ajustada, colour = serie), shape = 21, fill = "white", size = 1.7, stroke = 0.6, show.legend = FALSE) else NULL) +
    annotate("text", x = 2024.4, y = c(2.42, 2.12), label = c(txt_aapc("sem redistribuicao", L$sem), txt_aapc("com redistribuicao", L$com)),
             hjust = 1, size = (TAM - 0.5) / .pt, family = FONTE, colour = "grey15") +
    scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "22"), guide = "none") + escala_cores(L) +
    scale_x_continuous(breaks = seq(2000, 2024, 6), limits = c(1999.3, 2024.7), expand = c(0, 0)) +
    scale_y_continuous(limits = c(1.95, 6.75), breaks = 2:6, expand = c(0, 0), labels = function(x) fmt_num(x, L, 0)) +
    labs(title = paste0(letra, "   ", rotulo), x = NULL, y = if (mostra_y) L$ytaxa else NULL) + tema_graf() +
    theme(axis.text.y = if (mostra_y) element_text(size = TAM - 0.5, colour = "grey20") else element_blank())
}
# Escala log com a mesma amplitude relativa nos paineis D-G
R_LOG <- max(sapply(GR4, function(g) { v <- aj %>% filter(grupo == g, sexo == "Ambos") %>% pull(taxa); v <- v[v > 0]; sqrt(max(v) / min(v)) })) * 1.12
painel_grupo <- function(g, rotulo, letra, L, mostra_y) {
  S <- serie(g, "Ambos"); v <- S$pts$taxa[S$pts$taxa > 0]
  lim <- sqrt(max(v) * min(v)) * c(1 / R_LOG, R_LOG)
  cand <- c(0.05, 0.1, 0.2, 0.3, 0.5, 1, 2, 3, 5, 10); br <- cand[cand >= lim[1] & cand <= lim[2]]
  ggplot() +
    geom_point(data = S$pts, aes(ano, taxa, fill = serie, colour = serie, shape = serie), size = 1.25, stroke = 0.25) +
    geom_line(data = S$lin, aes(ano, ajustada, colour = serie, group = interaction(serie, seg), linetype = instavel), linewidth = 0.65) +
    (if (nrow(S$jp)) geom_point(data = S$jp, aes(ano, ajustada, colour = serie), shape = 21, fill = "white", size = 1.7, stroke = 0.6, show.legend = FALSE) else NULL) +
    annotate("label", x = 1999.8, y = lim[2] / 1.04, label = paste0("n = ", fmt_num(n_obitos$n[n_obitos$grupo == g], L, 0), " ", L$obitos),
             hjust = 0, vjust = 1, size = (TAM - 0.5) / .pt, family = FONTE, colour = "grey30", fill = "white",
             label.size = 0, label.padding = unit(0.6, "mm")) +
    scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "22"), guide = "none") + escala_cores(L) +
    scale_x_continuous(breaks = c(2000, 2012, 2024), limits = c(1999.3, 2024.7), expand = c(0, 0)) +
    scale_y_log10(limits = lim, breaks = br, expand = c(0, 0),
                  labels = function(x) vapply(x, function(v) if (is.na(v)) NA_character_ else fmt_num(v, L, if (v < 1) 1 else 0), "")) +
    labs(title = paste0(letra, "   ", rotulo), x = NULL, y = if (mostra_y) L$ylog else NULL) + tema_graf()
}
figura1 <- function(L) {
  topo <- painel_sexo("Ambos", L$ambos, "A", L, TRUE) | painel_sexo("Masculino", L$masc, "B", L, FALSE) |
          painel_sexo("Feminino", L$fem, "C", L, FALSE)
  base <- painel_grupo(GR4[1], L$hl, "D", L, TRUE) | painel_grupo(GR4[2], L$lnh, "E", L, FALSE) |
          painel_grupo(GR4[3], L$mm, "F", L, FALSE) | painel_grupo(GR4[4], L$leu, "G", L, FALSE)
  rotx <- ggplot() + annotate("text", x = 0, y = 0, label = L$ano, size = TAM / .pt, family = FONTE) + theme_void()
  (topo / rotx / base / rotx) + plot_layout(heights = c(1, 0.06, 1, 0.06), guides = "collect") &
    theme(legend.position = "bottom", legend.text = element_text(size = TAM, family = FONTE), legend.margin = margin(0, 0, 0, 0))
}

# =============================================================================
# FIGURA 2
# =============================================================================
res <- read_excel(ARQ_ESPACIAL, sheet = "rmp_municipios") %>% mutate(cod = as.numeric(cod))
base_periodo <- function(p) malha %>% left_join(res %>% filter(periodo == p), by = "cod")
figura2 <- function(L) {
  d <- base_periodo("2000-2024")
  pA <- painel_mapa(d %>% mutate(classe = classe_rmp(rmp_eb_global, L)), COR_RMP, L$rmp_cl, paste0("A   ", L$sem), L$rmp_leg, L, extras = TRUE)
  pB <- painel_mapa(d %>% mutate(classe = classe_rmp(rmp_corr_eb_global, L)), COR_RMP, L$rmp_cl, paste0("B   ", L$com), L$rmp_leg, L)
  pC <- painel_mapa(base_periodo("2000-2004") %>% mutate(classe = classe_mal(pct_mal, L)), COR_MAL, L$mal_cl, "C   2000-2004", L$mal_leg, L, extras = TRUE)
  pD <- painel_mapa(base_periodo("2020-2024") %>% mutate(classe = classe_mal(pct_mal, L)), COR_MAL, L$mal_cl, "D   2020-2024", L$mal_leg, L)
  l1 <- (pA | pB) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
  l2 <- (pC | pD) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
  wrap_elements(full = l1) / wrap_elements(full = l2)
}

# =============================================================================
# FIGURA 3
# =============================================================================
sat_ag  <- read_excel(ARQ_SATSCAN, sheet = "aglomerados")
sat_mun <- read_excel(ARQ_SATSCAN, sheet = "municipios_aglom") %>% mutate(cod = as.numeric(cod))
rmb_uniao <- st_union(malha %>% filter(cod %in% RMB))

rotulo_aglom <- function(nome, n, L) ifelse(n == 1, nome,
  ifelse(L$suf == "pt", paste0(nome, " ", L$mais, " ", n - 1, " ", ifelse(n == 2, L$mun1, L$mun)),
         paste0(nome, " ", L$mais, " ", n - 1, " ", ifelse(n == 2, L$mun1, L$mun))))

classes_presentes <- function(L) {
  cl <- classe_rmp(sat_ag$rr, L); L$rmp_cl[L$rmp_cl %in% as.character(cl)]
}

mapa_aglom <- function(s, titulo, L, extras = FALSE) {
  ag <- sat_ag %>% filter(serie == s)
  mu <- sat_mun %>% filter(serie == s) %>% group_by(cod) %>% slice_min(cluster, n = 1, with_ties = FALSE) %>% ungroup() %>%
    left_join(ag %>% select(cluster, rr), by = "cluster") %>% mutate(classe = classe_rmp(rr, L))
  d <- malha %>% left_join(mu %>% select(cod, classe), by = "cod")
  cont <- do.call(rbind, lapply(seq_len(nrow(ag)), function(i) {
    cods <- sat_mun$cod[sat_mun$serie == s & sat_mun$cluster == ag$cluster[i]]
    g <- malha %>% filter(cod %in% cods) %>% st_buffer(50) %>% st_union() %>% st_buffer(-50)
    st_sf(cluster = ag$cluster[i], duracao = ifelse(ag$periodo_total[i], L$dur_cl[1], L$dur_cl[2]), geometry = g)
  }))
  cont$duracao <- factor(cont$duracao, levels = L$dur_cl)
  cont_ins <- suppressWarnings(st_intersection(cont, rmb_uniao)) %>% amplia()
  # Rotulo: sobre os municipios fora da RMB; se o aglomerado so tem municipios da RMB, na ampliacao
  lab <- do.call(rbind, lapply(seq_len(nrow(ag)), function(i) {
    cods <- sat_mun$cod[sat_mun$serie == s & sat_mun$cluster == ag$cluster[i]]
    fora <- setdiff(cods, RMB)
    g <- st_union(malha %>% filter(cod %in% if (length(fora)) fora else cods))
    p <- st_coordinates(suppressWarnings(st_point_on_surface(g)))
    if (!length(fora)) p <- amplia_xy(p[1], p[2])
    data.frame(cluster = ag$cluster[i], x = p[1], y = p[2])
  }))
  cores <- setNames(COR_RMP, L$rmp_cl)
  camadas <- camadas_rmb(L)
  ggplot() +
    geom_sf(data = d, aes(fill = classe), colour = "white", linewidth = LW_MUN) +
    camadas[[1]] + camadas[[2]] + camadas[[3]] + camadas[[4]] +
    geom_sf(data = amplia(d %>% filter(cod %in% RMB)), aes(fill = classe), colour = "white", linewidth = LW_MUN * 2) +
    geom_sf(data = amplia(st_sf(geometry = rmb_uniao)), fill = NA, colour = "grey40", linewidth = 0.15) +
    geom_sf(data = cont, aes(linetype = duracao), fill = NA, colour = "black", linewidth = 0.35) +
    geom_sf(data = cont_ins, aes(linetype = duracao), fill = NA, colour = "black", linewidth = 0.35, show.legend = FALSE) +
    ggrepel::geom_label_repel(data = lab, aes(x, y, label = cluster), size = (TAM - 0.5) / .pt, family = FONTE,
                              label.padding = unit(0.35, "mm"), label.size = 0.25, label.r = unit(0.3, "mm"),
                              min.segment.length = 0, box.padding = 0.35, segment.size = 0.25, seed = 2026) +
    rotulo_rmb(L) +
    (if (extras) escala_norte() else NULL) +
    scale_fill_manual(values = cores, limits = classes_presentes(L), na.value = "white", na.translate = FALSE,
                      name = L$rr_leg,
                      guide = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0.5,
                                           override.aes = list(colour = "grey55", linewidth = 0.15))) +
    scale_linetype_manual(values = setNames(c("solid", "22"), L$dur_cl), drop = FALSE, name = L$dur_leg,
                          guide = guide_legend(order = 2, nrow = 1, title.position = "top", title.hjust = 0,
                                               override.aes = list(fill = NA, linewidth = 0.4))) +
    coord_sf(xlim = lim_x, ylim = lim_y, expand = FALSE, datum = NA) +
    labs(title = titulo, x = NULL, y = NULL) + tema_mapa() + tema_legenda
}

painel_C <- function(L) {
  ag <- sat_ag %>% mutate(serie = factor(serie, levels = c("sem", "com"))) %>% arrange(serie, cluster)
  linhas <- list(); y <- 0
  for (s in c("sem", "com")) {
    y <- y + 1.1
    linhas[[length(linhas) + 1]] <- tibble(y = y, tipo = "titulo", texto = ifelse(s == "sem", L$sem, L$com))
    a <- ag %>% filter(serie == s)
    for (i in seq_len(nrow(a))) {
      y <- y + 1
      linhas[[length(linhas) + 1]] <- tibble(y = y, tipo = "aglom",
        texto = paste0(a$cluster[i], "  ", rotulo_aglom(a$nome[i], a$n_locais[i], L)),
        ini = a$ano_ini[i], fim = a$ano_fim[i], classe = as.character(classe_rmp(a$rr[i], L)),
        oe = paste0(fmt_num(a$obs[i], L, 0), " / ", fmt_num(a$esp[i], L, 1)),
        rr = fmt_num(a$rr[i], L, 2), p = fmt_num(a$p[i], L, 3))
    }
    y <- y + 0.9
  }
  ln <- bind_rows(linhas); lim_y <- c(max(ln$y) + 0.7, -0.4)
  ag_l <- ln %>% filter(tipo == "aglom"); ti <- ln %>% filter(tipo == "titulo")
  tema0 <- theme_void(base_family = FONTE, base_size = TAM) + theme(plot.margin = margin(0, 1, 0, 1, "mm"))
  esc_y <- scale_y_reverse(limits = rev(lim_y) * c(1, 1), expand = c(0, 0))
  p_lab <- ggplot() +
    geom_text(data = ag_l, aes(1, y, label = texto), hjust = 1, size = TAM / .pt, family = FONTE) +
    geom_text(data = ti, aes(1, y, label = texto), hjust = 1, size = (TAM + 0.5) / .pt, family = FONTE, fontface = "bold") +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + scale_y_reverse(limits = c(lim_y[1], lim_y[2]), expand = c(0, 0)) + tema0
  p_tl <- ggplot() +
    geom_vline(xintercept = seq(2000, 2024, 6), colour = "grey90", linewidth = 0.25) +
    geom_rect(data = ag_l, aes(xmin = 1999.6, xmax = 2024.4, ymin = y - 0.3, ymax = y + 0.3), fill = "#efefef") +
    geom_rect(data = ag_l, aes(xmin = ini - 0.4, xmax = fim + 0.4, ymin = y - 0.3, ymax = y + 0.3, fill = classe)) +
    annotate("text", x = 2012, y = 0, label = L$periodo_aglom, size = (TAM + 0.5) / .pt, family = FONTE, fontface = "bold") +
    scale_fill_manual(values = setNames(COR_RMP, L$rmp_cl), guide = "none") +
    scale_x_continuous(breaks = seq(2000, 2024, 6), limits = c(1999.4, 2024.6), expand = c(0, 0)) +
    scale_y_reverse(limits = c(lim_y[1], lim_y[2]), expand = c(0, 0)) + tema0 +
    theme(axis.text.x = element_text(size = TAM - 0.5, colour = "grey30", margin = margin(1, 0, 0, 0, "mm")),
          axis.ticks.x = element_line(colour = "grey50", linewidth = 0.25), axis.ticks.length.x = unit(0.8, "mm"))
  p_st <- ggplot() +
    geom_text(data = ag_l, aes(1.0, y, label = oe), hjust = 1, size = TAM / .pt, family = FONTE) +
    geom_text(data = ag_l, aes(1.42, y, label = rr), hjust = 1, size = TAM / .pt, family = FONTE, fontface = "bold") +
    geom_text(data = ag_l, aes(1.86, y, label = p), hjust = 1, size = TAM / .pt, family = FONTE) +
    annotate("text", x = c(1.0, 1.42, 1.86), y = 0, label = c(L$obs_esp, "RR", "p"), hjust = 1,
             size = (TAM + 0.5) / .pt, family = FONTE, fontface = "bold") +
    scale_x_continuous(limits = c(0.25, 1.9), expand = c(0, 0)) + scale_y_reverse(limits = c(lim_y[1], lim_y[2]), expand = c(0, 0)) + tema0
  (p_lab | p_tl | p_st) + plot_layout(widths = c(0.39, 0.35, 0.26))
}

figura3 <- function(L) {
  mapas <- (mapa_aglom("sem", paste0("A   ", L$sem), L, extras = TRUE) | mapa_aglom("com", paste0("B   ", L$com), L)) +
    plot_layout(guides = "collect") & theme(legend.position = "bottom", legend.box = "horizontal")
  tit_c <- ggplot() + annotate("text", x = 0, y = 0, label = paste0("C   ", L$painelC), hjust = 0,
                               size = (TAM + 0.5) / .pt, family = FONTE, fontface = "bold") +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + theme_void()
  wrap_elements(full = mapas) / tit_c / wrap_elements(full = painel_C(L)) + plot_layout(heights = c(1.25, 0.05, 0.95))
}

# =============================================================================
for (id in IDIOMAS) {
  L <- TXT[[id]]
  salva_figura(figura1(L), paste0("Fig1_tendencias_", L$suf), 150)
  salva_figura(figura2(L), paste0("Fig2_mapas_", L$suf), 190)
  salva_figura(figura3(L), paste0("Fig3_aglomerados_", L$suf), 200)
}
message("Pronto. Figuras em: ", DIR_FIGURAS)
