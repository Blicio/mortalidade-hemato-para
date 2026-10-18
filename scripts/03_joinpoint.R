# =============================================================================
# 03_joinpoint.R
# Regressao joinpoint das taxas padronizadas anuais e analises de sensibilidade
# que so mudam a serie ou a especificacao.
#
# Entrada : resultados_taxas.xlsx (script 02)
# Saida   : joinpoint_resultados.xlsx
#             tabela_artigo       TABELA 2 do manuscrito
#             sens_especificacao  TABELA S2 (minimo de 3 x minimo de 2 observacoes)
#             tabela_S1_parcial   colunas (a), (b) e (d) da TABELA S1 (a coluna (c) vem do script 04)
#             modelos, segmentos_apc, aapc, bic_por_k, juncao_pandemia, ajustados
#
# Cenarios
#   "2000-2024"                 analise principal (inclui a sensibilidade (b), grupo C81-C96 + D45-D47)
#   "sem 2020-2021"             sensibilidade (a)
#   "inicio em 2001"            sensibilidade (d), AAPC 2001-2024
#   "minimo 2 nas extremidades" sensibilidade (e)
#
# Corresponde a: Metodo (analise de tendencia; sensibilidade a, b, d, e),
#                Metodo suplementar (S1-S5), Tabela 2, Tabelas S1 e S2, Figura 1.
# Tempo: 3 a 6 minutos.
# =============================================================================
# Pasta de trabalho = pasta dos scripts (no RStudio, e ajustada sozinha)
if (!file.exists("00_config.R")) { .d <- Sys.getenv("DIR_SCRIPTS"); if (!nzchar(.d) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) .d <- dirname(rstudioapi::getSourceEditorContext()$path); if (nzchar(.d)) setwd(.d) }
source("00_config.R"); source("00_funcoes.R")
exige(ARQ_TAXAS, "02_taxas_padronizadas.R")
message("\n===== 03. Joinpoint =====")

taxas <- read_excel(ARQ_TAXAS, sheet = "taxas_padronizadas") %>% mutate(ano = as.integer(ano))

# ---- 1. Roda todos os estratos em cada cenario ----------------------------------------
estratos <- taxas %>% distinct(grupo, sexo, faixa_ampla, correcao)
cenarios <- c("2000-2024", "sem 2020-2021", "inicio em 2001", "minimo 2 nas extremidades")

saida <- list()
for (cen in cenarios) {
  message("Cenario: ", cen)
  est <- estratos
  # A serie iniciada em 2001 so entra na Tabela S1 (total e sexos, C81-C96)
  if (cen == "inicio em 2001") est <- est %>% filter(grupo == P, faixa_ampla == "Todas")
  for (i in seq_len(nrow(est))) {
    e <- est[i, ]
    d <- taxas %>% filter(grupo == e$grupo, sexo == e$sexo, faixa_ampla == e$faixa_ampla, correcao == e$correcao)
    if (cen == "sem 2020-2021")  d <- d %>% filter(!ano %in% c(2020L, 2021L))
    if (cen == "inicio em 2001") d <- d %>% filter(ano >= 2001L)
    r <- tendencia(d, min_fim = if (cen == "minimo 2 nas extremidades") 2 else MIN_FIM)
    id <- tibble(cenario = cen, grupo = e$grupo, sexo = e$sexo, faixa_ampla = e$faixa_ampla,
                 correcao = e$correcao, obitos_media_ano = mean(d$obitos))
    saida[[length(saida) + 1]] <- lapply(r, function(x) if (nrow(x) > 0) bind_cols(id[rep(1, nrow(x)), ], x) else NULL)
    if (i %% 50 == 0) message("  ", i, " de ", nrow(est), " estratos")
  }
}
junta <- function(nome) bind_rows(lapply(saida, `[[`, nome))
modelos   <- junta("modelo") %>%
  mutate(aviso = ifelse(obitos_media_ano < 10, "menos de 10 obitos/ano: estimativa instavel", ""))
segmentos <- junta("segmentos")
aapc      <- junta("aapc")
ajustados <- junta("ajustado")
bic_k     <- junta("bic_k")

# ---- 2. Conferencias ----------------------------------------------------------------------
pega <- function(tab, cen, corr, gr = P, sx = "Ambos", fa = "Todas")
  tab %>% filter(cenario == cen, grupo == gr, sexo == sx, faixa_ampla == fa, correcao == corr)
SEM <- "sem redistribuicao"; COM <- "com redistribuicao"

# Implementacao de referencia
m1 <- pega(modelos, "2000-2024", SEM); stopifnot(m1$K == 1, m1$juncoes == "2008")
s1 <- pega(segmentos, "2000-2024", SEM)
confere(s1$apc[1], 3.483, "APC 2000-2008 sem", 0.002); confere(s1$apc_inf[1], 1.805, "IC inf", 0.002)
confere(s1$apc_sup[1], 5.188, "IC sup", 0.002);        confere(s1$apc[2], 0.058, "APC 2008-2024 sem", 0.002)
a1 <- pega(aapc, "2000-2024", SEM)
confere(a1$aapc, 1.187, "AAPC sem", 0.002); confere(a1$aapc_inf, 0.742, "AAPC inf", 0.002); confere(a1$aapc_sup, 1.633, "AAPC sup", 0.002)
stopifnot(pega(modelos, "2000-2024", COM)$K == 0)
a2 <- pega(aapc, "2000-2024", COM)
confere(a2$aapc, -0.166, "AAPC com", 0.002); confere(a2$aapc_inf, -0.569, "inf", 0.002); confere(a2$aapc_sup, 0.239, "sup", 0.002)
confere(pega(segmentos, "sem 2020-2021", SEM)$apc[1], 3.460, "APC 2000-2008 sem pandemia", 0.002)
confere(pega(aapc, "sem 2020-2021", COM)$aapc, -0.086, "AAPC com, sem pandemia", 0.002)
j1 <- pega(segmentos, "2000-2024", SEM, fa = "0-19")
stopifnot(nrow(j1) == 2, j1$fim[1] == 2015)
confere(j1$apc[1], 1.895, "APC 0-19 2000-2015", 0.002); confere(j1$apc[2], -3.122, "APC 0-19 2015-2024", 0.002)
confere(pega(aapc, "2000-2024", COM, fa = "0-19")$aapc, 0.416, "AAPC 0-19 com", 0.002)
confere(pega(aapc, "minimo 2 nas extremidades", COM, fa = "0-19")$aapc, -1.58, "AAPC 0-19 com, minimo 2", 0.01)

# Valores publicados (Resultados e Tabela 2)
MM <- "Mieloma e plasmocitarias"
confere(pega(aapc, "2000-2024", COM, gr = MM)$aapc, 5.13, "AAPC mieloma com")
confere(pega(aapc, "2000-2024", COM, gr = MM)$aapc_inf, 3.40, "AAPC mieloma com inf")
confere(tail(pega(segmentos, "2000-2024", SEM, gr = MM)$apc, 1), 2.78, "APC mieloma desde 2004")
confere(tail(pega(segmentos, "2000-2024", COM, gr = MM)$apc, 1), 2.80, "APC mieloma desde 2007")
confere(pega(aapc, "2000-2024", COM, fa = "65+")$aapc, 0.93, "AAPC 65+ com")
confere(pega(aapc, "2000-2024", SEM, fa = "65+")$aapc, 2.19, "AAPC 65+ sem")
confere(pega(aapc, "2000-2024", COM, fa = "20-44")$aapc, -1.37, "AAPC 20-44 com")
confere(pega(aapc, "2000-2024", COM, gr = "Linfoma de Hodgkin")$aapc, -2.08, "AAPC Hodgkin com")
confere(pega(segmentos, "2000-2024", SEM, gr = "Leucemias")$apc[1], 4.96, "APC leucemias ate 2007")
confere(pega(segmentos, "2000-2024", SEM, sx = "Masculino")$apc[1], 1.67, "APC masc ate 2016")
confere(pega(aapc, "2000-2024", COM, sx = "Feminino")$aapc, 0.56, "AAPC fem com")

# Tabela S1, colunas (a), (b) e (d)
D4 <- "C81-C96 + D45-D47 (sensib.)"
confere(pega(aapc, "sem 2020-2021", SEM)$aapc, 1.24, "S1 (a) ambos sem")
confere(pega(aapc, "sem 2020-2021", COM, sx = "Feminino")$aapc, 0.61, "S1 (a) fem com")
confere(pega(aapc, "2000-2024", SEM, gr = D4)$aapc, 1.47, "S1 (b) ambos sem")
confere(pega(aapc, "2000-2024", COM, gr = D4, sx = "Feminino")$aapc, 0.81, "S1 (b) fem com")
confere(pega(aapc, "inicio em 2001", SEM)$aapc, 1.32, "S1 (d) ambos sem")
stopifnot(pega(modelos, "inicio em 2001", SEM)$juncoes == "2006",
          pega(modelos, "inicio em 2001", SEM, sx = "Masculino")$juncoes == "2015",
          pega(modelos, "inicio em 2001", SEM, sx = "Feminino")$juncoes == "2005")
confere(pega(aapc, "inicio em 2001", COM, sx = "Feminino")$aapc, -0.23, "S1 (d) fem com")
message("Conferencias passaram (referencia, Tabela 2 e Tabela S1).")

# ---- 3. Juncoes no periodo da pandemia ----------------------------------------------------
covid <- modelos %>% filter(cenario == "2000-2024", K > 0) %>% rowwise() %>%
  mutate(juncao_2019_2021 = any(as.integer(strsplit(juncoes, "; ")[[1]]) %in% 2019:2021)) %>%
  ungroup() %>% filter(juncao_2019_2021) %>% select(grupo, sexo, faixa_ampla, correcao, K, juncoes)

# ---- 4. Tabela 2 ---------------------------------------------------------------------------
tab_seg <- segmentos %>%
  mutate(txt = paste0(inicio, "-", fim, ifelse(fim - inicio <= 3, " (instavel)", ""), ": ",
                      fmt_ic(apc, apc_inf, apc_sup, p))) %>%
  group_by(cenario, grupo, sexo, faixa_ampla, correcao) %>%
  summarise(segmentos_apc = paste(txt, collapse = " | "), .groups = "drop")
tabela_artigo <- aapc %>%
  mutate(aapc_txt = fmt_ic(aapc, aapc_inf, aapc_sup, p)) %>%
  select(cenario, grupo, sexo, faixa_ampla, correcao, aapc_txt) %>%
  left_join(tab_seg, by = c("cenario", "grupo", "sexo", "faixa_ampla", "correcao")) %>%
  left_join(modelos %>% select(cenario, grupo, sexo, faixa_ampla, correcao, metodo, aviso),
            by = c("cenario", "grupo", "sexo", "faixa_ampla", "correcao")) %>%
  filter(cenario == "2000-2024", grupo != D4,
         (grupo == P & faixa_ampla == "Todas") | (grupo == P & sexo == "Ambos") |
         (sexo == "Ambos" & faixa_ampla == "Todas")) %>%
  mutate(correcao = factor(correcao, levels = CORRECOES),
         faixa_ampla = factor(faixa_ampla, levels = names(FAIXA_AMPLA)),
         sexo = factor(sexo, levels = SEXOS),
         grupo = factor(grupo, levels = names(GRUPOS))) %>%
  arrange(grupo, sexo, faixa_ampla, correcao) %>%
  mutate(across(c(grupo, correcao, faixa_ampla, sexo), as.character)) %>%
  select(grupo, sexo, faixa_ampla, correcao, segmentos_apc, aapc_2000_2024 = aapc_txt, metodo, aviso)

# ---- 5. Tabela S2: minimo de 3 x minimo de 2 observacoes ------------------------------------
sens_especificacao <- aapc %>%
  filter(cenario %in% c("2000-2024", "minimo 2 nas extremidades"), grupo != D4) %>%
  mutate(txt = fmt_ic(aapc, aapc_inf, aapc_sup, p),
         cenario = ifelse(cenario == "2000-2024", "minimo_3", "minimo_2")) %>%
  select(grupo, sexo, faixa_ampla, correcao, cenario, txt) %>%
  tidyr::pivot_wider(names_from = cenario, values_from = txt) %>%
  left_join(modelos %>% filter(cenario == "2000-2024") %>%
              select(grupo, sexo, faixa_ampla, correcao, juncoes_minimo_3 = juncoes),
            by = c("grupo", "sexo", "faixa_ampla", "correcao")) %>%
  left_join(modelos %>% filter(cenario == "minimo 2 nas extremidades") %>%
              select(grupo, sexo, faixa_ampla, correcao, juncoes_minimo_2 = juncoes),
            by = c("grupo", "sexo", "faixa_ampla", "correcao")) %>%
  mutate(mudou = juncoes_minimo_3 != juncoes_minimo_2) %>%
  select(grupo, sexo, faixa_ampla, correcao, juncoes_minimo_3, minimo_3, juncoes_minimo_2, minimo_2, mudou)

# ---- 6. Tabela S1, colunas (a), (b) e (d) ---------------------------------------------------
col_s1 <- function(cen, gr, rot) {
  aapc %>% filter(cenario == cen, grupo == gr, faixa_ampla == "Todas") %>%
    left_join(modelos %>% select(cenario, grupo, sexo, faixa_ampla, correcao, juncoes),
              by = c("cenario", "grupo", "sexo", "faixa_ampla", "correcao")) %>%
    transmute(sexo, correcao, coluna = rot,
              valor = paste0(fmt_ic(aapc, aapc_inf, aapc_sup, p),
                             ifelse(cen == "inicio em 2001" & juncoes != "", paste0(" [junção ", juncoes, "]"), "")))
}
tabela_S1_parcial <- bind_rows(
  col_s1("2000-2024", P, "Análise principal"),
  col_s1("sem 2020-2021", P, "(a) Sem 2020-2021"),
  col_s1("2000-2024", D4, "(b) Com D45-D47"),
  col_s1("inicio em 2001", P, "(d) Início em 2001"))

# ---- 7. Exporta ---------------------------------------------------------------------------------
write_xlsx(list(tabela_artigo = tabela_artigo, sens_especificacao = sens_especificacao,
                tabela_S1_parcial = tabela_S1_parcial, modelos = modelos, segmentos_apc = segmentos,
                aapc = aapc, bic_por_k = bic_k, juncao_pandemia = covid, ajustados = ajustados),
           ARQ_JOINPOINT)
tabela_artigo %>% filter(grupo == P, faixa_ampla == "Todas") %>%
  select(sexo, correcao, segmentos_apc, aapc_2000_2024) %>% print(n = Inf, width = Inf)
message("Estratos com juncao em 2019-2021: ", nrow(covid))
message("Gravado: ", ARQ_JOINPOINT)
