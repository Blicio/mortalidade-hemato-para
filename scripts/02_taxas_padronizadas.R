# =============================================================================
# 02_taxas_padronizadas.R
# Redistribuicao das causas mal definidas e taxas padronizadas por idade.
#
# Entrada : ARQ_PRINCIPAL (abas populacao, qualidade_estrato, obitos_cid_sexo_faixa, Taxas_PA)
# Saida   : resultados_taxas.xlsx
#             fatores_correcao   fator F por ano x sexo x faixa (equacao 1)
#             taxas_padronizadas taxas anuais por grupo x sexo x faixa ampla, sem e com correcao
#             taxas_especificas  taxas por faixa quinquenal (C81-C96)
#             tabela1_artigo     TABELA 1 do manuscrito (quinquenios 2000-2004 e 2020-2024)
#             tabela1_valores    mesma tabela em formato numerico
#           joinpoint_entrada.csv (para conferir no Joinpoint Regression Program do NCI)
#
# Corresponde a: Metodo (Qualidade da informacao; Taxas de mortalidade), Tabela 1,
#                texto dos Resultados (primeiro paragrafo).
# =============================================================================
# Pasta de trabalho = pasta dos scripts (no RStudio, e ajustada sozinha)
if (!file.exists("00_config.R")) { .d <- Sys.getenv("DIR_SCRIPTS"); if (!nzchar(.d) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) .d <- dirname(rstudioapi::getSourceEditorContext()$path); if (nzchar(.d)) setwd(.d) }
source("00_config.R"); source("00_funcoes.R")
exige(ARQ_PRINCIPAL, "(planilha principal do TabNet)")
message("\n===== 02. Taxas padronizadas =====")

# ---- 1. Leitura ------------------------------------------------------------------
pop <- read_excel(ARQ_PRINCIPAL, sheet = "populacao") %>%
  transmute(ano = as.integer(ano), sexo, faixa, pop = as.numeric(pop))
qe <- read_excel(ARQ_PRINCIPAL, sheet = "qualidade_estrato") %>%
  mutate(ano = as.integer(ano), obitos_total = as.numeric(obitos_total),
         obitos_r00_r99 = as.numeric(obitos_r00_r99))
oc <- read_excel(ARQ_PRINCIPAL, sheet = "obitos_cid_sexo_faixa") %>%
  mutate(ano = as.integer(ano), obitos = as.numeric(obitos))

stopifnot(nrow(pop) == 25 * 2 * 17)
stopifnot(sum(oc$obitos[oc$definicao == "principal"]) == 6911)
stopifnot(sum(oc$obitos[oc$definicao == "sensibilidade"]) == 451)
stopifnot(sum(qe$obitos_total) == 863916, sum(qe$obitos_r00_r99) == 103916)
stopifnot(!any(oc$faixa == "ignorada"))   # nenhum obito hematologico sem idade

# ---- 2. Fator de correcao ----------------------------------------------------------
fat <- calcula_fatores(qe)
fatores <- fat$fatores
stopifnot(nrow(fatores) == 25 * 2 * 17)
stopifnot(abs(sum(fatores$tot) - 863916) < 1e-6, abs(sum(fatores$mal) - 103916) < 1e-6)
stopifnot(all(fatores$fator >= 1))

# ---- 3. Taxas anuais ------------------------------------------------------------------
calcula_estrato <- function(sx, fa, gr) {
  ob <- aplica_fatores(oc %>% filter(cid %in% GRUPOS[[gr]]), fat)
  if (sx != "Ambos") ob <- ob %>% filter(sexo == sx)
  ob <- ob %>% group_by(ano, faixa) %>%
    summarise(obitos = sum(obitos), obitos_corr = sum(obitos_corr), .groups = "drop")
  p <- if (sx == "Ambos") pop else pop %>% filter(sexo == sx)
  p <- p %>% group_by(ano, faixa) %>% summarise(pop = sum(pop), .groups = "drop")
  base <- p %>% filter(faixa %in% FAIXA_AMPLA[[fa]]) %>%
    left_join(ob, by = c("ano", "faixa")) %>%
    mutate(obitos = coalesce(obitos, 0), obitos_corr = coalesce(obitos_corr, 0)) %>%
    left_join(SEGI, by = "faixa")
  sem <- base %>% group_by(ano) %>%
    summarise(dsr_dobson(obitos, pop, peso), populacao = sum(pop), .groups = "drop") %>%
    mutate(correcao = "sem redistribuicao")
  com <- base %>% group_by(ano) %>%
    summarise(dsr_dobson(obitos_corr, pop, peso), populacao = sum(pop), .groups = "drop") %>%
    mutate(correcao = "com redistribuicao")
  bind_rows(sem, com) %>% mutate(sexo = sx, faixa_ampla = fa, grupo = gr) %>%
    select(grupo, sexo, faixa_ampla, correcao, ano, everything())
}
esp <- expand_grid(sx = SEXOS, fa = names(FAIXA_AMPLA), gr = names(GRUPOS))
taxas <- bind_rows(lapply(seq_len(nrow(esp)), function(i) calcula_estrato(esp$sx[i], esp$fa[i], esp$gr[i])))

especificas <- bind_rows(lapply(SEXOS, function(sx) {
  ob <- aplica_fatores(oc %>% filter(definicao == "principal"), fat)
  if (sx != "Ambos") ob <- ob %>% filter(sexo == sx)
  ob <- ob %>% group_by(ano, faixa) %>%
    summarise(obitos = sum(obitos), obitos_corr = sum(obitos_corr), .groups = "drop")
  p <- if (sx == "Ambos") pop else pop %>% filter(sexo == sx)
  p %>% group_by(ano, faixa) %>% summarise(pop = sum(pop), .groups = "drop") %>%
    left_join(ob, by = c("ano", "faixa")) %>%
    mutate(across(c(obitos, obitos_corr), ~ coalesce(.x, 0)), sexo = sx,
           taxa_sem = 1e5 * obitos / pop, taxa_com = 1e5 * obitos_corr / pop)
})) %>% mutate(faixa = factor(faixa, levels = FAIXAS)) %>% arrange(sexo, ano, faixa)

# Conferencias contra a implementacao de referencia (calculo independente)
pega <- function(gr, sx, fa, corr, a, col) taxas %>%
  filter(grupo == gr, sexo == sx, faixa_ampla == fa, correcao == corr, ano == a) %>% pull(!!col)
confere(pega(P, "Ambos", "Todas", "sem redistribuicao", 2000L, "taxa_padr"), 3.2616, "TP 2000 sem", 0.001)
confere(pega(P, "Ambos", "Todas", "sem redistribuicao", 2000L, "ic95_inf"),  2.7449, "IC inf 2000", 0.001)
confere(pega(P, "Ambos", "Todas", "sem redistribuicao", 2000L, "ic95_sup"),  3.8420, "IC sup 2000", 0.001)
confere(pega(P, "Ambos", "Todas", "com redistribuicao", 2000L, "taxa_padr"), 4.4681, "TP 2000 com", 0.001)
confere(pega(P, "Ambos", "Todas", "sem redistribuicao", 2024L, "taxa_padr"), 4.0738, "TP 2024 sem", 0.001)
confere(pega(P, "Ambos", "Todas", "com redistribuicao", 2024L, "taxa_padr"), 4.3674, "TP 2024 com", 0.001)
confere(pega(P, "Ambos", "Todas", "com redistribuicao", 2024L, "obitos"),    402.28, "obitos corr 2024", 0.01)
confere(pega(P, "Masculino", "Todas", "com redistribuicao", 2000L, "taxa_padr"), 5.8029, "TP masc 2000", 0.001)
confere(pega(P, "Feminino",  "Todas", "com redistribuicao", 2024L, "taxa_padr"), 3.7315, "TP fem 2024", 0.001)
confere(pega(P, "Ambos", "65+", "com redistribuicao", 2024L, "taxa_padr"), 27.1557, "TP 65+ 2024", 0.001)
confere(pega("Leucemias", "Ambos", "Todas", "sem redistribuicao", 2024L, "taxa_padr"), 2.1001, "TP leucemias 2024", 0.001)
taxas_pa <- read_excel(ARQ_PRINCIPAL, sheet = "Taxas_PA", n_max = 25)
stopifnot(abs(taxas_pa[[5]][1] - 3.26) < 1e-9)    # aba Taxas_PA da planilha do TabNet

# ---- 4. Tabela 1: quinquenios extremos -------------------------------------------------
# Obitos e populacao somados nos cinco anos; padronizacao direta; IC95% de Dobson.
taxa_periodo <- function(gr, sx, fa, a0, a1) {
  ob <- aplica_fatores(oc %>% filter(cid %in% GRUPOS[[gr]], ano >= a0, ano <= a1), fat)
  if (sx != "Ambos") ob <- ob %>% filter(sexo == sx)
  ob <- ob %>% group_by(faixa) %>% summarise(obitos = sum(obitos), obitos_corr = sum(obitos_corr), .groups = "drop")
  p <- pop %>% filter(ano >= a0, ano <= a1)
  if (sx != "Ambos") p <- p %>% filter(sexo == sx)
  base <- p %>% group_by(faixa) %>% summarise(pop = sum(pop), .groups = "drop") %>%
    filter(faixa %in% FAIXA_AMPLA[[fa]]) %>%
    left_join(ob, by = "faixa") %>% mutate(across(c(obitos, obitos_corr), ~ coalesce(.x, 0))) %>%
    left_join(SEGI, by = "faixa")
  bind_rows(dsr_dobson(base$obitos, base$pop, base$peso) %>% mutate(correcao = "sem"),
            dsr_dobson(base$obitos_corr, base$pop, base$peso) %>% mutate(correcao = "com"))
}

linhas_t1 <- tribble(
  ~rotulo,                                             ~gr,                          ~sx,         ~fa,
  "Total",                                             P,                            "Ambos",     "Todas",
  "Masculino",                                         P,                            "Masculino", "Todas",
  "Feminino",                                          P,                            "Feminino",  "Todas",
  "0 a 19",                                            P,                            "Ambos",     "0-19",
  "20 a 44",                                           P,                            "Ambos",     "20-44",
  "45 a 64",                                           P,                            "Ambos",     "45-64",
  "65 ou mais",                                        P,                            "Ambos",     "65+",
  "Linfoma de Hodgkin (C81)",                          "Linfoma de Hodgkin",         "Ambos",     "Todas",
  "Linfomas não Hodgkin (C82-C86)",               "Linfomas nao Hodgkin",       "Ambos",     "Todas",
  "Mieloma e neoplasias de plasmócitos (C88, C90)", "Mieloma e plasmocitarias", "Ambos",     "Todas",
  "Leucemias (C91-C95)",                               "Leucemias",                  "Ambos",     "Todas",
  "Outras e não especificadas (C96)¹",       "Outras e nao especificadas", "Ambos",     "Todas")

n_total <- sum(oc$obitos[oc$definicao == "principal"])
tabela1_valores <- bind_rows(lapply(seq_len(nrow(linhas_t1)), function(i) {
  l <- linhas_t1[i, ]
  d <- oc %>% filter(cid %in% GRUPOS[[l$gr]], faixa %in% FAIXA_AMPLA[[l$fa]])
  if (l$sx != "Ambos") d <- d %>% filter(sexo == l$sx)
  n <- sum(d$obitos)
  r1 <- taxa_periodo(l$gr, l$sx, l$fa, 2000, 2004); r2 <- taxa_periodo(l$gr, l$sx, l$fa, 2020, 2024)
  tibble(variavel = l$rotulo, obitos = n, pct = 100 * n / n_total,
         tp_0004_sem = r1$taxa_padr[1], li_0004_sem = r1$ic95_inf[1], ls_0004_sem = r1$ic95_sup[1],
         tp_0004_com = r1$taxa_padr[2], li_0004_com = r1$ic95_inf[2], ls_0004_com = r1$ic95_sup[2],
         tp_2024_sem = r2$taxa_padr[1], li_2024_sem = r2$ic95_inf[1], ls_2024_sem = r2$ic95_sup[1],
         tp_2024_com = r2$taxa_padr[2], li_2024_com = r2$ic95_inf[2], ls_2024_com = r2$ic95_sup[2])
}))

tabela1_artigo <- tabela1_valores %>%
  transmute(`Variável` = variavel,
            `Óbitos, n (%)` = paste0(num(obitos, 0), " (", num(pct, 2), ")"),
            `2000-2004 sem correção` = fmt_taxa(tp_0004_sem, li_0004_sem, ls_0004_sem),
            `2000-2004 com correção` = fmt_taxa(tp_0004_com, li_0004_com, ls_0004_com),
            `2020-2024 sem correção` = fmt_taxa(tp_2024_sem, li_2024_sem, ls_2024_sem),
            `2020-2024 com correção` = fmt_taxa(tp_2024_com, li_2024_com, ls_2024_com))

# Conferencia com a Tabela 1 publicada
t1 <- function(lin, col) tabela1_valores[[col]][tabela1_valores$variavel == lin]
confere(t1("Total", "tp_0004_sem"), 3.35, "T1 total 2000-2004 sem"); confere(t1("Total", "li_0004_sem"), 3.12, "T1 IC inf")
confere(t1("Total", "ls_0004_sem"), 3.59, "T1 IC sup");              confere(t1("Total", "tp_0004_com"), 4.48, "T1 total 2000-2004 com")
confere(t1("Total", "tp_2024_sem"), 4.07, "T1 total 2020-2024 sem"); confere(t1("Total", "tp_2024_com"), 4.36, "T1 total 2020-2024 com")
confere(t1("Masculino", "tp_0004_sem"), 3.92, "T1 masc");            confere(t1("Feminino", "tp_2024_com"), 3.87, "T1 fem")
confere(t1("0 a 19", "obitos"), 1523, "T1 n 0-19", 0.5);             confere(t1("65 ou mais", "tp_0004_sem"), 14.26, "T1 65+ sem")
confere(t1("65 ou mais", "tp_2024_com"), 23.53, "T1 65+ com");       confere(t1("65 ou mais", "ls_0004_com"), 23.28, "T1 65+ IC sup")
confere(t1("Leucemias (C91-C95)", "obitos"), 4060, "T1 n leucemias", 0.5)
confere(t1("Mieloma e neoplasias de plasmócitos (C88, C90)", "tp_2024_sem"), 0.76, "T1 mieloma")
confere(t1("Linfoma de Hodgkin (C81)", "tp_2024_com"), 0.14, "T1 Hodgkin")
message("Conferencias passaram (implementacao de referencia e Tabela 1).")

# Proporcao de obitos por causa mal definida no estado (nota da Tabela 1)
prop_ano <- qe %>% group_by(ano) %>% summarise(pct = 100 * sum(obitos_r00_r99) / sum(obitos_total))
confere(prop_ano$pct[prop_ano$ano == 2000], 27.58, "% mal definidas 2000")
confere(prop_ano$pct[prop_ano$ano == 2024], 6.88, "% mal definidas 2024")

# ---- 5. Entrada para o Joinpoint do NCI (conferencia externa opcional) ---------------
jp_entrada <- taxas %>% filter(obitos > 0) %>%
  transmute(grupo, sexo, faixa_ampla, correcao, ano, taxa_padr = round(taxa_padr, 4), ep = round(ep, 4))
write.csv(jp_entrada, file.path(DIR_RESULTADOS, "joinpoint_entrada.csv"), row.names = FALSE)

# ---- 6. Exporta -------------------------------------------------------------------------
write_xlsx(list(fatores_correcao   = fatores,
                taxas_padronizadas = taxas,
                taxas_especificas  = especificas %>% mutate(faixa = as.character(faixa)),
                tabela1_artigo     = tabela1_artigo,
                tabela1_valores    = tabela1_valores,
                prop_mal_definidas = prop_ano,
                joinpoint_entrada  = jp_entrada), ARQ_TAXAS)
print(tabela1_artigo, n = Inf, width = Inf)
message("Gravado: ", ARQ_TAXAS)
