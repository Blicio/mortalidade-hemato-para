# =============================================================================
# 04_sensibilidade_C76_C80.R
# Sensibilidade (c): redistribuicao adicional dos obitos por neoplasias de
# localizacao mal definida, secundaria ou nao especificada (C76-C80) entre os
# demais obitos por neoplasia (capitulo II), e montagem da TABELA S1.
#
#   G = obitos_cap_II / (obitos_cap_II - obitos_C76_C80), por ano x sexo x faixa
#   serie sem correcao: obitos x G
#   serie com correcao: obitos x F x G   (F = fator das causas mal definidas, equacao 1)
#
# Entrada : ARQ_PRINCIPAL (abas obitos_cap2_faixa, populacao, qualidade_estrato,
#           obitos_cid_sexo_faixa) e joinpoint_resultados.xlsx (script 03)
# Saida   : sensibilidade_resultados.xlsx
#             tabela_S1_artigo  TABELA S1 completa (analise principal e colunas a-d)
#             taxas_C76_C80     taxas anuais com a redistribuicao adicional
#             fator_G           fator de redistribuicao de C76-C80
#             aapc_C76_C80, modelos_C76_C80
#
# Corresponde a: Metodo (Analise de sensibilidade, item c) e Tabela S1.
# =============================================================================
# Pasta de trabalho = pasta dos scripts (no RStudio, e ajustada sozinha)
if (!file.exists("00_config.R")) { .d <- Sys.getenv("DIR_SCRIPTS"); if (!nzchar(.d) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) .d <- dirname(rstudioapi::getSourceEditorContext()$path); if (nzchar(.d)) setwd(.d) }
source("00_config.R"); source("00_funcoes.R")
exige(ARQ_JOINPOINT, "03_joinpoint.R")
message("\n===== 04. Sensibilidade C76-C80 e Tabela S1 =====")

# ---- 1. Leitura -------------------------------------------------------------------
pop <- read_excel(ARQ_PRINCIPAL, sheet = "populacao") %>%
  transmute(ano = as.integer(ano), sexo, faixa, pop = as.numeric(pop))
qe <- read_excel(ARQ_PRINCIPAL, sheet = "qualidade_estrato") %>%
  mutate(ano = as.integer(ano), obitos_total = as.numeric(obitos_total), obitos_r00_r99 = as.numeric(obitos_r00_r99))
oc <- read_excel(ARQ_PRINCIPAL, sheet = "obitos_cid_sexo_faixa") %>%
  mutate(ano = as.integer(ano), obitos = as.numeric(obitos)) %>% filter(definicao == "principal")
cap2 <- read_excel(ARQ_PRINCIPAL, sheet = "obitos_cap2_faixa") %>%
  mutate(ano = as.integer(ano), obitos_cap_II = as.numeric(obitos_cap_II), obitos_C76_C80 = as.numeric(obitos_C76_C80)) %>%
  filter(faixa %in% FAIXAS)

# ---- 2. Fatores ---------------------------------------------------------------------
fat <- calcula_fatores(qe)
fator_G <- cap2 %>% mutate(G = obitos_cap_II / (obitos_cap_II - obitos_C76_C80))
G_ambos <- cap2 %>% group_by(ano, faixa) %>%      # so para o obito hematologico de sexo ignorado
  summarise(G = sum(obitos_cap_II) / sum(obitos_cap_II - obitos_C76_C80), .groups = "drop")
stopifnot(nrow(fator_G) == 25 * 2 * 17, all(fator_G$G >= 1))

ob <- aplica_fatores(oc, fat)
ob <- bind_rows(ob %>% filter(sexo != "Ignorado") %>% left_join(fator_G %>% select(ano, sexo, faixa, G), by = c("ano", "sexo", "faixa")),
                ob %>% filter(sexo == "Ignorado") %>% left_join(G_ambos, by = c("ano", "faixa"))) %>%
  mutate(obitos_G = obitos * G, obitos_FG = obitos_corr * G)
stopifnot(!anyNA(ob$G))

# ---- 3. Taxas e joinpoint ----------------------------------------------------------------
serie <- function(sx, var) {
  d <- if (sx == "Ambos") ob else ob %>% filter(sexo == sx)
  d <- d %>% group_by(ano, faixa) %>% summarise(o = sum(.data[[var]]), .groups = "drop")
  p <- if (sx == "Ambos") pop else pop %>% filter(sexo == sx)
  p %>% group_by(ano, faixa) %>% summarise(pop = sum(pop), .groups = "drop") %>%
    left_join(d, by = c("ano", "faixa")) %>% mutate(o = coalesce(o, 0)) %>%
    left_join(SEGI, by = "faixa") %>% group_by(ano) %>%
    summarise(dsr_dobson(o, pop, peso), populacao = sum(pop), .groups = "drop")
}
res <- list(); taxas_c <- list()
for (sx in SEXOS) for (cr in CORRECOES) {
  d <- serie(sx, if (cr == "sem redistribuicao") "obitos_G" else "obitos_FG")
  r <- tendencia(d)
  id <- tibble(sexo = sx, correcao = cr)
  taxas_c[[length(taxas_c) + 1]] <- bind_cols(id[rep(1, nrow(d)), ], d)
  res[[length(res) + 1]] <- list(aapc = bind_cols(id, r$aapc), modelo = bind_cols(id, r$modelo))
}
taxas_C76 <- bind_rows(taxas_c)
aapc_c   <- bind_rows(lapply(res, `[[`, "aapc"))
modelo_c <- bind_rows(lapply(res, `[[`, "modelo"))

pc <- function(sx, cr) aapc_c %>% filter(sexo == sx, correcao == cr)
confere(pc("Ambos", "sem redistribuicao")$aapc, 1.20, "S1 (c) ambos sem")
confere(pc("Ambos", "sem redistribuicao")$aapc_inf, 0.72, "S1 (c) ambos sem inf")
confere(pc("Ambos", "com redistribuicao")$aapc, -0.32, "S1 (c) ambos com")
confere(pc("Masculino", "sem redistribuicao")$aapc, 0.57, "S1 (c) masc sem")
confere(pc("Masculino", "com redistribuicao")$aapc, -0.37, "S1 (c) masc com")
confere(pc("Feminino", "sem redistribuicao")$aapc, 1.67, "S1 (c) fem sem")
confere(pc("Feminino", "com redistribuicao")$aapc_sup, 1.22, "S1 (c) fem com sup")
message("Conferencias da coluna (c) passaram.")

# ---- 4. Tabela S1 completa -----------------------------------------------------------------
parcial <- read_excel(ARQ_JOINPOINT, sheet = "tabela_S1_parcial")
col_c <- aapc_c %>% transmute(sexo, correcao, coluna = "(c) Com C76-C80",
                              valor = fmt_ic(aapc, aapc_inf, aapc_sup, p))
ordem_col <- c("Análise principal", "(a) Sem 2020-2021", "(b) Com D45-D47", "(c) Com C76-C80", "(d) Início em 2001")
tabela_S1 <- bind_rows(parcial, col_c) %>%
  mutate(coluna = factor(coluna, levels = ordem_col)) %>%
  tidyr::pivot_wider(names_from = coluna, values_from = valor) %>%
  mutate(sexo = factor(sexo, levels = SEXOS), correcao = factor(correcao, levels = CORRECOES)) %>%
  arrange(sexo, correcao) %>%
  transmute(Sexo = recode(as.character(sexo), "Ambos" = "Ambos os sexos"),
            `Série` = ifelse(correcao == "sem redistribuicao", "Sem correção", "Com correção"),
            across(all_of(ordem_col)))

# ---- 5. Exporta --------------------------------------------------------------------------------
write_xlsx(list(tabela_S1_artigo = tabela_S1, aapc_C76_C80 = aapc_c, modelos_C76_C80 = modelo_c,
                taxas_C76_C80 = taxas_C76, fator_G = fator_G), ARQ_SENSIB)
print(tabela_S1, width = Inf)
message("Gravado: ", ARQ_SENSIB)
