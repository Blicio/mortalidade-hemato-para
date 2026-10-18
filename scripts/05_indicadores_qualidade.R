# =============================================================================
# 05_indicadores_qualidade.R
# Indicadores adicionais de qualidade da informacao (TABELA S3) e proporcao de
# obitos por causa mal definida por faixa etaria (dados da FIGURA S1).
#
# Entrada : ARQ_PRINCIPAL (abas obitos_cid_sexo_faixa e qualidade_estrato)
# Saida   : qualidade_resultados.xlsx
#             tabela_S3_artigo  TABELA S3 (obitos por mieloma, categorias sem
#                               especificacao, distribuicao das leucemias e idade
#                               media aproximada, por quinquenio)
#             mal_definidas_faixa  % R00-R99 por ano e faixa ampla (Figura S1)
#
# Idade media aproximada: ponto medio de cada faixa quinquenal (82,5 anos para
# 80 ou mais), ponderado pelo numero de obitos; nao padronizada.
# Corresponde a: Metodo (Qualidade da informacao, ultimo periodo), Tabela S3, Figura S1.
# =============================================================================
# Pasta de trabalho = pasta dos scripts (no RStudio, e ajustada sozinha)
if (!file.exists("00_config.R")) { .d <- Sys.getenv("DIR_SCRIPTS"); if (!nzchar(.d) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) .d <- dirname(rstudioapi::getSourceEditorContext()$path); if (nzchar(.d)) setwd(.d) }
source("00_config.R"); source("00_funcoes.R")
message("\n===== 05. Indicadores de qualidade =====")

oc <- read_excel(ARQ_PRINCIPAL, sheet = "obitos_cid_sexo_faixa") %>%
  mutate(ano = as.integer(ano), obitos = as.numeric(obitos)) %>% filter(definicao == "principal")
qe <- read_excel(ARQ_PRINCIPAL, sheet = "qualidade_estrato") %>%
  mutate(ano = as.integer(ano), obitos_total = as.numeric(obitos_total), obitos_r00_r99 = as.numeric(obitos_r00_r99))

oc <- oc %>% mutate(quinquenio = quinquenio_de(ano),
                    meio = c(seq(2.5, 77.5, 5), 82.5)[match(faixa, FAIXAS)])

# ---- 1. Tabela S3 ----------------------------------------------------------------------
MM <- GRUPOS[["Mieloma e plasmocitarias"]]; LEU <- GRUPOS[["Leucemias"]]; LNH <- GRUPOS[["Linfomas nao Hodgkin"]]
soma <- function(d, cond) sum(d$obitos[cond])
idade <- function(d, cids) { s <- d$cid %in% cids; sum(d$obitos[s] * d$meio[s]) / sum(d$obitos[s]) }

ind <- oc %>% group_by(quinquenio) %>% group_modify(function(d, k) tibble(
  `Neoplasias hematológicas (C81-C96)`               = sum(d$obitos),
  `Mieloma e neoplasias de plasmócitos (C88, C90)`    = soma(d, d$cid %in% MM),
  `Mieloma, sexo masculino`                               = soma(d, d$cid %in% MM & d$sexo == "Masculino"),
  `Mieloma, sexo feminino`                                = soma(d, d$cid %in% MM & d$sexo == "Feminino"),
  `Mieloma, 45 a 64 anos`                                 = soma(d, d$cid %in% MM & d$faixa %in% FAIXA_AMPLA[["45-64"]]),
  `Mieloma, 65 anos ou mais`                              = soma(d, d$cid %in% MM & d$faixa %in% FAIXA_AMPLA[["65+"]]),
  `Leucemia não especificada (C95), % das leucemias`  = 100 * soma(d, d$cid == "C95") / soma(d, d$cid %in% LEU),
  `Outros tipos e não especificados (C85), % dos linfomas não Hodgkin` = 100 * soma(d, d$cid == "C85") / soma(d, d$cid %in% LNH),
  `C85, C95 e C96, % de todas as neoplasias hematológicas` = 100 * soma(d, d$cid %in% c("C85", "C95", "C96")) / sum(d$obitos),
  `Linfoide (C91)`                                        = 100 * soma(d, d$cid == "C91") / soma(d, d$cid %in% LEU),
  `Mieloide (C92)`                                        = 100 * soma(d, d$cid == "C92") / soma(d, d$cid %in% LEU),
  `Linfoma de Hodgkin (C81)`                              = idade(d, "C81"),
  `Linfomas não Hodgkin (C82-C86)`                   = idade(d, LNH),
  `Mieloma e neoplasias de plasmócitos (C88, C90) `  = idade(d, MM),
  `Leucemias (C91-C95)`                                   = idade(d, LEU))) %>% ungroup()

longa <- ind %>% tidyr::pivot_longer(-quinquenio, names_to = "indicador", values_to = "valor")
blocos <- c(rep("Óbitos, n", 6), rep("Categorias sem especificação, %", 3),
            rep("Distribuição das leucemias, %", 2), rep("Idade média aproximada ao óbito, anos", 4))
ordem <- names(ind)[-1]
tabela_S3 <- longa %>%
  mutate(txt = ifelse(indicador %in% ordem[1:6], num(valor, 0), num(valor, 1))) %>%
  select(-valor) %>% tidyr::pivot_wider(names_from = quinquenio, values_from = txt) %>%
  mutate(bloco = blocos[match(indicador, ordem)], indicador = trimws(indicador)) %>%
  select(bloco, Indicador = indicador, all_of(names(QUINQUENIOS)))

v <- function(col, q) ind[[col]][ind$quinquenio == q]
confere(v("Neoplasias hematológicas (C81-C96)", "2005-2009"), 1221, "S3 n 2005-2009", 0.5)
confere(v("Mieloma e neoplasias de plasmócitos (C88, C90)", "2020-2024"), 321, "S3 mieloma 2020-2024", 0.5)
confere(v("Mieloma, 65 anos ou mais", "2010-2014"), 91, "S3 mieloma 65+", 0.5)
confere(v("Leucemia não especificada (C95), % das leucemias", "2000-2004"), 27.9, "S3 C95", 0.06)
confere(v("C85, C95 e C96, % de todas as neoplasias hematológicas", "2020-2024"), 26.4, "S3 inespecificas", 0.06)
confere(v("Mieloide (C92)", "2010-2014"), 29.0, "S3 C92", 0.06)
confere(v("Leucemias (C91-C95)", "2020-2024"), 47.1, "S3 idade leucemias", 0.06)
confere(v("Linfoma de Hodgkin (C81)", "2000-2004"), 51.5, "S3 idade Hodgkin", 0.06)

# ---- 2. Figura S1: % de mal definidas por faixa ampla -------------------------------------
fat <- calcula_fatores(qe)$fatores
amplas <- names(FAIXA_AMPLA)[-1]
mal_faixa <- fat %>%
  mutate(faixa_ampla = amplas[vapply(faixa, function(f) which(vapply(FAIXA_AMPLA[-1], function(x) f %in% x, logical(1))), integer(1))]) %>%
  group_by(ano, faixa_ampla) %>% summarise(pct = 100 * sum(mal) / sum(tot), .groups = "drop")
mf <- function(a, f) mal_faixa$pct[mal_faixa$ano == a & mal_faixa$faixa_ampla == f]
confere(mf(2000, "0-19"), 19.6, "S1 0-19 2000", 0.06); confere(mf(2024, "0-19"), 5.6, "S1 0-19 2024", 0.06)
confere(mf(2000, "65+"), 35.3, "S1 65+ 2000", 0.06);   confere(mf(2024, "65+"), 7.4, "S1 65+ 2024", 0.06)
message("Conferencias passaram (Tabela S3 e Figura S1).")

write_xlsx(list(tabela_S3_artigo = tabela_S3, indicadores_valores = ind, mal_definidas_faixa = mal_faixa), ARQ_QUALIDADE)
print(tabela_S3, n = Inf, width = Inf)
message("Gravado: ", ARQ_QUALIDADE)
