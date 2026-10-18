# =============================================================================
# 06_analise_espacial.R
# Analise espacial: RMP por padronizacao indireta, suavizacao bayesiana empirica,
# indice de Moran (ajuste de Assuncao e Reis), LISA com correcao de
# Benjamini-Hochberg e correlacao com a proporcao de causas mal definidas.
#
# Entrada : ARQ_PRINCIPAL (abas Faixa_5anos, populacao, obitos_municipio,
#           qualidade_municipal, obitos_cid_sexo_faixa) e resultados_taxas.xlsx (02)
#           malha_PA_2020.gpkg (baixada pelo geobr na primeira vez)
# Saida   : espacial_resultados.xlsx
#             rmp_municipios   O, E, RMP bruta, suavizadas, corrigida, % mal definidas,
#                              LISA, por municipio e periodo (base das Figuras 2, S3-S5)
#             tabela_S4_artigo TABELA S4
#             moran_global, lisa_resumo, lisa_resumo_sem_ajuste, correlacoes
#           rmp_municipios.gpkg (periodo completo, para QGIS)
#
# Corresponde a: Metodo (Analise espacial; sensibilidade espacial), Resultados
#                (autocorrelacao), Tabela S4, Figuras 2 e S3-S5.
# Os valores de p por permutacao dependem da semente (2026) e da versao do spdep.
# =============================================================================
# Pasta de trabalho = pasta dos scripts (no RStudio, e ajustada sozinha)
if (!file.exists("00_config.R")) { .d <- Sys.getenv("DIR_SCRIPTS"); if (!nzchar(.d) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) .d <- dirname(rstudioapi::getSourceEditorContext()$path); if (nzchar(.d)) setwd(.d) }
source("00_config.R"); source("00_funcoes.R")
carrega(c("sf", "spdep"))
exige(ARQ_TAXAS, "02_taxas_padronizadas.R")
message("\n===== 06. Analise espacial =====")
set.seed(2026)

# ---- 1. Leitura -------------------------------------------------------------------
fx <- read_excel(ARQ_PRINCIPAL, sheet = "Faixa_5anos")
col_faixas <- grep("^De ", names(fx), value = TRUE)
stopifnot(length(col_faixas) == 17)
pop_mun <- fx %>%
  select(cod = 2, ano = Ano, sexo = Sexo, all_of(col_faixas)) %>%      # coluna 2 = codigo de 6 digitos
  tidyr::pivot_longer(all_of(col_faixas), names_to = "rotulo", values_to = "pop") %>%
  mutate(faixa = FAIXAS[match(rotulo, col_faixas)], cod = as.numeric(as.character(cod)),
         cod = ifelse(cod == MOJUI, SANTAREM, cod), ano = as.integer(ano), pop = as.numeric(pop)) %>%
  group_by(cod, ano, sexo, faixa) %>% summarise(pop = sum(pop), .groups = "drop")
stopifnot(n_distinct(pop_mun$cod) == 143, !anyNA(pop_mun$cod))

pop_uf <- read_excel(ARQ_PRINCIPAL, sheet = "populacao") %>%
  transmute(ano = as.integer(ano), sexo, faixa, pop = as.numeric(pop))
ob_mun <- read_excel(ARQ_PRINCIPAL, sheet = "obitos_municipio") %>%
  transmute(ano = as.integer(ano), cod = as.numeric(cod_mun), municipio, obitos = as.numeric(obitos))
stopifnot(sum(ob_mun$obitos) == 6906, n_distinct(ob_mun$cod) == 143)
qual_mun <- read_excel(ARQ_PRINCIPAL, sheet = "qualidade_municipal") %>%
  filter(!is.na(cod_mun_analise)) %>%
  transmute(ano = as.integer(ano), cod = as.numeric(cod_mun_analise),
            tot = as.numeric(obitos_total), mal = as.numeric(obitos_r00_r99))
oc <- read_excel(ARQ_PRINCIPAL, sheet = "obitos_cid_sexo_faixa") %>%
  filter(definicao == "principal", sexo != "Ignorado") %>%
  mutate(ano = as.integer(ano), obitos = as.numeric(obitos))
fatores <- read_excel(ARQ_TAXAS, sheet = "fatores_correcao") %>%
  transmute(ano = as.integer(ano), sexo, faixa, fator = as.numeric(fator))
oc <- oc %>% left_join(fatores, by = c("ano", "sexo", "faixa")) %>% mutate(obitos_corr = obitos * fator)
stopifnot(!anyNA(oc$obitos_corr))

# ---- 2. Observados, esperados e RMP por periodo (equacao 3) -----------------------
ebest_global <- function(n, x) {          # mesmo calculo de spdep::EBest (Marshall)
  r <- n / x; m <- sum(n) / sum(x)
  s2 <- sum(x * (r - m)^2) / sum(x)
  a <- max(s2 - m / (sum(x) / length(x)), 0)
  m + (a / (a + m / x)) * (r - m)
}
calcula_periodo <- function(nome) {
  a0 <- PERIODOS[[nome]][1]; a1 <- PERIODOS[[nome]][2]
  taxas_uf <- oc %>% filter(ano >= a0, ano <= a1) %>% group_by(sexo, faixa) %>%
    summarise(obitos = sum(obitos), obitos_corr = sum(obitos_corr), .groups = "drop") %>%
    right_join(pop_uf %>% filter(ano >= a0, ano <= a1) %>% group_by(sexo, faixa) %>%
                 summarise(pop = sum(pop), .groups = "drop"), by = c("sexo", "faixa")) %>%
    mutate(across(c(obitos, obitos_corr), ~ coalesce(.x, 0)), taxa = obitos / pop, taxa_corr = obitos_corr / pop)
  esperados <- pop_mun %>% filter(ano >= a0, ano <= a1) %>% group_by(cod, sexo, faixa) %>%
    summarise(pop = sum(pop), .groups = "drop") %>%
    left_join(taxas_uf %>% select(sexo, faixa, taxa, taxa_corr), by = c("sexo", "faixa")) %>%
    group_by(cod) %>% summarise(E = sum(pop * taxa), E_corr = sum(pop * taxa_corr), pop = sum(pop), .groups = "drop")
  obs <- ob_mun %>% filter(ano >= a0, ano <= a1) %>% group_by(cod) %>% summarise(O = sum(obitos), .groups = "drop")
  qual <- qual_mun %>% filter(ano >= a0, ano <= a1) %>% group_by(cod) %>%
    summarise(tot = sum(tot), mal = sum(mal), .groups = "drop")
  esperados %>% left_join(obs, by = "cod") %>% left_join(qual, by = "cod") %>%
    mutate(O = coalesce(O, 0), pct_mal = 100 * mal / tot, fator_mun = tot / (tot - mal),
           O_corr = O * fator_mun, rmp = O / E, rmp_eb_global = ebest_global(O, E),
           rmp_corr = O_corr / E_corr, rmp_corr_eb_global = ebest_global(O_corr, E_corr), periodo = nome)
}
nomes <- ob_mun %>% distinct(cod, municipio)
rmp <- bind_rows(lapply(names(PERIODOS), calcula_periodo)) %>% left_join(nomes, by = "cod")

b <- rmp %>% filter(periodo == "2000-2024", cod == BELEM)
s <- rmp %>% filter(periodo == "2000-2024", cod == SANTAREM)
b1 <- rmp %>% filter(periodo == "2000-2004", cod == BELEM)
stopifnot(b$O == 2480, s$O == 336, b1$O == 461)
confere(b$E, 1436.0961, "E Belem", 0.01);           confere(b$rmp, 1.7269, "RMP Belem", 0.001)
confere(b$rmp_eb_global, 1.72401, "RMP EB Belem", 0.001); confere(b$pct_mal, 4.2254, "% mal def Belem", 0.001)
confere(b$rmp_corr, 1.59299, "RMP corrigida Belem", 0.001)
confere(s$E, 289.2192, "E Santarem", 0.01);         confere(s$rmp_eb_global, 1.15859, "RMP EB Santarem", 0.001)
confere(b1$E, 196.5733, "E Belem 2000-2004", 0.01); confere(b1$rmp_eb_global, 2.33199, "RMP EB Belem 2000-2004", 0.001)
confere(sum(rmp$E[rmp$periodo == "2000-2024"]), 6910, "soma E", 0.01)

# ---- 3. Malha e vizinhanca --------------------------------------------------------------
if (!file.exists(ARQ_MALHA)) {
  carrega("geobr")
  st_write(geobr::read_municipality(code_muni = "PA", year = 2020, showProgress = FALSE), ARQ_MALHA, quiet = TRUE)
}
malha_bruta <- st_read(ARQ_MALHA, quiet = TRUE)
nomes_ibge <- malha_bruta %>% st_drop_geometry() %>%
  transmute(cod = as.numeric(code_muni) %/% 10, nome = name_muni) %>% filter(cod != MOJUI)
malha <- malha_bruta %>% st_make_valid() %>%
  mutate(cod = as.numeric(code_muni) %/% 10, cod = ifelse(cod == MOJUI, SANTAREM, cod)) %>%
  group_by(cod) %>% summarise(.groups = "drop") %>% arrange(cod)
stopifnot(nrow(malha) == 143, setequal(malha$cod, nomes$cod))

viz <- poly2nb(malha, queen = TRUE)
message("Municipios sem vizinho: ", sum(card(viz) == 0))
pesos <- nb2listw(viz, style = "W", zero.policy = TRUE)
centro <- st_point_on_surface(st_transform(malha, CRS_PROJ))
dist_belem <- tibble(cod = malha$cod,
                     dist_belem_km = as.numeric(st_distance(centro, centro[malha$cod == BELEM, ])) / 1000)
rmp <- rmp %>% left_join(dist_belem, by = "cod")

# ---- 4. Suavizacao local, Moran global e LISA ------------------------------------------------
traduz <- c("High-High" = "Alto-Alto", "Low-Low" = "Baixo-Baixo", "High-Low" = "Alto-Baixo", "Low-High" = "Baixo-Alto")
NSIM_LISA <- 9999   # com 999, o menor p seria 0,001 e nada sobreviveria ao BH para 143 testes
resultados <- list(); moran <- list()
for (nome in names(PERIODOS)) {
  d <- malha %>% st_drop_geometry() %>% left_join(rmp %>% filter(periodo == nome), by = "cod")
  stopifnot(identical(d$cod, malha$cod))
  d$rmp_eb_local <- EBlocal(d$O, d$E, viz, zero.policy = TRUE)$est
  mi      <- EBImoran.mc(n = d$O, x = d$E, listw = pesos, nsim = 999, zero.policy = TRUE)
  mi_corr <- EBImoran.mc(n = d$O_corr, x = d$E_corr, listw = pesos, nsim = 999, zero.policy = TRUE)
  moran[[nome]] <- tibble(periodo = nome, moran_I = unname(mi$statistic), p = mi$p.value,
                          moran_I_corrigido = unname(mi_corr$statistic), p_corrigido = mi_corr$p.value)
  lm <- localmoran_perm(d$rmp_eb_global, pesos, nsim = NSIM_LISA, zero.policy = TRUE)
  col_p <- grep("Sim", grep("^Pr\\(z", colnames(lm), value = TRUE), value = TRUE)
  if (length(col_p) == 0) col_p <- grep("^Pr\\(z", colnames(lm), value = TRUE)
  quad <- as.character(attr(lm, "quadr")$mean)
  d$lisa_I <- lm[, "Ii"]; d$lisa_p <- lm[, col_p[1]]
  d$lisa_p_bh <- p.adjust(d$lisa_p, method = "BH")
  d$lisa_cluster <- ifelse(d$lisa_p_bh < 0.05, traduz[quad], "Nao significativo")
  d$lisa_cluster_sem_ajuste <- ifelse(d$lisa_p < 0.05, traduz[quad], "Nao significativo")
  resultados[[nome]] <- d
}
res <- bind_rows(resultados); moran <- bind_rows(moran)

correlacoes <- res %>% group_by(periodo) %>% summarise(
  rho_rmp_x_mal_definidas = cor.test(rmp_eb_global, pct_mal, method = "spearman", exact = FALSE)$estimate,
  p_mal_definidas         = cor.test(rmp_eb_global, pct_mal, method = "spearman", exact = FALSE)$p.value,
  rho_rmp_x_dist_belem    = cor.test(rmp_eb_global, dist_belem_km, method = "spearman", exact = FALSE)$estimate,
  p_dist_belem            = cor.test(rmp_eb_global, dist_belem_km, method = "spearman", exact = FALSE)$p.value,
  municipios_rmp_acima_1  = sum(rmp_eb_global > 1), .groups = "drop")
resumo_lisa <- res %>% count(periodo, lisa_cluster) %>%
  tidyr::pivot_wider(names_from = lisa_cluster, values_from = n, values_fill = 0)
resumo_lisa_sem_ajuste <- res %>% count(periodo, lisa_cluster_sem_ajuste) %>%
  tidyr::pivot_wider(names_from = lisa_cluster_sem_ajuste, values_from = n, values_fill = 0)

# Valores publicados (Moran e Spearman sao deterministicos; os p dependem das permutacoes)
mo <- function(p, col) moran[[col]][moran$periodo == p]
confere(mo("2000-2024", "moran_I"), 0.16, "Moran 2000-2024");          confere(mo("2000-2004", "moran_I"), 0.10, "Moran 2000-2004")
confere(mo("2000-2004", "moran_I_corrigido"), -0.06, "Moran 2000-2004 corrigido")
confere(mo("2020-2024", "moran_I"), 0.17, "Moran 2020-2024")
confere(correlacoes$rho_rmp_x_mal_definidas[correlacoes$periodo == "2000-2004"], -0.30, "Spearman 2000-2004")
message("Conferencias passaram.")

# ---- 5. Tabela S4 ----------------------------------------------------------------------------
fmt_p <- function(p) ifelse(p < 0.001, "< 0,001", num(p, 3))
lista <- function(per, tipo) {
  cods <- res$cod[res$periodo == per & res$lisa_cluster == tipo]
  if (!length(cods)) return("-")
  nm <- nomes_ibge$nome[match(cods, nomes_ibge$cod)]
  # ordem: Belem primeiro, depois pela RMP (como no texto)
  paste(nm[order(-res$rmp_eb_global[res$periodo == per & res$lisa_cluster == tipo])], collapse = ", ")
}
tabela_S4 <- moran %>% left_join(correlacoes, by = "periodo") %>%
  mutate(periodo = factor(periodo, levels = names(PERIODOS))) %>% arrange(periodo) %>%
  rowwise() %>%
  transmute(`Período` = as.character(periodo),
            `Moran sem correção (p)` = paste0(num(moran_I), " (", fmt_p(p), ")"),
            `Moran com correção (p)` = paste0(num(moran_I_corrigido), " (", fmt_p(p_corrigido), ")"),
            `LISA Alto-Alto` = lista(as.character(periodo), "Alto-Alto"),
            `LISA Baixo-Baixo` = lista(as.character(periodo), "Baixo-Baixo"),
            `Spearman RMP x % mal definidas, rho (p)` = paste0(num(rho_rmp_x_mal_definidas), " (", fmt_p(p_mal_definidas), ")")) %>%
  ungroup()

# ---- 6. Exporta -------------------------------------------------------------------------------
tabela_mun <- res %>%
  left_join(nomes_ibge, by = "cod") %>%
  select(periodo, cod, municipio, nome, pop, O, E, rmp, rmp_eb_global, rmp_eb_local,
         O_corr, E_corr, rmp_corr, rmp_corr_eb_global, pct_mal, dist_belem_km,
         lisa_I, lisa_p, lisa_p_bh, lisa_cluster, lisa_cluster_sem_ajuste) %>%
  arrange(periodo, desc(rmp_eb_global))
write_xlsx(list(rmp_municipios = tabela_mun, tabela_S4_artigo = tabela_S4, moran_global = moran,
                lisa_resumo = resumo_lisa, lisa_resumo_sem_ajuste = resumo_lisa_sem_ajuste,
                correlacoes = correlacoes), ARQ_ESPACIAL)
st_write(malha %>% left_join(res %>% filter(periodo == "2000-2024"), by = "cod"),
         file.path(DIR_RESULTADOS, "rmp_municipios.gpkg"), delete_dsn = TRUE, quiet = TRUE)
print(tabela_S4, width = Inf)
message("Gravado: ", ARQ_ESPACIAL)
