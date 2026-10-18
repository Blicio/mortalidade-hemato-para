# =============================================================================
# 08_satscan_resultados.R
# Le os resultados do SaTScan e monta a TABELA S5 e os dados da FIGURA 3.
#
# Entrada (em DIR_SATSCAN, gerados pelo SaTScan a partir dos .prm do script 07):
#   resultado_sem_correcao.col.txt / .gis.txt
#   resultado_com_correcao.col.txt / .gis.txt
#   e malha_PA_2020.gpkg (nomes dos municipios com acento)
# Saida   : satscan_resultados.xlsx
#             tabela_S5_artigo   TABELA S5 (aglomerados com p < 0,05 e municipios)
#             aglomerados        uma linha por aglomerado (painel C da Figura 3)
#             municipios_aglom   municipio x aglomerado (paineis A e B da Figura 3)
#
# Os arquivos ASCII do SaTScan nao tem cabecalho (PrintAsciiColumnHeaders=n).
# Ordem das colunas (SaTScan 10.3.3, Poisson espaco-tempo):
#   .col.txt: CLUSTER LOC_ID LATITUDE LONGITUDE RADIUS SPAN START_DATE END_DATE
#             NUMBER_LOC LLR P_VALUE OBSERVED EXPECTED ODE REL_RISK POPULATION GINI
#   .gis.txt: LOC_ID CLUSTER P_VALUE CLU_OBS CLU_EXP CLU_ODE CLU_RR CLU_POP
#             LOC_OBS LOC_EXP LOC_ODE LOC_RR LOC_POP LOC_LAT LOC_LONG GINI
# Corresponde a: Resultados (varredura), Tabela S5 e Figura 3.
# =============================================================================
# Pasta de trabalho = pasta dos scripts (no RStudio, e ajustada sozinha)
if (!file.exists("00_config.R")) { .d <- Sys.getenv("DIR_SCRIPTS"); if (!nzchar(.d) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) .d <- dirname(rstudioapi::getSourceEditorContext()$path); if (nzchar(.d)) setwd(.d) }
source("00_config.R"); source("00_funcoes.R")
carrega("sf")
exige(ARQ_MALHA, "06_analise_espacial.R")
message("\n===== 08. Resultados do SaTScan =====")

SERIES <- c(sem = "sem_correcao", com = "com_correcao")
for (s in SERIES) for (ext in c(".col.txt", ".gis.txt"))
  exige(file.path(DIR_SATSCAN, paste0("resultado_", s, ext)), paste0("SaTScan com scan_", s, ".prm (script 07)"))

nomes <- st_read(ARQ_MALHA, quiet = TRUE) %>% st_drop_geometry() %>%
  transmute(cod = as.numeric(code_muni) %/% 10, nome = name_muni) %>% filter(cod != MOJUI)

le_col <- function(s) {
  read.table(file.path(DIR_SATSCAN, paste0("resultado_", s, ".col.txt")), stringsAsFactors = FALSE,
             col.names = c("cluster", "loc_central", "lat", "lon", "raio_km", "span_km", "inicio", "fim",
                           "n_locais", "llr", "p", "obs", "esp", "ode", "rr", "pop", "gini")) %>%
    mutate(ano_ini = as.integer(substr(inicio, 1, 4)), ano_fim = as.integer(substr(fim, 1, 4)))
}
le_gis <- function(s) {
  read.table(file.path(DIR_SATSCAN, paste0("resultado_", s, ".gis.txt")), stringsAsFactors = FALSE,
             col.names = c("cod", "cluster", "p", "clu_obs", "clu_esp", "clu_ode", "clu_rr", "clu_pop",
                           "loc_obs", "loc_esp", "loc_ode", "loc_rr", "loc_pop", "lat", "lon", "gini"))
}

aglom <- bind_rows(lapply(names(SERIES), function(k) le_col(SERIES[k]) %>% mutate(serie = k)))
munic <- bind_rows(lapply(names(SERIES), function(k) le_gis(SERIES[k]) %>% mutate(serie = k)))

# Apenas aglomerados significativos
aglom <- aglom %>% filter(p < 0.05) %>%
  left_join(nomes, by = c("loc_central" = "cod")) %>%
  mutate(periodo_total = ano_ini == 2000 & ano_fim == 2024,
         tipo = ifelse(rr > 1, "Risco alto", "Risco baixo"),
         rotulo = ifelse(n_locais == 1, nome,
                         paste0(nome, " e mais ", n_locais - 1, ifelse(n_locais == 2, " município", " municípios"))))
munic <- munic %>% semi_join(aglom, by = c("serie", "cluster")) %>% left_join(nomes, by = "cod")

# Conferencias com os Resultados e a Figura 3
ag <- function(s, k, col) aglom[[col]][aglom$serie == s & aglom$cluster == k]
stopifnot(sum(aglom$serie == "sem") == 5, sum(aglom$serie == "com") == 6)
confere(ag("sem", 1, "rr"), 2.14, "RR Belem sem"); confere(ag("com", 1, "rr"), 1.95, "RR Belem com")
confere(ag("sem", 5, "rr"), 1.27, "RR Ananindeua/Barcarena sem"); confere(ag("com", 5, "rr"), 1.38, "idem com")
stopifnot(ag("sem", 2, "n_locais") == 49, ag("com", 2, "n_locais") == 51, ag("com", 4, "n_locais") == 18)
stopifnot(ag("com", 3, "ano_fim") == 2002, ag("com", 6, "obs") == 2)
confere(ag("com", 6, "esp"), 22.2, "esperados Marajo", 0.05)
message("Conferencias passaram.")

# ---- Tabela S5 --------------------------------------------------------------------------
# Nomes regionais atribuidos pelos autores a partir da localizacao dos municipios
REGIAO <- tribble(~serie, ~cluster, ~regiao,
  "sem", 1, "Belém",                     "sem", 2, "Sudeste paraense e entorno",
  "sem", 3, "Nordeste paraense",               "sem", 4, "Região do Guamá",
  "sem", 5, "Ananindeua e Barcarena",
  "com", 1, "Belém",                     "com", 2, "Sudeste paraense e entorno",
  "com", 3, "Nordeste paraense",               "com", 4, "Região do Guamá",
  "com", 5, "Ananindeua e Barcarena",          "com", 6, "Marajó e Baixo Tocantins")
lista_mun <- munic %>% group_by(serie, cluster) %>%
  summarise(municipios = paste(ordena_nomes(nome), collapse = ", "), .groups = "drop")
tabela_S5 <- aglom %>% left_join(REGIAO, by = c("serie", "cluster")) %>% left_join(lista_mun, by = c("serie", "cluster")) %>%
  mutate(serie = factor(serie, levels = c("sem", "com"))) %>% arrange(serie, cluster) %>%
  transmute(`Série` = ifelse(serie == "sem", "Sem correção", "Com correção"),
            `Nº` = cluster,
            Aglomerado = ifelse(n_locais > 2 & !is.na(regiao), paste0(regiao, " (", n_locais, " municípios)"), regiao),
            `Período` = paste0(ano_ini, "-", ano_fim), Tipo = tipo,
            `Municípios incluídos` = municipios,
            `Obs.` = obs, `Esp.` = round(esp, 1), RR = rr, p = p)

write_xlsx(list(tabela_S5_artigo = tabela_S5, aglomerados = aglom, municipios_aglom = munic), ARQ_SATSCAN)
print(tibble::as_tibble(tabela_S5) %>% select(-`Municípios incluídos`), n = Inf)
message("Gravado: ", ARQ_SATSCAN)
