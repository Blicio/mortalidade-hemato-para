# =============================================================================
# 07_satscan_entrada.R
# Arquivos de entrada e de parametros da varredura espaco-temporal de Kulldorff
# (SaTScan 10.3.3, Poisson discreto, analise retrospectiva).
#
# Entrada : ARQ_PRINCIPAL (Faixa_5anos, populacao, obitos_municipio,
#           qualidade_municipal, obitos_cid_sexo_faixa), resultados_taxas.xlsx (02),
#           malha_PA_2020.gpkg (06) e 07_satscan_parametros_modelo.prm (nesta pasta)
# Saida (em DIR_SATSCAN):
#   obitos.cas               municipio, obitos, ano
#   obitos_corrigidos.cas    municipio, round(obitos x F do municipio no quinquenio), ano
#   esperados.pop            municipio, ano, obitos esperados (padronizacao indireta
#                            com as taxas estaduais do proprio ano, por sexo e faixa)
#   esperados_corrigidos.pop idem, com as taxas estaduais corrigidas (fator F estadual)
#   municipios.geo           municipio, latitude, longitude (centroide calculado na
#                            projecao policonica, SIRGAS 2000)
#   scan_sem_correcao.prm e scan_com_correcao.prm
#
# Depois de rodar: abra o SaTScan, File > Open Session, escolha cada .prm e
# clique em Execute. Os resultados (resultado_*.txt, .col.txt e .gis.txt) ficam
# na mesma pasta e sao lidos pelo script 08.
#
# Parametros (iguais nos dois .prm): espaco-tempo com aglomerados puramente
# espaciais; risco alto e baixo; janela espacial ate 25% da populacao em risco
# (aqui, os obitos esperados); janela temporal de 1 ano ate 50% do periodo;
# unidade de tempo de 1 ano; 999 replicacoes de Monte Carlo; aglomerados
# secundarios sem sobreposicao geografica.
# Corresponde a: Metodo (varredura), Metodo suplementar, Tabela S5 e Figura 3.
# =============================================================================
# Pasta de trabalho = pasta dos scripts (no RStudio, e ajustada sozinha)
if (!file.exists("00_config.R")) { .d <- Sys.getenv("DIR_SCRIPTS"); if (!nzchar(.d) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) .d <- dirname(rstudioapi::getSourceEditorContext()$path); if (nzchar(.d)) setwd(.d) }
source("00_config.R"); source("00_funcoes.R")
carrega("sf")
exige(ARQ_TAXAS, "02_taxas_padronizadas.R")
exige(ARQ_MALHA, "06_analise_espacial.R")
message("\n===== 07. Entradas do SaTScan =====")
dir.create(DIR_SATSCAN, showWarnings = FALSE, recursive = TRUE)

# ---- 1. Leitura ------------------------------------------------------------------
fx <- read_excel(ARQ_PRINCIPAL, sheet = "Faixa_5anos")
col_faixas <- grep("^De ", names(fx), value = TRUE)
pop_mun <- fx %>%
  select(cod = 2, ano = Ano, sexo = Sexo, all_of(col_faixas)) %>%
  tidyr::pivot_longer(all_of(col_faixas), names_to = "rotulo", values_to = "pop") %>%
  mutate(faixa = FAIXAS[match(rotulo, col_faixas)], cod = as.numeric(as.character(cod)),
         cod = ifelse(cod == MOJUI, SANTAREM, cod), ano = as.integer(ano), pop = as.numeric(pop)) %>%
  group_by(cod, ano, sexo, faixa) %>% summarise(pop = sum(pop), .groups = "drop")
pop_uf <- read_excel(ARQ_PRINCIPAL, sheet = "populacao") %>%
  transmute(ano = as.integer(ano), sexo, faixa, pop = as.numeric(pop))
oc <- read_excel(ARQ_PRINCIPAL, sheet = "obitos_cid_sexo_faixa") %>%
  filter(definicao == "principal", sexo != "Ignorado") %>%
  mutate(ano = as.integer(ano), obitos = as.numeric(obitos))
fatores <- read_excel(ARQ_TAXAS, sheet = "fatores_correcao") %>% select(ano, sexo, faixa, fator)
ob_mun <- read_excel(ARQ_PRINCIPAL, sheet = "obitos_municipio") %>%
  transmute(ano = as.integer(ano), cod = as.numeric(cod_mun), obitos = as.numeric(obitos))
qual <- read_excel(ARQ_PRINCIPAL, sheet = "qualidade_municipal") %>% filter(!is.na(cod_mun_analise)) %>%
  transmute(ano = as.integer(ano), cod = as.numeric(cod_mun_analise),
            tot = as.numeric(obitos_total), mal = as.numeric(obitos_r00_r99))

# ---- 2. Obitos esperados por municipio e ano ----------------------------------------
taxas_uf <- oc %>% left_join(fatores, by = c("ano", "sexo", "faixa")) %>%
  group_by(ano, sexo, faixa) %>% summarise(d = sum(obitos), d_corr = sum(obitos * fator), .groups = "drop") %>%
  right_join(pop_uf, by = c("ano", "sexo", "faixa")) %>%
  mutate(across(c(d, d_corr), ~ coalesce(.x, 0)), taxa = d / pop, taxa_corr = d_corr / pop)
esperados <- pop_mun %>% left_join(taxas_uf %>% select(ano, sexo, faixa, taxa, taxa_corr), by = c("ano", "sexo", "faixa")) %>%
  group_by(cod, ano) %>% summarise(E = sum(pop * taxa), E_corr = sum(pop * taxa_corr), .groups = "drop")

# ---- 3. Obitos observados e corrigidos ------------------------------------------------
fator_mun <- qual %>% mutate(quinquenio = quinquenio_de(ano)) %>% group_by(cod, quinquenio) %>%
  summarise(F = sum(tot) / (sum(tot) - sum(mal)), .groups = "drop")
casos <- ob_mun %>% mutate(quinquenio = quinquenio_de(ano)) %>%
  left_join(fator_mun, by = c("cod", "quinquenio")) %>%
  mutate(obitos_corr = round(obitos * F)) %>% arrange(cod, ano)
stopifnot(nrow(casos) == 143 * 25, !anyNA(casos$obitos_corr))
stopifnot(sum(casos$obitos) == 6906, sum(casos$obitos_corr) == 7628)
confere(sum(esperados$E), 6910, "soma dos esperados", 0.01)
confere(sum(esperados$E_corr), 7807.04, "soma dos esperados corrigidos", 0.01)

# ---- 4. Coordenadas -----------------------------------------------------------------------
malha <- st_read(ARQ_MALHA, quiet = TRUE) %>% st_make_valid() %>%
  mutate(cod = as.numeric(code_muni) %/% 10, cod = ifelse(cod == MOJUI, SANTAREM, cod)) %>%
  group_by(cod) %>% summarise(.groups = "drop") %>% arrange(cod)
xy <- suppressWarnings(malha %>% st_transform(CRS_PROJ) %>% st_centroid()) %>% st_transform(4674) %>% st_coordinates()
geo <- data.frame(cod = malha$cod, lat = xy[, 2], lon = xy[, 1])

# ---- 5. Grava os arquivos ---------------------------------------------------------------
grava <- function(d, nome) write.table(d, file.path(DIR_SATSCAN, nome), sep = " ", quote = FALSE,
                                        row.names = FALSE, col.names = FALSE)
grava(casos %>% select(cod, obitos, ano), "obitos.cas")
grava(casos %>% select(cod, obitos_corr, ano), "obitos_corrigidos.cas")
grava(esperados %>% transmute(cod, ano, E = round(E, 6)), "esperados.pop")
grava(esperados %>% transmute(cod, ano, E = round(E_corr, 6)), "esperados_corrigidos.pop")
# Mantem o municipios.geo usado na varredura publicada se as coordenadas
# coincidirem (diferencas de calculo menores que 0,000001 grau, sem efeito no SaTScan)
arq_geo <- file.path(DIR_SATSCAN, "municipios.geo")
geo_antigo <- if (file.exists(arq_geo)) read.table(arq_geo, col.names = c("cod", "lat", "lon")) else NULL
if (is.null(geo_antigo) || nrow(geo_antigo) != nrow(geo) || any(geo_antigo$cod != geo$cod) ||
    max(abs(geo_antigo$lat - geo$lat), abs(geo_antigo$lon - geo$lon)) > 1e-6) grava(geo, "municipios.geo")

modelo <- "07_satscan_parametros_modelo.prm"
exige(modelo, "(modelo de parametros que acompanha os scripts)")
txt <- readChar(modelo, file.info(modelo)$size, useBytes = TRUE)
caminho <- function(f) gsub("/", "\\\\", normalizePath(file.path(DIR_SATSCAN, f), mustWork = FALSE))
escreve_prm <- function(cas, pop, saida, nome) {
  t <- txt
  t <- sub("{ARQ_CASOS}", caminho(cas), t, fixed = TRUE)
  t <- sub("{ARQ_POPULACAO}", caminho(pop), t, fixed = TRUE)
  t <- sub("{ARQ_COORDENADAS}", caminho("municipios.geo"), t, fixed = TRUE)
  t <- sub("{ARQ_RESULTADO}", caminho(saida), t, fixed = TRUE)
  writeChar(t, file.path(DIR_SATSCAN, nome), eos = NULL, useBytes = TRUE)
}
escreve_prm("obitos.cas", "esperados.pop", "resultado_sem_correcao.txt", "scan_sem_correcao.prm")
escreve_prm("obitos_corrigidos.cas", "esperados_corrigidos.pop", "resultado_com_correcao.txt", "scan_com_correcao.prm")
message("Arquivos do SaTScan gravados em: ", DIR_SATSCAN)
