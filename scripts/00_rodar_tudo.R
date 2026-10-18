# =============================================================================
# 00_rodar_tudo.R
# Roda a analise completa na ordem certa (scripts 02 a 10).
#
# Antes: ajuste os caminhos em 00_config.R.
# O script 07 grava as entradas e os .prm do SaTScan. Se os resultados do
# SaTScan (resultado_*.col.txt e .gis.txt) ainda nao existirem em DIR_SATSCAN,
# a execucao para no script 08: rode o SaTScan com os dois .prm e chame este
# arquivo de novo (ou rode 08, 09 e 10).
# O script 01 (conferencia com os microdados do SIM) e opcional e fica de fora.
# Tempo total: cerca de 3 a 5 minutos.
# =============================================================================
# Pasta de trabalho = pasta dos scripts (no RStudio, e ajustada sozinha)
if (!file.exists("00_config.R")) { .d <- Sys.getenv("DIR_SCRIPTS"); if (!nzchar(.d) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) .d <- dirname(rstudioapi::getSourceEditorContext()$path); if (nzchar(.d)) setwd(.d) }
scripts <- c("02_taxas_padronizadas.R", "03_joinpoint.R", "04_sensibilidade_C76_C80.R",
             "05_indicadores_qualidade.R", "06_analise_espacial.R", "07_satscan_entrada.R",
             "08_satscan_resultados.R", "09_figuras_principais.R", "10_figuras_suplementares.R")
inicio <- Sys.time()
for (s in scripts) {
  message("\n>>> ", s)
  source(s, local = new.env(), encoding = "UTF-8")
}
message("\nAnalise completa em ", round(difftime(Sys.time(), inicio, units = "mins"), 1), " minutos.")
