# =============================================================================
# config_local_exemplo.R
# Copie este arquivo como config_local.R (na mesma pasta) e ajuste os caminhos
# para usar pastas diferentes das do repositorio. So os caminhos definidos aqui
# mudam; os demais seguem o padrao de 00_config.R. O config_local.R nao vai para
# o GitHub (.gitignore).
# =============================================================================
Sys.setenv(
  ARQ_PRINCIPAL  = "C:/caminho/para/mortalidade_neoplasias_hematologicas_PA_2000_2024.xlsx",
  DIR_RESULTADOS = "C:/caminho/para/resultados",
  DIR_SATSCAN    = "C:/caminho/para/satscan",
  DIR_FIGURAS    = "C:/caminho/para/figuras",
  ARQ_MALHA      = "C:/caminho/para/malha_PA_2020.gpkg",
  ARQ_UF         = "C:/caminho/para/uf_BR_2020.gpkg",
  DIR_MICRODADOS = "C:/caminho/para/microdados_SIM"
)
