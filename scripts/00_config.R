# =============================================================================
# 00_config.R
# Mortalidade por neoplasias hematologicas no Para, Amazonia brasileira, 2000-2024
#
# Configuracao comum a todos os scripts: caminhos, pacotes e definicoes do
# Metodo. Os demais scripts comecam com source("00_config.R"), entao basta
# ajustar os caminhos AQUI.
#
# Versao de 18/10/2026, alinhada ao manuscrito submetido a Cancer Causes & Control.
# =============================================================================

# ---- 1. Caminhos -------------------------------------------------------------
# Por padrao, os caminhos sao relativos ao repositorio (a pasta que contem
# scripts/, dados/ e satscan/). Para usar outras pastas, crie nesta pasta um
# arquivo config_local.R com Sys.setenv(NOME = "caminho") para cada caminho que
# quiser trocar (veja config_local_exemplo.R). Esse arquivo nao vai para o GitHub.
# Use barras "/" (nao "\").
if (file.exists("config_local.R")) source("config_local.R")
cfg <- function(var, padrao) { v <- Sys.getenv(var, ""); if (nzchar(v)) v else padrao }
RAIZ <- normalizePath("..", winslash = "/", mustWork = FALSE)

DIR_PROJETO    <- cfg("DIR_PROJETO",    RAIZ)
ARQ_PRINCIPAL  <- cfg("ARQ_PRINCIPAL",  file.path(RAIZ, "dados", "mortalidade_neoplasias_hematologicas_PA_2000_2024.xlsx"))
DIR_RESULTADOS <- cfg("DIR_RESULTADOS", file.path(RAIZ, "resultados"))
DIR_SATSCAN    <- cfg("DIR_SATSCAN",    file.path(RAIZ, "satscan"))
DIR_FIGURAS    <- cfg("DIR_FIGURAS",    file.path(RAIZ, "figuras"))
ARQ_MALHA      <- cfg("ARQ_MALHA",      file.path(RAIZ, "dados", "malha_PA_2020.gpkg"))  # baixada pelo geobr se faltar
ARQ_UF         <- cfg("ARQ_UF",         file.path(RAIZ, "dados", "uf_BR_2020.gpkg"))     # idem

# Script 01 (conferencia com os microdados do SIM): pasta propria, fora do repositorio
DIR_MICRODADOS <- cfg("DIR_MICRODADOS", file.path(RAIZ, "microdados"))

for (d in c(DIR_RESULTADOS, DIR_FIGURAS)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# Arquivos intermediarios (gerados por um script e lidos pelos seguintes)
ARQ_TAXAS     <- file.path(DIR_RESULTADOS, "resultados_taxas.xlsx")        # 02
ARQ_JOINPOINT <- file.path(DIR_RESULTADOS, "joinpoint_resultados.xlsx")    # 03
ARQ_SENSIB    <- file.path(DIR_RESULTADOS, "sensibilidade_resultados.xlsx")# 04
ARQ_QUALIDADE <- file.path(DIR_RESULTADOS, "qualidade_resultados.xlsx")    # 05
ARQ_ESPACIAL  <- file.path(DIR_RESULTADOS, "espacial_resultados.xlsx")     # 06
ARQ_SATSCAN   <- file.path(DIR_RESULTADOS, "satscan_resultados.xlsx")      # 08

# ---- 2. Pacotes ----------------------------------------------------------------
# carrega(c("dplyr", ...)) instala o que faltar e carrega em silencio.
carrega <- function(pacotes) {
  faltam <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltam) > 0) install.packages(faltam)
  suppressPackageStartupMessages(for (p in pacotes) library(p, character.only = TRUE))
  invisible(TRUE)
}
carrega(c("readxl", "writexl", "dplyr", "tidyr"))

exige <- function(arq, script) {
  if (!file.exists(arq)) stop("Arquivo nao encontrado: ", arq, "\nRode antes o script ", script, ".", call. = FALSE)
}

# ---- 3. Definicoes do Metodo ----------------------------------------------------
ANOS <- 2000:2024
FAIXAS <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
            "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
            "75-79", "80+")

# Populacao mundial padrao de Segi modificada por Doll (80+ agregado)
SEGI <- tibble(faixa = FAIXAS,
               peso  = c(12000, 10000, 9000, 9000, 8000, 8000, 6000, 6000, 6000, 6000,
                         5000, 4000, 4000, 3000, 2000, 1000, 1000))
stopifnot(sum(SEGI$peso) == 100000)

# Faixas do estudo de Sergipe (Ferreira et al., 2024), usadas na tendencia
FAIXA_AMPLA <- list("Todas" = FAIXAS, "0-19" = FAIXAS[1:4], "20-44" = FAIXAS[5:9],
                    "45-64" = FAIXAS[10:13], "65+" = FAIXAS[14:17])

CID_PRINCIPAL <- c("C81", "C82", "C83", "C84", "C85", "C86", "C88",
                   "C90", "C91", "C92", "C93", "C94", "C95", "C96")
GRUPOS <- list(
  "C81-C96 (principal)"         = CID_PRINCIPAL,
  "Linfoma de Hodgkin"          = "C81",
  "Linfomas nao Hodgkin"        = c("C82", "C83", "C84", "C85", "C86"),
  "Mieloma e plasmocitarias"    = c("C88", "C90"),
  "Leucemias"                   = c("C91", "C92", "C93", "C94", "C95"),
  "Outras e nao especificadas"  = "C96",
  "C81-C96 + D45-D47 (sensib.)" = c(CID_PRINCIPAL, "D45", "D46", "D47")
)
P <- "C81-C96 (principal)"
SEXOS <- c("Ambos", "Masculino", "Feminino")
CORRECOES <- c("sem redistribuicao", "com redistribuicao")

QUINQUENIOS <- list("2000-2004" = c(2000, 2004), "2005-2009" = c(2005, 2009),
                    "2010-2014" = c(2010, 2014), "2015-2019" = c(2015, 2019),
                    "2020-2024" = c(2020, 2024))
PERIODOS <- c(QUINQUENIOS, list("2000-2024" = c(2000, 2024)))
quinquenio_de <- function(ano) names(QUINQUENIOS)[(ano - 2000) %/% 5 + 1]

# Territorio: 143 unidades (Mojui dos Campos, instalado em 2013, somado a Santarem)
MOJUI <- 150475; SANTAREM <- 150680; BELEM <- 150140
RMB <- c(150140, 150080, 150442, 150150, 150635, 150650, 150240)  # Regiao Metropolitana de Belem
CRS_PROJ <- 5880   # SIRGAS 2000 / Brazil Polyconic

# ---- 4. Figuras -------------------------------------------------------------------
# Cancer Causes & Control: 174 mm de largura (pagina inteira), fonte sem serifa
# de 7-8 pt no tamanho final, TIFF 600 dpi (LZW) para submissao.
FONTE   <- cfg("FONTE_FIGURAS", if (.Platform$OS.type == "windows") "Arial" else "Liberation Sans")
IDIOMAS <- strsplit(cfg("IDIOMAS_FIGURAS", "pt,en"), ",")[[1]]
LARG_MM <- 174
TAM     <- 7
