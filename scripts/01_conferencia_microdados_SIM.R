# =============================================================================
# 01_conferencia_microdados_SIM.R
# Mortalidade por neoplasias hematologicas no Para, Amazonia brasileira, 2000-2024
#
# PAPEL NA VERSAO FINAL DO ESTUDO
#   Os dados da analise vem do TabNet (planilha ARQ_PRINCIPAL). Este script e
#   uma CONFERENCIA independente: refaz as contagens a partir dos microdados do
#   SIM (pacote microdatasus) e compara celula a celula com a planilha. Na
#   execucao de 15/10/2026 nao houve nenhuma divergencia (4.925 valores), e
#   532 dos 6.911 obitos C81-C96 (7,7%) ocorreram fora do Para (Metodo
#   suplementar: o arquivo por residencia inclui esses obitos).
#
#   Os denominadores por interpolacao geometrica entre os Censos, calculados
#   aqui, NAO sao os da analise principal. O manuscrito usa as estimativas
#   populacionais do Ministerio da Saude (aba populacao e Faixa_5anos da
#   planilha); a interpolacao serve apenas para a comparacao descrita no
#   Metodo suplementar (Denominadores).
#
# O que este script faz
#   1. Baixa os microdados do SIM (DO por residencia, Para, 2000-2024) com o
#      pacote microdatasus e guarda uma copia de cada ano em dados_brutos/.
#   2. Baixa a populacao por municipio, sexo e faixa etaria dos Censos de
#      2000, 2010 (SIDRA, tabela 200) e 2022 (SIDRA, tabela 9514) com o sidrar.
#   3. Reagrega Mojui dos Campos (150475) a Santarem (150680): 143 unidades.
#   4. Interpola geometricamente a populacao entre os Censos e extrapola
#      2023-2024 pela taxa geometrica 2010-2022 (so para comparacao).
#   5. Gera tabelas por estado e municipio a partir dos microdados.
#   6. Confere os obitos contra a planilha do TabNet e informa cada divergencia.
#
# Como rodar: ajuste os caminhos em 00_config.R (DIR_MICRODADOS e ARQ_PRINCIPAL)
# e rode tudo (Ctrl+Shift+S). Na primeira vez o download leva de 10 a 30 minutos;
# depois os dados ficam em dados_brutos/ e o script roda em segundos.
# Nao e necessario para reproduzir os resultados do artigo (scripts 02 a 10).
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Configuracao
# -----------------------------------------------------------------------------
# Pasta de trabalho = pasta dos scripts (no RStudio, e ajustada sozinha)
if (!file.exists("00_config.R")) { .d <- Sys.getenv("DIR_SCRIPTS"); if (!nzchar(.d) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) .d <- dirname(rstudioapi::getSourceEditorContext()$path); if (nzchar(.d)) setwd(.d) }
source("00_config.R")
PASTA_PROJETO   <- DIR_MICRODADOS
PLANILHA_TABNET <- ARQ_PRINCIPAL

ANOS <- 2000:2024

DIR_BRUTOS <- file.path(PASTA_PROJETO, "dados_brutos")
DIR_SAIDA  <- file.path(PASTA_PROJETO, "saida")
dir.create(DIR_BRUTOS, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_SAIDA,  recursive = TRUE, showWarnings = FALSE)

options(timeout = 1800)  # downloads do DATASUS podem ser lentos


# -----------------------------------------------------------------------------
# 0.1 Pacotes
# -----------------------------------------------------------------------------
pacotes_cran <- c("dplyr", "tidyr", "purrr", "stringr", "readxl", "writexl",
                  "sidrar", "remotes")
faltando <- setdiff(pacotes_cran, rownames(installed.packages()))
if (length(faltando) > 0) install.packages(faltando)

if (!requireNamespace("microdatasus", quietly = TRUE)) {
  # O microdatasus fica no GitHub. No Windows, se pedir compilacao,
  # instale o Rtools da versao do seu R antes (cran.r-project.org/bin/windows/Rtools).
  remotes::install_github("rfsaldanha/microdatasus")
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
})


# -----------------------------------------------------------------------------
# 0.2 Definicoes do Metodo
# -----------------------------------------------------------------------------
FAIXAS <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
            "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
            "70-74", "75-79", "80+")
SEXOS  <- c("Masculino", "Feminino")

COD_MOJUI    <- "150475"
COD_SANTAREM <- "150680"

# Totais dos Censos (IBGE) usados so como verificacao
TOTAL_CENSO <- c("2000" = 6192307, "2010" = 7581051, "2022" = 8120131)

# Categorias da definicao de caso (mesmos rotulos da planilha do TabNet)
classificar_grupo <- function(cid3) {
  case_when(
    cid3 == "C81"                               ~ "Linfoma de Hodgkin",
    cid3 %in% c("C82", "C83", "C84", "C85", "C86") ~ "Linfomas nao Hodgkin",
    cid3 %in% c("C88", "C90")                   ~ "Mieloma e plasmocitarias",
    cid3 %in% sprintf("C%d", 91:95)             ~ "Leucemias",
    cid3 == "C96"                               ~ "Outras e nao especificadas",
    cid3 %in% c("D45", "D46", "D47")            ~ "D45-D47",
    TRUE                                        ~ NA_character_
  )
}

# Codigo de idade do SIM: 1o digito = unidade (0 minutos, 1 horas, 2 dias,
# 3 meses, 4 anos, 5 = 100 anos ou mais); 2o e 3o digitos = quantidade.
decodificar_idade <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA
  x <- str_pad(x, 3, side = "left", pad = "0")
  unidade <- substr(x, 1, 1)
  valor   <- suppressWarnings(as.integer(substr(x, 2, 3)))
  case_when(
    unidade %in% c("0", "1", "2", "3") ~ 0L,
    unidade == "4"                     ~ valor,
    unidade == "5"                     ~ 100L + valor,
    TRUE                               ~ NA_integer_
  )
}

faixa_de_idade <- function(idade) {
  ifelse(is.na(idade), NA_character_, FAIXAS[pmin(idade %/% 5, 16) + 1])
}


# -----------------------------------------------------------------------------
# 1. Microdados do SIM
# -----------------------------------------------------------------------------
VARS_SIM <- c("DTOBITO", "SEXO", "IDADE", "CAUSABAS", "CODMUNRES", "CODMUNOCOR")

baixar_sim_ano <- function(ano) {
  arq <- file.path(DIR_BRUTOS, sprintf("sim_do_PA_%d.rds", ano))
  if (file.exists(arq)) return(readRDS(arq))
  message("Baixando SIM ", ano, " ...")
  bruto <- microdatasus::fetch_datasus(year_start = ano, year_end = ano,
                                       uf = "PA", information_system = "SIM-DO")
  ausentes <- setdiff(VARS_SIM, names(bruto))
  if (length(ausentes) > 0)
    stop("SIM ", ano, ": variaveis ausentes: ", paste(ausentes, collapse = ", "))
  bruto <- as.data.frame(bruto)[, VARS_SIM]
  bruto[] <- lapply(bruto, function(v) trimws(as.character(v)))
  bruto$ano <- ano
  saveRDS(bruto, arq)
  bruto
}

sim <- map(ANOS, baixar_sim_ano) |> bind_rows()

sim <- sim |>
  mutate(
    uf_res     = substr(CODMUNRES, 1, 2),
    uf_ocor    = substr(CODMUNOCOR, 1, 2),
    cod_mun    = substr(CODMUNRES, 1, 6),
    cod_mun    = if_else(cod_mun == COD_MOJUI, COD_SANTAREM, cod_mun),
    sexo       = case_when(SEXO %in% c("1", "M") ~ "Masculino",
                           SEXO %in% c("2", "F") ~ "Feminino",
                           TRUE                  ~ NA_character_),
    idade      = decodificar_idade(IDADE),
    faixa      = faixa_de_idade(idade),
    cid3       = toupper(substr(CAUSABAS, 1, 3)),
    grupo      = classificar_grupo(cid3),
    definicao  = case_when(grupo == "D45-D47" ~ "sensibilidade",
                           !is.na(grupo)      ~ "principal",
                           TRUE               ~ NA_character_),
    mal_definida = coalesce(substr(cid3, 1, 1) == "R", FALSE),
    ano_dtobito  = suppressWarnings(as.integer(substr(DTOBITO, 5, 8)))
  )

# Os arquivos DO do microdatasus sao por UF de RESIDENCIA. Se aparecer
# residente de outra UF, o arquivo nao e o esperado: o script para.
n_outra_uf <- sum(sim$uf_res != "15", na.rm = TRUE)
if (n_outra_uf > 0) {
  stop(n_outra_uf, " registros com residencia fora do Para. ",
       "Verifique se o microdatasus baixou os arquivos por residencia (DORES).")
}

n_ano_diverge <- sum(sim$ano_dtobito != sim$ano, na.rm = TRUE)
if (n_ano_diverge > 0)
  warning(n_ano_diverge, " registros com ano de DTOBITO diferente do ano do arquivo.")


# -----------------------------------------------------------------------------
# 2. Populacao dos Censos (SIDRA)
# -----------------------------------------------------------------------------
# Tabela 200 (Censos 2000 e 2010): classificacao C58, grupos de idade
MAPA_200 <- c(
  "1140" = "0-4",   "1141" = "5-9",   "1142" = "10-14", "1143" = "15-19",
  "1144" = "20-24", "1145" = "25-29", "1146" = "30-34", "1147" = "35-39",
  "1148" = "40-44", "1149" = "45-49", "1150" = "50-54", "1151" = "55-59",
  "1152" = "60-64", "1153" = "65-69", "1154" = "70-74", "1155" = "75-79",
  "6802" = "80+",   "6803" = "80+",   "92963" = "80+",  "92964" = "80+",
  "92965" = "80+"
)
# Tabela 9514 (Censo 2022): classificacao C287, idade
MAPA_9514 <- c(
  "93070" = "0-4",   "93084" = "5-9",   "93085" = "10-14", "93086" = "15-19",
  "93087" = "20-24", "93088" = "25-29", "93089" = "30-34", "93090" = "35-39",
  "93091" = "40-44", "93092" = "45-49", "93093" = "50-54", "93094" = "55-59",
  "93095" = "60-64", "93096" = "65-69", "93097" = "70-74", "93098" = "75-79",
  "49108" = "80+",   "49109" = "80+",   "60040" = "80+",   "60041" = "80+",
  "6653"  = "80+"
)

API_200  <- paste0("/t/200/n6/in%20n3%2015/v/93/p/2000,2010/c2/4,5/c1/0/c58/",
                   paste(names(MAPA_200), collapse = ","))
API_9514 <- paste0("/t/9514/n6/in%20n3%2015/v/93/p/2022/c2/4,5/c286/113635/c287/",
                   paste(names(MAPA_9514), collapse = ","))

baixar_sidra <- function(api, nome) {
  arq <- file.path(DIR_BRUTOS, paste0(nome, ".rds"))
  if (file.exists(arq)) return(readRDS(arq))
  message("Baixando SIDRA ", nome, " ...")
  x <- sidrar::get_sidra(api = api)
  saveRDS(x, arq)
  x
}

# Localiza colunas pelo nome sem depender da acentuacao (Windows/UTF-8)
coluna <- function(df, padrao) {
  n <- grep(padrao, names(df), value = TRUE, useBytes = TRUE)
  if (length(n) != 1)
    stop("Coluna nao encontrada ou ambigua (", padrao, "): ",
         paste(names(df), collapse = " | "))
  n
}

padronizar_sidra <- function(x, mapa) {
  tibble(
    ano       = as.integer(x[[coluna(x, "^Ano \\(C.{1,2}digo\\)$")]]),
    cod_mun7  = as.character(x[[coluna(x, "^Munic.{1,2}pio \\(C.{1,2}digo\\)$")]]),
    sexo_cod  = as.character(x[[coluna(x, "^Sexo \\(C.{1,2}digo\\)$")]]),
    idade_cod = as.character(x[[coluna(x, "^(Grupo de idade|Idade) \\(C.{1,2}digo\\)$")]]),
    valor     = suppressWarnings(as.numeric(x[["Valor"]]))
  ) |>
    mutate(
      valor   = coalesce(valor, 0),   # "-" no SIDRA = zero
      faixa   = unname(mapa[idade_cod]),
      sexo    = recode(sexo_cod, "4" = "Masculino", "5" = "Feminino"),
      cod_mun = substr(cod_mun7, 1, 6),
      cod_mun = if_else(cod_mun == COD_MOJUI, COD_SANTAREM, cod_mun)
    )
}

censo <- bind_rows(
  padronizar_sidra(baixar_sidra(API_200,  "sidra_t200_censo_2000_2010"), MAPA_200),
  padronizar_sidra(baixar_sidra(API_9514, "sidra_t9514_censo_2022"),     MAPA_9514)
)

if (anyNA(censo$faixa)) stop("Codigo de idade do SIDRA sem correspondencia no mapa.")
if (!all(censo$sexo %in% SEXOS)) stop("Codigo de sexo inesperado no SIDRA.")

censo <- censo |>
  group_by(ano, cod_mun, sexo, faixa) |>
  summarise(pop = sum(valor), .groups = "drop")

# Verificacoes: 143 unidades em cada Censo e total proximo do oficial
verif_censo <- censo |>
  group_by(ano) |>
  summarise(municipios = n_distinct(cod_mun), pop = sum(pop), .groups = "drop") |>
  mutate(total_oficial = TOTAL_CENSO[as.character(ano)],
         dif_pct = round(100 * (pop / total_oficial - 1), 2))
print(verif_censo)
if (any(verif_censo$municipios != 143))
  stop("Numero de municipios diferente de 143 em algum Censo.")
if (any(abs(verif_censo$dif_pct) > 0.5))
  warning("Populacao de algum Censo difere mais de 0,5% do total oficial. ",
          "A tabela 200 (2000 e 2010) e da amostra; confira antes de seguir.")

MUNICIPIOS <- sort(unique(censo$cod_mun))


# -----------------------------------------------------------------------------
# 3. Interpolacao geometrica (municipio x sexo x faixa)
# -----------------------------------------------------------------------------
# Usa o ano do Censo como referencia do ano-calendario (data de referencia
# dos Censos: 1o de agosto). Quando um dos extremos e zero, usa interpolacao
# linear, porque a geometrica nao e definida.
interpolar <- function(p0, p1, t0, t1, t) {
  r   <- (t - t0) / (t1 - t0)
  geo <- p0 * (p1 / p0)^r
  lin <- p0 + (p1 - p0) * r
  ifelse(p0 > 0 & p1 > 0, geo, pmax(lin, 0))
}

pop_censo <- censo |>
  pivot_wider(names_from = ano, values_from = pop, names_prefix = "p",
              values_fill = 0) |>
  crossing(ano = ANOS) |>
  mutate(pop = case_when(
    ano <= 2010 ~ interpolar(p2000, p2010, 2000, 2010, ano),
    TRUE        ~ interpolar(p2010, p2022, 2010, 2022, ano)  # 2023-2024: extrapolacao
  )) |>
  select(ano, cod_mun, sexo, faixa, pop) |>
  mutate(faixa = factor(faixa, levels = FAIXAS)) |>
  arrange(ano, cod_mun, sexo, faixa)

pop_estado <- pop_censo |>
  group_by(ano, sexo, faixa) |>
  summarise(pop = sum(pop), .groups = "drop")

# Plausibilidade da extrapolacao: o Para cresceu cerca de 0,6% ao ano em 2010-2022
total_ano <- function(a) sum(pop_estado$pop[pop_estado$ano == a])
razao_24_22 <- total_ano(2024) / total_ano(2022)
if (razao_24_22 < 0.99 || razao_24_22 > 1.03)
  warning("Crescimento 2022-2024 extrapolado fora do plausivel (", round(100 * (razao_24_22 - 1), 1),
          "%). Revise a extrapolacao ou use outra fonte para 2023-2024.")


# -----------------------------------------------------------------------------
# 4. Tabelas do Metodo
# -----------------------------------------------------------------------------
casos <- sim |> filter(!is.na(grupo))
validos <- function(d) d |> filter(!is.na(sexo), !is.na(faixa))

# 4.1 Qualidade por estrato (ano x sexo x faixa): T, M e fator F = T / (T - M)
qualidade_estrato <- sim |>
  validos() |>
  count(ano, sexo, faixa, name = "obitos_total") |>
  left_join(sim |> validos() |> filter(mal_definida) |>
              count(ano, sexo, faixa, name = "obitos_mal_definidos"),
            by = c("ano", "sexo", "faixa")) |>
  complete(ano = ANOS, sexo = SEXOS, faixa = FAIXAS,
           fill = list(obitos_total = 0L, obitos_mal_definidos = 0L)) |>
  mutate(fator_F = ifelse(obitos_total > obitos_mal_definidos,
                          obitos_total / (obitos_total - obitos_mal_definidos),
                          NA_real_)) |>
  arrange(ano, sexo, factor(faixa, levels = FAIXAS))

# 4.2 Obitos no estado: ano x sexo x faixa x grupo, com e sem redistribuicao
GRUPOS <- c("Linfoma de Hodgkin", "Linfomas nao Hodgkin", "Mieloma e plasmocitarias",
            "Leucemias", "Outras e nao especificadas", "D45-D47")

obitos_estado <- casos |>
  validos() |>
  count(ano, sexo, faixa, grupo, name = "obitos") |>
  complete(ano = ANOS, sexo = SEXOS, faixa = FAIXAS, grupo = GRUPOS,
           fill = list(obitos = 0L)) |>
  mutate(definicao = if_else(grupo == "D45-D47", "sensibilidade", "principal")) |>
  left_join(select(qualidade_estrato, ano, sexo, faixa, fator_F),
            by = c("ano", "sexo", "faixa")) |>
  mutate(obitos_corrigidos = obitos * coalesce(fator_F, 1)) |>
  left_join(pop_estado |> mutate(faixa = as.character(faixa)),
            by = c("ano", "sexo", "faixa")) |>
  mutate(faixa = factor(faixa, levels = FAIXAS)) |>
  arrange(ano, sexo, faixa, grupo)

# 4.3 Obitos por municipio: ano x municipio x sexo x faixa x definicao
obitos_municipio <- casos |>
  validos() |>
  filter(cod_mun %in% MUNICIPIOS) |>
  count(ano, cod_mun, sexo, faixa, definicao, name = "obitos") |>
  complete(ano = ANOS, cod_mun = MUNICIPIOS, sexo = SEXOS, faixa = FAIXAS,
           definicao = c("principal", "sensibilidade"),
           fill = list(obitos = 0L)) |>
  mutate(faixa = factor(faixa, levels = FAIXAS)) |>
  arrange(ano, cod_mun, sexo, faixa, definicao)

# 4.4 Qualidade por municipio (proporcao de causas mal definidas)
qualidade_municipio <- sim |>
  filter(cod_mun %in% MUNICIPIOS) |>
  group_by(ano, cod_mun) |>
  summarise(obitos_total = n(),
            obitos_mal_definidos = sum(mal_definida), .groups = "drop") |>
  complete(ano = ANOS, cod_mun = MUNICIPIOS,
           fill = list(obitos_total = 0L, obitos_mal_definidos = 0L)) |>
  mutate(prop_mal_definidos = ifelse(obitos_total > 0,
                                     obitos_mal_definidos / obitos_total, NA_real_))

# 4.5 Registros que ficam fora das analises estratificadas (C81-C96)
ignorados <- casos |>
  filter(definicao == "principal") |>
  group_by(ano) |>
  summarise(obitos_c81_c96       = n(),
            sexo_ignorado        = sum(is.na(sexo)),
            idade_ignorada       = sum(is.na(faixa)),
            municipio_ignorado   = sum(!cod_mun %in% MUNICIPIOS),
            .groups = "drop")

# 4.6 Residentes do Para que morreram em outra UF (responde a pendencia das Notas)
ocorrencia_fora <- casos |>
  filter(definicao == "principal") |>
  group_by(ano) |>
  summarise(obitos_c81_c96 = n(),
            ocorreram_fora_do_PA = sum(uf_ocor != "15", na.rm = TRUE),
            .groups = "drop")


# -----------------------------------------------------------------------------
# 5. Conferencia com a planilha do TabNet
# -----------------------------------------------------------------------------
comparar <- function(r, t, chaves, nome) {
  d <- full_join(r, t, by = chaves, suffix = c("_R", "_TabNet")) |>
    mutate(across(c(obitos_R, obitos_TabNet), ~ coalesce(as.numeric(.x), 0)),
           diferenca = obitos_R - obitos_TabNet)
  list(
    resumo = tibble(tabela = nome,
                    celulas = nrow(d),
                    celulas_divergentes = sum(d$diferenca != 0),
                    total_R = sum(d$obitos_R),
                    total_TabNet = sum(d$obitos_TabNet),
                    maior_diferenca_absoluta = max(abs(d$diferenca))),
    detalhe = d |> filter(diferenca != 0) |> mutate(tabela = nome, .before = 1) |>
      mutate(across(all_of(chaves), as.character))
  )
}

conferencia <- NULL
if (file.exists(PLANILHA_TABNET)) {
  ler <- function(aba) readxl::read_excel(PLANILHA_TABNET, sheet = aba)
  # A aba "qualidade" saiu da versao final da planilha; os mesmos totais anuais
  # sao obtidos das abas qualidade_estrato (todas as causas e R00-R99) e obitos_cid.
  if ("qualidade" %in% readxl::excel_sheets(PLANILHA_TABNET)) {
    tn_qual <- ler("qualidade")
  } else {
    tn_qual <- ler("qualidade_estrato") |>
      group_by(ano) |>
      summarise(obitos_total = sum(obitos_total), obitos_mal_definidos = sum(obitos_r00_r99), .groups = "drop") |>
      left_join(ler("obitos_cid") |> filter(definicao == "principal") |>
                  group_by(ano) |> summarise(obitos_c81_c96 = sum(obitos), .groups = "drop"), by = "ano")
  }
  tn_cid  <- ler("obitos_cid")
  tn_sf   <- ler("obitos_sexo_faixa")
  tn_mun  <- ler("obitos_municipio")

  r_qual <- sim |>
    group_by(ano) |>
    summarise(obitos_total = n(),
              obitos_mal_definidos = sum(mal_definida),
              obitos_c81_c96 = sum(definicao == "principal", na.rm = TRUE),
              .groups = "drop") |>
    pivot_longer(-ano, names_to = "indicador", values_to = "obitos")
  t_qual <- tn_qual |>
    pivot_longer(-ano, names_to = "indicador", values_to = "obitos")

  r_cid <- casos |> count(ano, cid = cid3, name = "obitos")
  t_cid <- tn_cid |> select(ano, cid, obitos)

  r_sf <- casos |> filter(definicao == "principal") |> validos() |>
    count(ano, sexo, faixa, name = "obitos")
  t_sf <- tn_sf |> select(ano, sexo, faixa, obitos)

  r_mun <- casos |> filter(definicao == "principal", cod_mun %in% MUNICIPIOS) |>
    count(ano, cod_mun, name = "obitos")
  t_mun <- tn_mun |> transmute(ano, cod_mun = as.character(cod_mun), obitos)

  comps <- list(
    comparar(r_qual, t_qual, c("ano", "indicador"), "qualidade (ano)"),
    comparar(r_cid,  t_cid,  c("ano", "cid"),       "obitos por CID (ano)"),
    comparar(r_sf,   t_sf,   c("ano", "sexo", "faixa"), "obitos por sexo e faixa"),
    comparar(r_mun,  t_mun,  c("ano", "cod_mun"),   "obitos por municipio")
  )
  conferencia <- list(
    resumo  = bind_rows(map(comps, "resumo")),
    detalhe = bind_rows(map(comps, "detalhe"))
  )
  message("\n===== CONFERENCIA COM O TABNET =====")
  print(as.data.frame(conferencia$resumo))
  if (sum(conferencia$resumo$celulas_divergentes) == 0) {
    message("Nenhuma divergencia: os microdados reproduzem a planilha do TabNet.")
  } else {
    message("Ha divergencias. Veja a aba 'conferencia_detalhe' em saida/tabelas_metodo.xlsx. ",
            "Diferencas pequenas podem vir de atualizacoes do SIM entre as duas extracoes.")
  }
} else {
  warning("Planilha do TabNet nao encontrada em PLANILHA_TABNET; conferencia nao realizada.")
}


# -----------------------------------------------------------------------------
# 6. Resumo e gravacao
# -----------------------------------------------------------------------------
message("\n===== RESUMO =====")
message("Obitos C81-C96: ", sum(casos$definicao == "principal"),
        " | D45-D47: ", sum(casos$definicao == "sensibilidade"))
message("Sexo ignorado (C81-C96): ", sum(ignorados$sexo_ignorado),
        " | idade ignorada: ", sum(ignorados$idade_ignorada),
        " | municipio ignorado: ", sum(ignorados$municipio_ignorado))
message("C81-C96 de residentes do Para que morreram em outra UF: ",
        sum(ocorrencia_fora$ocorreram_fora_do_PA))
message("Populacao interpolada do Para: ",
        paste(sprintf("%d = %s", c(2000, 2010, 2022, 2024),
                      format(round(sapply(c(2000, 2010, 2022, 2024), total_ano)),
                             big.mark = ".", decimal.mark = ",")),
              collapse = " | "))

tabelas <- list(
  obitos_estado       = obitos_estado,
  qualidade_estrato   = qualidade_estrato,
  pop_estado          = pop_estado,
  obitos_municipio    = obitos_municipio,
  qualidade_municipio = qualidade_municipio,
  pop_municipio       = pop_censo,
  ignorados           = ignorados,
  ocorrencia_fora     = ocorrencia_fora,
  verificacao_censo   = verif_censo
)
if (!is.null(conferencia)) {
  tabelas$conferencia_resumo  <- conferencia$resumo
  tabelas$conferencia_detalhe <- conferencia$detalhe
}

saveRDS(tabelas, file.path(DIR_SAIDA, "tabelas_metodo.rds"))
saveRDS(select(sim, ano, sexo, idade, faixa, cid3, grupo, definicao,
               mal_definida, cod_mun, uf_ocor),
        file.path(DIR_SAIDA, "sim_PA_2000_2024_processado.rds"))
writexl::write_xlsx(map(tabelas, ~ mutate(.x, across(where(is.factor), as.character))),
                    file.path(DIR_SAIDA, "tabelas_metodo.xlsx"))
writeLines(capture.output(sessionInfo()), file.path(DIR_SAIDA, "sessionInfo.txt"))

message("\nArquivos gravados em: ", normalizePath(DIR_SAIDA))
