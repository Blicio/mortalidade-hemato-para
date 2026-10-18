# =============================================================================
# 00_funcoes.R
# Funcoes usadas por mais de um script: fator de correcao das causas mal
# definidas, taxa padronizada com IC95% de Dobson, regressao joinpoint e
# formatacao de numeros. Carregado pelos scripts com source("00_funcoes.R").
# =============================================================================

# ---- Fator de correcao das causas mal definidas (equacao 1) ------------------
# F = T / (T - M) por ano x sexo x faixa etaria quinquenal.
# Antes do calculo, os obitos de sexo ignorado sao distribuidos entre os sexos
# (dentro de ano x faixa do TabNet) e os de idade ignorada entre as faixas
# (dentro de ano x sexo), na proporcao observada.
calcula_fatores <- function(qe) {
  ign_sexo <- qe %>%
    filter(sexo == "Ignorado") %>%
    select(ano, faixa_tabnet, tot_ign = obitos_total, mal_ign = obitos_r00_r99)

  passo1 <- qe %>%
    filter(sexo != "Ignorado") %>%
    left_join(ign_sexo, by = c("ano", "faixa_tabnet")) %>%
    mutate(tot_ign = coalesce(tot_ign, 0), mal_ign = coalesce(mal_ign, 0)) %>%
    group_by(ano, faixa_tabnet) %>%
    mutate(soma_tot = sum(obitos_total), soma_mal = sum(obitos_r00_r99),
           tot = obitos_total   + ifelse(soma_tot > 0, tot_ign * obitos_total   / soma_tot, tot_ign / 2),
           mal = obitos_r00_r99 + ifelse(soma_mal > 0, mal_ign * obitos_r00_r99 / soma_mal, mal_ign / 2)) %>%
    ungroup()

  passo2 <- passo1 %>% group_by(ano, sexo, faixa) %>%
    summarise(tot = sum(tot), mal = sum(mal), .groups = "drop")
  ign_idade <- passo2 %>% filter(faixa == "ignorada") %>% select(ano, sexo, tot_ign = tot, mal_ign = mal)

  fatores <- passo2 %>%
    filter(faixa != "ignorada") %>%
    left_join(ign_idade, by = c("ano", "sexo")) %>%
    mutate(tot_ign = coalesce(tot_ign, 0), mal_ign = coalesce(mal_ign, 0)) %>%
    group_by(ano, sexo) %>%
    mutate(tot = tot * (1 + tot_ign / sum(tot)), mal = mal * (1 + mal_ign / sum(mal))) %>%
    ungroup() %>%
    mutate(prop_mal = mal / tot, fator = tot / (tot - mal)) %>%
    select(ano, sexo, faixa, tot, mal, prop_mal, fator)

  # Fator dos dois sexos juntos: usado so no obito hematologico de sexo ignorado
  fatores_ambos <- fatores %>% group_by(ano, faixa) %>%
    summarise(tot = sum(tot), mal = sum(mal), .groups = "drop") %>%
    mutate(fator = tot / (tot - mal))

  list(fatores = fatores, fatores_ambos = fatores_ambos)
}

# Aplica os fatores aos obitos hematologicos (ano x sexo x faixa)
aplica_fatores <- function(dados, fat) {
  d <- dados %>% group_by(ano, sexo, faixa) %>% summarise(obitos = sum(obitos), .groups = "drop")
  conhecido <- d %>% filter(sexo != "Ignorado") %>%
    left_join(fat$fatores %>% select(ano, sexo, faixa, fator), by = c("ano", "sexo", "faixa"))
  ignorado <- d %>% filter(sexo == "Ignorado") %>%
    left_join(fat$fatores_ambos %>% select(ano, faixa, fator), by = c("ano", "faixa"))
  bind_rows(conhecido, ignorado) %>% mutate(obitos_corr = obitos * fator)
}

# ---- Taxa padronizada (equacao 2) com IC95% de Dobson -------------------------
# Limites do numero de obitos pela aproximacao de Byar.
dsr_dobson <- function(obitos, pop, peso, z = qnorm(0.975)) {
  O <- sum(obitos); W <- sum(peso)
  dsr <- sum(peso * obitos / pop) / W
  v   <- sum(peso^2 * obitos / pop^2) / W^2
  if (O > 0) {
    O_inf <- O * (1 - 1 / (9 * O) - z / (3 * sqrt(O)))^3
    O_sup <- (O + 1) * (1 - 1 / (9 * (O + 1)) + z / (3 * sqrt(O + 1)))^3
    ic_inf <- dsr + sqrt(v / O) * (O_inf - O)
    ic_sup <- dsr + sqrt(v / O) * (O_sup - O)
  } else {
    ic_inf <- 0; ic_sup <- NA_real_
  }
  tibble(obitos = O, taxa_bruta = 1e5 * O / sum(pop), taxa_padr = 1e5 * dsr,
         ic95_inf = 1e5 * max(ic_inf, 0), ic95_sup = 1e5 * ic_sup, ep = 1e5 * sqrt(v))
}

# ---- Regressao joinpoint (Metodo suplementar, equacoes S1-S5) -----------------
# Busca em grade nos anos observados, ate KMAX juncoes, minimo de 'min_fim'
# observacoes entre a juncao e cada extremo (excluindo a juncao) e MIN_ENTRE
# entre juncoes consecutivas; numero de juncoes pelo BIC. Modelo log-linear por
# minimos quadrados ponderados (peso = inverso da variancia do log da taxa).
KMAX <- 3; MIN_FIM <- 3; MIN_ENTRE <- 2

jp_desenho <- function(anos, jp) {
  X <- cbind(1, anos)
  for (t in jp) X <- cbind(X, pmax(anos - t, 0))
  X
}
jp_valido <- function(anos, jp, min_fim = MIN_FIM) {
  if (length(jp) == 0) return(TRUE)
  if (sum(anos < jp[1]) < min_fim) return(FALSE)
  if (sum(anos > jp[length(jp)]) < min_fim) return(FALSE)
  if (length(jp) > 1) for (i in seq_len(length(jp) - 1))
    if (sum(anos > jp[i] & anos < jp[i + 1]) < MIN_ENTRE) return(FALSE)
  TRUE
}
jp_ajusta <- function(anos, y, w, jp) {
  X <- jp_desenho(anos, jp); sw <- sqrt(w)
  f <- .lm.fit(X * sw, y * sw)
  list(b = f$coefficients, sse = sum(f$residuals^2), X = X, jp = jp)
}
jp_combinacoes <- function(cand, K) {
  if (K == 0) return(list(numeric(0)))
  if (length(cand) < K) return(list())
  m <- combn(cand, K)
  lapply(seq_len(ncol(m)), function(j) m[, j])
}

joinpoint <- function(anos, taxa, ep, kmax = KMAX, min_fim = MIN_FIM) {
  o <- order(anos); anos <- anos[o]; taxa <- taxa[o]; ep <- ep[o]
  n <- length(anos); y <- log(taxa); w <- 1 / (ep / taxa)^2
  cand <- anos[-c(1, n)]
  escolhido <- NULL; bic_k <- list()
  for (K in 0:kmax) {
    melhor <- NULL
    for (jp in jp_combinacoes(cand, K)) {
      if (!jp_valido(anos, jp, min_fim)) next
      a <- jp_ajusta(anos, y, w, jp)
      if (is.null(melhor) || a$sse < melhor$sse - 1e-12) melhor <- a
    }
    if (is.null(melhor)) next
    bic <- log(melhor$sse / n) + 2 * (K + 1) / n * log(n)
    bic_k[[length(bic_k) + 1]] <- data.frame(K = K, juncoes = paste(melhor$jp, collapse = "; "), bic = bic)
    if (is.null(escolhido) || bic < escolhido$bic) { escolhido <- melhor; escolhido$K <- K; escolhido$bic <- bic }
  }
  K <- escolhido$K; jp <- escolhido$jp; b <- escolhido$b; X <- escolhido$X
  gl <- n - (2 * K + 2); s2 <- escolhido$sse / gl
  V  <- s2 * solve(crossprod(X * sqrt(w))); tq <- qt(0.975, gl)
  cortes <- c(anos[1], jp, anos[n])

  segmentos <- bind_rows(lapply(0:K, function(i) {
    cc <- numeric(length(b)); cc[2] <- 1
    if (i > 0) cc[3:(2 + i)] <- 1
    sl <- sum(cc * b); se <- sqrt(drop(t(cc) %*% V %*% cc))
    data.frame(inicio = cortes[i + 1], fim = cortes[i + 2], apc = 100 * (exp(sl) - 1),
               apc_inf = 100 * (exp(sl - tq * se) - 1), apc_sup = 100 * (exp(sl + tq * se) - 1),
               p = 2 * pt(-abs(sl / se), gl))
  }))

  # AAPC: media das inclinacoes ponderada pela extensao dos segmentos
  cc <- numeric(length(b))
  for (i in 0:K) {
    L <- cortes[i + 2] - cortes[i + 1]
    cc[2] <- cc[2] + L
    if (i > 0) cc[3:(2 + i)] <- cc[3:(2 + i)] + L
  }
  cc <- cc / (anos[n] - anos[1])
  sl <- sum(cc * b); se <- sqrt(drop(t(cc) %*% V %*% cc))
  if (K == 0) { q <- tq; p <- 2 * pt(-abs(sl / se), gl) } else { q <- qnorm(0.975); p <- 2 * pnorm(-abs(sl / se)) }
  aapc <- data.frame(inicio = anos[1], fim = anos[n], aapc = 100 * (exp(sl) - 1),
                     aapc_inf = 100 * (exp(sl - q * se) - 1), aapc_sup = 100 * (exp(sl + q * se) - 1), p = p)

  list(modelo = data.frame(metodo = "joinpoint (WLS, BIC)", n = n, K = K,
                           juncoes = paste(jp, collapse = "; "), bic = escolhido$bic, gl = gl),
       segmentos = segmentos, aapc = aapc,
       ajustado = data.frame(ano = anos, taxa = taxa, ajustada = exp(drop(X %*% b))),
       bic_k = bind_rows(bic_k))
}

# Estratos com algum ano sem obito: APC unica por quasi-Poisson da taxa bruta (S5)
apc_poisson <- function(anos, obitos, populacao) {
  m <- glm(obitos ~ anos + offset(log(populacao)), family = quasipoisson())
  b <- coef(m)[["anos"]]; se <- sqrt(vcov(m)["anos", "anos"])
  p <- summary(m)$coefficients["anos", 4]; gl <- m$df.residual; tq <- qt(0.975, gl)
  seg <- data.frame(inicio = min(anos), fim = max(anos), apc = 100 * (exp(b) - 1),
                    apc_inf = 100 * (exp(b - tq * se) - 1), apc_sup = 100 * (exp(b + tq * se) - 1), p = p)
  list(modelo = data.frame(metodo = "quasi-Poisson (ha anos sem obito)", n = length(anos),
                           K = 0, juncoes = "", bic = NA_real_, gl = gl),
       segmentos = seg,
       aapc = seg %>% rename(aapc = apc, aapc_inf = apc_inf, aapc_sup = apc_sup),
       ajustado = data.frame(ano = anos, taxa = NA_real_, ajustada = NA_real_),
       bic_k = data.frame())
}

# Escolhe o metodo conforme o estrato
tendencia <- function(d, min_fim = MIN_FIM) {
  d <- d %>% arrange(ano)
  if (all(d$taxa_padr > 0)) joinpoint(d$ano, d$taxa_padr, d$ep, min_fim = min_fim)
  else apc_poisson(d$ano, d$obitos, d$populacao)
}

# ---- Formatacao no padrao das tabelas do artigo -------------------------------
num <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")
fmt_ic <- function(v, li, ls, p = NULL, d = 2) {
  paste0(num(v, d), if (!is.null(p)) ifelse(p < 0.05, "*", "") else "", " (", num(li, d), "; ", num(ls, d), ")")
}
fmt_taxa <- function(v, li, ls, d = 2) paste0(num(v, d), " (", num(li, d), "-", num(ls, d), ")")

# Ordem alfabetica que ignora acentos (Agua Azul do Norte logo depois de Abel Figueiredo)
sem_acento <- function(x) chartr("áàâãäéêèíóôõöúüçÁÀÂÃÉÊÍÓÔÕÚÇ",
                                 "aaaaaeeeioooouucAAAAEEIOOOUC", x)
ordena_nomes <- function(x) x[order(tolower(sem_acento(x)), x)]

# Conferencia contra os valores publicados no manuscrito
confere <- function(x, esperado, nome, tol = 0.006) {
  if (length(x) != 1 || is.na(x) || abs(x - esperado) > tol)
    stop(sprintf("Conferencia falhou em %s: %s (esperado %s)", nome,
                 paste(round(x, 4), collapse = ","), esperado), call. = FALSE)
  invisible(TRUE)
}
