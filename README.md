# Mortalidade por neoplasias hematológicas no Pará, 2000-2024

Dados e scripts do estudo **"Tendência temporal e distribuição espacial da mortalidade por neoplasias hematológicas no Pará, Amazônia brasileira, 2000-2024"** (manuscrito submetido à *Cancer Causes & Control*).

Estudo ecológico com os óbitos de residentes no Pará por neoplasias hematológicas malignas (CID-10 C81-C96) registrados no Sistema de Informações sobre Mortalidade (SIM/DATASUS). Inclui taxas padronizadas por idade com e sem redistribuição das causas mal definidas, regressão joinpoint, razões de mortalidade padronizadas municipais com suavização bayesiana empírica, autocorrelação espacial (Moran e LISA) e varredura espaço-temporal de Kulldorff (SaTScan).

*English summary below.*

## Estrutura

```
dados/      planilha do TabNet (óbitos, causas mal definidas, populações) e malhas do IBGE
satscan/    entradas e resultados da varredura espaço-temporal (SaTScan 10.3.3)
scripts/    scripts R, numerados na ordem de execução
```

As pastas `resultados/` e `figuras/` são criadas pelos scripts.

## Como reproduzir

1. Instale o R (4.6.1 ou superior) e, de preferência, o RStudio.
2. Baixe o repositório (botão **Code > Download ZIP**) e descompacte.
3. Abra `scripts/00_rodar_tudo.R` no RStudio e clique em **Source**.

A análise completa leva cerca de 3 minutos. Os pacotes que faltarem são instalados automaticamente. Cada script confere os próprios resultados contra os valores publicados e interrompe a execução se algum número divergir.

A varredura espaço-temporal é feita no programa [SaTScan](https://www.satscan.org), fora do R. O script `07_satscan_entrada.R` gera os arquivos de entrada e os parâmetros (`scan_sem_correcao.prm` e `scan_com_correcao.prm`). Os resultados da execução original já estão em `satscan/` e são lidos pelo script 08.

Para usar pastas diferentes das do repositório, copie `scripts/config_local_exemplo.R` como `scripts/config_local.R` e ajuste os caminhos.

## Correspondência com o artigo

| Script | Conteúdo | Produz |
|---|---|---|
| `01_conferencia_microdados_SIM.R` | conferência opcional da planilha contra os microdados do SIM | (verificação) |
| `02_taxas_padronizadas.R` | fator de correção das causas mal definidas, taxas padronizadas (Segi, IC95% de Dobson) | Tabela 1 |
| `03_joinpoint.R` | regressão joinpoint e sensibilidades (a), (b), (d) e (e) | Tabela 2, Tabela S2 |
| `04_sensibilidade_C76_C80.R` | sensibilidade (c), redistribuição de C76-C80 | Tabela S1 |
| `05_indicadores_qualidade.R` | mieloma, categorias sem especificação, idade média aproximada | Tabela S3 |
| `06_analise_espacial.R` | RMP, bayesiano empírico, Moran, LISA, Spearman | Tabela S4 |
| `07_satscan_entrada.R` | entradas e parâmetros do SaTScan | |
| `08_satscan_resultados.R` | leitura dos aglomerados | Tabela S5 |
| `09_figuras_principais.R` | figuras em português e inglês | Figuras 1, 2 e 3 |
| `10_figuras_suplementares.R` | figuras em português e inglês | Figuras S1 a S5 |

As funções comuns (equações 1 e 2 e S1 a S5 do Método) estão em `00_funcoes.R`, e as definições (faixas etárias, pesos de Segi, grupos da CID-10, quinquênios) em `00_config.R`.

## Dados

- **Óbitos**: SIM/DATASUS, TabNet, arquivo "Mortalidade - Pará" (óbitos por residência), extraídos em 14 a 16/10/2026. A aba `Notas` da planilha descreve cada tabulação e as verificações feitas.
- **Populações**: estimativas populacionais por município, idade e sexo, 2000-2024, do Ministério da Saúde (DATASUS).
- **Malhas**: IBGE, 2020, obtidas com o pacote `geobr`.

Todos os dados são públicos e agregados, sem informação individual identificável.

## Verificação

Rodados com estes dados, os scripts reproduzem a Tabela 1 (todas as células), a Tabela 2, as Tabelas S1 a S5 e os números citados nos Resultados. Os valores de p por permutação (Moran e LISA) usam a semente 2026 e podem variar na terceira casa decimal com outra versão do pacote `spdep`. Os nomes regionais dos aglomerados na Tabela S5 foram atribuídos pelos autores e estão no script 08.

## Licenças

- Código (`scripts/`): [MIT](LICENSE).
- Dados compilados (`dados/` e `satscan/`): [CC BY 4.0](LICENSE-dados.md), com citação das fontes originais (SIM/DATASUS, Ministério da Saúde e IBGE).

## Citação

Cardoso PP et al. Tendência temporal e distribuição espacial da mortalidade por neoplasias hematológicas no Pará, Amazônia brasileira, 2000-2024. Manuscrito submetido à *Cancer Causes & Control*, 2026.

---

## English summary

Data and R code for an ecological study of mortality from hematological malignancies (ICD-10 C81-C96) in Pará, Brazilian Amazon, 2000-2024, using the Brazilian Mortality Information System. The analysis covers age-standardized rates with and without proportional redistribution of ill-defined causes, joinpoint regression, municipal standardized mortality ratios with empirical Bayes smoothing, Moran's I and LISA, and Kulldorff's space-time scan statistic (SaTScan 10.3.3).

To reproduce: install R (4.6.1 or later), download this repository and run `scripts/00_rodar_tudo.R` (about 3 minutes). Each script checks its output against the published values. Code is released under the MIT license; compiled data under CC BY 4.0.
