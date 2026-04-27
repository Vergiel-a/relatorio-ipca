# Relatório IPCA

Relatório mensal automatizado do IPCA (Índice Nacional de Preços ao Consumidor Amplo), gerado com R e Quarto e publicado via Posit Connect Cloud.

## Fontes de dados

| Série | Fonte | Conteúdo |
|-------|-------|-----------|
| 433 | BCB/SGS via `rbcb` | IPCA variação mensal |
| 13521 | BCB/SGS via `rbcb` | Meta de inflação anual |
| Tabela 7060 | IBGE/SIDRA via `sidrar` | IPCA por grupos de despesa |

## Estrutura

```
.
├── R/
│   ├── coleta.R       # funções de coleta via rbcb e sidrar
│   ├── tratamento.R   # transformações e agregações
│   └── graficos.R     # visualizações ggplot2
├── relatorio_ipca.qmd # documento principal
└── _quarto.yml        # configuração do projeto Quarto
```

## Como rodar

### Pré-requisitos

```r
install.packages(c(
  "dplyr", "ggplot2", "lubridate", "scales", "stringr",
  "forcats", "slider", "tibble", "rbcb", "sidrar"
))
```

> `rbcb` pode exigir instalação via `remotes::install_github("wilsonfreitas/rbcb")`.

### Renderizar o relatório

```bash
quarto render relatorio_ipca.qmd
```

O arquivo `relatorio_ipca.html` será gerado na raiz do projeto.

### Atualizar dados e re-renderizar

O projeto usa `freeze: auto` — o Quarto só re-executa os chunks cujo código mudou. Para forçar re-execução completa:

```bash
quarto render relatorio_ipca.qmd --no-freeze
```
