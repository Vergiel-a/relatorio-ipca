# Funções de tratamento e transformação dos dados do IPCA

#' Calcula o IPCA acumulado em 12 meses (janela móvel)
#'
#' @param df tibble com colunas `date` e `ipca_mm` (variação % mensal)
#' @return tibble original acrescido de `ipca_12m` (% acumulado 12 meses)
calcular_acumulado_12m <- function(df) {
  df |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      ipca_12m = slider::slide_dbl(
        ipca_mm,
        .f     = \(x) (prod(1 + x / 100) - 1) * 100,
        .before = 11,
        .complete = TRUE
      )
    )
}

#' Calcula o IPCA acumulado no ano (reinicia em janeiro)
#'
#' @param df tibble com colunas `date` e `ipca_mm`
#' @return tibble original acrescido de `ipca_ano` (% acumulado no ano corrente)
calcular_acumulado_ano <- function(df) {
  df |>
    dplyr::arrange(date) |>
    dplyr::mutate(ano = lubridate::year(date)) |>
    dplyr::group_by(ano) |>
    dplyr::mutate(
      ipca_ano = (cumprod(1 + ipca_mm / 100) - 1) * 100
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-ano)
}

#' Prepara dados para o gráfico sazonal (IPCA mensal por ano)
#'
#' @param df tibble com colunas `date` e `ipca_mm`
#' @param ano_inicio primeiro ano a incluir (padrão 2015)
#' @return tibble com colunas `mes` (int 1–12), `ano` (int), `ipca_mm`,
#'   `cor` (character hex) e `destaque` (logical, TRUE para o ano atual)
preparar_sazonal <- function(df, ano_inicio = 2015) {
  ano_atual <- lubridate::year(max(df$date, na.rm = TRUE))

  paleta <- grDevices::colorRampPalette(c("#bdc3e0", "#282f6b"))

  df |>
    dplyr::filter(lubridate::year(date) >= ano_inicio) |>
    dplyr::mutate(
      mes      = lubridate::month(date),
      ano      = lubridate::year(date),
      destaque = ano == ano_atual
    ) |>
    dplyr::arrange(ano) |>
    dplyr::mutate(
      cor = paleta(dplyr::n_distinct(ano))[as.integer(factor(ano))]
    ) |>
    dplyr::select(mes, ano, ipca_mm, cor, destaque)
}

#' Prepara contribuições dos grupos ao IPCA cheio
#'
#' A contribuição de cada grupo é: variacao * peso / 100.
#' A soma das contribuições dos 9 grupos deve aproximar o IPCA cheio mensal
#' (usado como sanity check interno).
#'
#' @param df_grupos tibble com colunas `data`, `grupo`, `variacao`, `peso`
#' @return list com dois elementos:
#'   - `historico`: tibble com `data`, `grupo`, `variacao`, `peso`, `contribuicao`
#'   - `mes_atual`: recorte do mês mais recente disponível
preparar_contribuicoes <- function(df_grupos) {
  historico <- df_grupos |>
    dplyr::mutate(contribuicao = variacao * peso / 100)

  # Sanity check: soma das contribuições vs. IPCA cheio implícito
  soma_contribuicoes <- historico |>
    dplyr::group_by(data) |>
    dplyr::summarise(soma = sum(contribuicao, na.rm = TRUE), .groups = "drop")

  meses_discrepantes <- soma_contribuicoes |>
    dplyr::filter(abs(soma) > 5)  # limite conservador; ajuste conforme necessário

  if (nrow(meses_discrepantes) > 0) {
    warning(
      "Soma das contribuicoes dos grupos difere do esperado em ",
      nrow(meses_discrepantes), " mes(es). Verifique os dados."
    )
  }

  mes_atual <- historico |>
    dplyr::filter(data == max(data, na.rm = TRUE))

  list(
    historico  = historico,
    mes_atual  = mes_atual
  )
}

#' Expande a meta de inflação anual para frequência mensal
#'
#' A série 13521 fornece a meta em base anual. Esta função faz join por ano,
#' atribuindo a mesma meta a todos os meses daquele ano.
#'
#' @param df_ipca tibble com coluna `date` (Date)
#' @param df_meta tibble com coluna `date` (Date) e `meta_inflacao` (numeric)
#' @return tibble com colunas `date`, `ipca_mm` e `meta_inflacao`
preparar_meta_mensal <- function(df_ipca, df_meta) {
  meta_anual <- df_meta |>
    dplyr::mutate(ano = lubridate::year(date)) |>
    dplyr::select(ano, meta_inflacao)

  df_ipca |>
    dplyr::mutate(ano = lubridate::year(date)) |>
    dplyr::left_join(meta_anual, by = "ano") |>
    dplyr::select(-ano)
}
