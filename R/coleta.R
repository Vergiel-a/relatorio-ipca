# Funções de coleta de dados para o relatório do IPCA

#' Coleta o IPCA mensal (série 433) via rbcb
#'
#' @return tibble com colunas `data` (Date) e `ipca_mm` (numeric, variação % mensal)
coletar_ipca_mensal <- function() {
  rbcb::get_series(
    code = 433,
    start_date = "1980-01-01"
  ) |>
    dplyr::rename(ipca_mm = `433`) |>
    tibble::as_tibble()
}

#' Coleta a meta de inflação (série 13521) via rbcb
#'
#' @return tibble com colunas `date` (Date) e `meta_inflacao` (numeric, meta % a.a.)
coletar_meta_inflacao <- function() {
  rbcb::get_series(
    code = 13521,
    start_date = "1980-01-01"
  ) |>
    dplyr::rename(meta_inflacao = `13521`) |>
    tibble::as_tibble()
}

#' Coleta o IPCA por grupos (tabela 7060) via sidrar
#'
#' Busca variação mensal (v=63) e peso (v=66) para os 9 grupos do IPCA,
#' cobrindo toda a série histórica disponível.
#'
#' @return tibble tidy com colunas `data` (Date), `grupo` (character),
#'   `variacao` (numeric, % mensal) e `peso` (numeric, %)
coletar_ipca_grupos <- function() {
  codigos_grupos <- c(7170, 7445, 7486, 7558, 7625, 7660, 7712, 7766, 7786)

  variacao <- sidrar::get_sidra(
    api = "/t/7060/n1/all/v/63/p/all/c315/7170,7445,7486,7558,7625,7660,7712,7766,7786"
  ) |>
    tibble::as_tibble() |>
    dplyr::select(
      data  = `Mês (Código)`,
      grupo = `Geral, grupo, subgrupo, item e subitem`,
      variacao = Valor
    )

  peso <- sidrar::get_sidra(
    api = "/t/7060/n1/all/v/66/p/all/c315/7170,7445,7486,7558,7625,7660,7712,7766,7786"
  ) |>
    tibble::as_tibble() |>
    dplyr::select(
      data  = `Mês (Código)`,
      grupo = `Geral, grupo, subgrupo, item e subitem`,
      peso  = Valor
    )

  dplyr::left_join(variacao, peso, by = c("data", "grupo")) |>
    dplyr::mutate(
      data = lubridate::ym(data)
    )
}
