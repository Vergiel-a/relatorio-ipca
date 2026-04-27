# Funções de visualização do IPCA com ggplot2

.cores <- list(
  azul   = "#282f6b",
  laranja = "#d97706",
  verde  = "#059669",
  cinza  = "#6b7280"
)

.tema <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom"
    )
}

#' Gráfico de barras do IPCA mensal (últimos 24 meses)
#'
#' @param df tibble com colunas `date` e `ipca_mm`
#' @return objeto ggplot
grafico_ipca_mensal <- function(df) {
  dados <- df |>
    dplyr::arrange(date) |>
    dplyr::slice_tail(n = 24) |>
    dplyr::mutate(
      rotulo = format(round(ipca_mm, 2), nsmall = 2, decimal.mark = ","),
      pos_texto = dplyr::if_else(ipca_mm >= 0, ipca_mm + 0.02, ipca_mm - 0.02),
      vjust_texto = dplyr::if_else(ipca_mm >= 0, 0, 1)
    )

  y_min <- min(dados$ipca_mm, na.rm = TRUE) - 0.1
  y_max <- max(dados$ipca_mm, na.rm = TRUE) + 0.2

  ggplot2::ggplot(dados, ggplot2::aes(x = date, y = ipca_mm)) +
    ggplot2::geom_col(fill = .cores$azul, width = 25) +
    ggplot2::geom_text(
      ggplot2::aes(y = pos_texto, label = rotulo, vjust = vjust_texto),
      size = 2.8, color = "gray30"
    ) +
    ggplot2::scale_x_date(date_labels = "%b\n%Y", date_breaks = "3 months") +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", suffix = "%"),
      limits = c(y_min, y_max)
    ) +
    ggplot2::labs(
      title = "IPCA — Variação Mensal",
      x     = NULL,
      y     = "Var. % mensal"
    ) +
    .tema()
}

#' Gráfico do IPCA acumulado em 12 meses com meta variável e banda
#'
#' @param df tibble com colunas `date`, `ipca_12m` (saída de calcular_acumulado_12m)
#'   e `meta_inflacao` (saída de preparar_meta_mensal)
#' @return objeto ggplot
grafico_ipca_12m <- function(df) {
  dados <- df |>
    dplyr::filter(!is.na(ipca_12m), !is.na(meta_inflacao)) |>
    dplyr::mutate(
      meta_sup = meta_inflacao + 1.5,
      meta_inf = meta_inflacao - 1.5
    )

  ultimo <- dplyr::slice_tail(dados, n = 1)
  rotulo_ultimo <- sprintf("%.2f%%", ultimo$ipca_12m) |>
    stringr::str_replace("\\.", ",")

  ggplot2::ggplot(dados, ggplot2::aes(x = date)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = meta_inf, ymax = meta_sup),
      fill = .cores$verde, alpha = 0.15
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = meta_inflacao),
      color = .cores$verde, linetype = "dashed", linewidth = 0.7
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = ipca_12m),
      color = .cores$azul, linewidth = 1
    ) +
    ggplot2::annotate(
      "label",
      x      = max(dados$date),
      y      = max(dados$ipca_12m, na.rm = TRUE),
      label  = rotulo_ultimo,
      hjust  = 1.1,
      vjust  = 1.3,
      size   = 3.5,
      fill   = "white",
      color  = .cores$azul,
      fontface = "bold",
      label.size = 0.3
    ) +
    ggplot2::scale_x_date(date_labels = "%Y") +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", suffix = "%")
    ) +
    ggplot2::labs(
      title = "IPCA — Acumulado 12 Meses e Meta",
      x     = NULL,
      y     = "Var. % 12 meses"
    ) +
    .tema()
}

#' Gráfico sazonal do IPCA: uma linha por ano
#'
#' @param df_saz tibble retornado por preparar_sazonal(), com colunas
#'   `mes`, `ano`, `ipca_mm`, `cor`, `destaque`
#' @return objeto ggplot
grafico_sazonal <- function(df_saz) {
  # Separar ano atual dos demais para sobrepor em destaque
  dados_hist <- dplyr::filter(df_saz, !destaque)
  dados_atual <- dplyr::filter(df_saz, destaque)
  ano_atual <- unique(dados_atual$ano)

  # Paleta nomeada para a legenda
  cores_anos <- df_saz |>
    dplyr::distinct(ano, cor) |>
    dplyr::arrange(ano) |>
    tibble::deframe()

  ggplot2::ggplot(mapping = ggplot2::aes(
    x = mes, y = ipca_mm, group = ano, color = factor(ano)
  )) +
    ggplot2::geom_line(data = dados_hist, linewidth = 0.5, alpha = 0.7) +
    ggplot2::geom_line(
      data = dados_atual,
      linewidth = 1.6, color = .cores$azul
    ) +
    ggplot2::scale_color_manual(
      values = cores_anos,
      name   = "Ano",
      guide  = ggplot2::guide_legend(nrow = 2)
    ) +
    ggplot2::scale_x_continuous(
      breaks = 1:12,
      labels = c("Jan","Fev","Mar","Abr","Mai","Jun",
                 "Jul","Ago","Set","Out","Nov","Dez")
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", suffix = "%")
    ) +
    ggplot2::labs(
      title    = paste0("IPCA — Comparação Sazonal (destaque: ", ano_atual, ")"),
      x        = NULL,
      y        = "Var. % mensal"
    ) +
    .tema()
}

#' Gráfico de barras horizontais com contribuições dos grupos ao IPCA
#'
#' @param df tibble com colunas `grupo` e `contribuicao` (recorte de um único mês,
#'   tipicamente `mes_atual` retornado por preparar_contribuicoes())
#' @return objeto ggplot
grafico_contribuicoes <- function(df) {
  dados <- df |>
    dplyr::mutate(
      grupo = forcats::fct_reorder(grupo, contribuicao),
      cor   = dplyr::if_else(contribuicao >= 0, .cores$azul, .cores$laranja),
      rotulo = sprintf("%+.2f p.p.", contribuicao) |>
        stringr::str_replace("\\.", ","),
      hjust_rot = dplyr::if_else(contribuicao >= 0, -0.15, 1.15)
    )

  lim_x <- max(abs(dados$contribuicao), na.rm = TRUE) * 1.35

  ggplot2::ggplot(dados, ggplot2::aes(x = contribuicao, y = grupo, fill = cor)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = rotulo, hjust = hjust_rot),
      size = 3, color = "gray20"
    ) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.4, color = .cores$cinza) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(decimal.mark = ",", suffix = " p.p."),
      limits = c(-lim_x, lim_x)
    ) +
    ggplot2::labs(
      title = "IPCA — Contribuição dos Grupos (p.p.)",
      x     = "Contribuição (p.p.)",
      y     = NULL
    ) +
    .tema()
}
