# Hello, everyone!
#
# This is a package for triangle-based actuary calculations.
# It helps to count reserves and other useful things.

#' Vytvoření trojúhelníku z primárních dat
#'
#' Funkce vezme zdrojová data a podle zadaných parametrů vytvoří trojúhelníkovou matici
#' (běžně používanou v aktuárských aplikacích).
#'
#' @param data \code{data.frame} – zdrojová data.
#' @param row_num \code{character} – jméno proměnné určující čísla řádků (obvykle vznik roku).
#' @param col_num \code{character} – jméno proměnné určující čísla sloupců (obvykle rok vývoje).
#' @param value \code{character} – jméno proměnné s hodnotami, které se sčítají.
#' @param cond_variable \code{character} (volitelně) – jméno jedné nebo více kategoriálních proměnných,
#'   podle kterých se filtrují data.
#' @param cond_value \code{character} (volitelně) – hodnoty proměnných zadaných v \code{cond_variable}.
#' @param rows \code{integer} (volitelně) – maximální počet řádků matice. Pokud není zadán,
#'   použije se maximum z \code{row_num}.
#'
#' @return Matice (\code{matrix}) o rozměru \code{rows x rows} s trojúhelníkovou strukturou.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' df <- data.frame(
#'   accident = c(1,1,2,2,3,3),
#'   dev = c(0,1,0,1,0,1),
#'   value = c(100, 50, 200, 80, 150, 60)
#' )
#'
#' create_triangle(df,
#'                 row_num = "accident",
#'                 col_num = "dev",
#'                 value   = "value")
#' }
create_triangle <- function(data, row_num, col_num, value, cond_variable = NULL, cond_value = NULL, rows = NULL){
  if(is.null(rows)){
    rows = max(data[[row_num]])
  }

  if(length(cond_variable) > 0){
    for (c in seq_along(cond_variable)) {
      data <- data |> dplyr::filter(!!rlang::sym(cond_variable[c]) == cond_value[c])
    }
  }
  trg_matrix <- matrix(NA, nrow = rows, ncol = rows)
  temp_data <- data |> dplyr::group_by(!!rlang::sym(row_num), !!rlang::sym(col_num)) |>
    dplyr::summarise(value = sum(!!rlang::sym(value)), .groups = "drop")

  for (i in 1:rows) {
    for (j in 1:rows){
      if(i + j > rows + 1) next
      act_val <- temp_data |>
        dplyr::filter(!!rlang::sym(row_num) == i,
                      !!rlang::sym(col_num) == j) |>
        dplyr::pull(value)
      act_val <- ifelse(length(act_val) == 0, 0, act_val)
      trg_matrix[i, j] <- act_val
    }
  }
  return(trg_matrix)
}

##############################
#' Vytvoří kumulativní trojúhelník
#'
#' Funkce převede trojúhelník z nekumulativní podoby na kumulativní,
#' tj. v každém řádku sčítá hodnoty po směru sloupců.
#'
#' @param trg \code{matrix}. Trojúhelník v nekumulativní podobě.
#' @return \code{matrix}. Trojúhelník v kumulativní podobě.
#' @export
create_cumulative_triangle <- function(trg) {
  rows <- nrow(trg)
  for (j in 1:rows) {
    for (i in 1:rows) {
      if (i + j > rows + 1 || j == 1) next
      trg[i, j] <- trg[i, j] + trg[i, j - 1]
    }
  }
  return(trg)
}

##############################
#' Vytvoří dekumulovaný trojúhelník
#'
#' Funkce převede trojúhelník z kumulativní podoby zpět na nekumulativní,
#' tj. odečítá předchozí sloupce.
#'
#' @param trg \code{matrix}. Trojúhelník v kumulativní podobě.
#' @return \code{matrix}. Trojúhelník v nekumulativní podobě.
#' @export
create_decumulative_triangle <- function(trg) {
  rows <- nrow(trg)
  for (j in rows:1) {
    for (i in rows:1) {
      if (i + j > rows + 1 || j == 1) next
      trg[i, j] <- trg[i, j] - trg[i, j - 1]
    }
  }
  return(trg)
}

##############################
#' Načte diagonálu z trojúhelníku
#'
#' Funkce vybere hodnoty z dané diagonály trojúhelníku a převede je na vektor.
#'
#' @param trg \code{matrix}. Trojúhelník.
#' @param diag_num \code{numeric}. Číslo diagonály (počítáno shora). Výchozí je počet řádků trojúhelníku (hlavní diagonála).
#' @return \code{numeric}. Vektor hodnot na zvolené diagonále.
#' @export
diag_reader <- function(trg, diag_num = nrow(trg)) {
  i <- diag_num
  j <- 1
  values <- numeric()
  while (i > 0) {
    values <- c(values, trg[i, j])
    i <- i - 1
    j <- j + 1
  }
  return(values)
}
