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

#' Součet po diagonálách trojúhelníku
#'
#' Postupně sčítá hodnoty po jednotlivých diagonálách (od hlavní po poslední).
#' (tj. pro fixní rozdíl \eqn{i-j}). Nevyžaduje \code{diag_reader}.
#'
#' @param trg Trojúhelník (matrix/data.frame) se spodním trojúhelníkem hodnot (nad trojúhelníkem bývá \code{NA}).
#' @param order Pořadí diagonál ve výstupu: \code{"main_to_last"} (hlavní → nejkratší)
#'   nebo \code{"last_to_main"} (nejkratší → hlavní). Default \code{"main_to_last"}.
#' @return Numeric vektor délky \code{nrow(trg)} se součty po diagonálách.
#' @export
diag_sums <- function(trg, order = c("main_to_last", "last_to_main")) {
  order <- match.arg(order)
  x <- as.matrix(trg)
  n <- nrow(x)
  # k = 0,...,n-1 je offset pro diagonály i-j = k (hlavní = 0, nejkratší = n-1)
  sums <- vapply(0:(n - 1), function(k) {
    i <- (1 + k):n
    j <- 1:(n - k)
    sum(x[cbind(i, j)], na.rm = TRUE)
  }, numeric(1))
  if (order == "main_to_last") sums else rev(sums)
}


#' Agregace kumulativního trojúhelníku na roční trojúhelník
#'
#' Vytvoří roční trojúhelník součtem \code{period} po sobě jdoucích období na diagonálách
#' kumulativního trojúhelníku.
#'
#' @param cum_trg \code{matrix} kumulativního trojúhelníku.
#' @param period Integer; kolik dílčích období tvoří 1 rok (např. \code{4} = čtvrtletí).
#'
#' @return \code{matrix} ročního trojúhelníku (\code{years} x \code{years}), kde \code{years = floor(nrow(cum_trg)/period)}.
#' @seealso \code{\link{diag_reader}}
#' @export
create_annual_triangle <- function(cum_trg, period = 4){
  rows = nrow(cum_trg)
  years = floor(rows / period)
  trg <- matrix(NA, ncol = years, nrow = years)
  diag_num <- rows
  for(d in years:1){
    act_diag <- diag_reader(trg = cum_trg, diag_num = diag_num)
    diag_length <- floor(length(act_diag) / period)
    for (n_in_diag in 1:diag_length) {
      trg[d-n_in_diag+1, n_in_diag] <- sum(act_diag[((n_in_diag-1)*period+1):(n_in_diag*period)])
    }
    diag_num <- diag_num - period
  }
  return(trg)
}


#' Run-off kontrola kumulativního trojúhelníku
#'
#' Porovnává součty hlavní diagonály s předchozími diagonálami a vrací rozdíly
#' (indikativní míra nedosběru/rozběhu).
#'
#' @param cum_trg \code{matrix} kumulativního trojúhelníku.
#'
#' @details Pro každou diagonálu kromě hlavní se počítá rozdíl součtu „ocasu“ hlavní diagonály
#'  a součtu dané diagonály.
#'
#' @return Numerický vektor rozdílů (od nejstarší po nejnovější).
#' @seealso \code{\link{diag_reader}}
#' @export
create_run_off_check <- function(cum_trg){
  main_diag <- diag_reader(cum_trg)
  run_offs <- as.numeric()
  for (i in 1:(nrow(cum_trg)-1)){
    second_diag <- diag_reader(trg = cum_trg, diag_num = nrow(cum_trg) - i)
    run_offs[i] <- sum(main_diag[(1+i):length(main_diag)]) - sum(second_diag)
  }
  return(rev(run_offs))
}


#' Trojúhelník vývojových faktorů (link ratios) z kumulativního trojúhelníku
#'
#' @param cum_trg \code{matrix} kumulativního trojúhelníku.
#'
#' @details Pro \eqn{j>1} a existující hodnoty platí \eqn{DF_{i,j} = C_{i,j}/C_{i,j-1}}.
#'  První sloupec je \code{NA}. Buňky za „okrajem“ trojúhelníku jsou \code{NA}.
#'
#' @return \code{matrix} se stejnými rozměry jako \code{cum_trg} s vývojovými faktory.
#' @export
create_chl_coef_triangle <- function(cum_trg){
  chl_trg <- matrix(NA, nrow = nrow(cum_trg), ncol = ncol(cum_trg))
  rows <- nrow(cum_trg)
  for (i in 1:nrow(cum_trg)) {
    for (j in 1:ncol(cum_trg)) {
      if(i+j > rows + 1 | j == 1){next}
      chl_trg[i, j] <- cum_trg[i,j] / cum_trg[i,j-1]
    }
  }
  return(chl_trg)
}


#' Výpočet průměrných CH-L koeficientů po sloupcích
#'
#' Počítá pro každý sloupec (vývojový věk) poměr součtu kumulativ za \code{j} a \code{j-1}
#' v posledních \code{act_length} diagonálách. \code{"full"} znamená použít všechny dostupné diagonály.
#'
#' @param cum_trg \code{matrix} kumulativního trojúhelníku.
#' @param chl_length Vektor \code{character}/\code{numeric}; délky oken (např. \code{c("full", 3, 5)}).
#'
#' @return \code{data.frame} s řádky = nastavení délek a sloupci = vývojové věky (0,1,2,\dots).
#'  První sloupec (věk 0) je \code{NA}.
#' @importFrom dplyr coalesce
#' @export
create_chl_coefs <- function(cum_trg, chl_length = "full") {
  chl_names <- as.numeric()
  rows <- nrow(cum_trg)
  chl_results <- matrix(NA, nrow = length(chl_length), ncol = ncol(cum_trg))
  for (i in 1:length(chl_length)) {
    act_length <- ifelse(chl_length[i] == "full", rows, as.numeric(chl_length[i]))
    chl_names[i] <- act_length
    for (j in 1:(ncol(cum_trg))) {
      chl_results[i, j] <- coalesce(
        sum(cum_trg[max((rows - j + 1) - act_length + 1, 1):(rows - j + 1 ), j], na.rm = TRUE) /
          sum(cum_trg[max((rows - j + 1) - act_length + 1, 1):(rows - j + 1 ), j - 1], na.rm = TRUE),
        1
      )
    }
  }
  chl_results[, 1] <- NA
  colnames(chl_results) <- 0:(ncol(chl_results)-1)
  df_results <- data.frame(CH_L_lengths = paste0("chain_ladder - ", chl_length))
  df_results <- cbind(df_results, chl_results)
  return(df_results)
}


#' Produkt vývojových koeficientů (ultimate multiplikátory)
#'
#' Vypočítá postupné součiny (od aktuálního věku dále) v řádku s CH-L koeficienty.
#'
#' @param chl_coefs data.frame/matrix s CH-L koeficienty (sloupce = věky).
#' @param name Popisek prvního prvku výsledku.
#' @return Vektor stejné délky jako počet sloupců vstupu; první prvek je \code{name},
#' druhý prvek je \code{NA} (kompatibilita se strukturou koeficientů podle věku)
#' a od třetí pozice jsou postupné součiny koeficientů pro daný vývojový věk směrem k ultimu.
#' @export
create_product_coefs <- function(chl_coefs, name = "Product"){
  mat <- if (is.data.frame(chl_coefs)) as.matrix(chl_coefs) else chl_coefs
  product_coefs <- vector("list", ncol(mat))
  for (i in seq_len(ncol(mat))) {
    product_coefs[[i]] <- tryCatch({
      suppressWarnings(prod(as.numeric(mat[1, i:ncol(mat)]), na.rm = TRUE))
    }, error = function(e) NA)
  }
  product_coefs[[1]] <- name
  product_coefs[[2]] <- NA
  unlist(product_coefs, use.names = FALSE)
}

#' Průměrné vývojové faktory napříč diagonálami
#'
#' Z diagonální reprezentace trojúhelníku vývojových faktorů \code{chl_trg} počítá průměry
#'  po řádcích přes \code{avg_length} posledních diagonál.
#'
#' @param chl_trg \code{matrix} vývojových faktorů (např. z \code{\link{create_chl_coef_triangle}}).
#' @param avg_length Vektor \code{character}/\code{numeric}; délky oken, \code{"full"} = všechny dostupné.
#'
#' @return \code{data.frame} s řádky = nastavení délek a sloupci = vývojové věky (0,1,2,\dots).
#' @seealso \code{\link{create_chl_coef_triangle}}, \code{\link{diag_reader}}
#' @export
create_avg_coefs <- function(chl_trg, avg_length = "full") {
  rows <- nrow(chl_trg)
  for(i in rows:1){
    if(i == rows){
      diag_data <- diag_reader(chl_trg, diag_num = i)
    } else{
      diag_data <- cbind(diag_data, c(diag_reader(trg = chl_trg, diag_num =  i), rep(NA, rows - i)))
    }
  }
  avg_names <- as.numeric()
  avg_results <- matrix(NA, nrow = length(avg_length), ncol = ncol(chl_trg))
  for (i in 1:length(avg_length)) {
    act_length <- ifelse(avg_length[i] == "full", rows, as.numeric(avg_length[i]))
    avg_names[i] <- act_length
    avg_results[i,] <- rowMeans(diag_data[,1:act_length], na.rm = TRUE)
  }
  colnames(avg_results) <- 0:(ncol(avg_results)-1)
  df_results <- data.frame(Average_DF_lengths = paste0("Average DF - ", avg_length))
  df_results <- cbind(df_results, avg_results)
  return(df_results)
}


#' Pojmenování řádků a sloupců trojúhelníku
#'
#' Nastaví názvy řádků dle \code{claim_period_names} a názvy sloupců jako \code{0:(ncol(trg)-1)}.
#'
#' @param trg \code{matrix} trojúhelníku.
#' @param claim_period_names \code{character} vektor názvů (např. roky/čtvrtletí).
#'
#' @return \code{matrix} s nastavenými \code{rownames} a \code{colnames}.
#' @export
triangle_namer <- function(trg, claim_period_names){
  claim_period_names_short <- claim_period_names[1:nrow(trg)]
  colnames(trg) <- 0:(ncol(trg)-1)
  rownames(trg) <- claim_period_names_short
  return(trg)
}


#' Plotly vizualizace vybraných sloupců z matice (čas na ose X jako číslo)
#'
#' Vykreslí vybrané vývojové věky (sloupce) jako čáry s body. Osa X je \code{as.numeric(rownames)}.
#'
#' @param data_matrix \code{matrix} nebo \code{data.frame} s \code{rownames} = časové značky.
#' @param columns Vektor názvů sloupců (nebo čísel převeditelných na \code{character}) k vykreslení.
#'
#' @return Objekt \code{plotly} s více trasami.
#' @importFrom plotly plot_ly add_trace
#' @export
create_chl_trg_visualization <- function(data_matrix, columns) {
  columns = as.character(columns)
  data <- as.data.frame(data_matrix)
  data$time <- rownames(data_matrix)
  if (any(!columns %in% colnames(data))) {
    stop("One or more specified columns do not exist in the data matrix.")
  }
  selected_data <- data[, c("time", columns)]
  initial_column <- columns[1]
  plot <- plot_ly(
    data = selected_data,
    x = ~as.numeric(time),
    y = as.numeric(selected_data[[initial_column]]),
    type = 'scatter', mode = 'lines+markers', name = initial_column
  )
  if (length(columns) > 1) {
    for (col in columns[-1]) {
      plot <- add_trace(plot, y = as.numeric(selected_data[[col]]), name = col)
    }
  }
  return(plot)
}


#' Plotly vizualizace vybraných sloupců z matice (čas na ose X jako text)
#'
#' Stejné jako \code{\link{create_chl_trg_visualization}}, ale osu X vykreslí jako kategorii (text).
#'
#' @inheritParams create_chl_trg_visualization
#'
#' @return Objekt \code{plotly}.
#' @importFrom plotly plot_ly add_trace
#' @export
create_chl_trg_visualization_pvzp <- function(data_matrix, columns) {
  columns = as.character(columns)
  data <- as.data.frame(data_matrix)
  data$time <- rownames(data_matrix)
  if (any(!columns %in% colnames(data))) {
    stop("One or more specified columns do not exist in the data matrix.")
  }
  selected_data <- data[, c("time", columns)]
  initial_column <- columns[1]
  plot <- plot_ly(
    data = selected_data,
    x = ~time,
    y = as.numeric(selected_data[[initial_column]]),
    type = 'scatter', mode = 'lines+markers', name = initial_column
  )
  if (length(columns) > 1) {
    for (col in columns[-1]) {
      plot <- add_trace(plot, y = as.numeric(selected_data[[col]]), name = col)
    }
  }
  return(plot)
}

#' Dopsání poslední diagonály z nového trojúhelníku
#'
#' Rozšíří starý trojúhelník o řádek a sloupec s \code{NA} a doplní je
#' hodnotami z nového trojúhelníku po diagonále (aktuální období).
#'
#' @param old_trg matrix/data.frame se starým trojúhelníkem.
#' @param new_trg matrix/data.frame s novým trojúhelníkem (o 1 delší hrana).
#' @return \code{matrix} rozměru \code{new_trg} s doplněnou poslední diagonálou.
#' @importFrom dplyr coalesce
#' @export
diag_writer <- function(old_trg, new_trg){
  old_trg <- as.matrix(old_trg)
  new_trg <- as.matrix(new_trg)
  old_trg <- rbind(old_trg, rep(NA_real_, ncol(old_trg)))
  old_trg <- cbind(old_trg, rep(NA_real_, nrow(old_trg)))
  matrix(
    coalesce(as.numeric(old_trg), as.numeric(new_trg)),
    nrow = nrow(new_trg),
    byrow = FALSE
  )
}
