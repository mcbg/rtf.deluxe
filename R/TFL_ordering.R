# Function that order TFL output by numbering.


tfl_number_to_numeric_vector = function(tfl_number) {
  # check validity
  if (!all(grep('^(\\d+\\.)*\\d$', tfl_number))) stop('invalid numbering: ', tfl_number)

  # derive numeric vector
  vec = tfl_number |>
    strsplit('\\.') |>
    unlist() |>
    as.numeric()
  return(vec)
}

tfl_number_less_than = \(tfl_number, tfl_number2) {

  x = tfl_number |> tfl_number_to_numeric_vector()
  y = tfl_number2 |> tfl_number_to_numeric_vector()

  N = max(length(x), length(y))
  for (i in seq_len(N)) {
    if (length(x) < i) {
      return(TRUE)
    }
    else if (length(y) < i) {
      return(FALSE)
    }
    else if (x[i] < y[i]) {
      return(TRUE)
    }
    else if (x[i] > y[i]) {
      return(FALSE)
    }
  }
  warning('duplicated tfl number')
  return(TRUE)
}

#' Sort metadata by numbering
#'
#' The `numbering` value contains sections (for example 14.2.1) stored as
#' characters. This function sorts the metadata to make sure the numbering values
#' are in the correct order. This allows us to generate RTF-code for each output and then
#' combine them in the correct order.
#'
#' @param metadata a list generated from read_metadata
#' @returns metadata ordered by `numbering` values.
#' @export
sort_metadata = \(metadata) {
  numbering = sapply(metadata, with, numbering)
  ordered_indices = tfl_number_order(numbering)
  metadata_sorted = metadata[ordered_indices]
  return(metadata_sorted)
}

tfl_number_order = \(tfl_number_list) {
  if (length(tfl_number_list) == 1) {
    return(1)
  }

  n = length(tfl_number_list)
  res = seq_along(tfl_number_list)
  # sorting algorithm
  for (i in 1:(n-1)) {
    for (j in 1:(n-i)) {
      x = tfl_number_list[[res[j]]]
      y = tfl_number_list[[res[j + 1]]]
      if (tfl_number_less_than(y, x)) {
        temp = res[j]
        res[j] <- res[j+1]
        res[j+1] <- temp
      }
    }
  }
  return(res)
}
