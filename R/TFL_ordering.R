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

  for (i in seq_along(x)) {
    if (length(x) < i) {
      return(TRUE)
    }
    if (length(y) < i) {
      return(FALSE)
    }
    if (x[i] < y[i]) {
      return(TRUE)
    }
    else if (x[i] > y[i]) {
      return(FALSE)
    }
  }
  warning('duplicated tfl number')
  return(TRUE)
}

tfl_number_order = \(tfl_number_list) {
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
