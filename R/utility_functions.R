# list filter -------------------------------------------------------------

#' @export
list_filter = \(input_list, expr) {
  captured_expression = substitute(expr)
  matching_entries = sapply(input_list, \(x) {
    eval(captured_expression, envir = x)
  })
  input_list[matching_entries]
}

# functions ---------------------------------------------------------------
# this are pipe friendly versions of base R functions, where text
# is the first argument.

deluxe_sub = \(text, regex, replace) sub(regex, replace, text, perl=TRUE)
deluxe_gsub = \(text, regex, replace) gsub(regex, replace, text, perl=TRUE)
deluxe_split = \(text, sep) strsplit(text, split=sep)[[1]]
deluxe_substr = \(text, i_start, i_end) substr(text, i_start, i_end)[[1]]

# functions, blankout grouping --------------------------------------------

blankout_duplicates_dataset = \(x_dataset, variables) {
  ans = copy(x_dataset)
  ans[, (variables) := lapply(.SD, blankout_duplicates), .SDcols=variables]
  return(ans)
}

# functions, split table --------------------------------------------------

split_multi_per_entry = \(dataset, parameter, parameter_values_per_entry) {
  result = list() # a list of data.tables
  x_list = dataset |> split(by=parameter)
  x_index = seq_along(x_list)
  page_index = (x_index - 1) %/% parameter_values_per_entry + 1

  for (i in unique(page_index)) {
    subtables_on_page_index = which(page_index == i)
    subtables_on_page = x_list[subtables_on_page_index] |> rbindlist()
    result[[i]] = subtables_on_page
  }
  return(result)
}

# output utility ------------------------------------------------------------

#' @export
blankout_duplicates = \(x_vector) {
  loop_indices = seq_along(x_vector) |> setdiff(1)
  ans = x_vector
  for (i in loop_indices) {
    if (x_vector[i] == x_vector[i-1]) {
      ans[i] = ''
    }
  }
  return(copy(ans))
}
