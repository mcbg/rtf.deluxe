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

#' @export
blankout_duplicates_dataset = \(x_dataset, variables) {
  ans = copy(x_dataset)
  ans[, (variables) := lapply(.SD, blankout_duplicates), .SDcols=variables]
  return(ans)
}

# functions, split table --------------------------------------------------

#' Split dataset into list of datasets
#'
#' This is so you can manually decide how to split a table into pages.
#' For example you could have 4 unique values of `Visit` per page.
#'
#' @import data.table
#' @export
#' @examples
#' iris$Species = as.character(iris$Species)
#' split_into_list(iris, 'Species', 2)
split_into_list = \(dataset, by_variable, values_per_page, blankout=TRUE) {
  dataset = data.table::as.data.table(dataset)
  unique_values = dataset[, unique(get(by_variable))]
  dataset[, page__ := match(get(by_variable), unique_values) %/% (values_per_page + 1)]
  page_list = dataset |> split(by='page__', keep.by=FALSE)

  if (blankout) {
    for (i in seq_along(page_list)) {
      page_list[[i]][[by_variable]] <- rtf.deluxe::blankout_duplicates(page_list[[i]][[by_variable]])
    }
  }
  return(page_list)
}

# output utility ------------------------------------------------------------

#' @export
blankout_duplicates = \(x_vector) {
  loop_indices = seq_along(x_vector) |> setdiff(1)
  answer = x_vector
  for (i in loop_indices) {
    if (x_vector[i] == x_vector[i - 1]) {
      answer[i] = ''
    }
  }
  return(answer)
}
