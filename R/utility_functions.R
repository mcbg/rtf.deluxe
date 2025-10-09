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
blankout_duplicates_dataset = \(dataset, grouping_variables) {
  # checks
  if(!'data.table' %in% class(dataset)) stop('dataset must be data.table')

  # define
  combined_group = dataset[, do.call(paste, .SD), .SDcols = grouping_variables]
  loop_indices = seq_along(combined_group) |> setdiff(1)
  answer = dataset |> copy()

  for (i in loop_indices) {
    if (combined_group[i] == combined_group[i - 1]) {
      for (variable in grouping_variables) {
        answer[[variable]][i] = ''
      }
    }
  }

  return(answer)
}

# combine to single column  -----------------------------------------------

derive_single_column_subgroup = \(dataset_group, group, subgroup) {
  value_variables = names(dataset_group) |> setdiff(c(group, subgroup))

  # header row
  header_row = data.table(group__ = dataset_group[1, get(group)])

  for (variable in value_variables) {
    header_row[[variable]] = ''
  }

  # body rows
  body_rows = copy(dataset_group)
  body_rows[, group__ := paste0('    ', get(subgroup))]
  body_rows[, (group) := NULL]
  body_rows[, (subgroup) := NULL]

  # collect
  answer = rbind(header_row, body_rows)
  setnames(answer, old = 'group__', new = ' ')

  return(answer)
}

#' @export
derive_single_column = \(dataset, group, subgroup) {
  answer = dataset |>
    split(by=group, keep.by = TRUE) |>
    lapply(derive_single_column_subgroup, group = group, subgroup=subgroup) |>
    rbindlist()
  return(answer)
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
#' split_into_list(iris, 'Species', 2, blankout=FALSE)
split_into_list = \(dataset, by_variable, values_per_page, blankout=TRUE) {
  dataset = data.table::as.data.table(dataset)

  # define page
  dataset[, group__ := do.call(paste, .SD), .SDcols = by_variable]
  unique_values = dataset$group__ |> unique()
  dataset[, page__ := (match(group__, unique_values) - 1) %/% (values_per_page)]
  dataset[, group__ := NULL]

  # split into pages
  page_list = dataset |> split(by='page__', keep.by=FALSE)

  if (blankout) {
    page_list_blanked = page_list |> lapply(blankout_duplicates_dataset, by_variable)
    return(page_list_blanked)
  }
  else {
    return(page_list)
  }
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
