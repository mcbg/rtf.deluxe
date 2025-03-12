# These function group columns allowing header this groupings.
remove_head = \(x_vector, n) {
  if (length(x_vector) <= n) {
    return(NA)
  }
  else if (n == 0) {
    return(x_vector)
  }
  else {
    res = x_vector[-seq_len(n)]
    return(res)
  }
}

na_replace = \(x_vector, replace) {
  x_vector[is.na(x_vector)] = replace
  return(x_vector)
}

get_row_list = \(column_names) {
  column_list = column_names |> strsplit(split = '\t')
  n_header_row = column_list |> sapply(length) |> unlist() |> max()
  row_list = seq_len(n_header_row) |> lapply(\(i) {
    column_list |>
      lapply(remove_head, i - 1) |>
      lapply(na_replace, '') |>
      sapply(paste, collapse='\t')
  })
  return(row_list)
}

get_values_and_cellwidth = \(column_names, base_width) {
  column_names |>
    get_row_list() |>
    lapply(derive_merged_cell_values, base_width)
}

rtf_create_hierarchical_header = \(column_names, base_cell_width_cm) {
  header_row_list = get_values_and_cellwidth(column_names, base_cell_width_cm) |>
    rev()

  # derive border
  n_rows = length(header_row_list)
  border_values = rep('', n_rows)
  border_values[1] = '\\clbrdrt\\brdrs'
  border_values[n_rows] = border_values[n_rows] |>
    paste0('\\clbrdrb\\brdrs')

  # create rtf
  ans = mapply(\(values_and_width, border_control_word) {
    values = values_and_width$values
    width = values_and_width$width
    rtf_create_row(values, width, bold=TRUE,
      border_control_words = border_control_word)
  }, header_row_list, border_values)

  return(ans)
}

# base_width: width of each unmerged cell
derive_merged_cell_values = \(values, base_width) {
  # return list(values, width)

  # derive merged_cell_values
  merged_cell_values = c()
  merged_cell_values_index = c()
  j = 1
  for (i in seq_along(values)) {
    is_blank = is.na(values[i])
    is_following = i > 1 && !is_blank && !is.na(values[i - 1]) && values[i] == values[i - 1]
    if (is_blank) {
      merged_cell_values[j] = ''
      merged_cell_values_index[j] = i
      j = j + 1
    }
    else if (!is_following) {
      merged_cell_values[j] = values[i]
      merged_cell_values_index[j] = i
      j = j + 1
    }
  }

  # derive header matrix
  rows_list = list()
  for (i in seq_along(merged_cell_values)) {
    x = merged_cell_values[i]
    is_blank = x == ''
    if (is_blank) {
      res = rep(0, length(values))
      j = merged_cell_values_index[i]
      res[j] = 1
    }
    else {
      res_bool = values == x & !is.na(values)
      res = res_bool |> as.numeric()
    }
    rows_list[[i]] = res
  }

  header_matrix = do.call(rbind, rows_list)
  cell_widths = header_matrix %*% base_width |>
    as.vector()
  merged_values_clean = merged_cell_values |> deluxe_sub('\t.*', '')
  merged_values_clean[is.na(merged_values_clean)] = ''
  list(values = merged_values_clean, width = cell_widths)
}

# function, widen by dose -------------------------------------------------------------------

# the RTF functionality creates a hierachical header structure using tab separation
# so a table (data.frame or data.table) with variables named var1\theader var2\theader
# will be rendered thus:
# +-------------------+
# |      Header       |
# +--------+----------+
# |  Var1  |   Var2   |
# +--------+----------+

#' @export
widen_header_by = \(subdata, by_variable) {
  SD_list_unordered = subdata |> split(by=by_variable)
  SD_list = SD_list_unordered[order(names(SD_list_unordered))]
  statistic = SD_list[[1]][, .(Statistic)]
  SD_new_names = lapply(SD_list, \(subdata) {
    #if (!identical(subdata$Statistic, statistic$Statistic)) stop('wrong order')
    single_label = subdata[1, get(by_variable)]

    # remove
    subdata[[by_variable]] = NULL
    subdata[, Statistic := NULL]

    # new names
    names(subdata) = paste(names(subdata), single_label, sep='\t')

    return(subdata)
  })
  names(SD_new_names) = NULL

  # collect to one dataset
  ans = do.call(cbind, c(list(statistic), SD_new_names))

  return(ans)
}
