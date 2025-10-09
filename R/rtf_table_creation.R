
# checks ------------------------------------------------------------------

check_table = \(tfl_table) {
  if ('data.table' %in% class(tfl_table) & !exists('data.table'))
    stop('table with class `data.table` but data.table is not loaded')
  if (tfl_table |> nrow() == 0)
    stop('table with 0 rows')
}

# functions, create table ---------------------------------------------------------------

# rtf_create_table returns a list of rtf code, where each page
# corresponds to an entry of the list.
rtf_create_table = \(
  table_input, # data.frame, data.table, list, flextable
  include_header=TRUE,
  cell_text_control_words='\\qc',
  margin_cm=0.25,
  max_rows_per_page = getOption('rtf.deluxe.max_rows_per_page'),
  cell_width_cm=NULL # use to manually specify
) {

  # extra data from table_input
  if ('data.frame' %in% class(table_input)) {
    header = names(table_input)
    if (!include_header) header = rep('', length(names(table_input))) # length zero string if header isnt included

    dataset = table_input
  }
  else if ('flextable' %in% class(table_input)) {
    dataset = table_input$body$dataset
    header = table_input$header$content$data |>
      tail(1) |> # ignore multiple row headers (TEMPORARY FIX)
      sapply(\(x) x$txt)
  }
  else {
    stop('invalid class ', class(dataset))
  }

  # check
  check_table(dataset)

  # derive cellx control words
  if (is.null(cell_width_cm)) {
    largest_nchar_rows = sapply(dataset, character_count_largest_word)
    largest_nchar_header = header |>
      deluxe_sub('\t.*', '') |>
      sapply(character_count_largest_word)
    largest_nchar = pmax(largest_nchar_rows, largest_nchar_header)
    cell_width_cm = largest_nchar |>
      character_to_cm() |>
      sapply(max, 2)
  }

  # create header
  if (include_header) {
    header_rtf = rtf_create_hierarchical_header(header, base_cell_width_cm=cell_width_cm, cell_text_control_words=cell_text_control_words) |> unlist()
  }
  else {
    header_rtf = ''
  }

  # prepare rows
  table_rows = dataset_to_row_list(dataset)
  table_rows_length = length(table_rows)
  table_rows_indices = seq_len(table_rows_length)
  table_rows_page_index = 1 + table_rows_indices %/% max_rows_per_page

  # prepare pages
  number_of_pages = local({
    extra_pages = ifelse(table_rows_length %% max_rows_per_page == 0, 0, 1)
    (table_rows_length %/% max_rows_per_page) + extra_pages
  })

  # if a table cant fit on 1 page, the table will be split into
  # sub-tables that each have their own page
  sub_tables = lapply(seq_len(number_of_pages), \(page_index) {
    sub_table_row_index = which(page_index == table_rows_page_index)
    sub_table_contents = table_rows[sub_table_row_index]
    sub_table_length = length(sub_table_contents)

    # split last row to add border
    contents_excl_last = sub_table_contents[-sub_table_length]
    contents_last = sub_table_contents[[sub_table_length]]

    # create sub-table for page
    rows_rtf = sapply(contents_excl_last, rtf_create_row, cell_width_cm=cell_width_cm, text_control_words=cell_text_control_words) |> unlist() # is empty it return list() if unlist() is not used

    last_row_border = ifelse(include_header, '\\clbrdrb\\brdrs', '')
    last_row_rtf = rtf_create_row(contents_last, border_control_words=last_row_border, cell_width_cm=cell_width_cm, text_control_words=cell_text_control_words)

    # paragraph is to ensure that \\page works
    c('{', header_rtf, rows_rtf, last_row_rtf, '}')
  })
  return(sub_tables)
}

# functions, create row ---------------------------------------------------

rtf_create_row = \(cells, cell_width_cm, bold = FALSE, border_control_words = '', text_control_words) {
  # derive cellx
  cell_width_twips = (cell_width_cm) |>
    cm_to_twip()
  cellx_values = cell_width_twips |>
    cumsum() |>
    ceiling()
  cellx_control_words = paste0('\\cellx', cellx_values)
  cell_control_words = c(
    text_control_words,
    ifelse(bold, '\\b', '\\b0'),
    ' '
  ) |>
    paste(collapse = '')

  # derive cells
  rtf_cells = cells |> seq_along() |> sapply(\(i) {
    cell = cells[i]

    # align left for the first cell
    if (i == 1)  control_words = sub('\\\\qc', '\\qr', cell_control_words)
    else control_words = cell_control_words

    # collect rtf code
    rtf_code = paste0('\\pard\\intbl', control_words, cell, '\\cell')

    return(rtf_code)
  })

  # collect rtf code
  answer = c('\\trowd',
    '\\trgaph108',
    '\\trpaddl108\\trpaddt57\\trpaddb57\\trpaddr108\\trpaddfl3\\trpaddft3\\trpaddfb3', # cell margins
    paste0('\\clvertalc', border_control_words, cellx_control_words),
    rtf_cells,
    '\\trqc\\row') # trqc centers the table

  return(answer)
}
