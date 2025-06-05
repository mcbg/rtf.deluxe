# list filter -------------------------------------------------------------

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

# output i/o ------------------------------------------------------------

# arguments ggsave
plot_configuration = list(
  width  = 22,
  height = 12,
  units = 'cm',
  device = png,
  type = 'cairo',
  dpi = 600
)

save_figure_default = \(...) do.call(ggsave, c(list(...), plot_configuration))

save_metadata = \(file_name, ...) {
  metadata = list(...)
  jsonlite::write_json(metadata, file_name)
}

replace_with_nonbreaking_spaces = \(dataset) {
  ans = lapply(dataset, deluxe_gsub, ' ', '\u00a0')
  names(ans) = names(dataset) |> deluxe_gsub(' ', '\u00a0')
  class(ans) = class(dataset)
  return(copy(ans))
}

save_output_and_metadata = \(
  tfl_output,
  # metadata
  name,
  numbering,
  title,
  subtitle,
  footnotes=NULL,
  type='table',
  program_name,
  nonbreaking_space=(type == 'table')
) {


  # save table
  if ('list' %in% class(tfl_output)) {
    if (nonbreaking_space) {
      tfl_output = tfl_output |> lapply(replace_with_nonbreaking_spaces)
    }
    filename = name |> paste0('.rds')
    table_path = tfl.path |>
      file.path(filename)
    saveRDS(tfl_output, file=table_path)
  }
  else if ('data.table' %in% class(tfl_output)) {
    if (nonbreaking_space) {
      tfl_output = tfl_output |> replace_with_nonbreaking_spaces()
    }
    has_any_nas = sapply(tfl_output, anyNA) |> any()
    if (has_any_nas) stop('NAs in table/listing: ', title)
    if (nrow(tfl_output) == 0) stop('0 row table: ', title)
    # make all character
    tfl_output_clean = tfl_output |> lapply(as.character) |> as.data.table()
    filename = name |> paste0('.rds')
    table_path = tfl.path |>
      file.path(filename)
    saveRDS(tfl_output_clean, file=table_path)
  }
  # save figure
  else if ('ggplot' %in% class(tfl_output)) {
    filename = name |> paste0('.png')
    fig_path = tfl.path |> file.path(filename)
    save_figure_default(filename=fig_path, plot=tfl_output)
  }
  else {
    stop('invalid class')
  }

  # add tracing to footer
  tracing_note = local({
    dt = Sys.Date()
    tm = format(Sys.time(), '%H:%M')
    footnote = paste0('Program name: ', program_name,', Date: ', dt, ', Time: ', tm)
  })
  extended_footnote = c(footnotes, tracing_note)

  # export metadata
  metadata_file_name = tfl.path |> file.path(name |> paste0('.json'))
  save_metadata(
    file_name = metadata_file_name,
    name = name,
    numbering = numbering,
    title = title,
    type = type,
    subtitle = subtitle,
    footnotes = extended_footnote,
    program_name = program_name
  )
}

save_output_and_metadata_by_list = \(single_output, single_metadata_list) {
  with(single_metadata_list, {
    # defaults
    if (!exists('footnotes')) {
      footnotes = NULL
    }

    if (!exists('subtitle')) {
      subtitle = ''
    }

    # save
    save_output_and_metadata(tfl_output=single_output, name=name, title=title, subtitle=subtitle, numbering=numbering, type=type, footnotes=footnotes, program_name=program_name)
  })
}

save_outputs_by_list = \(output_list, metadata_function) {
  metadata_list = names(output_list) |> lapply(metadata_function)
  for (i in seq_along(output_list)) {
    single_output = output_list[[i]]
    single_metadata = metadata_list[[i]]
    save_output_and_metadata_by_list(single_output=single_output, single_metadata_list=single_metadata)
  }
}
