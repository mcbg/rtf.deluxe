# i/o
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

# metadata


#' @export
save_output_and_metadata = \(
  tfl_output,
  tfl.path,
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
