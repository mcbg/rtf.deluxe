# i/o

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

#' @export
replace_with_nonbreaking_spaces = \(dataset) {
  answer_list = lapply(dataset, deluxe_gsub, ' ', '\u00a0')
  answer_dataframe = as.data.frame(answer_list)
  names(answer_dataframe) = names(dataset) |> deluxe_gsub(' ', '\u00a0')

  return(answer_dataframe)
}


# metadata ----------------------------------------------------------------

#' Save output and metadata
#'
#' This functions saves the output and the metadata
#' as a JSON-file. Tables and listings are saved as RDS-files, whereas
#' figures are saved as PNG-files. A footnote is added with the time, date and program name
#' for traceability.
#'
#' @param output output can either be a data.frame, data.table, or a list of data.frames
#' , where each entry will be shown on it's own page. For figures provide a ggplot object.
#' @param path the path to the folder where files will be saved
#' @param name Name of the output. This will also be the file name.
#' @param number Appendix section number of output. For example 14.2.1 for a efficacy table.
#' @param title The title of the output. This is used for the header and the table of contents.
#' @param subtitle Optional subtitle
#' @param footnotes a character vector, where each entry will be shown on a separate line.
#' @param type 'table', 'listing', or 'figure'
#' @param program_name the name of the program that generated the output.
#' @param nonbreaking_space a bool indicating if normal spaces should be replaced with nonbreaking spaces when calculating the column width of tables.
#' @export
#' @examples
#' save_output_and_metadata(
#'   output=mtcars,
#'   path='myoutput',
#'   name='cars',
#'   number='1.1',
#'   title='Cars',
#'   subtitle='',
#'   footnotes='Source: 1974 Motor Trend US magazine',
#'   type='listing',
#'   program_name='example.r'
#' )
#'
save_output_and_metadata = \(
  output,
  path,
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
  if ('list' %in% class(output)) {
    if (nonbreaking_space) {
      output = output |> lapply(replace_with_nonbreaking_spaces)
    }
    filename = name |> paste0('.rds')
    table_path = path |> file.path(filename)
    saveRDS(output, file=table_path)
  }
  else if ('data.frame' %in% class(output) | 'data.table' %in% class(output)) {
    if (nonbreaking_space) {
      output_ready = output |> replace_with_nonbreaking_spaces()
    }
    else {
      output_ready = output
    }

    # checks
    has_any_nas = sapply(output_ready, anyNA) |> any()
    if (has_any_nas) stop('NAs in table/listing: ', title)
    if (nrow(output_ready) == 0) stop('0 row table: ', title)

    # make all character
    output_clean = output_ready |> lapply(as.character) |> as.data.frame(check.names=FALSE)

    # save table
    filename = name |> paste0('.rds')
    table_path = path |> file.path(filename)
    saveRDS(output_clean, file=table_path)
  }
  # save figure
  else if ('ggplot' %in% class(output)) {
    filename = name |> paste0('.png')
    fig_path = path |> file.path(filename)
    save_figure_default(filename=fig_path, plot=output)
    save_figure_default(filename=fig_path, plot=output)
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
  metadata_file_name = path |> file.path(name |> paste0('.json'))
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

#' Read all JSON metadata in folder
#'
#' @param path path that contains JSON metadata files
#' @returns A list where each entry contains the metadata of a output.
#' The metadata is stored as a list.
#' @export
read_metadata = \(path) {
  path |> list.files(pattern = '*.json', full.names = TRUE) |>
    lapply(jsonlite::read_json, simplifyVector = TRUE) |>
    sort_metadata()
}

# output, wrappers ------------------------------------------------------------

#' @export
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
    save_output_and_metadata(output=single_output, name=name, title=title, subtitle=subtitle, numbering=numbering, type=type, footnotes=footnotes, program_name=program_name)
  })
}

#' @export
save_outputs_by_list = \(output_list, metadata_function) {
  metadata_list = names(output_list) |> lapply(metadata_function)
  for (i in seq_along(output_list)) {
    single_output = output_list[[i]]
    single_metadata = metadata_list[[i]]
    save_output_and_metadata_by_list(single_output=single_output, single_metadata_list=single_metadata)
  }
}

# RTF i/o -----------------------------------------------------------------

#' Creates RTF-file with output
#'
#' @param metadata metadata object that you get from `read_metadata`.
#' @param metadata_path the path to the RDS files and PNG files of the output
#' @param output_filename path of the resulting RTF-file
#' @param document_title the title of the document on the front page
#' @param title_page_info A data.frame with a `field` and `value` columns. This information is shown on title page.
#' @param header_rtf Raw RTF-code that can be added to header
#' @export
write_rtf = \(metadata, metadata_path, output_filename, document_title, title_page_info, header_rtf='') {
  rtf_code = metadata |>
    create_tfl_document_by_metadata(
      header_text=header_rtf,
      output_directory=metadata_path,
      document_title=document_title,
      title_page_info=title_page_info)

  # write to disk
  cat(rtf_code, file=output_filename, sep='\n')
}

