# Functions to assemble RTF-files

combine_pages_list = \(pages_list) {
  N = length(pages_list)
  if (N == 1) {
    return(pages_list[[1]])
  }
  tail_pages = pages_list[-1] |>
    lapply(append, '\\page', after = FALSE) |>
    unlist()
  result = c(pages_list[[1]], tail_pages)
  return(result)
}

#' @export
assemble_rtfs_files = \(file_names, titles, numbering, header_text = '') {
  # sort
  tfl_ordering = numbering |> tfl_number_order()
  file_names_sorted = file_names[tfl_ordering]
  titles_sorted = titles[tfl_ordering]


  # create code for each output
  name = basename(file_names)
  N = length(file_names)
  references = paste0('ref', seq_along(file_names))
  rtf_content_list = lapply(seq_along(file_names), \(i) {
    single_file_name = file_names_sorted[i]
    single_title = titles_sorted[i]
    rtf_content = readLines(single_file_name)
    bookmark = rtf_create_bookmark(references[i])
    #page_break = ifelse(i == N, '', '\\page')
    page_break='' # TEMP
    output_code = c(bookmark, extract_file(rtf_content), page_break)
    return(output_code)
  })

  # combine code into one file
  rtf_toc = create_table_of_contents(rtf_content_list, titles_sorted, references)
  rtf_all_content = c(
    rtf_toc, '\\page',
    rtf_content_list |> unlist()
  )
  full_document = rtf_add_head_and_tail(rtf_all_content, header_text = header_text)
  return(full_document)
}

#' @export
create_tfl_document_by_metadata = \(metadata, header_text='', output_directory) {

  # sort metadata
  tfl_ordering = metadata |>
    sapply(`[[`, 'numbering') |>
    tfl_number_order()
  metadata_sorted = metadata[tfl_ordering]

  # derive references
  references = sapply(metadata_sorted, with, {
    type_format = c('figure' = 'Figure', 'table' = 'Table', 'listing' = 'Listing')
    paste(type_format[type], numbering, sep='_')
  })

  # content
  rtf_content_list = metadata_sorted |>
    seq_along() |>
    lapply(\(i) {
      rtf_create_output_by_metadata(metadata_sorted[[i]], reference = references[i], output_directory=output_directory)
    })
  rtf_content = rtf_content_list |> unlist()

  # table of content
  output_full_titles = sapply(metadata_sorted, with, {
    type_format = c('figure' = 'Figure', 'table' = 'Table', 'listing' = 'Listing')
    full_title = paste(type_format[type], numbering, title)
    full_title
  })
  rtf_toc = create_table_of_contents(rtf_content_list, output_full_titles, references = references)

  # combine
  full_document = rtf_add_head_and_tail(c(rtf_toc, '\\page', rtf_content), header_text=header_text)
  return(full_document)
}

#' @export
create_standalone_rtf_file = \(output, output_title=NULL) {
  # add each output to document
  rtf_pages = rtf_create_table(output)
  combined = combine_pages_list(rtf_pages)

  # combine
  full_document = rtf_add_head_and_tail(combined, header_text = '')
  return(full_document)
}
