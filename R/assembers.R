
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
assemble_rtfs_files = \(file_names, output_titles) {
  name = basename(file_names)
  rtf_content_list = lapply(seq_along(file_names), \(i) {
    single_file_name = file_names[i]
    single_title = output_titles[i]
    rtf_content = readLines(single_file_name)
    reference = gsub('\\W', '_', single_title) # used to link table of contents entries to output headers
    bookmark = rtf_create_bookmark(reference)
    c(bookmark, extract_file(rtf_content), '\\page')
  })
  rtf_toc = create_table_of_contents(rtf_content_list, output_titles)
  rtf_all_content = c(
    rtf_toc,'\\page',
    rtf_content_list |> unlist()
  )
  full_document = rtf_add_head_and_tail(rtf_all_content, header_text = '')
  return(full_document)
}

#' @export
create_tfl_document_by_metadata = \(metadata, header_text='') {
  # sort metadata
  tfl_ordering = metadata |>
    sapply(`[[`, 'original_numbering') |>
    tfl_number_order()
  metadata_sorted = metadata[tfl_ordering]

  # add each output to document

  id_lookup = sapply(metadata_sorted, with, name)
  names(id_lookup) = sapply(metadata_sorted, with, name)

  # content
  rtf_content_list = metadata_sorted |>
    lapply(rtf_create_output_by_metadata, output_directory = tfl.path, id_lookup=id_lookup)
  rtf_content = rtf_content_list |> unlist()

  # table of content
  output_full_titles = sapply(metadata, with, {
    type_format = c('figure' = 'Figure', 'table' = 'Table', 'listing' = 'Listing')
    full_title = paste(type_format[type], original_numbering, title)
    full_title
  })
  rtf_toc = create_table_of_contents(rtf_content_list, output_titles)

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
