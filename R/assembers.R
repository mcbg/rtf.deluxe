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
create_tfl_document_by_metadata = \(metadata, header_text) {
  # sort metadata
  tfl_ordering = metadata |>
    sapply(`[[`, 'original_numbering') |>
    tfl_number_order()
  metadata_sorted = metadata[tfl_ordering]

  # add each output to document

  id_lookup = sapply(metadata_sorted, with, name)
  names(id_lookup) = sapply(metadata_sorted, with, name)
  rtf_content_list = metadata_sorted |>
    lapply(rtf_create_output, output_directory = tfl.path, id_lookup=id_lookup)
  rtf_content = rtf_content_list |> unlist()
  output_titles = sapply(metadata, with, title)
  rtf_toc = create_table_of_contents(rtf_content_list, output_titles)

  # combine
  full_document = rtf_add_head_and_tail(c(rtf_toc, '\\page', rtf_content))
  return(full_document)
}

