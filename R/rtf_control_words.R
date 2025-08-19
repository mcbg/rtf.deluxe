#' @export
derive_text_control_words = \(size_pt, align) {
  size_half_points = size_pt * 2
  size_control_word = paste0('\\fs', size_half_points)
  align_control_word = c(left='\\ql', center='\\qc', right='\\qr')[align]

  # combine
  control_words = paste0(size_control_word, align_control_word)

  return(control_words)
}
