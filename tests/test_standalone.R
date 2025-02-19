devtools::load_all()

create_standalone_rtf_file(iris) |>
  cat(file='test.rtf', sep='\n')
