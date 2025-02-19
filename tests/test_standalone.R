devtools::load_all()

# 1
create_standalone_rtf_file(iris) |>
  cat(file='test.rtf', sep='\n')

# 2
new_iris = iris
names(new_iris) = names(iris) |> paste(c('sepal', 'sepal', 'petal', 'petal', ''), sep = '\t')
create_standalone_rtf_file(new_iris) |>
  cat(file='test2.rtf', sep='\n')

