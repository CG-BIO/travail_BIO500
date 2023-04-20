import_the_data = function(file_paths){
  
  
  imported_data = lapply(file_paths, function(x) read.table(x, sep=';'))
}

