import_the_data = function(file_paths){
  imported_data <- lapply(file_paths, function(x) {
    if (grepl(".csv", x)) {
      read.table(x, sep = ",")
    } else {
      read.table(x, sep = ";")
    }
  })
}
