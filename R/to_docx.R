#'to_docx
#'
#'Sends a data.frame to a word doc.
#'@param table a dataframe or tibble
#'@param path a string. Where you want to save the file.
#'@export to_docx


to_docx = function(table, path) {
  file_name = basename(path)
  dir_name = dirname(path)
  if (dir_name == ".") {
    dir_name = getwd()
  }

  rmarkdownpath = system.file("rmd", "docx_table.Rmd", package = "papertools")
  rmarkdown::render(rmarkdownpath,
                    output_file = file_name,
                    output_dir = dir_name)
}
