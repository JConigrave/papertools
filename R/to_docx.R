
#'to_docx
#'
#'Sends a data.frame to a word doc.
#'@param table a dataframe or tibble
#'@param path a string. Where you want to save the file.
#'@export to_docx

to_docx = function(table,path){
  rmarkdownpath = system.file("rmd", "docx_table.Rmd", package = "papertools")
  rmarkdown::render(rmarkdownpath,output_file = path, output_dir = getwd())
}
