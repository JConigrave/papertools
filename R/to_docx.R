#'to_docx
#'
#'Sends a data.frame to a word doc.
#'@param table a dataframe or list of data.frames
#'@param path a string. Where you want to save the file.
#'@export to_docx


to_docx = function(table, path, table_name = NULL) {

  file.opened <- function(path) {
    suppressWarnings("try-error" %in% class(try({
      x = file(path,
               open = "w")
      close.connection(x)
    }, silent = T)))
  }

  file_name = basename(path)
  dir_name = dirname(path)
  file_path = paste0(dir_name, "/", file_name)
  if (dir_name == ".") {
    dir_name = getwd()
  }

  if (file.opened(file_path)) {
    stop("Target file is currently open, cannot write.")
  }

if(!"list" %in% class(table)){
  table = list(table)
}

  if(!is.null(table_name)){
    if(length(table_name) != length(table)){
      stop("'table_name' is not the same length as 'table'. If names are provided, they must be provided for each object.")
    }
  }

  rmarkdownpath = system.file("rmd", "docx_table.Rmd", package = "papertools")
  rmarkdown::render(rmarkdownpath,
                    output_file = file_name,
                    output_dir = dir_name,
                    quiet = T)
  message(paste0("saved to: '", dir_name, "/", file_name, "'"))
}
