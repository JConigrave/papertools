file.opened <- function(path) {
  suppressWarnings("try-error" %in% class(try({
    x = file(path,
             open = "w")
    close.connection(x)
  }, silent = T)))
}

#'to_docx
#'
#'Sends a data.frame to a word doc.
#'@param table a dataframe or list of data.frames
#'@param path a string. Where you want to save the file.
#'@export to_docx


to_docx = function(table, path, table_name = NULL) {
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

#'zotero_notes
#'
#'Sends a data.frame to a word doc.
#'@param csv an exported csv from zotero. Study type stored in the extra field
#'@param path a string. must end in .html
#'@param title a string. Defaults to "Zotero notes"
#'@param date a string. Defaults to current date
#'@importFrom magrittr %>%
#'@importFrom dplyr select mutate arrange
#'@export zotero_notes

zotero_notes = function(csv, path, title = "Zotero notes", date = format(Sys.time(), '%d %B, %Y')){
  x = read.csv(csv)
  x$Author = gsub(",.*",", et al.",x$Author)
  x$Notes = as.character(x$Notes)
  x$Notes[is.na(x$Notes)] = "   "

  file_name = basename(path)
  dir_name = dirname(path)
  file_path = paste0(dir_name, "/", file_name)
  if (dir_name == ".") {
    dir_name = getwd()
  }

  if (file.opened(file_path)) {
    stop("Target file is currently open, cannot write.")
  }

  rmarkdownpath = system.file("rmd", "zotero_notes.Rmd", package = "papertools")

  final_table = x %>%
    dplyr::select(
      Author,
      Year = "Publication.Year",
      Title,
      Journal = "Publication.Title",
      Type = Extra,
      Notes
    ) %>%
    dplyr::mutate(Type = tolower(Type)) %>%
    dplyr::arrange(desc(Year), Author)

  rmarkdown::render(rmarkdownpath,
                    output_file = file_name,
                    output_dir = dir_name,
                    quiet = T)
  message(paste0("saved to: '", dir_name, "/", file_name, "'"))


}



