file.opened <- function(path) {
  suppressWarnings("try-error" %in% class(try({
    x = file(path,
             open = "w")
    close.connection(x)
  }, silent = T)))
}

globalVariables(c("Author","Title","Extra","Notes","Type","Year"))

#'clean_docx
#'
#'Removes random tags and a mark
#'@param docx_path a character vector
#'@importFrom dplyr %>%
#'@importFrom officer read_docx body_replace_all_text
#'@importFrom utils capture.output

clean_docx = function(docx_path){
x = officer::read_docx(docx_path)
x = x %>%
  officer::body_replace_all_text("\\(#tab:destroythistag\\)","") %>%
  officer::body_replace_all_text("\\`","") %>%
  print(docx_path) %>%
  capture.output()
}

#'to_docx
#'
#'Sends a data.frame to a word doc.
#'@param table a dataframe or list of data.frames
#'@param path a string. Where you want to save the file.
#'@param table_name string. If provided, allows tables to be named
#'@param table_note a string. Allows for notes
#'@importFrom papaja apa_table
#'@export to_docx


to_docx = function(table, path, table_name = NULL, table_note = NULL) {
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

  rmarkdownpath = system.file("rmd", "docx_table2.Rmd", package = "papertools")
  rmarkdown::render(rmarkdownpath,
                    output_file = file_name,
                    output_dir = dir_name,
                    quiet = T)
  clean_docx(paste0(dir_name, "/", file_name))
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
#'@importFrom dplyr select mutate arrange desc
#'@importFrom utils read.csv
#'@export zotero_notes
#csv = "C:/Users/jcon4884/Dropbox (Sydney Uni)/2_Grog Survey App - 1087192/10_reporting_publicity/papers/paper_5 - patterns of drinking meta analysis_James/7_paper/Literature review/Exported Items.csv"
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
x$Title = as.character(x$Title)
x$Publication.Title = as.character(x$Publication.Title)
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



