#' runif_change
#'
#'Takes in R commands and the path to a location to store temporary files. The function will create the file if it doesn't already exist, store the objects there, and only re-run if there has been a change to a reference object.
#' @param code a code chunk wrapped in\ \{\}
#' @param object.names a character vector. The names to be saved and loaded
#' @param compare an object. The object which if changed, will cause the code to run.
#' @param path a string. The path to the tempory file to be used. Must end in '.rData'
#' @export runif_change

# data = mtcars
# object.names = c("obj1")
# compare = mtcars
# path = "C:/Users/jcon4884/Desktop/temp.rData"
# code =  {obj1 = 5 + 1}


runif_change = function(code = NULL, #the code to be run
                        object.names = NULL, #the object names to be saved/loaded
                        compare = NULL, #the comparator object to test update required
                        path = NULL #temp file to save
) {
  if (!file.exists(path)) { #if there is no save file
    code #run code - then create the save file
    if (!dir.exists(dirname(path))) { #if the folder doesnt exist
      dir.create(dirname(path), recursive = T) #make it
    }
    previous = get("compare") #get the comparator file
    save(list = c(object.names, "previous"), #save it into the temp file with other objects
         file =  path)
  } else {
    load(path) #if there is a temp file load it

    if (identical(get("compare"), previous)) { #if the comparator is identical to the last version
      inls = all(object.names %in% ls()) #test if all objects are in the old environment
      if (!inls) {#if they're not there
        message("required objects are not in temporary file") #tell the user
        code #create the objects
        previous = get("compare") #save the comparitor to previous
        othernames = ls()[!ls() %in% c("code", "object.names", "compare", "path", "inls")]
        #print(othernames)
        save(list = c(object.names, "previous", othernames), #save all objects
             #save objects for retrieval
             file = path)
      } else{
        message("identical, skipping analyses") #if the comparator is identical, and files are in there
        for (i in seq_along(object.names)) {
          assign(object.names[i], get(object.names[i]), envir = .GlobalEnv) #just load them
          assign(object.names[i], get(object.names[i]))
        }
      }
    } else{ #if the comparator is not identical
      message("changes have been made to the dataset, re-running analysis") #tell everyone
      code #run the code
      if (!all(object.names %in% ls())) {
        notall = object.names[!object.names %in% ls()]
        stop(paste0("not all object names are in the enviornment:",paste(notall,collapse = ", ")))
      } else{
        previous = get("compare")
        othernames = ls()[!ls() %in% c("code", "object.names", "compare", "path")]
        #print(othernames)
        save(list = c(object.names, "previous", othernames),
             #save objects for retrieval
             file =  path)
      }
    }
  }
}
