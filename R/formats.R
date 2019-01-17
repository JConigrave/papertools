
#' glue_bracket
#'
#' @param x a character or numeric
#' @param ... a function. If provided will transform effects and confidence intervals.
#' @param round a scalar.
#' @importFrom dplyr %>%
#' @export glue_bracket

glue_bracket = function(x, ..., round = 2) {
  others = list(...) %>% data.frame

  bracks = lapply(seq_along(others[, 1]), function(x)
    paste(digits(as.numeric(others[x, ]), round), collapse = ", ")) %>% unlist
  out = paste0(digits(as.numeric(x), round), " (", bracks, ")")
  return(out)
}

#' to_c
#'
#' takes in a vector, converts it to a concatenate command
#' @param x a character vector
#' @export to_c

to_c = function(x){
  x = paste0("'",x, "'")
  x = paste0(x,collapse = ",")
  x = paste0("c(",x,")")
  message(x)
}

#' m_iqr
#'
#' takes in a numeric vector, calculates a median and iqr and returns as text.
#' @param x a character vector
#' @param round a numeric.
#' @param quantiles a bool.
#' @param na.rm a bool.
#' @param in.brack a bool.
#' @importFrom stats quantile IQR
#' @importFrom dplyr %>%
#' @export m_iqr


m_iqr = function(x,round = 1,quantiles = F,na.rm =T, in.brack = F){
  median = digits(median(x,na.rm=na.rm), round)

  if(quantiles == F){
  IQR = IQR(x,na.rm=na.rm) %>% digits(round)
    if(!in.brack){
  text = paste0(median," (IQR = ",IQR,")")
    }else{
     text = paste0("(median = ",median,", IQR = ",IQR,")")
    }

  }else{
    quant = stats::quantile(x, na.rm = na.rm)
    q.25 = quant[2] %>% digits(round)
    q.75 = quant[4] %>% digits(round)

    if(!in.brack){
    text = paste0(median, " (IQR = ",q.25,", ",q.75,")")
    }else{
      text = paste0("(median = ",median, ", IQR = [",q.25,", ",q.75,"])")
    }
  }

  return(text)
}
