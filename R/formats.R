
#' glue_bracket
#'
#' @param x a character or numeric
#' @param ... a function. If provided will transform effects and confidence intervals.
#' @param round a scalar.
#' @param brackets a vector with character strings to replace left and right brackets.
#' @param collapse a string. separator for numbers within brackets
#' @importFrom dplyr %>%
#' @export glue_bracket

glue_bracket = function(x, ..., round = NULL, brackets = c("(",")"), collapse = ", ") {
  # warnings and errors -----------------------------------
  if(length(brackets)!=2) stop("brackets must be length 2")

  # grab extra numbers ------------------------------------
  others = list(...) %>% unlist

  # round if requested ------------------------------------
  if(!is.null(round)){
    x = digits(as.numeric(x),round)
    others = digits(as.numeric(others),round)
  }

  bracks = paste(others, collapse = collapse)

  out = paste0(x, " ",brackets[1],bracks, brackets[2])
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

#' round p
#'
#' Rounds p value to specified digits and uses less symbol if result it zero.
#' @param p a p value, or vector of p-values
#' @param n a numeric. The number of digits to round to.
#' @param stars a numeric vector, add a star every time p is less than a respective star
#' @export round_p
round_p =  function(p, n = 2, stars = c(0.05)){
  rounded = digits(p,n)
  lapply(seq_along(rounded), function(x){
    #message(x)
    original = p[x]
    r_original = rounded[x]
    r = rounded[x]

    if(as.numeric(r) == 0){
      r = strsplit(r,split="")[[1]]
      r[length(r)] = 1
      r = paste(r,collapse = "")
    }

    #  add stars --------------
    stars_to_add = c()
    if(!is.null(stars)){
     stars_to_add = lapply(stars,function(s){
       if(as.numeric(original) < s){
         return("*")
       }else{
         return(NA)
       }
      }) %>% unlist %>%
       na.omit %>%
       paste(collapse = "")

    }

    if(r_original < as.numeric(r)){
      r = paste0("< ",r)
    }

    r = paste0(r,stars_to_add)

    return(r)
  }) %>% unlist

}
