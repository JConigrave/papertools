
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
