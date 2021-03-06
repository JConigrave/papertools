#' as_word
#'
#' Takes a numeral and converts to a word with optional sentence case. Based off the English package.
#' @param x a numeric. Number to convert to english
#' @param sentence a Bool. If true, the first letter is capitalised
#' @param hyphenate a Bool. If true, compound numbers are hyphenated
#' @importFrom dplyr %>%
#' @export as_word

as_word = function(x = NULL,
                   sentence = F,
                   hyphenate = T) {
  x = as.character(english::english(as.numeric
                                    (x)))
  if (sentence == T) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  }

  if (hyphenate) {
    compounds = english::english(1:9) %>%
      paste("y", .)
    compounds_hyphen = english::english(1:9) %>%
      paste("y", ., sep = "-")

    for (n in seq_along(compounds)) {
      x <- gsub(compounds[n], compounds_hyphen[n], x)
    }

  }

  return(x)
}

#' simpl_frac
#'
#' Simplified fractions for presentation
#' @param p a proportion
#' @param sentence If true, the first letter is capitalised
#' @param percent If true, true percent is pasted to the final value
#' @param denom valid denominator values, a sequence
#' @param round the number of digits to round summary percent to
#' @export simpl_frac

simpl_frac = function(p,
                       sentence = F,
                       percent = T,
                       round = 1,
                       denom = 1:20) {

  odds = prob2odds(p)

  res = data.frame(denom = denom)
  res$num = p * denom
  res$remain = round(abs(round(res$num,0) - res$num),2)
  res$num = round(res$num,0)

  # penalise harder fractions
  res$remain = ifelse(res$remain %% 5 != 0 & res$denom > 10, res$remain + .025, res$remain)
  res$remain = ifelse(res$num > 5, res$remain + .025, res$remain)

  res = dplyr::filter(dplyr::arrange(res, remain, denom), num != 0 & remain < 0.3)
  perc = round(p*100, round)

  if(nrow(res) == 0){
    simpl = hash_replace("few")
  }else{

    num = as_word(res$num[1])
    denom = as_word(res$denom[1])

    simpl = hash_replace("##num## in ##denom##")
  }

  if(p > .9){
    simpl = "most"
  }

  if(p == 1){
    simpl = "all"
  }

  if(sentence){
    substr(simpl, 1, 1) <- toupper(substr(simpl, 1, 1))
  }

  if(percent){
    simpl = hash_replace("##simpl## (##perc##%)")
  }

  return(simpl)

}


#' remove_lead
#'
#' Takes in a numeral, removes leading zeros
#' @param x a numeric.
#' @export remove_lead

remove_lead = function(x) sub("^(-)?0[.]",
                              "\\1.", x)

#' logit2prob
#'
#' Converts logits to prob
#' @param logit a numeric. A logit.
#' @export logit2prob

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#' prob2odds
#'
#' Converts probability to odds
#' @param p a scalar. Probability
#' @export prob2odds

prob2odds <- function(p){
 return(p / (1-p))
}


#' q_alpha
#'
#' Calculates and rounds alphas
#' @param x a dataframe
#' @param round a numeric. Result will be rounded to this number.
#' @export q_alpha
#' @importFrom psych alpha

q_alpha = function(x, round = 2){
  x = psych::alpha(x)$total$std.alpha
  return(round(x, round))
}


#' multi_grepl_n
#'
#' Returns the number of times grepl matched a pattern; vectorised.
#' @param pattern vector of character string patterns
#' @param x a vector of strings to match with patterns
#' @param tolower a bool. Indicates whether or not to make each pattern and input vector lowercase
#' @return A vector containing the number of times each pattern was matched
#' @importFrom dplyr %>%
#' @export multi_grepl_n


multi_grepl_n = function(pattern, x, tolower = T) {
  if (tolower) {
    pattern = tolower(pattern)
    x = tolower(x)
  }
  out = lapply(pattern, function(p) {
    sum(grepl(p, x))
  }) %>% unlist
  return(out)
}

#' n_percent
#'
#' Returns the number than meet a condition, and the percentage in brackets
#' @param logical a logical vector to count
#' @param round scalar, the number of digits
#' @param pattern a character string using glue syntax. Variable names are 'n' and 'p'.
#' @param na.rm bool, whether to remove NAs
#' @importFrom dplyr %>%
#' @importFrom stats na.omit
#' @return a character
#' @export n_percent

n_percent = function(logical, round = 2, na.rm = T, pattern = "{n} ({p}%)"){
  if(na.rm){
    logical = na.omit(logical)
  }
  n = sum(logical)
  total_length = length(logical)
  p = (n / total_length) %>%
    "*"(100) %>%
    digits(round)

  out = glue::glue(pattern)

  return(out)
}

#' hash_replace
#'
#' Finds are replaces hash codes, replaces with objects of the same name
#' @param string text to search in
#' @param sample default NULL. If provided, samples a vector
#' @param envir the environment to call replacements from. Defaults to parent frame
#' @export hash_replace

hash_replace = function(string,sample = NULL, envir = parent.frame()){
  while(grepl(".*##(.+)##.*",string)){
    hash = gsub(".*##(.+)##.*", "\\1", string)
    replace = as.character(eval(parse(text = hash), envir = envir))

    if(!is.null(sample)) {
      replace = sample(replace, sample)
    }

    to_replace = paste0("##",hash,"##")
    to_replace = gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", to_replace) # escape regex

    string = gsub(to_replace,replace,string)
  }
  return(string)
}

#' brief
#'
#' Summarises text based on desired number of sentences
#' @param x character including all text
#' @param n number of sentences desired
#' @export brief

brief = function(x,n){

  top = lexRankr::lexRank(x,n)
  order_of_appearance = order(as.integer(gsub("_","",top$sentenceId)))
  ordered_top = top[order_of_appearance, "sentence"]
  return(cat(paste(ordered_top, collapse = "\n")))

}

#' c_sentence
#'
#' concatenates words with commas. Replaces the last comma with an and.
#' @param x a vector
#' @param and a character string. The joining word
#' @export c_sentence

c_sentence = function(x, and = "and"){
  last = x[length(x)]
  first = paste0(x[-length(x)], collapse = ", ")
  return(paste0(first, ", ",and," ", last))
}

#' clippy
#'
#' sends a table to the clipboard so it can be pasted into excel - a wrapper for write.table
#' @param x the data.frame
#' @param row.names a bool. TRUE preserves rownames
#' @export clippy

clippy = function(x, row.names = F){
  utils::write.table(x , "clipboard", sep = "\t", quote = FALSE, qmethod = "double", row.names = row.names)
}


