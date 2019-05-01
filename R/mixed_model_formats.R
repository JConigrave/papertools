#' mm_table_binomial
#'
#' Creates a table of fixed and random effects.
#' @param model a model of glmerMod
#' @param fixed_names a vector of predictor names
#' @param round a scalar, defaults to 2
#' @param round_p a scalar. The number of digits to round p to.
#' @param simple_names a bool. If True, simple names are given
#' @param collapse a string. Value to separate confidence intervals with
#' @param brackets a vector. passed to glue, bracket.
#' @export mm_table_binomial
#' @importFrom dplyr %>% left_join bind_rows
#' @importFrom papaja apa_table
#' @importFrom tibble rownames_to_column
#' @importFrom purrr modify_if
#' @importFrom sjstats re_var icc

mm_table_binomial = function(model, round = 2, round_p = 3, fixed_names = NULL, simple_names = F, collapse = " - ", brackets = c("(",")")) {

  # define fixed effects -------------------------------------------
  fixed_table = stats::coef(summary(model)) %>%
    data.frame %>%
    rownames_to_column() %>%
    dplyr::left_join(stats::confint(model, method = "Wald") %>%
                data.frame() %>%
                rownames_to_column(),
              by = "rowname")

  names(fixed_table) = c("Predictors", "$\\beta$", "SE", "z", "p", "ci2.5", "ci97.5")
  fixed_table = fixed_table %>%
    mutate(OR = glue_bracket(exp(.$`$\\beta$`), exp(.$ci2.5), exp(.$ci97.5) ,round = round, collapse = collapse, brackets = brackets))

  fixed_table = fixed_table %>%
    mutate(p = papertools::round_p(p, round_p)) %>%
    purrr::modify_if(is.numeric,function(x) digits(x,round)) %>%
    dplyr::select("Predictors", `$\\beta$`, SE, "OR (95\\% CI)" = OR, p)

  if (!is.null(fixed_names)) {
    fixed_table$`Predictors` = fixed_names
  }

 #define random effects ------------------------------------------------

  re_vars = sjstats::re_var(model) %>%
    data.frame %>%
    tibble::rownames_to_column() %>%
    mutate(type =  sub('.*\\_', '',rowname)) %>%
    mutate(rowname = gsub('_[^_]+$', "",rowname))

  if(!simple_names){
  sigma_name = "$\\sigma^2$"
  tau_name = "$\\tau_{00}$ "
  }else{
    sigma_name = "sigma^2"
    tau_name = "tau_"

  }

  re_vars$rowname[re_vars$type == 2] = sigma_name
  re_vars$rowname[re_vars$type == "tau.00"] = paste0(tau_name,
                                                    re_vars$rowname[re_vars$type == "tau.00"])
  iccs = sjstats::icc(model) %>%
    data.frame %>%
    tibble::rownames_to_column() %>%
    mutate(rowname = paste("ICC", rowname)) %>%
    mutate(type = "ICC")

  random_effects = rbind(re_vars, iccs) %>%
    dplyr::select(Predictors = rowname, "$\\beta$" = ".") %>%
    purrr::modify_if(is.numeric,function(x) as.character(digits(x,round)))

  #merged table ---------------------------------------
  table_out = fixed_table %>%
    dplyr::bind_rows(tibble::tibble(Predictors = "**Random Effects**"),
              random_effects)

  table_out[is.na(table_out)] = " "

  if(simple_names){
    table_out = table_out %>%
      dplyr::rename(beta = "$\\beta$", OR = "OR (95\\% CI)")
  }

  return(table_out)
}


globalVariables(c(".","coef", "p","$\\beta$","SE","OR","rowname"))
