#' mm_table
#'
#' Creates a table of fixed and random effects.
#' @param model a model of glmerMod
#' @param fixed_names a vector of predictor names
#' @param round a scalar, defaults to 2
#' @param round_p a scalar. The number of digits to round p to.
#' @param simple_names a bool. If True, simple names are given
#' @param collapse a string. Value to separate confidence intervals with
#' @param brackets a vector. passed to glue, bracket.
#' @export mm_table
#' @importFrom dplyr %>% left_join bind_rows
#' @importFrom papaja apa_table
#' @importFrom tibble rownames_to_column
#' @importFrom purrr modify_if

#round = 2; round_p = 3; fixed_names = NULL; simple_names = F; collapse = " - "; brackets = c("(",")")

mm_table = function(model, round = 2, round_p = 3, fixed_names = NULL, simple_names = F, collapse = " - ", brackets = c("(",")")) {

  # define fixed effects -------------------------------------------
  fixed_table = stats::coef(summary(model)) %>%
    data.frame %>%
    rownames_to_column() %>%
    dplyr::left_join(stats::confint(model, method = "Wald") %>%
                data.frame() %>%
                rownames_to_column(),
              by = "rowname")

  fixed_table = fixed_table %>%
    dplyr::rename(Predictors = rowname,
                  b = Estimate,
                  SE = Std..Error,
                  p = Pr...t..,
                  lower = X2.5..,
                  upper = X97.5..)



  if (!is.null(fixed_names)) {
    fixed_table$`Predictors` = fixed_names
  }

 #define random effects ------------------------------------------------

  var_summary = insight::get_variance(model)
  taus = var_summary$var.intercept %>%
    data.frame() %>%
    tibble::rownames_to_column() %>%
    mutate(type = "tau.00")
  re_vars = tibble::tibble(rowname = "_sigma", . = var_summary$var.residual, type = "2" ) %>%
    rbind(.,taus)

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

  #round fixed table ------------------------
  rounded_fixed = fixed_table %>%
    mutate(b = digits(b, round),
           SE = digits(`SE`, round),
           p = papertools::round_p(p, round_p),
          lower = digits(lower, round),
          upper = digits(upper, round))

  #perform roundings and formating

  rounded_fixed = rounded_fixed %>%
    mutate(`95% CI` = glue::glue("[{lower}, {upper}]") %>% as.character())



  final_fixed = rounded_fixed %>%
    dplyr::select("Predictors", `$\\beta$` = b, `95% CI`, `$SE$` = SE, `$p$` = p)


  #merged table ---------------------------------------
  table_out = final_fixed %>%
    dplyr::bind_rows(tibble::tibble(Predictors = "**Random Effects**"),
              random_effects)

  table_out[is.na(table_out)] = " "

  if(simple_names){
    table_out = table_out %>%
      dplyr::rename(beta = "$\\beta$")
  }

  return(table_out)
}


globalVariables(c(".","coef", "p","$\\beta$","SE","OR","rowname","95% CI","Estimate",
                  "Pr...t..","Std..Error","X2.5..","X97.5..","b","lower","remain","upper"))
