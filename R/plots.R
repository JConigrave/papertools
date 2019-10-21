
#' pie_chart
#'
#' Rounds p value to specified digits and uses less symbol if result it zero.
#' @param vector a vector
#' @param colours a vector of hex codes.
#' @importFrom ggplot2 ggplot aes geom_bar coord_polar geom_text labs guides theme theme_classic scale_fill_manual position_stack element_blank element_text guide_legend
#' @export pie_chart
#'

pie_chart =  function(vector, colours = NULL) {
  obj = as.character(match.call()$vector)
  obj = obj[length(obj)]

  if (class(vector) != "factor")
    vector <- factor(vector)

  input = table(vector) %>% data.frame %>%
    mutate(Percent = Freq / sum(Freq, na.rm = T)) %>%
    mutate(Percent = Percent * 100)
  names(input)[1] = obj

  piechart = ggplot(input, aes(
    x = "",
    y = Percent,
    fill = eval(parse(text = names(input)[1]))
  )) +
    geom_bar(
      width = 1,
      size = 1,
      color = "white",
      stat = "identity"
    ) +
    coord_polar("y") +
    geom_text(aes(label = paste0(round(Percent), "%")),
              position = position_stack(vjust = 0.5)) +
    labs(
      x = NULL,
      y = NULL,
      fill = NULL,
      title = ""
    ) +
    guides(fill = guide_legend(reverse = TRUE))

  if (!is.null(colours)) {
    piechart = piechart + scale_fill_manual(values = colours)
  }
  piechart = piechart + theme_classic() +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(hjust = 0.5, color = "#666666")
    )
  return(piechart)
}

globalVariables(c("Freq","Percent"))

#' raincloud
#'
#' @param data data
#' @param ... variables
#' @param transition variable to transition over
#' @export raincloud

raincloud = function(data, ..., palette = "Spectral", transition = NULL){

  vars = tidyselect::vars_select(names(data), ...)
  trans = trans_name = tidyselect::vars_select(names(data), {{transition}})

  require("ggplot2", quietly = TRUE)

  vars = c(vars, trans)

  data = data[, names(data) %in% vars]

  if(length(trans) > 0){
  dat = suppressMessages(reshape2::melt(data, id = trans))
  names(dat)[1] = "trans"

  n_trans = length(unique(dat$trans))
  if(n_trans < 20) trans = 20
  if(n_trans > 50) trans = 50
  #message(n_trans)

  }else{
  dat =  suppressMessages(reshape2::melt(data))
  }

  if(dim(dat)[2] == 1){
    dat$variable = vars
  }

  #return(dat)

  dat$variable = factor(dat$variable)

p = ggplot2::ggplot(data = dat, ggplot2::aes(y = value, x = variable, fill = variable)) +
    gglayer::geom_flat_violin(position = ggplot2::position_nudge(x = .2, y = 0), alpha = .8, trim=FALSE) +
    ggplot2::geom_point(ggplot2::aes(y = value, color = variable), position = ggplot2::position_jitter(width = .15), size = .5, alpha = 0.8) +
    ggplot2::geom_boxplot(width = .1,  outlier.shape = NA, alpha = 0.5) +
    # +
    ggplot2::guides(fill = FALSE) +
    ggplot2::guides(color = FALSE) +
    ggplot2::scale_color_brewer(palette = palette) +
    ggplot2::scale_fill_brewer(palette = palette) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "", x ="")

 if(length(trans) > 0){
   p = p + gganimate::transition_states(trans, transition_length = 10, state_length = 3) +
     labs(title = hash_replace("'##trans_name##' : {closest_state}"))
 }

return(p)

}

globalVariables(c("value","type","variable"))
