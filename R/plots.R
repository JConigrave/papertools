
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
