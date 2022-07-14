
`%>%` <- magrittr::`%>%`

dat <- ecodata::gsi %>%
  dplyr::mutate(Year = Time %>%
                  stringr::str_trunc(4, "right", "") %>%
                  as.numeric(),
                Month = Time %>%
                  stringr::str_trunc(2, "left", "") %>%
                  as.numeric(),
                Month = ifelse(Month == 0.1, 10, Month))
dat

dat %>%
  ggplot2::ggplot(ggplot2::aes(x = Month,
                               y = Value,
                               color = Year,
                               group = Year)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw() +
  nmfspalette::scale_color_nmfs(palette = "regional web",
                                discrete = FALSE) +
  ggplot2::ggtitle("Gulf Stream Anomaly by month") +
  ggplot2::theme(legend.position = "bottom")


dat %>%
  ggplot2::ggplot(ggplot2::aes(x = Year + (Month - 1)/12,
                               y = Value,
                               color = Month)) +
  ggplot2::geom_line(color = "black") +
  ggplot2::geom_point() +
  ggplot2::theme_bw() +
  nmfspalette::scale_color_nmfs(palette = "regional web",
                                discrete = FALSE) +
  ggplot2::ggtitle("Gulf Stream Anomaly by year") +
  ggplot2::theme(legend.position = "bottom")

