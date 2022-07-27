
`%>%` <- magrittr::`%>%`

dat <- ecodata::ESP_heatwave_cod %>%
  dplyr::group_by(Time, Var, stock_id) %>%
  dplyr::summarise(max_value = max(Value))

dat$stock_id <- factor(dat$stock_id, levels = c("SNE", "GBK", "WGOM", "EGOM"))

dat %>%
  ggplot2::ggplot(ggplot2::aes(x = Time,
                               y = max_value)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::facet_grid(cols = ggplot2::vars(stock_id),
                      rows = ggplot2::vars(Var),
                      scales = "free_y") +
  ggplot2::theme_bw()
