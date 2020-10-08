# my_theme -----
# desc: base theme for all ggplot objects
my_theme <- theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = .5, size = 10,
                                  margin = ggplot2::margin(t = 0, r = 0,
                                                           b = 20, l = 0)),
        plot.subtitle = element_text(hjust = .5, size = 10,
                                     margin = ggplot2::margin(t = 0, r = 0,
                                                              b = 10, l = 0)),
        axis.title = element_text(hjust = .5, size = 8),
        axis.text = element_text(hjust = .5, size = 7),
        strip.text = element_text(hjust = .5, size = 10),
        legend.title = element_text(size =8),
        legend.text = element_text(size = 8),
        axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10,
                                                             b = 0, l = 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0,
                                                             b = 0, l = 0)))