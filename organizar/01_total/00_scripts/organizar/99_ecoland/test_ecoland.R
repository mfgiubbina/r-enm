ggplot(diamonds, aes(carat, log10(price))) +
  geom_hex(aes(alpha = ..count..), bins = 30, show.legend = FALSE) +
  # geom_point(alpha = 1e-2) +
  scale_fill_gradientn(colours = viridis::viridis(10)) +
  theme_classic()
