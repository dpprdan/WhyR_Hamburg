xy_axis <- data.frame(x = expand.grid(1:80,80:1)[,1],
                      y = expand.grid(1:80,80:1)[,2])

create_plot_data <- function(sample_image, row_nr = NULL){
  if(is.null(row_nr)){
    cbind(xy_axis,
          r = as.vector(t(sample_image[ , , 1])),
          g = as.vector(t(sample_image[ , , 2])),
          b = as.vector(t(sample_image[ , , 3])))
  } else {
    cbind(xy_axis,
          r = as.vector(t(ships_data[row_nr, , , 1])),
          g = as.vector(t(ships_data[row_nr, , , 2])),
          b = as.vector(t(ships_data[row_nr, , , 3])))
  }
}

plot_rgb_raster <- function(plot_data, r_layer = TRUE, g_layer = TRUE, b_layer = TRUE){
  ggplot(plot_data, aes(x, y, fill = rgb(if(r_layer) r else 0,
                                         if(g_layer) g else 0,
                                         if(b_layer) b else 0))) +
    guides(fill = FALSE) + scale_fill_identity() + theme_void() +
    geom_raster(hjust = 0, vjust = 0)
}

plot_sample_image <- function(sample_image, show_layers = FALSE, row_nr = NULL){
  plot_data <- create_plot_data(sample_image, row_nr)
  if (show_layers) {
    img_r <- plot_rgb_raster(plot_data, TRUE, FALSE, FALSE)
    img_g <- plot_rgb_raster(plot_data, FALSE, TRUE, FALSE)
    img_b <- plot_rgb_raster(plot_data, FALSE, FALSE, TRUE)
    img <- plot_rgb_raster(plot_data, TRUE, TRUE, TRUE)
  grid.arrange(img_r, img_g, img_b, ggplot() + theme_void(), img, ncol = 3, nrow = 2)
  } else {
    plot_rgb_raster(plot_data, TRUE, TRUE, TRUE)
  }
}

plot_sample_images <- function(ships_data, ships_labels, seed){
  set.seed(seed)
  sample_plots <- sample(1:dim(ships_data)[1], 12) %>%
    map(~ plot_sample_image(ships_data, show_layers = FALSE, row_nr = .x) +
          ggtitle(ifelse(ships_labels[.x, 2], "Ship", "Non-ship")))
  do.call("grid.arrange", c(sample_plots, ncol = 4, nrow = 3))
}
