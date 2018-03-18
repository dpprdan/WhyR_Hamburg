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

create_sequences <- function(maxlen, text){
  dataset <- map(
    seq(1, length(text) - maxlen - 1, by = 3), 
    ~list(sentece = text[.x:(.x + maxlen - 1)], next_char = text[.x + maxlen])
  )
  dataset <- transpose(dataset)
  x <- array(0, dim = c(length(dataset$sentece), maxlen, length(chars)))
  y <- array(0, dim = c(length(dataset$sentece), length(chars)))
  for(i in 1:length(dataset$sentece)){
    x[i,,] <- sapply(chars, function(x){
      as.integer(x == dataset$sentece[[i]])
    })
    y[i,] <- as.integer(chars == dataset$next_char[[i]])
  }
  list(x, y)
}

predict_text <- function(sentence, prediction_length, model){
  prediction <- c()
  x_test <- tokenize_characters(sentence, strip_non_alphanum = FALSE, simplify = TRUE)
  x_temp <- array(0, dim = c(1, maxlen, length(chars)))
  x_temp[1,,] <- sapply(chars, function(x){
    as.integer(x == x_test)
  })
  for(i in 1:prediction_length){
    preicted_letter <- model %>% predict(x_temp) %>% which.max() %>% chars[.]
    prediction <- c(prediction, preicted_letter)
    sentence <- paste0(substring(sentence, 2), preicted_letter)
    x_test <- tokenize_characters(sentence, strip_non_alphanum = FALSE, simplify = TRUE)
    x_temp <- array(0, dim = c(1, maxlen, length(chars)))
    x_temp[1,,] <- sapply(chars, function(x){
      as.integer(x == x_test)
    })
    print(paste0(prediction, collapse = ""))
  }
}
