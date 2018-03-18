# ConvNet TASK1
r <- matrix(sample_image[1:6400], 80, 80, byrow = TRUE) / 255
g <- matrix(sample_image[6401:12800], 80, 80, byrow = TRUE) / 255
b <- matrix(sample_image[12801:19200], 80, 80, byrow = TRUE) / 255

sample_image <- array(c(r,g,b), dim = c(80, 80, 3))

sample_image_rot90 <- array(c(rot90(r, 1), rot90(g, 1), rot90(b, 1)), dim = c(80, 80, 3))
sample_image_rot180 <- array(c(rot90(r, 2), rot90(g, 2), rot90(b, 2)), dim = c(80, 80, 3))
sample_image_rot270 <- array(c(rot90(r, 3), rot90(g, 3), rot90(b, 3)), dim = c(80, 80, 3))

# ConvNet TASK2
model1 %>%
  layer_conv_2d(
    input_shape = c(80, 80, 3),
    filter = 32, kernel_size = c(3, 3), strides = c(1, 1),
    activation = "relu")

model1 %>%
  layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2)) %>%
  layer_conv_2d(filter = 64, kernel_size = c(3, 3), strides = c(1, 1),
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(2, activation = "softmax")

# ConvNet TASK3
model2 <- keras_model_sequential()
model2 %>%
  layer_conv_2d(
    filter = 32, kernel_size = c(3, 3), padding = "same", 
    input_shape = c(80, 80, 3), activation = "relu") %>%
  layer_conv_2d(filter = 32, kernel_size = c(3, 3),
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(0.25) %>%
  layer_conv_2d(filter = 64, kernel_size = c(3, 3), padding = "same",
                activation = "relu") %>%
  layer_conv_2d(filter = 64, kernel_size = c(3, 3),
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(0.25) %>%
  layer_flatten() %>%
  layer_dense(512, activation = "relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(2, activation = "softmax")

model2 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adamax(lr = 0.0001, decay = 1e-6),
  metrics = "accuracy"
)

ships_fit2 <- model2 %>% fit(train[[1]], train[[2]], epochs = 20, batch_size = 32,
                             validation_split = 0.2,
                             callbacks = callback_tensorboard("logs/model2"))

predicted_probs <- model2 %>%
  predict_proba(test[[1]])

head(predicted_probs)

model2 %>% evaluate(test[[1]], test[[2]])

