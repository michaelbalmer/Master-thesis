
FLAGS <- flags(
  flag_integer('neurons1', 128),
  flag_integer('neurons2', 128),
  flag_numeric('lr', 0.001)
)

build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = FLAGS$neurons1, activation = "relu",
                input_shape = ncol(X_train)) %>%
    layer_dense(units = FLAGS$neurons2, 
                activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(lr = FLAGS$lr),
    metrics = list("mean_absolute_error")
  )
  
  model
}

model <- build_model()

early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

epochs <- 100

# Fit the model and store training stats
history <- model %>% fit(
  X_train,
  y_train,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 2,
  callbacks = list(early_stop)
)

