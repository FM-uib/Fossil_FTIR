

shuffle_train = function(n, p) runif(n) < p

fit_pls

bootstrap = function(data, i) {
  res = list()
  
  #start loop
  for (j in 1:i) {
    paste0("fitting model "j"/"i)
    #shuffle train/test split
    data$train = shuffle_train(nrow(data), .7)
    #fit model
    train_ = fit_pls
    #predict test set
    test_ = caret::predict()
    #save to list
    res[[paste0("iter_",j)]] = list("train" = train_, "test" = test_)
  }

  return(res)
}