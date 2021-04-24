###############################################################################
############################### KERAS #########################################
###############################################################################
# from: https://www.rpubs.com/juanhklopper/example_of_a_CNN
library(keras)
# load teh 60,000 version
mnist <- dataset_mnist()
# x is 3D, each entry (60,000) is a 28*28 array
x_train <- mnist$train$x
labels_train <- mnist$train$y # labels where y[1] is associated with x[1,,]
x_test <- mnist$test$x
labels_test <- mnist$test$y
head(labels_test)
# Reshape to become a tensor
# need a number of channels for training and testing
make_tensor <- function(dataset, nrow, ncol){
    array_reshape(dataset, c(nrow(dataset), nrow, ncol, 1))
}
x_train <- make_tensor(x_train, 28, 28)
x_test <- make_tensor(x_test, 28, 28)
img_dims <- c(28, 28, 1)
dim(x_train)

# create targets
num_classes <- 10
labels_train <- to_categorical(labels_train, num_classes) # shifts everything by 1 (so 0 is 1)
labels_test <- to_categorical(labels_test, num_classes)

which(labels_train[1,] == 1) - 1 # see what the first image is (based on index)
which(labels_train[29,] == 1) - 1

# set up the model (not run yet)
# changed the filter number in layer_conv_2d
model <- keras_model_sequential() %>% # FF CNN
    layer_conv_2d(filters = 32, # dims of output space from this convolution
                  kernel_size = c(3,3), # wXh of convolution space
                  activation = 'relu',
                  input_shape = img_dims) %>%
                  # all below here in layer_conv_2d are default params
                  # strides = c(1L, 1L), # strides along w and h of kernel
                  # padding = "valid",
                  # data_format = NULL, # defaults to channels last
                  # dilation_rate = c(1L, 1L),
                  # use_bias = TRUE,
                  # kernel_initializer = 'glorot_uniform', #initialization of weights
                  # bias_initializer = "zeros",
                  # kernel_regularizer = NULL,
                  # bias_regularizer = NULL,
                  # activity_regularizer = NULL,
                  # trainable = NULL, # will layer weights update during training
                  # weights = NULL) %>% # initial weights for layer
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_dropout(rate = .25) %>% # used to reduce overfitting
    # add another convolution
    layer_flatten() %>% # flatten through the layer_dense
    layer_dense(units = 128, # changed from 10 to 256
                activation = 'relu') %>%
    layer_dropout(rate = .5) %>% # minimize overfitting
    layer_dense(units = num_classes,
                activation = 'softmax') # choose classes

model %>% summary
devtools::install_github("andrie/deepviz")
library(deepviz)
model %>% plot_model()

# use categorical cross entropy as the loss function to categorize
compile(model, loss = loss_categorical_crossentropy,
                  optimizer = optimizer_adadelta(),
                  metrics = list(accuracy = 'accuracy'))

# train the model
batch_size = 128
epochs <- 10 # changed from 12 to 10 epochs
# run training
# ?????????????????????????????????????????????????????????????????????????????
# validate against split training data or testing data
# ?????????????????????????????????????????????????????????????????????????????
# this should be against test data, the validation split automatically pulls out
# indeces of the training data and the associated labels!!
hist <- fit(model, x_train, labels_train,
              batch_size = batch_size,
              epochs = epochs,
              validation_split = .2)
              # validation_data = list(x_test, labels_test)) ## wrong unless I pulled these from test!

# check model accuracy
# ?????????????????????????????????????????????????????????????????????????????
# should we evaluate the test or training data? 
# ?????????????????????????????????????????????????????????????????????????????
score <- evaluate(model, x_train, labels_train) 
cat('Test loss', score[1], "\n")
cat('Test accuracy', score[2], "\n")

# predict with the model
raw_prediction <- predict(model, x_test)
prediction <- apply(raw_prediction, 1, which.max)
# test the accuracty
labs <- apply(labels_test,1, function(x) which(x == 1))
sum(prediction != labs)
indx_wrong <- which(prediction != labs)
vals_wrong <- prediction[indx_wrong] - 1
hist(vals_wrong, main = "Values incorrectly classed", xlab = "Value")
vals_wrong <- as.factor(vals_wrong)
summary(vals_wrong)  # fairly even distribution

# library(mxnet)
# https://mxnet.apache.org/versions/1.7.0/api/r/docs/tutorials/mnist_competition
# broken right now

###############################################################################
############################### H2O.AI ########################################
###############################################################################
library(h2o)
# https://www.r-bloggers.com/2018/12/an-introduction-to-h2o-using-r/
# https://kuanhoong.wordpress.com/2016/02/01/r-and-deep-learning-cnn-for-handwritten-digits-recognition/
# more info and my adjustments ideas
# https://docs.h2o.ai/h2o-tutorials/latest-stable/tutorials/deeplearning/index.html
m <- x_train[1,,]
# check one
image(m, col = grey.colors(255))
# we need to rotate them
rotate <- function(x){
    t(apply(x, 2, rev))  
} 
# graph 6 of them
par(mfrow=c(2,3)) # change graphical parameters (2 rows and 3 cols of images)
lapply(1:6, # grab the first 6
       function(x) image(
           rotate(x_train[x,,]),
           col = grey.colors(255),
           xlab = labels_train[x]
           )
       )
par(mfrow = c(1,1)) # reset to default

# separate for training and testing
# # Alternatively we can do this directly in h2o as below
# load in kaggle data as this is closer to the fomat we want
training_kaggle <- read.csv("in_data/train.csv")
testing_kaggle <- read.csv("in_data/test.csv")
# now the first column is the label

library(caret)
inTrain <- createDataPartition(training_kaggle$label, p =.8, list = FALSE)
dim(inTrain) 
length(training_kaggle$label)*.8 # grabbed a subset of the indeces (not the items)
training_model <- training_kaggle[inTrain,]
testing_model <- training_kaggle[-inTrain,]

# h2o's split
# trainH <- as.h2o(training)
# splits <- h2o.splitFrame(data = trainH, ratios = c(.8), seed = 198)
# tr <- splits[[1]]
# ts <- splits[[2]]

# store into files
write.csv(training_model, file = "train-data.csv", row.names = FALSE)
write.csv(testing_model, file = "test-data.csv", row.names = FALSE)

# start h2o cluster
local.h2o <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)

# convert label to digit factors
training_model$label <- as.factor(training_model$label)
testing_model$label <- as.factor(testing_model$label)

# pass data to h2o
trData <- as.h2o(training_model)
tsData <- as.h2o(testing_model)


 #train using Tanh activation and 20 epochs
 res.dl <- h2o.deeplearning(x = 2:785, y = 1, trData, 
                            activation = "Tanh", hidden = rep(160, 5), epochs = 20) 
 
 # Casey's modified attempt
 
res.dl.cj <- h2o.deeplearning(x = 2:785, # column with predictors
                            y = 1, # the column with labels
                            trData, 
                            activation = "MaxoutWithDropout",
                            loss = "CrossEntropy",
                            input_dropout_ratio = .2, # % inputs to dropout
                            hidden_dropout_ratios = c(0, 0, 0, 0, .2),
                            hidden = rep(160, 5), 
                            epochs = 10) 
 

# now try to predict the testing dataset
pred.dl <- h2o.predict(object = res.dl, newdata = tsData[,-1])
pred.dl.df <- as.data.frame(pred.dl)
summary(pred.dl)
pred.dl.cj <- h2o.predict(object = res.dl.cj, newdata = tsData[,-1])
pred.dl.cj.df <- as.data.frame(pred.dl.cj)

test_labels <- testing_model[,1]
# check model accuracy
sum(diag(table(test_labels, pred.dl.df[,1])))/length(test_labels) # 96% for original
sum(diag(table(test_labels, pred.dl.cj.df[,1])))/length(test_labels) # 96.7% accuracy 

# now try to predict kaggle's test data
test_h2o <- as.h2o(testing_kaggle)
pred.dl.test <- h2o.predict(object = res.dl.cj, newdata = test_h2o)
df.test <- as.data.frame(pred.dl.test)
df.test <- data.frame(ImageId = seq(1, length(df.test$predict)),
                      Label = df.test$predict)
write.csv(df.test, file = "submission_to_kagggle.csv", row.names = FALSE)
h2o.shutdown(prompt = FALSE)