##################################################################################
# MXNet Deep Learning CNN Neural Network Tutorial
##################################################################################


# https://www.r-bloggers.com/image-recognition-tutorial-in-r-using-deep-convolutional-neural-networks-mxnet-package

# Here is how we will install this package from the repository

install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("mxnet")

# Now we are going to use EBImage to resize each image to 28×28 pixel and generate the training and testing datasets. 
# You may ask why I am resizing the images, well, for some reason my 
# machine does not like 64×64 pixel images and my PC crash whenever I run the model with the data. 
# Not good. But that's ok since we can get good results with smaller images too (you can try 
# running the model with the 64×64 size images if your PC does not crash though). Let's go on now:


# This script is used to resize images from 64x64 to 28x28 pixels

# Clear workspace
rm(list=ls())

# Load EBImage library
require(EBImage)

# Load data
X <- read.csv("olivetti_X.csv", header = F)
labels <- read.csv("olivetti_y.csv", header = F)

# Dataframe of resized images
rs_df <- data.frame()

# Main loop: for each image, resize and set it to greyscale
for(i in 1:nrow(X))
{
  # Try-catch
  result <- tryCatch({
    # Image (as 1d vector)
    img <- as.numeric(X[i,])
    # Reshape as a 64x64 image (EBImage object)
    img <- Image(img, dim=c(64, 64), colormode = "Grayscale")
    # Resize image to 28x28 pixels
    img_resized <- resize(img, w = 28, h = 28)
    # Get image matrix (there should be another function to do this faster and more neatly!)
    img_matrix <- img_resized@.Data
    # Coerce to a vector
    img_vector <- as.vector(t(img_matrix))
    # Add label
    label <- labels[i,]
    vec <- c(label, img_vector)
    # Stack in rs_df using rbind
    rs_df <- rbind(rs_df, vec)
    # Print status
    print(paste("Done",i,sep = " "))},
    # Error function (just prints the error). Btw you should get no errors!
    error = function(e){print(e)})
}


# Set names. The first columns are the labels, the other columns are the pixels.
names(rs_df) <- c("label", paste("pixel", c(1:784)))

# Train-test split
#-------------------------------------------------------------------------------
# Simple train-test split. No crossvalidation is done in this tutorial.

# Set seed for reproducibility purposes
set.seed(100)

# Shuffled df
shuffled <- rs_df[sample(1:400),]

# Train-test split
train_28 <- shuffled[1:360, ]
test_28 <- shuffled[361:400, ]

# Save train-test datasets
write.csv(train_28, "C://train_28.csv", row.names = FALSE)
write.csv(test_28, "C://test_28.csv", row.names = FALSE)

# Done!
print("Done!")

# This part of the tutorial should be pretty self explanatory, if you are not sure on what is the output, I suggest taking a look at the rs_df dataframe. It should be a 400×785 dataframe as follows:

# label, pixel1, pixel2, . , pixel784
# 0, 0.2, 0.3, . ,0.1


################################################################################

# Part 2 - Building the model

# Now for the fun part, let's build the model. Below you can find the script I used to train 
# and test the model. After the script you can find my comments and explanations on the code.

# Clean workspace
rm(list=ls())

# Load MXNet
require(mxnet)

# Loading data and set up
#-------------------------------------------------------------------------------

# Load train and test datasets
train <- read.csv("train_28.csv")
test <- read.csv("test_28.csv")

# Set up train and test datasets
train <- data.matrix(train)
train_x <- t(train[, -1])
train_y <- train[, 1]
train_array <- train_x
dim(train_array) <- c(28, 28, 1, ncol(train_x))

test_x <- t(test[, -1])
test_y <- test[, 1]
test_array <- test_x
dim(test_array) <- c(28, 28, 1, ncol(test_x))

# Set up the symbolic model
#-------------------------------------------------------------------------------

data <- mx.symbol.Variable('data')
# 1st convolutional layer
conv_1 <- mx.symbol.Convolution(data = data, kernel = c(5, 5), num_filter = 20)
tanh_1 <- mx.symbol.Activation(data = conv_1, act_type = "tanh")
pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
# 2nd convolutional layer
conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(5, 5), num_filter = 50)
tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
pool_2 <- mx.symbol.Pooling(data=tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
# 1st fully connected layer
flatten <- mx.symbol.Flatten(data = pool_2)
fc_1 <- mx.symbol.FullyConnected(data = flatten, num_hidden = 500)
tanh_3 <- mx.symbol.Activation(data = fc_1, act_type = "tanh")
# 2nd fully connected layer
fc_2 <- mx.symbol.FullyConnected(data = tanh_3, num_hidden = 40)
# Output. Softmax output since we'd like to get some probabilities.
NN_model <- mx.symbol.SoftmaxOutput(data = fc_2)

# Pre-training set up
#-------------------------------------------------------------------------------

# Set seed for reproducibility
mx.set.seed(100)

# Device used. CPU in my case.
devices <- mx.cpu()

# Training
#-------------------------------------------------------------------------------

# Train the model
model <- mx.model.FeedForward.create(NN_model,
                                     X = train_array,
                                     y = train_y,
                                     ctx = devices,
                                     num.round = 480,
                                     array.batch.size = 40,
                                     learning.rate = 0.01,
                                     momentum = 0.9,
                                     eval.metric = mx.metric.accuracy,
                                     epoch.end.callback = mx.callback.log.train.metric(100))

# Testing
#-------------------------------------------------------------------------------

# Predict labels
predicted <- predict(model, test_array)
# Assign labels
predicted_labels <- max.col(t(predicted)) - 1
# Get accuracy
sum(diag(table(test[, 1], predicted_labels)))/40

################################################################################
#                           OUTPUT
################################################################################
#
# 0.975
#


