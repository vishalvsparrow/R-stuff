rm(list= ls(all.names = TRUE))
setwd('C:\\Users\\vishal\\Google Drive\\CSUF\\ISDS-574\\HW2')

toyota_data = read.csv('toyota_clean2.csv',header = TRUE, na.strings = '')
View(toyota_data)


quantile_75th = quantile(toyota_data$Price,0.75)[['75%']]

high_price = rep(NA,nrow(toyota_data))
for(i in 1: length(high_price)){
  if(toyota_data$Price[i] >= quantile_75th){
    high_price[i] = 1
  }
  else if(toyota_data$Price[i] < quantile_75th){
    high_price[i] = 0
  }
}

View(toyota_data)

tmp.dat = toyota_data[,-1] # We only scale the dependent variables for the purpose of running kNN 
#tmp.dat = tmp.dat[,-30] # We do not add the high price variable anymore
scaled.dat = scale(tmp.dat)

scaled.dat = as.data.frame(scaled.dat)
scaled.dat$Price <- toyota_data$Price # Add the Price variable to the newly scaled, or normalized, data

#Partitioning the data, into 60% training and 40% validation sets
set.seed(12345)
id.train = sample(1:nrow(toyota_data),nrow(toyota_data)*0.6)
id.validation = setdiff(1:nrow(toyota_data),id.train)

id.train_scaled = sample(1:nrow(scaled.dat),nrow(scaled.dat)*0.6)
id.validation_scaled = setdiff(1:nrow(scaled.dat),id.train_scaled)

#Training a multiple linear regression model with forward selection
min.model = lm(Price ~ 1, data = toyota_data[id.train,])
max.model = lm(Price ~ ., data = toyota_data[id.train,])

summary(max.model)$adj.r.squared

forward.obj = step(min.model, scope = list(lower = min.model, upper = max.model), direction = 'forward')
summary(forward.obj)

obj_call = forward.obj$call
forward.model = lm(obj_call$formula, data = toyota_data[id.train,])
yhat = predict(forward.model, newdata = toyota_data[id.validation, ])

#install.packages('hydroGOF')
require('hydroGOF')

forward.rmse = rmse(toyota_data[id.validation, 'Price'], yhat) 
forward.rmse #<----------------------------------------------------------Forward selection RMSE = 1484.519

#my_RMSE(toyota_data[id.validation, 'Price'], yhat)

# Regression based KNN

#install.packages('FNN')
require('FNN')
#install.packages("hydroGOF")
require('hydroGOF')

# knn.bestK = function(train, test, y.train, y.test, k.max = 20) {
# default value k.max = 20, if no argument passed
#   pe = rep(NA, k.max)
#   for (ii in 1:k.max) {
#     y.hat = knn(train, test, y.train, k = ii, prob=F)
#     pe[ii] = sum(y.hat != y.test)
#   }
#   browser()
#   out = list(k.optimal = which.min(pe), pe.min = min(pe))
#   
#   return(out)
# }

# Selecting the best k for kNN regression
knn.bestk_reg = function(my_train, my_test, my_ytrain, validation_price, k.max = 20) {
  
  #each_rmse = rep(NA, k.max)
  each_rmse <- as.data.frame(matrix(0, ncol = 4, nrow = k.max))
  colnames(each_rmse) <- c("K", "RMSE", "MSE", "MAD") # MAD looks erroneous // NOT ERRONEOUS ANYMORE :)
  for (i in 1:k.max){
    
    knn_reg_obj_val = knn.reg(train = my_train,
                              test = my_test,
                              y = my_ytrain, k = i)
    
    each_rmse[i,1] <- i
    each_rmse[i,2] <- rmse(validation_price, knn_reg_obj_val$pred)
    each_rmse[i,3] <- mse(validation_price, knn_reg_obj_val$pred)
    each_rmse[i,4] <- mad(validation_price, median(knn_reg_obj_val$pred))
    
  }
  return(each_rmse)
}


# Learnt it the hard way.
# data[1:7, 2:(length(data) -1)] != data[1:7, 2:length(data) -1]
# Select the best k based off RMSE

knn_reg_errors = knn.bestk_reg(scaled.dat[id.train_scaled, 1:(length(scaled.dat)-1)],
                               scaled.dat[id.validation_scaled, 1:(length(scaled.dat)-1)],
                               scaled.dat[id.train_scaled,]$Price, scaled.dat[id.validation_scaled,]$Price)

knn_reg.rmse = knn_reg_errors[which.min(knn_reg_errors$RMSE),which(names(knn_reg_errors) == "K")]
knn_reg_errors[knn_reg.rmse, 2]#<-------------------------------------regression based kNN RMSE = 1793.279

# A simpler but naive alternative:--> which.min(knn_reg_errors$RMSE)

# knn.reg(train = toyota_data[id.train,2:length(toyota_data)],
#         test = toyota_data[id.validation, 2:length(toyota_data)],
#         y = toyota_data[id.train,1], k = i)

#knn_reg_obj = knn.reg(train = toyota_data[id.train,2:length(toyota_data)],
#                      y = toyota_data[id.train,1], k =3)
#predicted_price_val = knn_reg_obj_val$pred
#predicted_price_train = knn_reg_obj$pred

#knn_pred_price = knn_reg_obj_val

#original_price_val = toyota_data[id.validation,]$Price
#original_price_train = toyota_data[id.train,]$Price
#high_price_val = toyota_data[id.validation,]$high_price
#high_price_train = toyota_data[id.train,]$high_price

#cor(predicted_price_train, original_price_train)
#cor(predicted_price_val, original_price_val)
#cor(original_price_train, high_price_train)

# I will now calculate the RMSE for the predicted KNN prices on the validation set
#my_RMSE = function(original, predicted)
#{
#  return(sqrt(mean((original - predicted)^2)))
#}

#RMSE_validation = rmse(original_price_val, predicted_price_val)
#RMSE_train = rmse(original_price_train, predicted_price_train)

# We may observe something strange above. the RMSE for validation set is lower than for the training set.
# This may seem counter-intuitive, but one must note that the knn.reg() if not supplied
# with a test/validation set, then it automatically does the Cross Validation on the training set.
# Therefore, what we get is not really a model trained purely on training set, but rather a model
# that has already been trained using Cross Validation on the training set, giving different predicted
# values than it would if it were purely trained using the training set.
# This makes a point that methods with automatic cross-validation should not be compared with the same
# methods trained on a manually selected cross-validation set!

#temp = toyota_data[id.validation,1:3]
#temp[,2] = knn_reg_obj_val$pred
#temp[,3] = knn_reg_obj$pred[1:392]
#View(temp)

#--------------------- TIME TO GROW SOME REGRESSION TREES (or maybe aliens)------
# 
# .-""""-.        .-""""-.
# /        \      /        \
# /_        _\    /_        _\
# // \      / \\  // \      / \\
# |\__\    /__/|  |\__\    /__/|
#   \    ||    /    \    ||    /
#   \        /      \        /
#   \  __  /        \  __  / 
#   '.__.'          '.__.'
#

require('rpart')

# reg_toyota_data = toyota_data[,1:(length(toyota_data))]

reg_tree = rpart(Price ~ ., method = "anova", data = toyota_data)
summary(reg_tree)
printcp(reg_tree)
plotcp(reg_tree)

par(mfrow = c(1,2))
rsq.rpart(reg_tree)

plot(reg_tree, uniform=T, main="Regression Tree")
text(reg_tree, use.n=T, all=T, cex=.8)

which.min(reg_tree$cptable[,"xerror"])

prune_tree = prune(reg_tree,
                   cp = reg_tree$cptable[which.min(reg_tree$cptable[,"xerror"]),"CP"])

plot(prune_tree, uniform=T, main="Best Prune Tree")
text(prune_tree, use.n=T, all=T, cex=.8)

summary(prune_tree)

# We find that the tree with 3 splits is the best pruned tree

# Trying cross validation with separate training and test data

tree = rpart(Price ~. ,method = "anova" ,data = toyota_data[id.train,])
id.cp = which.min(tree$cptable[,"xerror"])

tree.std_dev = tree$cptable[id.cp ,5]
tree.cv_error = tree$cptable[id.cp,4]  #standard error of the lowest cv error

tree.cv_error + tree.std_dev
# How much standard error can we afford to prune the tree?

# We may have to settle down with 3 splits. Let's see!

pruned_tree2 = prune(tree, cp = tree$cptable[which.min(reg_tree$cptable[,"xerror"]),"CP"])
summary(pruned_tree2)

# We were correct in our assumption. 3 splits it is!

pData1 <- predict(tree, toyota_data[id.validation,])
summary(pData1)
summary(tree)

tree.rmse = rmse(toyota_data[id.validation, 'Price'], pData1)
tree.rmse
# RMSE = 1619.5318 when full tree is trained

pData2 <- predict(pruned_tree2, toyota_data[id.validation,])
summary(pData2)
summary(tree)
pruned_tree2.rmse = rmse(toyota_data[id.validation, 'Price'], pData2) #<---Regression Tree RMSE = 1639.08
pruned_tree2.rmse

# RMSE = 1696.206 with pruned tree. Saves us some time, huh?

#--------------------------- Classification section starts here ----------------------------#

classify_dat = toyota_data[,- which(colnames(toyota_data) == 'Price')]
classify_dat$high_price <- high_price
View(classify_dat)

# Fitting a Logistic Regression model
table(classify_dat$high_price)

dat.train = classify_dat[id.train,]
dat.valid = classify_dat[id.validation,]

min.model_logit = glm(high_price ~ 1, data = dat.train, family = 'binomial')
max.model_logit = glm(high_price ~ ., data = dat.train, family = 'binomial')

forward.logit = step(min.model_logit, scope = list(lower = min.model_logit, upper = max.model_logit),
                     direction = 'forward')

formula(forward.logit)
summary(forward.logit)

out = matrix(NA, 11, 3)
colnames(out) = c('OR', 'SE', 'pval')
rownames(out) = rownames(summary(forward.logit)$coefficients)

out[,1] = exp(summary(forward.logit)$coefficient[,1])
out[,2] = out[,1] *exp(summary(forward.logit)$coefficient[,2])
out[,3] = exp(summary(forward.logit)$coefficient[,4])

yhat = predict(forward.logit, newdata = dat.valid, type = 'response')
hist(yhat)

threshold = function(yhat, cutoff=.5) {
  temp_pred = rep(0, length(yhat))
  temp_pred[yhat > cutoff] = 1
  return(temp_pred)
}

yhat.class = threshold(yhat)
hist(yhat.class)
table(yhat.class)

# confusion matrix
cm <- table(actual = dat.valid$high_price, predicted = yhat.class)

# mean misclassification error
# sum(yhat.class != dat.valid$high_price)/length(id.validation)
mmce <- 1- (sum(diag(cm))/sum(cm))
mmce # 0.0918

# sensitivity
sentvty = cm[2,2]/sum(cm[2,2],cm[2,1]) # 0.7714
# specificity
specfty = cm[1,1]/sum(cm[1,1],cm[1,2]) # 0.9581
#FPR
fpr = cm[1,2]/sum(cm[1,2],cm[2,2])
#FNR
fnr = cm[2,1]/sum(cm[2,1],cm[1,1])

# A hand-written function to plot the ROC curve :)
ROC_curve = function(predicted, actual = dat.valid$high_price, roc_step = 0.002){
  
  roc_size = (1/roc_step) - 1
  
#  count =  1
#  for(i in seq(0,1, by = 0.001)){
#    count = count+1
#  }
  
  out = matrix(NA, roc_size, 3)
  colnames(out) <- c('Cut-off', '1-specificity', 'sensitivity')
  
  count = 1
  for(i in seq(0,1, by = roc_step)){
    
    if(i == 1) break;
    if(i == 0) next;
    
    x = threshold(predicted,i)
    cm <- table(actual, x)
    sentvty = cm[2,2]/sum(cm[2,2],cm[2,1])
    specfty = cm[1,1]/sum(cm[1,1],cm[1,2])
    
    out[count,1] <- i
    out[count,2] <- 1-specfty
    out[count,3] <- sentvty
    
    count = count+1
  }
  return(out)
}

roc_obj = ROC_curve(yhat, roc_step = 0.0001)
dev.off()
plot(roc_obj[,2],roc_obj[,3],xlab = '1 - Specificity', ylab = 'Sensitivity')

# Classification based kNN (the data is already normalized in the scaled.dat data.frame)

require(class)

scaled.dat <- scaled.dat[,-30] # Removing the Price feature
scaled.dat$high_price <- high_price # Adding the high_price feature


knn_class_obj = knn(train = scaled.dat[id.train, 1:(length(scaled.dat)-1)],
              test =  scaled.dat[id.validation, 1:(length(scaled.dat)-1)],
              scaled.dat[id.train,]$high_price,
              k = 11)
table(knn_class_obj)

knn.bestk_class = function(my_train, my_test, my_y.train, my_y.validation, k.max = 20){
  
  each_val_error <- as.data.frame(matrix(0, ncol = 3, nrow = k.max))
  colnames(each_val_error) <- c('k', 'training error', 'validation error')
  
  for(i in 1:k.max){
    
    knn_class_obj = knn(train = my_train,
                        test =  my_test,
                        my_y.train,
                        k = i)
    
    knn_class_obj_train = knn(train = my_train,
                        test =  my_train,
                        my_y.train,
                        k = i)
    
    each_val_error[i,1] <- i
    each_val_error[i,2] <- (sum(knn_class_obj_train != my_y.train))/length(my_y.train)
    each_val_error[i,3] <- (sum(knn_class_obj != my_y.validation))/length(my_y.validation)
    
  }
  
  return(each_val_error)
  
  
} 

knn_class_result = knn.bestk_class(scaled.dat[id.train, 1:(length(scaled.dat)-1)],
                                   scaled.dat[id.validation, 1:(length(scaled.dat)-1)],
                                   scaled.dat[id.train,]$high_price,
                                   scaled.dat[id.validation,]$high_price)

knn_class_result # Hurray! 

#lowest_class_error = knn_class_result[which.min(knn_class_result$`validation error`),
#                                which(names(knn_class_result) == "validation error")]

best_k_class = which.min(knn_class_result$`validation error`) # I am retrieving the index, not the k. Change this

best_k_class # <---------------------------------- the best k is k = 7 

# Where did CART go? email vishalvatnani@csu.fullerton.edu
# Just a manipulation check :D