
train_data <- read.csv("/home/johannes/code/masterproject/data_all.csv")


ddi.sample <- function(coll, n) {
	index <- sample(1:nrow(coll), n, replace=TRUE)
	return(coll[index,])
}

n = 700

none <- train_data[train_data$DDI == "NONE",]
rest <- train_data[train_data$DDI != "NONE",]

# DOWN sample none
none <- ddi.sample(none, 1000)

# recombine data
train_data <- rbind(rest, none)


# extract features
bag <- bag.make(train_data$BEFORE)
extracted_features_before <- bag$apply(train_data$BEFORE)
colnames(extracted_features_before) <- paste("b", colnames(extracted_features_before), sep = "_")

bag <- bag.make(train_data$BETWEEN)
extracted_features_between <- bag$apply(train_data$BETWEEN)
colnames(extracted_features_between) <- paste("i", colnames(extracted_features_between), sep = "_")

bag <- bag.make(train_data$AFTER)
extracted_features_after <- bag$apply(train_data$AFTER)
colnames(extracted_features_after) <- paste("a", colnames(extracted_features_after), sep = "_")

bag <- bag.make(train_data$P_BEFORE)
pos_before <- bag$apply(train_data$P_BEFORE)
colnames(pos_before) <- paste("pb", colnames(pos_before), sep = "_")

bag <- bag.make(train_data$P_BETWEEN)
pos_between <- bag$apply(train_data$P_BETWEEN)
colnames(pos_between) <- paste("pi", colnames(pos_between), sep = "_")

bag <- bag.make(train_data$P_AFTER)
pos_after <- bag$apply(train_data$P_AFTER)
colnames(pos_after) <- paste("pa", colnames(pos_after), sep = "_")

#combine extracted features
extracted_features <- data.frame(train_data$DDI, extracted_features_before,extracted_features_between,extracted_features_after, pos_before, pos_between, pos_after)
colnames(extracted_features)[1] <- "DDI"



# UP sample
# advise <- ddi.sample(extracted_features[extracted_features$DDI == "advise",], n)
# mechanism <- ddi.sample(extracted_features[extracted_features$DDI == "mechanism",], n)
# int <- ddi.sample(extracted_features[extracted_features$DDI == "int",], n)
# effect <- ddi.sample(extracted_features[extracted_features$DDI == "effect",], n)
# none <- ddi.sample(extracted_features[extracted_features$DDI == "NONE",], n)

# data <- rbind(advise, mechanism, int, effect, none)
data <- extracted_features
shuffle <- sample(1:nrow(data))
data <- data[shuffle,]


# svm.model <- svm(DDI ~ ., data = trainset[1:i,], type="C-classification", cost = 100, gamma = 1)
obj <- tune(svm, DDI ~ ., data = data, tunecontrol = tune.control(sampling = "cross", cross = 5), ranges = list(gamma=2^(-1:1), cost=2^(2:5)))
summary(obj)
plot(obj)


# get test/train data
index <- 1:nrow(data)
testindex <- sample(index, trunc(length(index)/10))

testset <- extracted_features[testindex,]
trainset <- extracted_features[-testindex,]

svm.model <- svm(DDI ~ ., data = trainset[1:i,], type="C-classification", cost = 8, gamma = 0.5)
svm.pred <- predict(svm.model, testset[,-1])

conf <- table(svm.pred, testset[,1])
print(conf)

accuracy <- (conf[1,1] + conf[2,2] + conf[3,3] + conf[4,4] + conf[5,5]) / sum(conf)
print(accuracy)



# Parameter tuning of ‘svm’:

# - sampling method: 5-fold cross validation 

# - best parameters:
#  gamma cost
#    0.5    8

# - best performance: 0.4198217 

# - Detailed performance results:
#    gamma cost     error  dispersion
# 1    0.5    4 0.4200960 0.023666746
# 2    1.0    4 0.5900556 0.016124334
# 3    2.0    4 0.6795720 0.008809003
# 4    0.5    8 0.4198217 0.023580123
# 5    1.0    8 0.5900556 0.016124334
# 6    2.0    8 0.6795720 0.008809003
# 7    0.5   16 0.4198217 0.023580123
# 8    1.0   16 0.5900556 0.016124334
# 9    2.0   16 0.6795720 0.008809003
# 10   0.5   32 0.4198217 0.023580123
# 11   1.0   32 0.5900556 0.016124334
# 12   2.0   32 0.6795720 0.008809003
