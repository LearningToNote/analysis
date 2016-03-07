# upsampling
# test accuracy with different sizes of trainset


####################################################################################################

bag_o_words <-
function(text.var, apostrophe.remove = FALSE, ...) {
    if (identical(list(), list(...))) {
        bag_o_words1(x = text.var, apostrophe.remove = apostrophe.remove, ...)
    } else {
        bag_o_words2(x = text.var, apostrophe.remove = apostrophe.remove)
    }
}

bag_o_words1 <-
function(x, apostrophe.remove = FALSE) {
    x <- gsub("\\|", "", x[!is.na(x)])
    x <- paste(x, collapse=" ")
    if(apostrophe.remove) {
        reg <- "[^[:alnum:]]"
        x <- gsub("'", "", x)
    } else {
        reg <- "[^[:alnum:]|\\']"
    }
    x <- strsplit(tolower(gsub(reg, " ", x)), "\\s+")[[1]]
    x[x != ""]
}

bag_o_words2 <-
function(x, apostrophe.remove = FALSE, ...) {
    unblanker(words(strip(clean(x),
        apostrophe.remove = apostrophe.remove, ...)))
}

unbag <- function(text.var, na.rm = TRUE) {
    text.var <- unlist(text.var)
    if (na.rm) text.var <- text.var[!is.na(text.var)]
    paste(text.var, collapse=" ")
}

breaker <-
function(text.var) {
    unblanker(unlist(strsplit(as.character(text.var),
        "[[:space:]]|(?=[|.!?*-])", perl=TRUE)))
}

word_split <-
function (text.var) {
    x <- reducer(Trim(clean(text.var)))
    sapply(x, function(x) {
            unblanker(unlist(strsplit(x, "[[:space:]]|(?=[.!?*-])", perl = TRUE)))
        }, simplify = FALSE
    )
}

####################################################################################################

library(e1071)

bag.make = function(x, n = 100000000000){

    bag = bag_o_words(x)
    bag = sort(table(bag), dec = TRUE)
    n = min(n, length(bag))
    counts = bag[1:n]
    bag = names(bag)[1:n]

    b = list(items = bag, counts = counts,

        apply = function(targ){
            target = apply(data.frame(targ), 1, bag_o_words)

            #idx = apply(target, 2, match, bag)
            m = matrix(0, nrow = length(targ), ncol=length(bag))
            #i ~ row, j ~ column
            for(i in 1:length(targ)) {
                for (j in 1:length(bag)) {
                    if (bag[j] %in% unlist(target[i])) {
                        m[i,j] = 1
                    } else {
                        m[i,j] = 0
                    }

                }
            }
            #sapply(1:nrow(m), function(r){
            #    m[r, idx[r,]] <<- 1
            #})
            colnames(m) = bag
            m
        })
    class(b) = 'bag'
    b
}


train_data <- read.csv("/home/johannes/code/masterproject/data_all.csv")


ddi.sample <- function(coll, n) {
	index <- sample(1:nrow(coll), n, replace=TRUE)
	return(coll[index,])
}

n = 700
runs = 5

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


# get test/train data
index <- 1:nrow(train_data)
testindex <- sample(index, trunc(length(index)/10))

testset <- extracted_features[testindex,]
trainset <- extracted_features[-testindex,]


# UP sample
advise <- ddi.sample(trainset[trainset$DDI == "advise",], n)
mechanism <- ddi.sample(trainset[trainset$DDI == "mechanism",], n)
int <- ddi.sample(trainset[trainset$DDI == "int",], n)
effect <- ddi.sample(trainset[trainset$DDI == "effect",], n)
none <- ddi.sample(trainset[trainset$DDI == "NONE",], n)

trainset <- rbind(advise, mechanism, int, effect, none)



len <- length(trainset)*.9
acc <- vector(mode='numeric')
is <- vector(mode='numeric')

for (i in seq(5,len, by=50)) {

	print("#######")
	print(i)

    tmp = 0

    for (j in seq(0,runs)) {
        shuffle <- sample(1:nrow(trainset))
        trainset <- trainset[shuffle,]

        svm.model <- svm(DDI ~ ., data = trainset[1:i,], type="C-classification", cost = 8, gamma = 0.5)
        svm.pred <- predict(svm.model, testset[,-1])

        conf <- table(svm.pred, testset[,1])

        accuracy <- (conf[1,1] + conf[2,2] + conf[3,3] + conf[4,4] + conf[5,5]) / sum(conf)
        tmp = tmp + accuracy
        print(accuracy)
    }

    accuracy = tmp / runs

    acc <- append(acc, accuracy)
    is <- append(is, i)

	print("#######")
}

print(acc)
print(is)

plot(is, acc)