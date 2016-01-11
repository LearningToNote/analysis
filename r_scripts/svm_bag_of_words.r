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


library(e1071)
library(qdap)

train_data = read.csv("~/Downloads/coolData2.csv")

true_pairs <- train_data[train_data$DDI == 1,]
false_pairs <- train_data[train_data$DDI == 0,]

false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs))
false_downsampled <- false_pairs[false_downsampled_index,]

ball<-rbind(true_pairs, false_downsampled)

bag <- bag.make(all$BEFORE)

extracted_features_before <- bag$apply(all$BEFORE)
colnames(extracted_features_before) <- paste("b", colnames(extracted_features_before), sep = "_")

extracted_features_between <- bag$apply(all$BETWEEN)
colnames(extracted_features_between) <- paste("i", colnames(extracted_features_between), sep = "_")

extracted_features_after <- bag$apply(all$AFTER)
colnames(extracted_features_after) <- paste("a", colnames(extracted_features_after), sep = "_")

index <- 1:nrow(all)

all <-cbind(all$DDI,extracted_features_before,extracted_features_between,extracted_features_after)
colnames(all)[1] <- "DDI"
testindex <- sample(index, trunc(length(index)/3))
testset <- all[testindex,]
trainset <- all[-testindex,]

svm.model <- svm(DDI ~ ., data = trainset, type="C-classification")
svm.pred <- predict(svm.model, testset[,-1])

pred <-as.data.frame(svm.pred)
result<-cbind(pred[,1], ball[testindex,-1])
colnames(result)[1] <- "DDI"
