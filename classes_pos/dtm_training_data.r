data = read.csv("~/Documents/python_hana/analysis/rData/data_classes.csv")

library(tm)
library(e1071)
library(SparseM)


true_pairs <- data[data$DDI != "NONE",]
false_pairs <- data[data$DDI == "NONE",]

false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs))
false_downsampled <- false_pairs[false_downsampled_index,]

data<-rbind(true_pairs, false_downsampled)

before_Corpus = Corpus(VectorSource(data$BEFORE))
before_dtm <- DocumentTermMatrix(before_Corpus, control = list(removeStopwords=FALSE))
colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")

between_Corpus = Corpus(VectorSource(data$BETWEEN))
between_dtm <- DocumentTermMatrix(between_Corpus, control = list(removeStopwords=FALSE))
colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")

after_Corpus = Corpus(VectorSource(data$AFTER))
after_dtm <- DocumentTermMatrix(after_Corpus, control = list(removeStopwords=FALSE))
colnames(after_dtm) <- paste("a", colnames(after_dtm), sep = "_")

extracted_features <- cbind(before_dtm, between_dtm, after_dtm)

ddi <- data$DDI

index <- 1:nrow(extracted_features)
testindex <- sample(index, trunc(length(index)/5))
testset <- extracted_features[testindex,]
trainset <- extracted_features[-testindex,]
svm.model <- svm(x = trainset, y=ddi[-testindex], type="C-classification", cost = 8, gamma = 0.5)

svm.pred <- predict(svm.model, testset)

conf <- table(svm.pred,ddi[testindex])
accuracy <- (conf[1,1] + conf[2,2] + conf[3,3] + conf[4,4] + conf[5,5]) / sum(conf)

# smallTDM <- removeSparseTerms(bigTDM, sparse= 0.8)dtm.to.sm <->
#   sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,
#                dims=c(dtm$nrow, dtm$ncol))
# }
# dtm <- DocumentTermMatrix(the_corpus,
#                  control=list(wordLengths=c(1, Inf),
#                  bounds=list(global=c(floor(length(the_corpus)*0.05), Inf))))