SET SCHEMA LEARNING_TO_NOTE;

DROP PROCEDURE R_PREDICT;
CREATE PROCEDURE R_PREDICT(IN train_data T_TD_POS_CLASSES, OUT result T_TD_POS_CLASSES, OUT stat T_R_STAT)
LANGUAGE RLANG AS
BEGIN
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

	true_pairs <- train_data[train_data$DDI != "NONE",]
	false_pairs <- train_data[train_data$DDI == "NONE",]

	false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs))
	false_downsampled <- false_pairs[false_downsampled_index,]

	all<-rbind(true_pairs, false_downsampled)

	bag <- bag.make(all$BEFORE)
	extracted_features_before <- bag$apply(all$BEFORE)
	colnames(extracted_features_before) <- paste("b", colnames(extracted_features_before), sep = "_")

	bag <- bag.make(all$BETWEEN)
	extracted_features_between <- bag$apply(all$BETWEEN)
	colnames(extracted_features_between) <- paste("i", colnames(extracted_features_between), sep = "_")

	bag <- bag.make(all$AFTER)
	extracted_features_after <- bag$apply(all$AFTER)
	colnames(extracted_features_after) <- paste("a", colnames(extracted_features_after), sep = "_")

	bag <- bag.make(all$P_BEFORE)
	pos_before <- bag$apply(all$P_BEFORE)
    colnames(pos_before) <- paste("pb", colnames(pos_before), sep = "_")

    bag <- bag.make(all$P_BETWEEN)
    pos_between <- bag$apply(all$P_BETWEEN)
    colnames(pos_between) <- paste("pi", colnames(pos_between), sep = "_")

    bag <- bag.make(all$P_AFTER)
    pos_after <- bag$apply(all$P_AFTER)
    colnames(pos_after) <- paste("pa", colnames(pos_after), sep = "_")

	index <- 1:nrow(all)

	extracted_features <- data.frame(all$DDI, extracted_features_before,extracted_features_between,extracted_features_after, pos_before, pos_between, pos_after)
	colnames(extracted_features)[1] <- "DDI"
	testindex <- sample(index, trunc(length(index)/3))
	testset <- extracted_features[testindex,]
	trainset <- extracted_features[-testindex,]

	svm.model <- svm(DDI ~ ., data = trainset, type="C-classification", cost = 100, gamma = 1)
	svm.pred <- predict(svm.model, testset[,-1])

	pred <-as.data.frame(svm.pred)
	result<-cbind(pred[,1], all[testindex,-1])
	colnames(result) <- c("DDI", "BEFORE", "BETWEEN", "AFTER", "P_BEFORE", "P_BETWEEN", "P_AFTER")

	conf <- table(svm.pred, testset[,1])
	print(conf)

	accuracy <- (conf[1,1] + conf[2,2] + conf[3,3] + conf[4,4] + conf[5,5]) / sum(conf)

	stat <-as.data.frame(matrix(c("accuracy", accuracy), nrow=1, ncol=2) )

	colnames(stat) <- c("NAME", "VALUE")
END;

