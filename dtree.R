convert.magic <- function(obj, type){
  FUN1 <- switch(type,
                 character = as.character,
                 numeric = as.numeric,
                 factor = as.factor)
  out <- lapply(obj, FUN1)
  as.data.frame(out)
}

extractFeatures <- function(data) {	
  character_cols <- names(Filter(function(x) x=="character", sapply(data, class)))	
  for (col in character_cols) {	
    data[,col] <- as.factor(data[,col])	
  }
  return(data)
}

#train_x <- subset(train, select = -c(Id, Hazard))
#train_x <- sparse.model.matrix(~., data = train_x)



eBayTrain = read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
eBayTest = read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)

cols.num <- c("condition", "cellular", "carrier", "color", "storage", "productline", "sold")
cols.num2 <- c("condition", "cellular", "carrier", "color", "storage", "productline")

eBayTrain[,cols.num] <- convert.magic(eBayTrain[,cols.num], "factor")
eBayTest[,cols.num2] <- convert.magic(eBayTest[,cols.num2], "factor")



levels(eBayTest$condition) <- levels(eBayTrain$condition)
levels(eBayTest$cellular) <- levels(eBayTrain$cellular)
levels(eBayTest$carrier) <- levels(eBayTrain$carrier)
levels(eBayTest$color) <- levels(eBayTrain$color)
levels(eBayTest$storage) <- levels(eBayTrain$storage)
levels(eBayTest$productline) <- levels(eBayTrain$productline)

eBayTrain[,cols.num2] <- convert.magic(eBayTrain[,cols.num2], "numeric")
eBayTest[,cols.num2] <- convert.magic(eBayTest[,cols.num2], "numeric")

str(eBayTrain)
str(eBayTest)


library(tm)   
library(data.table)
CorpusDescription = Corpus(VectorSource(c(eBayTrain$description, eBayTest$description)))    
CorpusDescription = tm_map(CorpusDescription, content_transformer(tolower), lazy=TRUE)    
CorpusDescription = tm_map(CorpusDescription, PlainTextDocument, lazy=TRUE)    
CorpusDescription = tm_map(CorpusDescription, removePunctuation, lazy=TRUE)    
CorpusDescription = tm_map(CorpusDescription, removeWords, c("100", "the", stopwords("english")), lazy=TRUE)   
CorpusDescription = tm_map(CorpusDescription, stemDocument, lazy=TRUE)
CorpusDescription = tm_map (CorpusDescription, removeWords, c("the", "this", "like", "doe", "devic"))

dtm = DocumentTermMatrix(CorpusDescription)    
sparse = removeSparseTerms(dtm, 0.98)    
DescriptionWords = as.data.frame(as.matrix(sparse))
setnames(DescriptionWords, "condition", "acondition")
str(DescriptionWords)

colnames(DescriptionWords) = make.names(colnames(DescriptionWords))
wordsTrain <- DescriptionWords[1:1861,]
wordsTest <- DescriptionWords[1862:2659,]
train1 <- cbind(eBayTrain,wordsTrain)
test1 <- cbind(eBayTest,wordsTest)

train1$description <- NULL
test1$description <- NULL
train1$UniqueID <- NULL
test1$UniqueID <- NULL

str(train1)
str(test1)
#train1 = convert.magic(train1, "numeric")
#test1 = convert.magic(test1, "numeric")


library(randomForest)
mrf2 = randomForest(sold ~ biddable + startprice + condition + cellular + carrier + color + storage + productline, data=train1)
plot(mrf2)

mrf3 = randomForest(sold ~ ., data=train1, ntree=300)
plot(mrf3)

predictRFtrain1 = predict(mrf3, newdata=train1)
table(train1$sold, predictRFtrain1)

predictRFtest = predict(mrf3, newdata=test1, type="prob")[,2]
PredTest = predictRFtest


MySubmission = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = PredTest)

write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)


library(caret)

train2 = train1 
train2$sold <- NULL
preproc = preProcess(train2)

normTrain = predict(preproc, train2)

normTest = predict(preproc, test1)

#cluster

distances = dist(normTrain, method="euclidean")
aHC = hclust(distances, method="ward.D")
hierGroups = cutree(aHC, k = 5)
table(hierGroups)

#compute partitions
HierCluster = split(normTrain, hierGroups)
lapply(split(normTrain, hierGroups), colMeans)

km = kmeans(normTrain, centers = 5)
table(km$cluster)
km$size














library(rpart)
library(rpart.plot)

mtree = rpart(sold ~ . - description - UniqueID, data = eBayTrain, method = "class")

mtree = rpart(sold ~ biddable + startprice + condition + cellular + carrier + color + storage + productline, 
              data=train1, method="class")
prp(mtree)

predictTrain = predict(mtree, newdata=eBayTrain)
table(train1$sold, predictTrain)

predictTest = predict(mtree, newdata=eBayTest, method="class")

