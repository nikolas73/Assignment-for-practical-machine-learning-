# Download and read the files

x<-download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile="pml-training.csv")
x<-read.csv("pml-training.csv")
# head(x)
y<-download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile="pml-testing.csv")
y<-read.csv("pml-testing.csv")
# head(y)

# Install and open the caret package

library(rpart)
library(caret)
library(randomForest)
# Find which columns have NAs and subset the original dataframe

## Make a loop, establishing an empty vector z and a munerator n, equal to 1.
z<-vector()
n=1
for (i in names(x)) {
        z[n]<-sum(is.na(x[i]))
        n=n+1
}
z2<-data.frame("names"=names(x), "NAs"=z)
z3<-as.character(z2[z2$NAs==0, ]$names)
# subset the original data
x2<-x[z3]
# head(x2)
y2<-y[z3[1:92]] ##because testing set does not have classe which is the last column in x and x2 training sets
# head(y2)

# Get rid of cells that are empty
x3<-apply(x2, 2, as.character)
x4<-apply(x3, 2, nchar)
x5<-apply(x4, 2, sum)
x8<-x5[x5>19000]
smallset<-names(x8)
x9<-x2[smallset]
x10<-x9[, 8:60]

#Correlation
library("corrplot", lib.loc="C:/R-3.1.1/library")
corMat<-cor(x10[, -53])
corrplot(corMat, order = "FPC", method = "color", type = "lower", tl.cex = 0.6, tl.col = rgb(0, 0, 0))

# Split training set into training and validation
inTrain = createDataPartition(y = x10$classe, p = 0.7, list = FALSE)
toTrain = x10[inTrain, ]
toValidate = x10[-inTrain, ]

# precict in Trainig set
library("randomForest")
modelFit2 <- train(toTrain$classe ~ ., data = toTrain, preProcess="pca", method="rf")
saveModelFit<-save(modelFit2, file="savemodelFit2")

predictsValid<-predict(modelFit2, toValidate)
results<-confusionMatrix(predictsValid, toValidate$classe)
predictsTest<-predict(modelFit2, y)
answers=as.character(predictsTest)

pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}
pml_write_files(answers)

