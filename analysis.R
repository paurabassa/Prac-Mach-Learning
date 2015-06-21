testing <- read.csv("pml-testing.csv")
 classes <- sapply(1:ncol(testing),function(i){class(testing[,i])}
