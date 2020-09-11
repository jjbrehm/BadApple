library(purrr)

#Create fake data where 10 is the number of Replications

array1<-matrix(sample(letters,100, replace=T), 10)
array2<-matrix(sample(letters,100, replace=T), 10)

#Split matrixes into list of row vectors.

rowvec1<-split(array1, row(array1))
rowvec2<-split(array2, row(array2))

map2(rowvec1, rowvec2,~sum(is.element(.x, .y)))

