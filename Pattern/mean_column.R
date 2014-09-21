df1 <-data.frame(X=rep(x=LETTERS[1:5], each=1), Y=1.1:5.5, Z=6.1:10.5)
df1
df1$Y.New <- ave(((df1$Y+df1$Z)/2), df1$X)
