library(gptk)

x<-as.matrix(seq(1, 35, by = 3))
y<- as.matrix(sin(pi*5*x/36)+x/3.6)
plot(y~x)

kern = kernCreate(x, "rbf")
Kxx = kernCompute(kern, x, x)

xs <- as.matrix(1:100)
Kxsx <- kernCompute(kern, xs, x)
options<- gpOptions()
options$learnScales <- T
options$kern$comp = list("rbf", "white")
model <- gpCreate(dim(x)[2], dim(y)[2], x, y, options)
gpLogLikelihood(model)
model = gpOptimise(model, display = TRUE, iters = 400)
gpLogLikelihood(model)
yPred <- gpPosteriorMeanVar(model, xs, varsigma.return=FALSE)
