#Perceptron Training:

#dot product function
dot <- function(v, w){
    return (sum(v*t(w)))
}

norm <- function(v) {
    sqrt(dot(v,v))
}

#we need a function that checks if the w works on the data set
sep <- function(P,N,w) { #Positive, Negative Set, w is "affine" dual to the line
    for (i in P) if (dot(i,w) <= 0) return(FALSE)
    for (i in N) if (dot(i,w) >= 0) return(FALSE)
    return(TRUE)
}

#data creation:
wst<- runif(10, min = 0, max = 1) #the weight vector
train <- data.frame(t(replicate(100, runif(10, min = -1, max = 1))))  #create 100 points in R^10
train$X11 <- 1 #extend the vector
train$class <- 0

#we need the bound based on wst:
R <- max(apply(train[,1:10],1, norm))

rho <- min(blah)

blah <- rep(0, 100)
for (i in 1:100){
    blah[i]<- abs(dot(wst, train[i, 1:10]))
}
min(blah)

bound <- (R^2*norm(wst)^2)/rho^2

norm(wst)

timestoconverge <- c(230,187, 173, 209, 166, 226, 221, 169, 198, 268)

for (i in  1:100) {
    if (dot(wst,train[i,1:10]) > 0) train[i,12] <- 1 else train[i, 12] <- -1
}

P <- train[train$class == 1,1:10]
N <- train[train$class == -1,1:10]

P <- split(P, 1:nrow(P))
N <- split(N, 1:nrow(N))


#The Algorithm:


w <- rep(0, 11)
separated <- FALSE
weights <- list(w)

while(separated == FALSE) {
    i <- sample (1: dim(train)[1], 1)
    v <- train[i, 1:11] #random vector
    if (dot(v,w) > 0 & train[i, 12] == 1) next()
    else if (dot(v,w) < 0 & train[i, 12] == -1) next()
    else if (dot(v,w) >= 0 & train[i, 12] == -1) 
        {weights[[length(weights)+1]] <- w; w <- w - v}
    else if (dot(v,w) <= 0 & train[i, 12] == 1) 
        {weights[[length(weights)+1]] <- w; w <- w + v}
    separated <- sep(P,N,w)
}

w

weights

#_______________ slightly better algo

for (j in 1:10) {
    w <- rep(0, 11)
    separated <- FALSE
    i = 0
    
    while(separated == FALSE) {
        j <- sample (1: dim(train)[1], 1)
        v <- train[j, 1:11] #random vector
        y <- train[j, 12]
        if (dot(v,w)* y <= 0) {i = i+1; w <- w + y*v
        } else next()
        separated <- sep(P,N,w)
    }
    print(i)
}

w <- rep(0, 11)
separated <- FALSE
i = 0

while(separated == FALSE) {
    j <- sample (1: dim(train)[1], 1)
    v <- train[j, 1:11] #random vector
    y <- train[j, 12]
    if (dot(v,w)* y <= 0) {i = i+1; w <- w + y*v
    } else next()
    separated <- sep(P,N,w)
}

print(i)

b = numeric(1000)


