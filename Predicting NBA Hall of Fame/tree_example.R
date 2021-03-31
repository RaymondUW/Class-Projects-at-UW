set.seed(32)
# a 2-dimensional example
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = .2, sd = 0.4), ncol = 2))
colnames(x) <- c("x", "y")


(cl <- kmeans(x, 2))
plot(x, col = cl$cluster)



##This part is unrelated, I'm just using the same data to fit a tree
#install.packages("rpart")
#install.packages("rpart.plot")


library(rpart)
## train an over-fitted classification tree (method = class)
tree <- rpart(cl$cluster ~ x[,1] + x[,2] ,
              control = rpart.control(cp = 0.0, minsplit = 30),
              method="class")

## plot the tree
require(rpart.plot)
prp(tree, extra=1, type=1,
      box.col=c('pink', 'palegreen3')[tree$frame$yval])

# We need to prune the tree to make it cost-complexity optimal
## we first look at the cost-complexity plot;
plotcp(tree)

#The full tree has the lowest cross-validation error and therefore defines the limit error (dashed line),
#which will help us to select the "optimal" tree using the one-standard-error rule. 
#The smallest model (look at the size of the tree)
#whose error is below the dashed line has size 2 (corresponding to 1 split). To get the corresponding cp
#value, we look at the cost-complexity table and fint the row that corresponds to nsplit=1
tree$cptable

##row 2

##now we prune
cp.opt <- tree$cptable[2, "CP"]
tree.pruned <- prune.rpart(tree, cp = cp.opt,method="class")
#plot of the pruned tree
prp(tree.pruned, extra=1, type=1,
      main="cp-optimal CART-tree",
      box.col= c('pink', 'palegreen3',
          'lightsteelblue 2','lightgoldenrod 1')[tree.pruned$frame$yval])

