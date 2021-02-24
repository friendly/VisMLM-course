Library(networktree)

set.seed(1)
d <- data.frame(income = 1:200, group = runif(200, -1, 1))
d <- cbind(d, rbind(
  mvtnorm::rmvnorm(100, mean = c(0, 0, 0),
          sigma = matrix(c(1, 0.5, 0.5, 0.5, 1, 0.5, 0.5, 0.5, 1), ncol = 3)),
  mvtnorm::rmvnorm(100, mean = c(0, 0, 0),
          sigma = matrix(c(1, -.1, 0.5, -.1, 1, 0.5, 0.5, 0.5, 1), ncol = 3))
))
colnames(d)[3:5] <- paste0("y", 1:3)

## Formula interface
tree2 <- networktree(y1 + y2 + y3 ~ income + group, data=d)

## plot 
plot(tree2, label.cex=2)

plot(tree2, terminal_panel = "box")
plot(tree2, terminal_panel = "matrix")

## Conditional version
tree3 <- networktree(y1 + y2 + y3 ~ income + group, data=d, 
                     method="ctree")
plot(tree3)

## Change control arguments
tree4 <- networktree(nodevars=d[,3:5], splitvars=d[,1:2],
                     alpha=0.01)
