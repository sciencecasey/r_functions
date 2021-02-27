## Problem 2
## Part (a)
start_time <- Sys.time()

iris.class <- unique(iris.data[,5])
n.class <- length(iris.class)
n.feature <- 4
n <- 100 # number of generated observations

generate.data <- function(i){
  mean.class <- apply(iris.data[iris.data$class == iris.class[i],1:n.feature], 2, mean)
  min.class <- apply(iris.data[iris.data$class == iris.class[i],1:n.feature], 2, min)
  max.class <- apply(iris.data[iris.data$class == iris.class[i],1:n.feature], 2, max)
  
  rndNum <- matrix(runif(n*n.feature), nrow = n.feature, ncol = n)
  
  iris.sim <- t(rndNum) %*% cov(iris.data[iris.data$class == iris.class[i],1:n.feature])
  max.iris.sim <- apply(iris.sim, 2, max)
  min.iris.sim <- apply(iris.sim, 2, min)
  
  iris.rdm <- t((t(iris.sim) - min.iris.sim)/(max.iris.sim - min.iris.sim)*(max.class - min.class) + min.class)
  mean.iris.rdm <- apply(iris.rdm, 2, mean)
  iris.rdm <- iris.rdm + mean.class - mean.iris.rdm
  
  return(iris.rdm)
}

## Generate synthetic data for all classes
for (i in 1:n.class){
  if(i == 1)
    iris.simulated <- cbind(as.data.frame(generate.data(i)), class = iris.class[i])
  
  else iris.simulated <- rbind(iris.simulated, cbind(as.data.frame(generate.data(i)), class = iris.class[i]))
}
end_time <- Sys.time()

write.csv(iris.simulated, "Iris Synthetic Data.csv")

## Part (b)
print(end_time - start_time)

## Part (d)
## Plots
for (i in 1:3) {
  par(mfrow = c(1, 1), mar = c(4, 3.8, 2, 0.2), oma = c(1, 0, 0, 0))
  data <- iris.data[iris.data$class == iris.class[i], ]
  data.sim <- iris.simulated[iris.simulated$class == iris.class[i], ]
  data.comb <-
    rbind(cbind(data, source = "Real"),
          cbind(data.sim, source = "Simulated"))
  
  plot(
    data[, 1],
    data[, 4],
    col = "red",
    main = iris.class[i],
    pch = 1,
    xlab = "Sepal length",
    ylab = "Petal width",
    xlim = c(min(data[,1]) - 0.2, max(data[,1]) + 0.2),
    ylim = c(min(data[,4]) - 0.2, max(data[,4]) + 0.2), 
    cex.axis = 0.5, 
    cex.lab = 0.7
  )
  
  minor.tick(nx = 2, ny = 2, tick.ratio = 0.5)
  points(data.sim[, 1], data.sim[, 4], col = "blue", pch = 20)
  legend("bottomright", c("Original", "Synthetic"), pch=c(1,20), col = c("red", "blue"), 
         inset=c(0,1), xpd=TRUE, horiz=TRUE, bty="n", cex = 0.7)
  
  plot(
    data[, 1],
    data[, 4],
    col = "red",
    main = iris.class[i],
    pch = 1,
    xlab = "Sepal length",
    ylab = "Petal width",
    xlim = c(min(data[,1]) - 0.2, max(data[,1]) + 0.2),
    ylim = c(min(data[,4]) - 0.2, max(data[,4]) + 0.2),
    cex.axis = 0.5, 
    cex.lab = 0.7
  )
  minor.tick(nx = 2, ny = 2, tick.ratio = 0.5)
  points(data.sim[, 1], data.sim[, 4], col = "blue", pch = 20)
  with(
    data.comb,
    dataEllipse(
      sepallength,
      petalwidth,
      as.factor(source),
      group.labels = "",
      levels = 0.2 * (1:4.5),
      plot.points = FALSE,
      lty = 1,
      fill = FALSE,
      fill.alpha = 0.1,
      lwd = 1,
      col = c("gray", "gray"),
      center.pch = FALSE,
      robust = TRUE
    )
  )
  legend("bottomright", c("Original", "Synthetic"), pch=c(1,20), col = c("red", "blue"), 
         inset=c(0,1), xpd=TRUE, horiz=TRUE, bty="n", cex = 0.7)
}