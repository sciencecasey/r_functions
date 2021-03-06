# Ellipse code from Dr. Rodriguez, through Annie Stevenson
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
