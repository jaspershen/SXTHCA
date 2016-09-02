plotHCA <- function(obj, order.new = NULL) {

  hc <- obj
  par(mar=c(5,5,4,2))

  order1 <- hc$order
  labels <- hc$labels
  merge <- hc$merge
  height <- hc$height

  if(is.null(order.new)) {order.new <- c(1:length(order1))}
  order <- order1[order.new]

  plot(x = 0, y = 0, xlim = c(0,length(labels)), ylim = c(0, max(height)), col = "white", xlab = "", xaxt = "n",
       axes = F, ylab = 'Height', cex.lab = 1.3)

  axis(side = 2, cex.axis = 1.3, cex.lab = 1.3)
  par(xpd = T)

  text(x = c(1:length(labels)), y = -1, labels = labels[order], srt = 90, pos = 2, offset = 0.1)
  class <- matrix(NA,nrow = nrow(merge), ncol=2)
  for (i in 1:nrow(merge)) {
    sample1 <- merge[i,1]
    sample2 <- merge[i,2]
    c <- height[i]
    if (sample1 < 0) {
      x1 <- which(labels[abs(sample1)] == labels[order])
      y1 <- 0
      if (sample2 < 0) {
        x2 <- which(labels[abs(sample2)] == labels[order])
        y2 <- 0
        segments(x0 = x1, y0 = c, x1 = x2, y1 = c)
        segments(x0 = x1, y0 = y1, x1 = x1, y1 = c)
        segments(x0 = x2, y0 = y2, x1 = x2, y1 = c)
      }
      if (sample2 > 0) {
        x2 <- class[sample2,1]
        y2 <- class[sample2,2]
        segments(x0 = x1, y0 = c, x1 = x2, y1 = c)
        segments(x0 = x1, y0 = y1, x1 = x1, y1 = c)
        segments(x0 = x2, y0 = y2, x1 = x2, y1 = c)
      }
    }

    if (sample1 > 0) {
      x1 <- class[sample1,1]
      y1 <- class[sample1,2]
      if (sample2 < 0) {
        x2 <- x2 <- which(labels[abs(sample2)] == labels[order])
        y2 <- 0
        segments(x0 = x1, y0 = c, x1 = x2, y1 = c)
        segments(x0 = x1, y0 = y1, x1 = x1, y1 = c)
        segments(x0 = x2, y0 = y2, x1 = x2, y1 = c)
      }
      if (sample2 > 0) {
        x2 <- class[sample2,1]
        y2 <- class[sample2,2]
        segments(x0 = x1, y0 = c, x1 = x2, y1 = c)
        segments(x0 = x1, y0 = y1, x1 = x1, y1 = c)
        segments(x0 = x2, y0 = y2, x1 = x2, y1 = c)
      }

    }


    class[i,1] <- (x1+x2)/2

    class[i,2] <- height[i]

  }

}
