# umap plotting function adapted from https://cran.r-project.org/web/packages/umap/vignettes/umap.html

plot.umap = function(x, metadata, fill, shape = NULL,
                     main="A UMAP visualization of the dataset",
                     pad=0.1, cex=0.6, add=FALSE, legend.suffix="",
                     cex.main=1, cex.legend=0.85, box = TRUE) {
  layout <- x
  if (is(x, "umap")) {
    layout <- x$layout
  }
  xylim <- range(layout)
  xylim <- xylim + ((xylim[2]-xylim[1])*pad)*c(-0.5, 0.5)
  if (!add) {
    par(mar=c(5, 5, 4, 2) + 0.1)  # Adjust the margins for the axes
    plot(xylim, xylim, type="n", frame=F, xlab="UMAP1", ylab="UMAP2") 
    if (box) {
      box()  # Add a box around the plot
    }
  }
  is_fill_discrete <- is.factor(metadata[[fill]]) || is.character(metadata[[fill]])
  if (is_fill_discrete) {
    fills <- get_palette(length(unique(metadata[[fill]])))
  } else {
    fills <- colorRampPalette(c("blue", "red"))(length(unique(metadata[[fill]])))
  }
  fill_factor <- factor(metadata[[fill]], levels = unique(metadata[[fill]]))
  fill_as_int <- as.integer(fill_factor)
  point_fill_color <- fills[fill_as_int]
  if (!is.null(shape)) {
    shape_levels <- unique(metadata[[shape]])
    if(length(shape_levels) > 5) {
      stop("Shape variable has more than 5 levels. Consider another approach.")
    }
    shape_pch <- 20 + (1:length(shape_levels))
    pch_vector <- sapply(metadata[[shape]], function(x) shape_pch[which(shape_levels == x)])
  }
  else {
    pch_vector <- rep(21, length(layout[,1]))
  }
  points(layout[,1], layout[,2], col=NA,
         pch=pch_vector, bg=point_fill_color, cex=cex)
  if (!add) {
    labels.u <- unique(metadata[[fill]])
    legend.pos <- "topleft"
    legend.text <- as.character(labels.u)
    if (add) {
      legend.pos <- "bottomleft"
      legend.text <- paste(as.character(labels.u), legend.suffix)
    }
    legend(legend.pos, legend=legend.text, inset=0.03,
           col=NA,
           pch=21, cex=cex.legend, pt.bg=fills, bty="n")
  }
  if (!is.null(shape)) {
    shapes <- unique(metadata[[shape]])
    legend("left", legend = as.character(shapes), pch = shape_pch,
           bty = "n", pt.cex = cex, cex = cex.legend)
  }
  mtext(side=3, main, cex=cex.main)
}