# umap plotting function adapted from https://cran.r-project.org/web/packages/umap/vignettes/umap.html

# I save here the get_palette function which is necessary to set the number of fill according to the number of levels in metadata.
# Function to generate a discrete color palette
get_palette <- function(n) {
  # Choose a suitable palette name and adjust the number of colors if necessary
  if (n <= 3) {
    return(brewer.pal(n, "Set1"))
  } else if (n <= 5) {
    return(brewer.pal(n, "Set2"))
  } else {
    return(brewer.pal(n, "Set3"))
  }
}

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
  
  # determine whether the fill variable is discrete or continuous
  is_fill_discrete <- is.factor(metadata[[fill]]) || is.character(metadata[[fill]])
  # index in increasing order for continuous otherwise index according to the order in which the levels appear 
  levels_fill <- if (is.numeric(metadata[[fill]])) {
    sort(unique(metadata[[fill]]))
  } else {
    unique(metadata[[fill]])
  }
  # if discrete use a discrete palette otherwise if continuous use a continuous palette
  if (is_fill_discrete) {
    fills <- get_palette(length(levels_fill)) #discrete palette
  } else {
    fills <- colorRampPalette(brewer.pal(9, "Blues"))(length(levels_fill)) #continuous palette
  }
  fill_factor <- factor(metadata[[fill]], levels = levels_fill)
  fill_as_int <- as.integer(fill_factor)
  point_fill_color <- fills[fill_as_int]
  
  # if shape is set use shape compatible with a fill (max 5 levels)
  if (!is.null(shape)) {
    shape_levels <- unique(metadata[[shape]])
    if(length(shape_levels) > 5) {
      stop("Shape variable has more than 5 levels. Consider another approach.")
    }
    shape_pch <- 20 + (1:length(shape_levels))
    pch_vector <- sapply(metadata[[shape]], function(x) shape_pch[which(shape_levels == x)])
  } else {
    pch_vector <- rep(21, length(layout[,1]))
  }
  
  # set datapoints
  points(layout[,1], layout[,2], col=NA,
         pch=pch_vector, bg=point_fill_color, cex=cex)
  
  # add the legend for fill
  if (!add) {
    legend.pos <- "topleft"
    if (is_fill_discrete) {
      labels.u <- levels(fill_factor)
      fills.u <- fills
    } else {
      labels.u <- levels(fill_factor)
      fills.u <- fills[order(as.numeric(labels.u))]
      labels.u <- labels.u[order(as.numeric(labels.u))]
    }
    legend.text <- as.character(labels.u)
    if (add) {
      legend.pos <- "bottomleft"
      legend.text <- paste(as.character(labels.u), legend.suffix)
    }
    legend(legend.pos, legend=legend.text, inset=0.03,
           col=NA,
           pch=21, cex=cex.legend, pt.bg=fills.u, bty="n")
  }
  
  # add the legend for shape
  if (!is.null(shape)) {
    shapes <- unique(metadata[[shape]])
    legend("left", legend = as.character(shapes), pch = shape_pch,
           bty = "n", pt.cex = cex, cex = cex.legend)
  }
  # add main title
  mtext(side=3, main, cex=cex.main)
}
