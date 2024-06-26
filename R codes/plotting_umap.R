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

#' @x UMAP coordinates from umap(...)$layout 
#' @metadata Dataframe containing the entry for fill and shape
#' @fill Vector used to map the colour fill (continuous or categorical)
#' @shape Vector used to map the shape (categorical with max 5 levels)
#' @maintTitle TRUE or FALSE whether or not the plot should have a main title
#' @main Character entry for the plot main title (if mainTitle is TRUE)
#' @pad Padding added to the axes range beyond the data range (small numeric e.g. 0.1)
#' @cex.point Size of the datapoints in cex unit
#' @add Determines whether to create a new UMAP plot ('add = FALSE') or add data points to an existing plot ('add = TRUE').
#' @legend.suffix
#' @cex.main Size of main title in cex unit (if mainTitle is TRUE)
#' @cex.legend Size of legend key in cex unit
#' @cex.axisTitle Size of axes titles in cex unit
#' @ cex.axisText Size of axes text in cex unit
#' @ legendfill.pos NULL or "left" "right" etc. to place legend fill key 
#' @ legendshape.pos NULL or "left" "right" etc. to place legend shape key 
#' @ box TRUE or FALSE whether a full 4-edges box should appear around the plot
#' @ color_order Character vector used to order the levels of a categorical predictor (ordering should also be applied to the predictor in the metadata itself)
plot.umap = function(x, metadata, fill, shape = NULL,
                     mainTitle=TRUE,
                     main="A UMAP visualization of the dataset",
                     pad=0.1, cex.point=0.6, add=FALSE, legend.suffix="",
                     cex.main=1, cex.legend=0.85, cex.axisTitle=1, cex.axisText=1,
                     legendfill.pos = "topleft", legendshape.pos="left", box = TRUE, color_order = color_order) {
  layout <- x
  if (is(x, "umap")) {
    layout <- x$layout
  }
  xylim <- range(layout)
  xylim <- xylim + ((xylim[2]-xylim[1])*pad)*c(-0.5, 0.5)
  if (!add) {
    par(mar=c(5, 5, 4, 2) + 0.1)  # Adjust the margins for the axes
    plot(xylim, xylim, type="n", frame=F, xlab="UMAP1", ylab="UMAP2",cex.lab=cex.axisTitle, cex.axis=cex.axisText) 
    if (mainTitle) {
      title(main="Your Main Title") #Add main title if main=TRUE
    }
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
  
  fill_factor <- factor(metadata[[fill]], levels = color_order) #reoder the factor according to color_order
  
  # if discrete use a discrete palette otherwise if continuous use a continuous palette
  if (is_fill_discrete) {
    palette <- get_palette(length(levels_fill))
    colors <- setNames(palette, levels(fill_factor))
    colors <- colors[color_order]
  } else {
    colors <- colorRampPalette(brewer.pal(9, "Blues"))(length(levels_fill)) #continuous palette
  }
  fill_as_int <- as.integer(fill_factor)
  point_fill_color <- colors[fill_as_int]
  
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
         pch=pch_vector, bg=point_fill_color, cex=cex.point)
  
  # add the legend for fill
  if (!add && !is.null(legendfill.pos)) {
    labels.u <- levels(fill_factor)
    fills.u <- colors # here we use the colors variable which already sorted by color_order
    legend.text <- as.character(labels.u)
    if (add) {
      legend.pos <- "bottomleft"
      legend.text <- paste(as.character(labels.u), legend.suffix)
    }
    legend(legendfill.pos, legend=legend.text, inset=0.03,
           col=NA, pt.cex = cex.point,
           pch=21, cex=cex.legend, pt.bg=fills.u, bty="n")
  }
  
  # add the legend for shape
  if (!is.null(shape) && !is.null(legendshape.pos)){
    shapes <- unique(metadata[[shape]])
    legend(legendshape.pos, legend = as.character(shapes), pch = shape_pch,
           bty = "n", pt.cex = cex.point, cex = cex.legend)
  }
  # add main title
  if(mainTitle){
  mtext(side=3, main, cex=cex.main)
  }
}
