## Copyright (C) 2009 Reinhard Seifert,
## biostatistician at Haukeland University Hospital Bergen, Norway.
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## These functions are distributed in the hope that they will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## The text of the GNU General Public License, version 2, is available
## as http://www.gnu.org/copyleft or by writing to the Free Software
## Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

## Improvements made 2012 by Max Gordon,
## orthopaedic surgeon and PhD-student at the Karolinska Institute
##
## The changes consist of adaptation for use with the rms package,
## multiple models plotting and some code optimization


#' Plot a spline in a Cox regression model
#'
#' This function is a more specialized version of the \code{\link{termplot}()} function. It
#' creates a plot with the spline against hazard ratio. The plot can additianally have
#' indicator of variable density and have multiple lines.
#'
#' @section Multiple models in one plot:
#'
#' The function allows for plotting multiple splines in one graph. Sometimes you
#' might want to show more than one spline for the same variable. This allows
#' you to create that comparison.
#'
#' Examples of a situation where I've used multiple splines in one plot is when
#' I want to look at a variables behavior in different time periods. This is another
#' way of looking at the proportional hazards assumption. The Schoenfeld residuals
#' can be a little tricky to look at when you have the splines.
#'
#' Another example of when I've used this is when I've wanted to plot adjusted and
#' unadjusted splines. This can very nicely demonstrate which of the variable span is
#' mostly confounded. For instance - younger persons may exhibit a higher risk for a
#' procedure but when you put in your covariates you find that the increased hazard
#' changes back to the basic
#'
#' @param models A single model or a list() with several models
#' @param term The term of interest. Can be either the name or the number of the
#'  covariate in the model.
#' @param se Boolean if you want the confidence intervals or not
#' @param cntrst By contrasting values you can have the median as a reference
#'  point making it easier to compare hazard ratios.
#' @param polygon_ci If you want a polygon as indicator for your confidence interval.
#'  This can also be in the form of a vector if you have several models. Sometimes
#'  you only want one model to have a polygon and the rest to be dotted lines. This
#'  gives the reader an indication of which model is important.
#' @param rug The rug is the density of the population along the spline variable. Often
#'  this is displayed as a jitter with bars that are thicker & more common when there
#'  are more observations in that area or a smooth density plot that looks like a
#'  mountain. Use "density" for the mountain view and "ticks" for the jitter format.
#' @param xlab The label of the x-axis
#' @param ylab The label of the y-axis
#' @param main The main title of the plot
#' @param xlim A vector with 2 elements containing the upper & the lower bound of the x-axis
#' @param ylim A vector with 2 elements containing the upper & the lower bound of the y-axis
#' @param col.term The color of the estimate line. If multiple lines you can have
#'  different colors by giving a vector.
#' @param col.se The color of the confidence interval. If multiple lines you can have
#'  different colors by giving a vector.
#' @param col.dens The color of the density plot. Ignored if you're using jitter
#' @param lwd.term The width of the estimated line. If you have more than one model then
#'  provide the function with a vector if you want to have different lines for
#'  different width for each model.
#' @param lty.term The typeof the estimated line, see lty. If you have more than one model
#'  then provide the function with a vector if you want to have different line types for
#'  for each model.
#' @param lwd.se The line width of your confidence interval. This is ignored if you're using
#'  polygons for all the confidence intervals.
#' @param lty.se The line type of your confidence interval.  This is ignored if you're using
#'  polygons for all the confidence intervals.
#' @param x.ticks The ticks for the x-axis if you desire other than the default.
#' @param y.ticks The ticks for the y-axis if you desire other than the default.
#' @param ylog Show a logarithmic y-axis. Not having a logarithmic axis might seem easier
#'  to understand but it's actually not really a good idea. The distance between HR 0.5 and
#'  2.0 should be the same. This will only show on a logarithmic scale and therefore it is
#'  strongly recommended to use the logarithmic scale.
#' @param cex Increase if you want larger font size in the graph.
#' @param plot.bty Type of box that you want. See the bty description in
#'  graphical parameters (par). If bty is one of "o" (the default),
#'  "l", "7", "c", "u", or "]" the resulting box resembles the corresponding
#'  upper case letter. A value of "n" suppresses the box.
#' @param y_axis_side The side that the y axis is to be plotted, see axis() for details
#' @param axes A boolean that is used to identify if axes are to be plotted
#' @param alpha The alpha level for the confidence intervals
#' @param ... Any additional values that are to be sent to the plot() function
#' @return The function does not return anything
#'
#' @example inst/examples/plotHR_example.R
#'
#' @importFrom Gmisc fastDoCall
#' @importFrom grDevices grey
#' @importFrom graphics axTicks axis box lines par plot polygon
#' @importFrom stats quantile
#' @author Reinhard Seifert, Max Gordon
#'
#' @export
#' @rdname plotHR
plotHR <- function(models,
                   term = 1,
                   se = TRUE,
                   cntrst = ifelse(inherits(models, "rms") ||
                     inherits(models[[1]], "rms"), TRUE, FALSE),
                   polygon_ci = TRUE,
                   rug = "density",
                   xlab = "",
                   ylab = "Hazard Ratio",
                   main = NULL,
                   xlim = NULL,
                   ylim = NULL,
                   col.term = "#08519C",
                   col.se = "#DEEBF7",
                   col.dens = grey(.9),
                   lwd.term = 3,
                   lty.term = 1,
                   lwd.se = lwd.term,
                   lty.se = lty.term,
                   x.ticks = NULL,
                   y.ticks = NULL,
                   ylog = TRUE,
                   cex = 1,
                   y_axis_side = 2,
                   plot.bty = "n",
                   axes = TRUE,
                   alpha = .05,
                   ...) {

  # If the user wants to compare different models the same graph
  # the first dataset is then choosen as the default dataset
  # for getting the rug data.
  if (length(class(models)) != 1 || !inherits(models, "list")) {
    models <- list(models)
  }

  # Create vectors of the colors, line types etc to
  # allow for specific settings for each model
  confint_style <- lapply(1:length(models),
                          function(i) {
                            ret <- expand.grid(c("col", "lty", "lwd"), c("term", "se")) |>
                              apply(FUN = paste, MARGIN = 1, collapse = ".") |>
                              c("polygon_ci") |>
                              sapply(function(x) {
                                var <- get(x)
                                if (length(var) == 1) return(var)
                                if (length(var) == length(models)) return(var[[i]])
                                stop("Invalid length of ", x, ": ", length(var), " - should be 1 or 0")
                              }, simplify = FALSE)
                          })

  # set plotting parameters
  par(las = 1, cex = cex)

  # Get the term number and it's label
  all.labels <- prGetModelVariables(models[[1]], remove_splines = FALSE)

  # Allow the term searched for be a string
  if (is.character(term)) {
    term <- grep(term, all.labels)
    if (length(term) != 1) {
      stop(
        "Could not find one match for term: '", term, "'",
        " among the terms '", paste(all.labels, collapse = "', '"), "'"
      )
    }
  }

  # pick the name of the main term which is going to be plotted
  term.label <- all.labels[term]

  if (length(ylim) == 2 &&
    is.vector(ylim)) {
    if (ylog == TRUE) {
      ylim <- log(ylim)
    }
  } else if (!is.null(ylim)) {
    warning(
      "You have provided an invalid ylim argument",
      " that doesn't consist of two elements",
      " in a vector - hence it will be ignored"
    )
    ylim <- NULL
  }

  boundaries <- list()
  boundaries$y <- NULL
  if (length(ylim) == 2) {
    boundaries$y <- c(min(ylim), max(ylim))
  }

  # Just add the boundary values
  getYBoundaries <- function(ylim, current_limits, variable) {
    # Infinite values don't count
    variable <- variable[!is.infinite(variable)]
    if (length(ylim) == 2) {
      return(c(
        max(min(ylim), min(current_limits, variable)),
        min(max(ylim), max(current_limits, variable))
      ))
    } else if (!is.null(current_limits)) {
      return(c(
        min(current_limits, variable),
        max(current_limits, variable)
      ))
    }

    return(c(
      min(variable),
      max(variable)
    ))
  }

  xvalues <- NULL
  multi_data <- list()
  for (m in models) {
    est_list <- list(
      model = m,
      ylog = ylog,
      cntrst = cntrst,
      xlim = xlim,
      alpha = alpha,
      term.label = term.label
    )

    # Re-use the same data for each model as they are assumed to be nested
    if (length(multi_data) > 0) {
      est_list$new_data <- attr(multi_data[[1]], "new_data")
    }

    line_data <- do.call(prPhEstimate, est_list)
    if (length(multi_data) == 0) {
      xvalues <- line_data$xvalues
    }

    multi_data <- append(multi_data, list(line_data))

    # Update the plot boundaries to the new fit
    boundaries$y <- getYBoundaries(
      ylim = ylim,
      current_limits = boundaries$y,
      variable = line_data$estimate
    )

    # Add 10 % to make room for the ticks rug
    if (rug == "ticks") {
      boundaries$y[1] <-
        min(boundaries$y) -
        (boundaries$y[2] - boundaries$y[1]) * .1
    }
  }

  # plot empty plot with coordinate system and labels
  boundaries$x <- range(xvalues[])
  if (!tolower(rug) %in% c("density", "ticks") &&
    rug != FALSE &&
    !is.null(rug)) {
    warning("Currently the rug option only supports 'density' or 'ticks'")
  }

  # Use first model for density data
  base_data <- prGetModelData(models[[1]])
  xvalues_4_density <- base_data[, term.label]

  ## get the quartiles of the main term
  quantiles <- quantile(xvalues_4_density,
    probs = c(0.025, 0.25, 0.50, 0.75, 0.975)
  )

  # Choose within limits
  if (length(xlim) == 2) {
    xvalues_4_density <-
      xvalues_4_density[xvalues_4_density >= min(xlim) &
        xvalues_4_density <= max(xlim)]
  }

  structure(list(
    models = models,
    multi_data = multi_data,
    main = main,
    boundaries = boundaries,
    se = se,
    confint_style = confint_style,
    xlab = xlab,
    ylab = ylab,
    ylog = ylog,
    xlim = xlim,
    ylim = ylim,
    plot.bty = plot.bty,
    col.dens = col.dens,
    quantiles = quantiles,
    rug = rug,
    xvalues_4_density = xvalues_4_density,
    ticks = list(
      x = x.ticks,
      y = y.ticks,
      y_axis_side = y_axis_side
    ),
    axes = axes
  ),
  class = "plotHR"
  )
}

#' @exportS3Method
#' @rdname plotHR
#' @param x Sent the `plotHR` object to plot
print.plotHR <- function(x, ...) {
  plot(x, ...)
}

#' @exportS3Method
#' @rdname plotHR
#' @param y Ignored in plot
plot.plotHR <- function(x, y, ...) {
  if (!missing(y)) stop("Unexpected y parameter")
  plot(
    y = x$boundaries$y,
    x = x$boundaries$x,
    xlab = x$xlab,
    ylab = x$ylab,
    main = x$main,
    xaxs = "i",
    yaxs = "i",
    type = "n",
    axes = FALSE,
    ...
  )

  # plot CI as polygon shade - if 'se = TRUE' (default)
  if (x$se) {
    # Plot the last on top
    for (i in length(x$models):1) {
      prPhConfIntPlot(
        model_data = x$multi_data[[i]],
        color = x$confint_style[[i]]$col.se,
        polygon = x$confint_style[[i]]$polygon_ci,
        lwd = x$confint_style[[i]]$lwd.se,
        lty = x$confint_style[[i]]$lty.se
      )
    }
  }

  if (x$rug == "density") {
    prPhDensityPlot(x$xvalues_4_density,
      color = x$col.dens
    )
  }

  # plot white lines (background color) for:
  # 2.5%tile, 1Q, median, 3Q and 97.5%tile
  # through confidence shade and density plot
  axis(
    side = 1,
    at = x$quantiles,
    labels = FALSE,
    lwd = 0,
    col.ticks = "white",
    lwd.ticks = 1,
    tck = 1
  )

  if (x$rug == "ticks") {
    prPhRugPlot(xvalues = x$xvalues_4_density)
  }

  # Plot the last fit on top, therefore use the reverse
  for (i in length(x$models):1) {
    current_i.forw <- order(x$multi_data[[i]]$xvalues)

    # Plots the actual regression line
    lines(
      x = x$multi_data[[i]]$xvalues[current_i.forw],
      y = x$multi_data[[i]]$estimate[current_i.forw],
      col = x$confint_style[[i]]$col.term,
      lwd = x$confint_style[[i]]$lwd.term,
      lty = x$confint_style[[i]]$lty.term
    )
  }

  # plot the axes
  if (x$axes) {
    axis(side = 1, at = x$ticks$x)
    if (is.null(x$ticks$y)) {
      x$ticks$y <- axTicks(2)
    } else if (x$ylog == TRUE) {
      # This is an assumption that the ticks
      # aren't provided in log
      x$ticks$y <- log(x$ticks$y)
    }


    if (x$ylog == TRUE) {
      y.ticks_labels <- ifelse(exp(x$ticks$y) >= 1,
        sprintf("%0.1f", exp(x$ticks$y)),
        sprintf("%0.2f", exp(x$ticks$y))
      )

      # Get familiar y-axis instead of the log
      axis(
        side = x$ticks$y_axis_side,
        at = x$ticks$y,
        labels = y.ticks_labels
      )
    } else {
      axis(side = x$ticks$y_axis_side, at = x$ticks$y)
    }
  }

  # plot a box around plotting panel if specified - not plotted by default
  box(bty = x$plot.bty)
}