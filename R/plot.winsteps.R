plot.winsteps <- function(items, persons, roundval = 1,
    scaley = 1/3, itemtype = "p", labels, main = "Item Map",
    lwd = 1, lcol = "darkgray", addlegend = TRUE,
    main = "Item and Person Distributions", ...) {

    # Plot an IRT item-person distribution
    # persons: numeric vector of person locations
    # items: numeric vector of item locations
    # roundval: integer vector, length 2, for rounding 1 person
    #   and 2 item locationsl, used to create bins for plotting
    # scaley: numeric, gives some control over how the y-axis is
    #   scaled in relation to the x
    # itemtype: string, for plotting items as points, "p", or lines
    #   "l"
    # labels: vector of item labels, to be plotted as text

    ni <- length(items)

    i <- round(items, roundvals[2])
    p <- round(persons, roundvals[1])
    xlim <- c(min(p, i), max(p, i))

    iscale <- seq(xlim[1], xlim[2], by = 10^-roundval)
    pscale <- seq(xlim[1], xlim[2], by = 10^-roundval)
    itab <- table(factor(i, levels = iscale))
    ptab <- table(factor(p, levels = pscale))

    itop <- max(itab)
    ptop <- max(ptab)

    layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE),
        c(4, 4), c(1, 7))

    par(mar = c(1, 0, 0, 0))
    plot(c(0, 1), c(0, 1), type = "n", xaxt = "n", yaxt = "n",
        bty = "n")
    text(.5, .5, "Item and Person Distributions", cex = 2)

    par(mar = c(1, 3, 2, 1))
    #plot(ptab, xlim = xlim, xlab = "Theta")
    barplot(-itab, axes = FALSE, xlim = c(-itop, 0), space = 0,
        axisnames = FALSE, horiz = TRUE)
    axis(3, labels = -pretty(c(0, -itop)), at = pretty(c(0, -itop)))

    par(mar = c(1, 1, 2, 3))
    #plot(ptab, xlim = xlim, xlab = "Theta")
    barplot(ptab, axes = FALSE, ylim = c(0, ptop), space = 0,
        axisnames = FALSE, horiz = TRUE)
    axis(3, labels = pretty(c(0, ptop)), at = pretty(c(0, ptop)))

    tscale <- barplot(ptab, space = 0, plot = FALSE)
    axis(2, labels = pscale, at = tscale, las = 1)

    if(!missing(labels)) {
        yvals <- vector(length = ni)
        for(i in 1:nrow(b))
            yvals[items == b[i, 1]] = 1:b[i, 2]
            text(items, yvals * iscale * scaley, labels = labels, ...)
        }
    else
        points(b[, 1], b[, 2] * iscale * scaley, type = itemtype, ...)

    axis(4, at = seq(0, ymax, by = iscale * scaley),
        labels = 0:(max(b[, 2])/scaley))

    mtext("Item Count", 4, 3, at = ymax/2, cex = 1.3)

    if(addlegend)
        legend("topright", legend = c("People", "Items"), pch = c(NA, 21),
            lty = c(1, NA), col = c(lcol, "black"), bty = "n", ...)
}
