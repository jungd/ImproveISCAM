#' Normal Probability
#'
#' @param xval this function calculates tail probability for the normal distribution.
#' @param mean mean
#' @param sd sd
#' @param direction is a string for finding the probability above,below, between, or outside
#' @param label the horizontal axis
#' @param xval2 This function calculates tail probability for the normal distribution.
#'
#' @return Normal Probability
#' @export
#'
#' @examples
#'normprob(2, direction="below")
#'normprob(2, direction="above")
#'normprob(-2, direction="between",xval2=2)
#'normprob(-2, direction="outside",xval2=2)

normprob <- function (xval, mean = 0, sd = 1, direction, label = NULL, xval2 = NULL){
  Description = "normprob(xval, mean, sd, direction, label, xval2) \n This function calculates tail probability for the normal distribution.  \n Direction is a String for finding the probability above (\"above\") or below (\"below\") the inputted value  \n If \"outside\" or \"between \" are specified, a second observation needs to be given at the end. \n  It is highly recommended that you indicate a label for the horizontal axis, with the quotation marks (e.g., \"sample proportions\") "
  if (as.character(xval) == "?")
    stop(Description)
  minx = mean - 4 * sd
  maxx = mean + 4 * sd
  thisx = seq(minx, maxx, 0.001)
  xlabel = "x-variable"
  if (!is.null(label))
    xlabel = label
  plot(thisx, dnorm(thisx, mean, sd), col = 3, xlim = c(minx,
                                                        maxx), type = "l", xlab = xlabel, ylab = "density")
  abline(h = 0, col = "gray")
  if (direction == "below") {
    probseq = seq(minx, max(minx, xval), 0.001)
    normprob = pnorm(xval, mean, sd)
    showprob = format(normprob, digits = 4)
    polygon(c(probseq, max(minx, xval), 0), c(dnorm(probseq,
                                                    mean, sd), 0, 0), col = "red", border = "red")
    text(minx, dnorm(mean, mean, sd)/2, labels = paste("P(X<=",
                                                       xval, ") \n =", showprob), col = "red", pos = 4)
  }
  else if (direction == "above") {
    probseq = seq(min(xval, maxx), maxx, 0.001)
    normprob = pnorm(xval, mean, sd, lower.tail = FALSE)
    showprob = format(normprob, digits = 4)
    polygon(c(min(maxx, xval), probseq, 1), c(0, dnorm(probseq,
                                                       mean, sd), 0), col = "red", border = "red")
    text(maxx, dnorm(mean, mean, sd)/2, labels = paste("P(X>=",
                                                       xval, ") \n =", showprob), col = "red", pos = 2)
  }
  else if (direction == "between") {
    if (is.null(xval2))
      stop("You need to specify a second observation value.")
    if (xval2 < xval) {
      temp = xval
      xval = xval2
      xval2 = temp
    }
    probseq = seq(xval, xval2, 0.001)
    normprob = pnorm(xval2, mean, sd) - pnorm(xval, mean,
                                              sd)
    showprob = format(normprob, digits = 4)
    polygon(c(xval, probseq, xval2), c(0, dnorm(probseq,
                                                mean, sd), 0), col = "red", border = "red")
    text(minx, dnorm(mean, mean, sd)/2, labels = paste("P(",
                                                       xval, "<= X<=", xval2, ") \n =", showprob), col = "red",
         pos = 4)
  }
  else if (direction == "outside") {
    if (is.null(xval2))
      stop("You need to specify a second observation value.")
    if (xval2 < xval) {
      temp = xval
      xval = xval2
      xval2 = temp
    }
    probseq1 = seq(minx, xval, 0.001)
    probseq2 = seq(xval2, maxx, 0.001)
    normprob = 1 - (pnorm(xval2, mean, sd) - pnorm(xval,
                                                   mean, sd))
    showprob = format(normprob, digits = 4)
    polygon(c(minx, probseq1, xval), c(0, dnorm(probseq1,
                                                mean, sd), 0), col = "red", border = "red")
    polygon(c(xval2, probseq2, maxx), c(0, dnorm(probseq2,
                                                 mean, sd), 0), col = "red", border = "red")
    text(minx, dnorm(mean, mean, sd)/2, labels = paste("P(X <=",
                                                       xval, ") and P(X>=", xval2, ") \n =", showprob),
         col = "red", pos = 2)
  }
  else stop("Use \"above\", \"below\", \"between\", or \"outside\" as the direction.")
  newtitle = substitute(paste("Normal(", mean == x3, ",  ",
                              SD == x4, ")"), list(x3 = mean, x4 = signif(sd, 4)))
  title(newtitle)
  cat(c("probability:", showprob), "\n")
}

