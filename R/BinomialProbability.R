#' Binomial Proability.
#'
#' @param k is the number of successes of interest (must be integer).
#' @param n the number of trials.
#' @param prob probabilty of success
#' @param lower.tail is a Boolean for finding the probability above (FALSE) or below (TRUE) the inputted value (inclusive)
#'
#' @return Right or left tail Probabilty of random variable X
#' @export
#'
#' @examples
#' binomprob(3,20,1/3,lower.tail = FALSE)
#' binomprob(3,20,1/3,lower.tail = TRUE)
binomprob <- function(k, n, prob, lower.tail){
  Description = "binomprob(k, n, prob, lower.tail) \n This function calculates tail probabilities from the binomial distribution.\r
  k is the number of successes of interest (must be integer), n and prob are the number of trials and success probability \n lower.tail is a Boolean for finding the probability above (FALSE) or below (TRUE) the inputted value (inclusive)"

  if(as.character(k)=="?") stop(Description)

  thisx = 0:n
  minx=max(0, n*prob-4*sqrt(prob*(1-prob)*n))
  maxx=min(n, n*prob+4*sqrt(prob*(1-prob)*n))
  myy=dbinom(floor(n*prob), n, prob)/2
  plot(thisx, dbinom(thisx, size=n, prob), xlab="Number of Successes", ylab="Probability",  type="h", xlim=c(minx, maxx))
  abline(h=0, col="gray")

  if (lower.tail) {
    this.prob=pbinom(k, n, prob)
    showprob=format(this.prob, digits=4)
    lines(0:k, dbinom(0:k, size=n, prob), col="red", type="h")
    text((minx+n*prob)/2, myy, labels=paste("P(X<=", k, ")\n =",showprob),pos=3, col="red")
    cat("Probability", k, "and below =", this.prob, "\n")
  }
  if (!lower.tail){
    this.prob=1-pbinom(k-1, n, prob)
    showprob=format(this.prob, digits=4)
    lines(k:n, dbinom(k:n, size=n, prob), col="red", type="h")
    text((maxx+n*prob)/2, myy, labels=paste("P(X>=",  k, ")\n =", showprob),pos=3, col="red")
    cat("Probability", k, "and above =", this.prob, "\n")
  }
  newtitle=substitute(paste("Binomial (", n==x1,", ", pi==x2, ")", ), list(x1=n, x2=prob));   title(newtitle)
  return(this.prob)
}
