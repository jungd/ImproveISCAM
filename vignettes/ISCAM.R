## ---- label = "SETUP", echo = FALSE, results= 'hide', message = FALSE, warning = FALSE----
library(knitr)
knitr::opts_chunk$set(comment = NA, fig.align = 'center', fig.height = 5, fig.width = 5, prompt = FALSE, 
highlight = TRUE, tidy = FALSE, warning = FALSE, message = FALSE, 
tidy.opts=list(blank = TRUE, width.cutoff = 80))

## ------------------------------------------------------------------------
library(ImproveISCAM)
binomprob(3,20,1/3,lower.tail = FALSE)
binomprob(3,20,1/3,lower.tail = TRUE)

## ------------------------------------------------------------------------
binomtest(3, 13, 1/2, "greater")

## ---- fig.width= 7, fig.height= 7----------------------------------------
binomtest(3, 13, conf.level=c(90,95))


## ------------------------------------------------------------------------
normprob(2, direction="above")

## ------------------------------------------------------------------------
InNorm(0.88, direction="above")
InNorm(0.88, direction="between")

## ---- fig.width= 5, fig.height=7-----------------------------------------
propztest(2.67, 18, 1/3, "greater")


## ---- fig.width=7, fig.height=7------------------------------------------
propztest(3, 13, conf.level=c(90,95))

