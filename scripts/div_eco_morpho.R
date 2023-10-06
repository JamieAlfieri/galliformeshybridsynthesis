library(ggplot2)
library(dplyr)
library(reshape)
library(broom)
library(car)


#read in data
dat <- read.csv("../data/data.csv")
#convert ri to ordinal variable
dat$rinum <- NA
dat$rinum[dat$ri =="sterile"] <- 3
dat$rinum[dat$ri =="male"] <- 2
dat$rinum[dat$ri =="both"] <- 1

fit <- glm(rinum ~ divergence + clutchsize +habitat + morpho_pc1 + morpho_pc2, data = dat)
summary(fit)
step(fit)

bestmodel <- glm(rinum ~ divergence + clutchsize, data = dat)
summary(bestmodel)
#calculate McFadden's R-squared for model
with(summary(bestmodel), 1 - deviance/null.deviance)



####
#Make function that modifies avPlots to allow vectorization
#of xlab
avPlots2 <- function(model, terms=~., intercept=FALSE, layout=NULL, ask, 
                     main, xlab, ...){
  terms <- if(is.character(terms)) paste("~",terms) else terms
  vform <- update(formula(model),terms)
  if(any(is.na(match(all.vars(vform), all.vars(formula(model))))))
    stop("Only predictors in the formula can be plotted.")
  terms.model <- attr(attr(model.frame(model), "terms"), "term.labels")
  terms.vform <- attr(terms(vform), "term.labels")
  terms.used <- match(terms.vform, terms.model)
  mm <- model.matrix(model) 
  model.names <- attributes(mm)$dimnames[[2]]
  model.assign <- attributes(mm)$assign
  good <- model.names[!is.na(match(model.assign, terms.used))]
  if (intercept) good <- c("(Intercept)", good)
  nt <- length(good)
  if (nt == 0) stop("No plots specified")
  if (missing(main)) main <- if (nt == 1) paste("Added-Variable Plot:", good) else "Added-Variable Plots"
  if (nt == 0) stop("No plots specified")
  if (nt > 1 & (is.null(layout) || is.numeric(layout))) {
    if(is.null(layout)){
      layout <- switch(min(nt, 9), c(1, 1), c(1, 2), c(2, 2), c(2, 2), 
                       c(3, 2), c(3, 2), c(3, 3), c(3, 3), c(3, 3))
    }
    ask <- if(missing(ask) || is.null(ask)) prod(layout)<nt else ask
    op <- par(mfrow=layout, ask=ask, no.readonly=TRUE, 
              oma=c(0, 0, 1.5, 0), mar=c(5, 4, 1, 2) + .1)
    on.exit(par(op))
  }
  if (missing(xlab)) xlab <- paste(good, "| others")
  if (length(xlab) == 1L) xlab <- rep(xlab, length(good))
  if (length(xlab) > length(good))
    warning("'xlab' not length 1 or the number of model names, truncating")
  res <- as.list(NULL)
  for (i in seq_along(good)) {
    term <- good[[i]]
    res[[term]] <- avPlot(model, term, main="", xlab=xlab[[i]], ...)
  }
  mtext(side=3,outer=TRUE,main, cex=1.2)
  invisible(res)
}




#make the added variable plot - this plot shows the partial correlation
#between each independent variable and the dependent variable, while
#controlling for the other independent variables

avPlots2(bestmodel, id = FALSE, main = "")


#I then export this plot as a pdf and edit axes in ppt
