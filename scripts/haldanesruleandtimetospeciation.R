#Haldane's Rule and Time to Speciation
#Haldane's Rule first
#read in data
dat <- read.csv("../data/data.csv")

#keep only male and both observations, get rid of "sterile" observations
dat <- dat[dat$ri %in% c("male", "both"),]

dat$rinum[dat$ri =="male"] <- 1
dat$rinum[dat$ri =="both"] <- 0

#make the model

fit <- glm(as.factor(rinum)~divergence, family=binomial, data = dat)

summary(fit)

newdat <- data.frame(divergence=seq(0, max(dat$divergence), length.out=1000))

# then we use the predict function to generate the expected values in
# the response variable.

newdat$result <- predict(fit, newdata = newdat, type="response")

#plot time
#set up parameters for multipaneled figure
parameter <- par(mfrow=c(1,2))
#note - I am limiting the x axis to 20 years - this gets rid of the Phasianus colchicus x Gallus gallus fertile male
#observation
parameter <- plot(dat$rinum ~ dat$divergence, pch=16,
                  xlim = c(xmin = 0, xmax = 20),
                  xlab = "Divergence (MYA)",
                  ylab = "Reproductive Isolation") + 
  lines(y=newdat$result, x=newdat$divergence) +
  segments(x0 = newdat$divergence[min(which(newdat$result > 0.5))],
           y0 = 0, x1 = newdat$divergence[min(which(newdat$result > 0.5))],
           y1 = 3.5, col = "dodgerblue", lwd = 2)

#I then export this plot as pdf and edit axes in ppt

#this is the age that Haldane's Rule is 50% effective?? 
newdat$divergence[min(which(newdat$result > 0.5))]

#Now do time to speciation
#read in data
dat <- read.csv("../data/data.csv")

#recode male and both as 0's
dat$rinum[dat$ri =="male"] <- 1
dat$rinum[dat$ri == "both"] <- 1
dat$rinum[dat$ri =="sterile"] <- 0

#make the model

fit <- glm(as.factor(rinum)~divergence, family=binomial, data = dat)

summary(fit)

newdat <- data.frame(divergence=seq(0, max(dat$divergence), length.out=1000))

# then we use the predict function to generate the expected values in
# the response variable.

newdat$result <- predict(fit, newdata = newdat, type="response")

#this is the age that speciation is 50% effective?? 
newdat$divergence[min(which(newdat$result < 0.5))]

#note - I am limiting the x axis to 20 years - this gets rid of the Phasianus colchicus x Gallus gallus fertile male
#observation
parameter <- plot(dat$rinum ~ dat$divergence, pch=16,
                  xlim = c(xmin = 0, xmax = 55),
                  xlab = "Divergence (MYA)",
                  ylab = "Reproductive Isolation") + 
  lines(y=newdat$result, x=newdat$divergence) +
  segments(x0 = newdat$divergence[min(which(newdat$result < 0.5))],
           y0 = 0, x1 = newdat$divergence[min(which(newdat$result < 0.5))],
           y1 = 3.5, col = "darkorange", lwd = 2)


