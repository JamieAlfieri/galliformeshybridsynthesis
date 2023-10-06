library(RColorBrewer)
#read in data
dat <- read.csv("../data/data.csv")

#convert reproductive isolation to an ordinal value, 1-3

dat$rinum <- NA
dat$rinum[dat$ri =="sterile"] <- 1
dat$rinum[dat$ri =="male"] <- 2
dat$rinum[dat$ri =="both"] <- 3

#fit linear model
fit <- glm(dat$rinum ~ dat$divergence)
summary(fit)
#residdeviance = 36.326
#nulldeviance = 84.341
#1 - (residdeviance/Null deviance) - mcfadden's pseudor2
#this measures how much better my model is(resid)
#than just the intercept
1-(36.326/84.341)

#plot 
plot(dat$rinum ~ dat$divergence)
abline(lm(dat$rinum ~ dat$divergence))
#add a residual column to the datasframe
dat$res <- fit$residuals
#subset the chicken residuals
chkres <- c(dat$res[dat$sp1 == "Gallus gallus"],
            dat$res[dat$sp2 == "Gallus gallus"])
#take a mean of residuals
mean_chkres <- mean(chkres)

#subset the common pheasant residuals
bd1 <- c(dat$res[dat$sp1 == "Phasianus colchicus"],
            dat$res[dat$sp2 == "Phasianus colchicus"])
#take a mean of residuals
mean_bd1 <- mean(bd1)

#subset the silver pheasant residuals
bd2 <- c(dat$res[dat$sp1 == "Lophura nycthemera"],
            dat$res[dat$sp2 == "Lophura nycthemera"])
#take a mean of residuals
mean_bd2 <- mean(bd2)

#subset the kalij pheasant
bd3 <- c(dat$res[dat$sp1 == "Lophura leucomelanos"],
            dat$res[dat$sp2 == "Lophura leucomelanos"])
#take a mean of residuals
mean_bd3 <- mean(bd3)

#subset the reeves pheasant
bd4 <- c(dat$res[dat$sp1 == "Syrmaticus reevesii"],
            dat$res[dat$sp2 == "Syrmaticus reevesii"])
#take a mean of residuals
mean_bd4 <- mean(bd4)

#create a null distribution based off of residuals of 10,000 sets of 13 random species
gallus.null.dist<-c()
for(i in 1:10000){
  gallus.null.dist[i] <- mean(sample(dat$res, 13))
}
bd1.null.dist<-c()
for(i in 1:10000){
  bd1.null.dist[i] <- mean(sample(dat$res, 15))
}
bd2.null.dist<-c()
for(i in 1:10000){
  bd2.null.dist[i] <- mean(sample(dat$res, 12))
}
bd3.null.dist<-c()
for(i in 1:10000){
  bd3.null.dist[i] <- mean(sample(dat$res, 11))
}
bd4.null.dist<-c()
for(i in 1:10000){
  bd4.null.dist[i] <- mean(sample(dat$res, 11))
}





#get p-values - p-values less than 0.0125 are significant (multiple comparisons correction)
sum(gallus.null.dist > mean_chkres)/10000
sum(bd1.null.dist > mean_bd1)/10000
sum(bd2.null.dist > mean_bd2)/10000
sum(bd3.null.dist > mean_bd3)/10000
sum(bd4.null.dist > mean_bd4)/10000


plot(density(gallus.null.dist),
     xlab = "Hybridization Success Residuals",
     ylab = "Density",
     main = "",
     ylim = c(ymin = 0, ymax = 5),
     xlim = c(xmin = -0.4, xmax = 0.5),
     yaxs = "i")
lines(density(bd1.null.dist))
lines(density(bd2.null.dist))
lines(density(bd3.null.dist))
lines(density(bd4.null.dist))

#make color palette
figcol <- brewer.pal(5, "Set1")
#set line width
linewidth <- 2


#add vertical lines
segments(x0 = mean_chkres, y0 = 0, x1 = mean_chkres,
         y1 = 3.5, col = figcol[1], lwd = linewidth)
segments(x0 = mean_bd1, y0 = 0, x1 = mean_bd1,
         y1 = 3.5, col = figcol[2], lwd = linewidth)
segments(x0 = mean_bd2, y0 = 0, x1 = mean_bd2,
         y1 = 3.5, col = figcol[3], lwd = linewidth)
segments(x0 = mean_bd3, y0 = 0, x1 = mean_bd3,
         y1 = 3.5, col = figcol[4], lwd = linewidth)

#because mean_bd1 equals mean_bd4, I am jittering
#bd4 by 0.04
segments(x0 = mean_bd4 - 0.04, y0 = 0, x1 = mean_bd4 - 0.04,
         y1 = 3.5, col = figcol[5], lwd = linewidth)

legendtext <- c(expression(italic("Gallus gallus"), italic("Phasianus colchicus"),
                           italic("Lophura nycthemera"), italic("Lophura leucomelanos"),
                           italic("Syrmaticus reevesii")))

legend(0.225,3.25, legend = legendtext, fill = figcol, cex = 0.85)

#I then export this plot as pdf, and edit axes in ppt
