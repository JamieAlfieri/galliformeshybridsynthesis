#this script tests for phylogenetic signal in mean residuals and
#domestication index, and then it fits a model between these two values
library(ape)
library(phytools)
library(caper)

#####
#read in and clean data
dat <- read.csv(file = "../data/data.csv")
dom <- read.csv(file = "../data/domesticationindex.csv")
tree <- read.tree(file = "../data/Galliformes_species.nwk")
nameindex <- read.csv("../data/namechange.csv")
tree$tip.label <- nameindex[match(tree$tip.label, nameindex$original_time_tree_tip_labels),2]
tree <- drop.tip(tree, c("deletethistip1", "deletethistip2", "deletethistip3", "deletethistip4", "deletethistip5", "deletethistip6", "deletethistip7"))
tree <- keep.tip(tree, dom$speciesname)

#convert reproductive isolation to an ordinal value, 1-3
dat$rinum <- NA
dat$rinum[dat$ri =="sterile"] <- 1
dat$rinum[dat$ri =="male"] <- 2
dat$rinum[dat$ri =="both"] <- 3

#fit linear model
fit <- glm(dat$rinum ~ dat$divergence)
summary(fit)
#add a residual column to the datasframe
dat$res <- fit$residuals

#make a meanresid column in dom
dom$meanres <- NA

#Take the mean residual of each species, and put in column in dom
for (i in 1:nrow(dom)){
  dom[i,3] <-  mean(c(dat$res[dat$sp1 == dom[i,1]],
                dat$res[dat$sp2 == dom[i,1]]))
}

#define function to scale values between 0 and 1
scale_values <- function(x){(x-min(x))/(max(x)-min(x))}
dom$count <- scale_values(dom$count)
dom$meanres <- scale_values(dom$meanres)

#####
###Check for phylogenetic signal in the residuals of the model
testmod <- glm(dom$meanres ~ dom$count)
resids <- testmod$residuals
names(resids) <- dom$speciesname
phylosig(tree, resids, test=T)
#There is a significant phylogenetic signal in the residuals of the model, so we need to run a
#phylogenetic model

#####
#Run PGLS
#need to reorder dom so it in same order as phylogeny tip labels
reorder_idx <- match(tree$tip.label, dom$speciesname)
dom <- dom[reorder_idx,]
#make a comparative data object
newdom <- comparative.data(tree, dom, speciesname, vcv=TRUE, vcv.dim=3)
#make the pgls model
mod1 <- pgls(meanres ~ count, data = newdom)

summary(mod1)

plot(dom$meanres ~ dom$count)
abline(a = coef(mod1)[1], b = coef(mod1)[2], col = "darkorange1", lty = 2, lwd = 2)

#####
#remove the 10 most and 12 least domesticated species to see if effect remains
#We chose 12 because 10 values will split observations containing count value of 3
#The pattern is the same if we take out the 10 least domesticated species
#sort by decreasing domestication
out.rm.dom <- dom[order(dom$count, decreasing = TRUE), ]
#get the top and bottom 10 domesticated species names
out.sp <- out.rm.dom$speciesname[c(1:10,65:76)]
#remove top and bottom 10
out.rm.dom <- out.rm.dom[-c(1:10,65:76),]
#remove tips from tree
out.tree <- drop.tip(tree, out.sp)

#reorder out.rm.dom to match order of the tree
reorder_idx_out <- match(out.tree$tip.label, out.rm.dom$speciesname)
out.rm.dom <- out.rm.dom[reorder_idx_out,]

new.out.dom <- comparative.data(out.tree, out.rm.dom, speciesname, vcv = TRUE, vcv.dim=3)

mod2 <- pgls(meanres ~ count, data = new.out.dom)
summary(mod2)
abline(a = coef(mod2)[1], b = coef(mod2)[2], col = "dodgerblue", lty = 2, lwd = 2)
###export plot and change axes labels in ppt


