library(ape)
library(devtools)
#devtools::install_github("YuLab-SMU/ggtree")
library(ggtree)
library(ggplot2)
tree <- read.tree(file = "../data/Galliformes_species.nwk")
dat <- read.csv(file = "../data/data.csv")
nameindex <- read.csv("../data/namechange.csv")


dat$fertility[dat$ri == "both"] <- "fertile"
dat$fertility[dat$ri == "male"] <- "fertile"
dat$fertility[dat$ri == "sterile"] <- "sterile"





tree$tip.label <- nameindex[match(tree$tip.label, nameindex$original_time_tree_tip_labels),2]


tree <- drop.tip(tree, c("deletethistip1", "deletethistip2", "deletethistip3", "deletethistip4", "deletethistip5", "deletethistip6", "deletethistip7"))


cols<-c("darkorange1","dodgerblue")[as.factor(dat$fertility)]
pl1 <- ggtree(tree, branch.length = "none") +
  layout_inward_circular(xlim = 50)
pl1


for(i in 1:nrow(dat)){
  pl1 <- pl1 + geom_taxalink(taxa1 = dat$sp1[i],
                             taxa2 = dat$sp2[i],
                             col = cols[i],
                             size = 0.5,
                             hratio = 3) 
}  
pl1
 
#I then export this plot as a pdf and edit legend in ppt
