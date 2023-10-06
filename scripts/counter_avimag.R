spe <- read.csv("../data/speciesnames.csv")
spe$counts <- NA
files <- list.files("../data/aviculturalmagazine")
for(i in 1:nrow(spe)){
  curcount <- 0
  for(j in 1:length(files)){
    names = scan(paste("../aviculturalmagazine/",files[j], sep=""), 
                 what=character(), 
                 quote=NULL,
                 sep="\n")
    idxs = grep(spe$species[i], names, ignore.case = TRUE)
    curcount <- curcount + length(idxs)
  }
  spe$counts[i] <- curcount
}

write.csv(spe, "../data/domesticationindex.csv")

#after obtaining this csv, I manually corrected for overlapping species/common names - for example, Brush turkey and turkey,
#chicken and greater prarie chicken, etc.