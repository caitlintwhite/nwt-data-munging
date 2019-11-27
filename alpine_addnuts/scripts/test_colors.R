library(DescTools)
library(tidyverse)
par(mfcol=c(2,2))
tmp <- 1:6
traitplot <- 1:6
names(tmp) <- c('red','green','blue')

# simplified treatments
simpletrts <- sort(unique(c(as.character(sdlplots$trt))))
simplecols <- viridis::viridis(n = 4, option = "D")

# choose color scheme here to color in spp in nmds points with..
traitcols <- c("Acquisitive" = "deeppink2", "Conservative" = "darkred", "Unknown" = "transparent")
#traitcols <- c("Acquisitive" = "grey20", "Conservative" = "grey80", "Unknown" = "black")
traitcols <- viridis::viridis(n=6, option = "C")
traitcols <- c("chocolate4", "#FCA636FF")
ColToGrey(traitcols)

par(mfrow = c(1,2))
barplot(traitplot, col=c(simplecols, traitcols))
barplot(traitplot, col=ColToGrey(c(simplecols, traitcols)))
barplot(tmp, col=c(simplecols))
barplot(tmp, col=ColToGrey(c(simplecols)))


s <- stringi::stri_paste(
  "Lorem ipsum dolor sit amet, consectetur adipisicing elit. Proin ",
  "nibh augue, suscipit a, scelerisque sed, lacinia in, mi. Cras vel ",
  "lorem. Etiam pellentesque aliquet tellus.")
cat(stringi::stri_wrap(s, 20, 0.0), sep="\n") # greedy
cat(stringi::stri_wrap(s, 2, 2.0), sep="\n") # dynamic
cat(stringi::stri_pad(stringi::stri_wrap(s), side='both'), sep="\n")
