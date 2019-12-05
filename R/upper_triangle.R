# .libPaths(c("~/Documents/Rpackagesv2",.libPaths()))
library(tidyverse)
library(HiTC)
library(here)

# arbitrary area of interest --------------------------------------------------
# chromosme 8q24 example - 8:126730818-127810818

# first plot hic-data as upper triangle ---------------------------------------

## import Hi-C data -------------------------------------------------
load("data/hicData.Rdata")

  ### subset to region of interest ----------------------------------
  hic_dat <- extractRegion(hiC[[paste0("chr", 8, "chr", 8)]],
                           chr = paste0("chr", 8),
                           from = 126730818, to = 127810818)
  hic_matrix <- as.matrix(intdata(hic_dat)) # extract values into matrix 

## create dataframe for plotting triangular heatmap -----------------

## determine number and size of bins 
nbins <- nrow(hic_matrix)
stepsize <- abs(126730818 - 127810818) / (2 * nbins)

## scale (needed for coding colors)  
vec <- hic_matrix
  ### raw hi-c has extreme values and need to keep contained
  vec[which(vec < 0)] <- 0    
  vec[which(vec > 28)] <- 28
breaks <- seq(0, 28, length.out = 100)
cols_num <- c(0:length(breaks) + 1)
cols_vec <- cut(vec, c(-Inf, breaks, Inf), labels = cols_num)
hicmcol <- matrix(as.numeric(as.character(cols_vec)), nrow = nrow(hic_matrix))

# make an empty tibble
tmp <- tibble(x = numeric(), y = numeric(), f = numeric(), g = character(), v = numeric())

for (i in (1:nrow(hic_matrix))) {
  y <- -.5
  
  x <- 126730818 + (i * 2 * stepsize) - (stepsize * 2)
  for (j in (i:ncol(hic_matrix))) {
    x <- x + stepsize
    y <- y + .5
    
    poly_dat <- tibble(
      x = c(x - stepsize, x, x + stepsize, x),
      y = c(y, y + .5, y, y - .5),
      f = hicmcol[i, j],
      g = paste0("bin_", i, "_", j),
      v = hic_matrix[i, j]
    )
    
    tmp <- bind_rows(tmp, poly_dat)
  }
}
rm(i, j)


ggplot(tmp, aes(x = x, y = y, text = paste0("Raw value: ", v))) +
  geom_polygon(aes(fill = f, group = g)) +
  scale_fill_viridis_c(100, name = "Score") +
  coord_cartesian(xlim = c(126730818, 127810818)) +
  ylim(0, (nbins * 0.5) + 1) +
  ylab("HIC Intensities") + 
  theme_classic()