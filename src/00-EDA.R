# Exploratory data analysis
# Load in packages and data
library(ggplot2)
library(data.table)
library(plyr)
library(dplyr)
library(MASS)
library(RColorBrewer)
library(Hmisc)
library(ggpubr)
library(ks)
library(tidyverse)
library(scales)

pairs_tsi <- read.csv(here::here("data", "pairs_tsi.csv"))

# Rounds

print(sort(unique(pairs_tsi$ROUND.M)))

# Structure

str(pairs_tsi)

# Contour plots

plot_contour <- function(sex, hh){
  dataset <- pairs_tsi %>% filter(same_hh == hh) %>% 
    filter(SEX.SOURCE == sex) %>% 
    dplyr::select(AGE_TRANSMISSION.SOURCE, AGE_INFECTION.RECIPIENT)
  
  if (sex == "M"){
    direction <- "Male to female"
  } else {
    direction <- "Female to male"
  }
  
  if (hh){
    end <- "within household"
  } else {
    end <- "out of household"
  }
  
  title <- paste(direction, end, sep = ", ")
  
  kde(dataset) %>% plot(xlab="Age of source",
                               ylab = "Age of recipient", 
                               main = title)
  points(dataset, pch = 4, col = alpha("black", 0.3))
}
set.seed(1001)
sexes <- c("M", "F")
hhs <- c(0, 1)

pdf("contours.pdf")
par(mfrow = c(2, 2))
for (sex in sexes){
  for (hh in hhs){
    plot_contour(sex, hh)
  }
}
dev.off()




get_contour <- function(kd_out=kd, prob="5%") {
  contour_95 <- with(kd_out, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                          z=estimate, levels=cont[prob])[[1]])
  as_tibble(contour_95) %>% 
    mutate(prob = prob)
}

plot_kernel <- function(sex, hh){
  d <- pairs_tsi %>% filter(same_hh == hh) %>% 
    filter(SEX.SOURCE == sex) %>%
    dplyr::select(AGE_TRANSMISSION.SOURCE, AGE_INFECTION.RECIPIENT) %>%
    as.matrix() %>%
    magrittr::set_colnames(c("x", "y")) %>% 
    as_tibble()
  
  kd <- ks::kde(d, compute.cont=TRUE, h=0.2)
  
  dat_out <- map_dfr(c("10%", "20%", "50%" ,"80%", "90%"), ~get_contour(kd, .)) %>% 
    group_by(prob) %>% 
    mutate(n_val = 1:n()) %>% 
    ungroup()
  
  ## clean kde output
  kd_df <- expand_grid(x=kd$eval.points[[1]], y=kd$eval.points[[2]]) %>% 
    mutate(z = c(kd$estimate %>% t))
  
  p1 <- ggplot(data=kd_df, aes(x, y)) +
    geom_tile(aes(fill=z)) +
    geom_path(aes(x, y, group = prob), 
              data=dat_out, colour = I("black")) +
    geom_text(aes(label = prob), data = 
                filter(dat_out, (prob%in% c("10%") & n_val==100 | prob%in% c("20%") & n_val==80) | prob%in% c("50%") & n_val==60 | prob%in% c("80%") & n_val==40 | prob%in% c("90%") & n_val==20),
              colour = I("black"), size =I(3))+
    xlim(15,50)+
    ylim(15,50)+
    geom_abline(intercept=0,slope=1,color='black')+
    scale_fill_gradient(low = "white", high = "red") +
    labs(x='Age of recipient at transmission ',
         y='Age of source at transmission') +
    theme_bw() +
    theme(legend.position = "none")
  
  print(p1)
}

plot_kernel("F", 1)

