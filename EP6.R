pacman::p_load(wnominate,dplyr,pscl,ggplot2,here,remotes,pkgbuild,emIRT)
mepsEP6 <- read.csv(here("Cleaned_data","EP6_clean_data","mep_info_for_wnominate.csv"),header=TRUE,strip.white=TRUE)
votesEP6 <- read.csv(here("Cleaned_data","EP6_clean_data","wnominate_ep6_votes.csv"),header = TRUE, strip.white = TRUE)

names6 <- mepsEP6[,1]
legData6 <- matrix(mepsEP6[,2],length(mepsEP6[,2]),1)
colnames(legData6) <- "EPG"
rc6 <- rollcall(votesEP6, yea = 1 , nay = 2 , missing = c(3,4,5),notInLegis = 0,legis.names=names6,legis.data = legData6,desc="EP6")
EPG6 <- mepsEP6$EPG
rcEM6 <- convertRC(rc6)
p6 <- makePriors(rcEM6$n, rcEM6$m, 1)


for (i in 1:10) {
  
  s6 <- getStarts(rcEM6$n, rcEM6$m, 1)
  
  resulti <-  binIRT(.rc = rcEM6,
                     .starts = s6,
                     .priors = p6,
                     .anchor_subject = 1,
                     .control = {list(threads = 8, checkfreq = 100)}
  )
  
  # Summary of result (optional, you can remove it if not needed)
  legislatorsi <- data.frame(
    MepId <- names6,
    MepId <- names6, EPG <- EPG6,
    coord1d <- resulti$means$x
  )
  legislatorsi$y <- 0 
  
  
  # Plotting with ggplot2
  p <- ggplot(legislatorsi, aes(x = d1, y = y, color = EPG, label = EPG)) +
    geom_point(size = 3) +
    geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
    labs(title = paste("emIRT EP6- Iteration", i),
         x = "Coordinate 1D",
         y = "") +
    theme_minimal() +
    scale_color_discrete(name = "EPG Labels") +
    xlim(-10, 10) +
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.title.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
  
  # Save the plot
  ggsave(filename = here("Results", "EP6", paste("emIRT_Plot_Iteration EP6", i, ".png", sep = "")), plot = p)
  
  
  # Save the CSV
  write.csv(legislatorsi, file = here("Results", "EP6", paste("EP6_Ideal_points_emIRT_Iteration", i, ".csv", sep = "")), row.names = FALSE)}
for (i in 1:10) {
  
  s6 <- getStarts(rcEM6$n, rcEM6$m, 1)
  
  resulti <-  binIRT(.rc = rcEM6,
                     .starts = s6,
                     .priors = p6,
                     .control = {list(threads = 8, checkfreq = 100)}
  )
  
  # Summary of result (optional, you can remove it if not needed)
  legislatorsi <- data.frame(
    MepId <- names6, EPG <- EPG6,
    coord1d <- resulti$means$x
  )
  legislatorsi$y <- 0 
  
  
  # Plotting with ggplot2
  p <- ggplot(legislatorsi, aes(x = d1, y = y, color = EPG, label = EPG)) +
    geom_point(size = 3) +
    geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
    labs(title = paste("emIRT EP6 unanchored- Iteration", i),
         x = "Coordinate 1D",
         y = "") +
    theme_minimal() +
    scale_color_discrete(name = "EPG Labels") +
    xlim(-10, 10) +
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.title.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
  
  # Save the plot
  ggsave(filename = here("Results", "EP6", paste("emIRT_Plot_Iteration unanchored EP6 ", i, ".png", sep = "")), plot = p)
  
  
  # Save the CSV
  write.csv(legislatorsi, file = here("Results", "EP6", paste("EP6_Ideal_points_emIRT_unanchored_Iteration_", i, ".csv", sep = "")), row.names = FALSE)}

LDATA6 <- map_values(rc6$votes)

resultkpascore6 <-kpascore(LDATA6)

v6 <- resultkpascore6$ZMAT2[,2]
v6 <- head(v6, 940)
v6 <- matrix(v6, nrow=940, ncol=1)
s6 <- getStarts(rcEM6$n, rcEM6$m, 1)
s6$x <- v6

result6EIGEN <- binIRT(.rc = rcEM6,
                         .starts = s6,
                         .priors = p6,
                         .anchor_subject = 1,
                         .control = {list(threads = 8, checkfreq = 100)}
)  

legislators6eigen <- data.frame(
  
  coord1d <- result6EIGEN$means$x
)
legislators6eigen$y <- 0

p6EIGEN <- ggplot(legislators6eigen, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP6 EIGENSTARTS",
       x = "Coordinate 1D",
       y = "") +
  theme_minimal() +
  scale_color_discrete(name = "EPG Labels") +
  xlim(-0.5, 0.5) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave(filename = here("Results", "EP6", "emIRT_Plot_EP6_eigenstarts.png"), plot = p6EIGEN)

result6EIGENun <- binIRT(.rc = rcEM6,
                         .starts = s6,
                         .priors = p6,
                         .control = {list(threads = 8, checkfreq = 100)}
)  

legislators6eigenun <- data.frame(
  MepId <- names6, EPG <- EPG6,
  coord1d <- result6EIGENun$means$x
)
legislators6eigenun$y <- 0

p6EIGENun <- ggplot(legislators6eigenun, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP6 EIGENSTARTS unanchored",
       x = "Coordinate 1D",
       y = "") +
  theme_minimal() +
  scale_color_discrete(name = "EPG Labels") +
  xlim(-0.5, 0.5) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave(filename = here("Results", "EP6", "emIRT_Plot_EP6_unanchored_eigenstarts.png"), plot = p6EIGENun)

# Save the CSV
write.csv(legislators6eigen, file = here("Results", "EP6", "emIRT_EP6_unanchored_eigenstarts.csv"), row.names = FALSE)

