pacman::p_load(wnominate,dplyr,pscl,ggplot2,here,remotes,pkgbuild,emIRT)
mepsEP9<-read.csv(here("Cleaned_data","EP9_clean_data","mep_info_for_wnominate.csv"),header=TRUE,strip.white=TRUE)
votesEP9 <- read.csv(here("Cleaned_data","EP9_clean_data","matrix_ep9_votes.csv"),header = TRUE, strip.white = TRUE)
votesEP9 <- votesEP9[,-1]
names9 <- mepsEP9[,1]
legData9 <- matrix(mepsEP9[,2],length(mepsEP9[,2]),1)
colnames(legData9) <- "EPG"

rc9 <- rollcall(votesEP9, yea = 1 , nay = 2 , missing = c(3,4,5,6),notInLegis = 0,legis.names=names9,legis.data = legData9,desc="EP9")
EPG9 <- mepsEP9$EPG
rcEM9 <- convertRC(rc9)
p9 <- makePriors(rcEM9$n, rcEM9$m, 1)


for (i in 1:10) {
  
  s9 <- getStarts(rcEM9$n, rcEM9$m, 1)
  
  resulti <-  binIRT(.rc = rcEM9,
                     .starts = s9,
                     .priors = p9,
                     .anchor_subject = 1,
                     .control = {list(threads = 8, checkfreq = 100)}
  )
  
  # Summary of result (optional, you can remove it if not needed)
  legislatorsi <- data.frame(
    EPG <- EPG9,
    coord1d <- resulti$means$x
  )
  legislatorsi$y <- 0 
  
  
  # Plotting with ggplot2
  p <- ggplot(legislatorsi, aes(x = d1, y = y, color = EPG, label = EPG)) +
    geom_point(size = 3) +
    geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
    labs(title = paste("emIRT EP9- Iteration", i),
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
  ggsave(filename = here("Results", "EP9", paste("emIRT_Plot_Iteration EP9", i, ".png", sep = "")), plot = p)
  
  
  # Save the CSV
  write.csv(legislatorsi, file = here("Results", "EP9", paste("EP9_Ideal_points_emIRT_Iteration", i, ".csv", sep = "")), row.names = FALSE)}
for (i in 1:10) {
  
  s9 <- getStarts(rcEM9$n, rcEM9$m, 1)
  
  resulti <-  binIRT(.rc = rcEM9,
                     .starts = s9,
                     .priors = p9,
                     .control = {list(threads = 8, checkfreq = 100)}
  )
  
  # Summary of result (optional, you can remove it if not needed)
  legislatorsi <- data.frame(
    EPG <- EPG9,
    coord1d <- resulti$means$x
  )
  legislatorsi$y <- 0 
  
  
  # Plotting with ggplot2
  p <- ggplot(legislatorsi, aes(x = d1, y = y, color = EPG, label = EPG)) +
    geom_point(size = 3) +
    geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
    labs(title = paste("emIRT EP9 unanchored- Iteration", i),
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
  ggsave(filename = here("Results", "EP9", paste("emIRT_Plot_Iteration unanchored EP9 ", i, ".png", sep = "")), plot = p)
  
  
  # Save the CSV
  write.csv(legislatorsi, file = here("Results", "EP9", paste("EP9_Ideal_points_emIRT_unanchored_Iteration_", i, ".csv", sep = "")), row.names = FALSE)}
LDATA9 <- map_values(rc9$votes)

resultkpascore9 <-kpascore(LDATA9)

v9 <- resultkpascore9$ZMAT2[,2]
v9 <- head(v9, 866)
v9 <- matrix(v9, nrow=866, ncol=1)
s9 <- getStarts(rcEM9$n, rcEM9$m, 1)
s9$x <- v9

result9EIGEN <- binIRT(.rc = rcEM9,
                         .starts = s9,
                         .priors = p9,
                         .anchor_subject = 1,
                         .control = {list(threads = 8, checkfreq = 100)}
)  

legislators9eigen <- data.frame(
  EPG <- EPG9,
  coord1d <- result9EIGEN$means$x
)
legislators9eigen$y <- 0

p9EIGEN <- ggplot(legislators9eigen, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP9 EIGENSTARTS",
       x = "Coordinate 1D",
       y = "") +
  theme_minimal() +
  scale_color_discrete(name = "EPG Labels") +
  xlim(-0.3, 0.3) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave(filename = here("Results", "EP9", "emIRT_Plot_EP9_eigenstarts.png"), plot = p9EIGEN)


# Save the CSV
write.csv(legislators9eigen, file = here("Results", "EP9", "emIRT_EP9_eigenstarts.csv"), row.names = FALSE)

result9EIGENun <- binIRT(.rc = rcEM9,
                           .starts = s9,
                           .priors = p9,
                           .control = {list(threads = 8, checkfreq = 100)}
)  

legislators9eigenun <- data.frame(
  EPG <- EPG9,
  coord1d <- result9EIGENun$means$x
)
legislators9eigenun$y <- 0

p9EIGENun <- ggplot(legislators9eigenun, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP9 EIGENSTARTS unanchored",
       x = "Coordinate 1D",
       y = "") +
  theme_minimal() +
  scale_color_discrete(name = "EPG Labels") +
  xlim(-0.3, 0.3) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave(filename = here("Results", "EP9", "emIRT_Plot_EP9_unanchored_eigenstarts.png"), plot = p9EIGENun)

# Save the CSV
write.csv(legislators9eigen, file = here("Results", "EP9", "emIRT_EP9_unanchored_eigenstarts.csv"), row.names = FALSE)
v9 <- resultkpascore9$XDATA[,1]
v9 <- head(v9, 866)
v9 <- matrix(v9, nrow=866, ncol=1)
s9 <- getStarts(rcEM9$n, rcEM9$m, 1)
s9$x <- v9

result9EIGENXDATA1 <- binIRT(.rc = rcEM9,
                             .starts = s9,
                             .priors = p9,
                             .anchor_subject = 1,
                             .control = {list(threads = 8, checkfreq = 100)}
)  

legislators9eigenXDATA1 <- data.frame(
  EPG <- EPG9,
  coord1d <- result9EIGENXDATA1$means$x
)
legislators9eigenXDATA1$y <- 0

p9EIGENXDATA1 <- ggplot(legislators9eigenXDATA1, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP9 EIGENSTARTS XDATA 1",
       x = "Coordinate 1D",
       y = "") +
  theme_minimal() +
  scale_color_discrete(name = "EPG Labels") +
  xlim(-1, 1) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave(filename = here("Results", "EP9", "emIRT_Plot_EP9_eigenstartsXDATA1.png"), plot = p9EIGENXDATA1)


# Save the CSV
write.csv(legislators9eigenXDATA1, file = here("Results", "EP9", "emIRT_EP9_eigenstartsXDATA1.csv"), row.names = FALSE)

result9EIGENunXDATA1 <- binIRT(.rc = rcEM9,
                               .starts = s9,
                               .priors = p9,
                               .control = {list(threads = 8, checkfreq = 100)}
)  

legislators9eigenunXDATA1 <- data.frame(
  EPG <- EPG9,
  coord1d <- result9EIGENunXDATA1$means$x
)
legislators9eigenunXDATA1$y <- 0

p9EIGENunXDATA1 <- ggplot(legislators9eigenunXDATA1, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP9 EIGENSTARTS XDATA1 unanchored",
       x = "Coordinate 1D",
       y = "") +
  theme_minimal() +
  scale_color_discrete(name = "EPG Labels") +
  xlim(-1, 1) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave(filename = here("Results", "EP9", "emIRT_Plot_EP9_unanchored_XDATA1eigenstarts.png"), plot = p9EIGENunXDATA1)

# Save the CSV
write.csv(legislators9eigenXDATA1, file = here("Results", "EP9", "emIRT_EP9_unanchored_XDATA1eigenstarts.csv"), row.names = FALSE)


v9 <- resultkpascore9$XDATA[,2]
v9 <- head(v9, 866)
v9 <- matrix(v9, nrow=866, ncol=1)
s9 <- getStarts(rcEM9$n, rcEM9$m, 1)
s9$x <- v9

result9EIGENXDATA2 <- binIRT(.rc = rcEM9,
                             .starts = s9,
                             .priors = p9,
                             .anchor_subject = 1,
                             .control = {list(threads = 8, checkfreq = 100)}
)  

legislators9eigenXDATA2 <- data.frame(
  EPG <- EPG9,
  coord1d <- result9EIGENXDATA2$means$x
)
legislators9eigenXDATA2$y <- 0

p9EIGENXDATA2 <- ggplot(legislators9eigenXDATA2, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP9 EIGENSTARTS XDATA 2",
       x = "Coordinate 1D",
       y = "") +
  theme_minimal() +
  scale_color_discrete(name = "EPG Labels") +
  xlim(-1, 1) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave(filename = here("Results", "EP9", "emIRT_Plot_EP9_eigenstartsXDATA2.png"), plot = p9EIGENXDATA2)


# Save the CSV
write.csv(legislators9eigenXDATA2, file = here("Results", "EP9", "emIRT_EP9_eigenstartsXDATA2.csv"), row.names = FALSE)

result9EIGENunXDATA2 <- binIRT(.rc = rcEM9,
                               .starts = s9,
                               .priors = p9,
                               .control = {list(threads = 8, checkfreq = 100)}
)  

legislators9eigenunXDATA2 <- data.frame(
  EPG <- EPG9,
  coord1d <- result9EIGENunXDATA2$means$x
)
legislators9eigenunXDATA2$y <- 0

p9EIGENunXDATA2 <- ggplot(legislators9eigenunXDATA2, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP9 EIGENSTARTS XDATA2 unanchored",
       x = "Coordinate 1D",
       y = "") +
  theme_minimal() +
  scale_color_discrete(name = "EPG Labels") +
  xlim(-1, 1) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave(filename = here("Results", "EP9", "emIRT_Plot_EP9_unanchored_XDATA2eigenstarts.png"), plot = p9EIGENunXDATA1)

# Save the CSV
write.csv(legislators9eigenXDATA1, file = here("Results", "EP9", "emIRT_EP9_unanchored_XDATA2eigenstarts.csv"), row.names = FALSE)

