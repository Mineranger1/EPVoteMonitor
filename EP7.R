pacman::p_load(wnominate,dplyr,pscl,ggplot2,here,remotes,pkgbuild,emIRT)
mepsEP7 <- read.csv(here("Cleaned_data","EP7_clean_data","mep_info_for_wnominate.csv"),header=TRUE,strip.white=TRUE)
votesEP7 <- read.csv(here("Cleaned_data","EP7_clean_data","matrix_ep7_votes.csv"),header = TRUE, strip.white = TRUE)

names7 <- mepsEP7[,1]
legData7 <- matrix(mepsEP7[,2],length(mepsEP7[,2]),1)
colnames(legData7) <- "EPG"
rc7 <- rollcall(votesEP7, yea = 1 , nay = 2 , missing = c(3,4,5),notInLegis = 0,legis.names=names7,legis.data = legData7,desc="EP7")
EPG7 <- mepsEP7$EPG
rcEM7 <- convertRC(rc7)
p7 <- makePriors(rcEM7$n, rcEM7$m, 1)


for (i in 1:10) {

  s7 <- getStarts(rcEM7$n, rcEM7$m, 1)
  
  resulti <-  binIRT(.rc = rcEM7,
                     .starts = s7,
                     .priors = p7,
                     .anchor_subject = 1,
                     .control = {list(threads = 8, checkfreq = 100)}
  )
  
  # Summary of result (optional, you can remove it if not needed)
  legislatorsi <- data.frame(
    EPG <- EPG7,
    coord1d <- resulti$means$x
  )
  legislatorsi$y <- 0 
  
  
  # Plotting with ggplot2
  p <- ggplot(legislatorsi, aes(x = d1, y = y, color = EPG, label = EPG)) +
    geom_point(size = 3) +
    geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
    labs(title = paste("emIRT EP7- Iteration", i),
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
  ggsave(filename = here("Results", "EP7", paste("emIRT_Plot_Iteration EP7", i, ".png", sep = "")), plot = p)
  
  
  # Save the CSV
  write.csv(legislatorsi, file = here("Results", "EP7", paste("EP7_Ideal_points_emIRT_Iteration", i, ".csv", sep = "")), row.names = FALSE)}
for (i in 1:10) {
  
  s7 <- getStarts(rcEM7$n, rcEM7$m, 1)
  
  resulti <-  binIRT(.rc = rcEM7,
                     .starts = s7,
                     .priors = p7,
                     .control = {list(threads = 8, checkfreq = 100)}
  )
  
  # Summary of result (optional, you can remove it if not needed)
  legislatorsi <- data.frame(
    EPG <- EPG7,
    coord1d <- resulti$means$x
  )
  legislatorsi$y <- 0 
  
  
  # Plotting with ggplot2
  p <- ggplot(legislatorsi, aes(x = d1, y = y, color = EPG, label = EPG)) +
    geom_point(size = 3) +
    geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
    labs(title = paste("emIRT EP7 unanchored- Iteration", i),
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
  ggsave(filename = here("Results", "EP7", paste("emIRT_Plot_Iteration unanchored EP7 ", i, ".png", sep = "")), plot = p)
  
  
  # Save the CSV
  write.csv(legislatorsi, file = here("Results", "EP7", paste("EP7_Ideal_points_emIRT_unanchored_Iteration_", i, ".csv", sep = "")), row.names = FALSE)}
LDATA7 <- map_values(rc7$votes)

resultkpascore7 <-kpascore(LDATA7)

v7 <- resultkpascore7$ZMAT2[,2]
v7 <- head(v7, 853)
v7 <- matrix(v7, nrow=853, ncol=1)
s7 <- getStarts(rcEM7$n, rcEM7$m, 1)
s7$x <- v7

result7EIGEN <- binIRT(.rc = rcEM7,
      .starts = s7,
      .priors = p7,
      .anchor_subject = 1,
      .control = {list(threads = 8, checkfreq = 100)}
)  

legislators7eigen <- data.frame(
  EPG <- EPG7,
  coord1d <- result7EIGEN$means$x
)
legislators7eigen$y <- 0

p7EIGEN <- ggplot(legislators7eigen, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP7 EIGENSTARTS",
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
ggsave(filename = here("Results", "EP7", "emIRT_Plot_EP7_eigenstarts.png"), plot = p7EIGEN)


# Save the CSV
write.csv(legislators7eigen, file = here("Results", "EP7", "emIRT_EP7_eigenstarts.csv"), row.names = FALSE)

result7EIGENun <- binIRT(.rc = rcEM7,
                           .starts = s7,
                           .priors = p7,
                           .control = {list(threads = 8, checkfreq = 100)}
)  

legislators7eigenun <- data.frame(
  EPG <- EPG7,
  coord1d <- result7EIGENun$means$x
)
legislators7eigenun$y <- 0

p7EIGENun <- ggplot(legislators7eigenun, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP7 EIGENSTARTS unanchored",
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
ggsave(filename = here("Results", "EP7", "emIRT_Plot_EP7_unanchored_eigenstarts.png"), plot = p7EIGENun)

# Save the CSV
write.csv(legislators7eigen, file = here("Results", "EP7", "emIRT_EP7_unanchored_eigenstarts.csv"), row.names = FALSE)


v7 <- resultkpascore7$XDATA[,1]
v7 <- head(v7, 853)
v7 <- matrix(v7, nrow=853, ncol=1)
s7 <- getStarts(rcEM7$n, rcEM7$m, 1)
s7$x <- v7

result7EIGENXDATA1 <- binIRT(.rc = rcEM7,
                       .starts = s7,
                       .priors = p7,
                       .anchor_subject = 1,
                       .control = {list(threads = 8, checkfreq = 100)}
)  

legislators7eigenXDATA1 <- data.frame(
  EPG <- EPG7,
  coord1d <- result7EIGENXDATA1$means$x
)
legislators7eigenXDATA1$y <- 0

p7EIGENXDATA1 <- ggplot(legislators7eigenXDATA1, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP7 EIGENSTARTS XDATA 1",
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
ggsave(filename = here("Results", "EP7", "emIRT_Plot_EP7_eigenstartsXDATA1.png"), plot = p7EIGENXDATA1)


# Save the CSV
write.csv(legislators7eigenXDATA1, file = here("Results", "EP7", "emIRT_EP7_eigenstartsXDATA1.csv"), row.names = FALSE)

result7EIGENunXDATA1 <- binIRT(.rc = rcEM7,
                         .starts = s7,
                         .priors = p7,
                         .control = {list(threads = 8, checkfreq = 100)}
)  

legislators7eigenunXDATA1 <- data.frame(
  EPG <- EPG7,
  coord1d <- result7EIGENunXDATA1$means$x
)
legislators7eigenunXDATA1$y <- 0

p7EIGENunXDATA1 <- ggplot(legislators7eigenunXDATA1, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP7 EIGENSTARTS XDATA1 unanchored",
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
ggsave(filename = here("Results", "EP7", "emIRT_Plot_EP7_unanchored_XDATA1eigenstarts.png"), plot = p7EIGENunXDATA1)

# Save the CSV
write.csv(legislators7eigenXDATA1, file = here("Results", "EP7", "emIRT_EP7_unanchored_XDATA1eigenstarts.csv"), row.names = FALSE)


v7 <- resultkpascore7$XDATA[,2]
v7 <- head(v7, 853)
v7 <- matrix(v7, nrow=853, ncol=1)
s7 <- getStarts(rcEM7$n, rcEM7$m, 1)
s7$x <- v7

result7EIGENXDATA2 <- binIRT(.rc = rcEM7,
                             .starts = s7,
                             .priors = p7,
                             .anchor_subject = 1,
                             .control = {list(threads = 8, checkfreq = 100)}
)  

legislators7eigenXDATA2 <- data.frame(
  EPG <- EPG7,
  coord1d <- result7EIGENXDATA2$means$x
)
legislators7eigenXDATA2$y <- 0

p7EIGENXDATA2 <- ggplot(legislators7eigenXDATA2, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP7 EIGENSTARTS XDATA 2",
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
ggsave(filename = here("Results", "EP7", "emIRT_Plot_EP7_eigenstartsXDATA2.png"), plot = p7EIGENXDATA2)


# Save the CSV
write.csv(legislators7eigenXDATA2, file = here("Results", "EP7", "emIRT_EP7_eigenstartsXDATA2.csv"), row.names = FALSE)

result7EIGENunXDATA2 <- binIRT(.rc = rcEM7,
                               .starts = s7,
                               .priors = p7,
                               .control = {list(threads = 8, checkfreq = 100)}
)  

legislators7eigenunXDATA2 <- data.frame(
  EPG <- EPG7,
  coord1d <- result7EIGENunXDATA2$means$x
)
legislators7eigenunXDATA2$y <- 0

p7EIGENunXDATA2 <- ggplot(legislators7eigenunXDATA2, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP7 EIGENSTARTS XDATA2 unanchored",
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
ggsave(filename = here("Results", "EP7", "emIRT_Plot_EP7_unanchored_XDATA2eigenstarts.png"), plot = p7EIGENunXDATA1)

# Save the CSV
write.csv(legislators7eigenXDATA1, file = here("Results", "EP7", "emIRT_EP7_unanchored_XDATA2eigenstarts.csv"), row.names = FALSE)