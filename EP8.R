pacman::p_load(wnominate,dplyr,pscl,ggplot2,here,remotes,pkgbuild,emIRT)
mepsEP8<-read.csv(here("Cleaned_data","EP8_clean_data","mep_info_for_wnominate.csv"),header=TRUE,strip.white=TRUE)
votesEP8 <- read.csv(here("Cleaned_data","EP8_clean_data","matrix_ep8_votes.csv"),header = TRUE, strip.white = TRUE)

names8 <- mepsEP8[,1]
legData8 <- matrix(mepsEP8[,2],length(mepsEP8[,2]),1)
colnames(legData8) <- "EPG"

rc8 <- rollcall(votesEP8, yea = 1 , nay = 2 , missing = c(3,4,5,6),notInLegis = 0,legis.names=names8,legis.data = legData8,desc="EP8")
EPG8 <- mepsEP8$EPG
rcEM8 <- convertRC(rc8)
p8 <- makePriors(rcEM8$n, rcEM8$m, 1)


for (i in 1:10) {
  
  s8 <- getStarts(rcEM8$n, rcEM8$m, 1)
  
  resulti <-  binIRT(.rc = rcEM8,
                     .starts = s8,
                     .priors = p8,
                     .anchor_subject = 13,
                     .control = {list(threads = 8, checkfreq = 100)}
  )
  
  # Summary of result (optional, you can remove it if not needed)
  legislatorsi <- data.frame(
    EPG <- EPG8,
    coord1d <- resulti$means$x
  )
  legislatorsi$y <- 0 
  
  
  # Plotting with ggplot2
  p <- ggplot(legislatorsi, aes(x = d1, y = y, color = EPG, label = EPG)) +
    geom_point(size = 3) +
    geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
    labs(title = paste("emIRT EP8- Iteration", i),
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
  ggsave(filename = here("Results", "EP8", paste("emIRT_Plot_Iteration EP8", i, ".png", sep = "")), plot = p)
  
  
  # Save the CSV
  write.csv(legislatorsi, file = here("Results", "EP8", paste("EP8_Ideal_points_emIRT_Iteration", i, ".csv", sep = "")), row.names = FALSE)}
for (i in 1:10) {
  
  s8 <- getStarts(rcEM8$n, rcEM8$m, 1)
  
  resulti <-  binIRT(.rc = rcEM8,
                     .starts = s8,
                     .priors = p8,
                     .control = {list(threads = 8, checkfreq = 100)}
  )
  
  # Summary of result (optional, you can remove it if not needed)
  legislatorsi <- data.frame(
    EPG <- EPG8,
    coord1d <- resulti$means$x
  )
  legislatorsi$y <- 0 
  
  
  # Plotting with ggplot2
  p <- ggplot(legislatorsi, aes(x = d1, y = y, color = EPG, label = EPG)) +
    geom_point(size = 3) +
    geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
    labs(title = paste("emIRT EP8 unanchored- Iteration", i),
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
  ggsave(filename = here("Results", "EP8", paste("emIRT_Plot_Iteration unanchored EP8 ", i, ".png", sep = "")), plot = p)
  
  
  # Save the CSV
  write.csv(legislatorsi, file = here("Results", "EP8", paste("EP8_Ideal_points_emIRT_unanchored_Iteration_", i, ".csv", sep = "")), row.names = FALSE)}
LDATA8 <- map_values(rc8$votes)

resultkpascore8 <-kpascore(LDATA8)

v8 <- resultkpascore8$ZMAT2[,2]
v8 <- head(v8, 858)
v8 <- matrix(v8, nrow=858, ncol=1)
s8 <- getStarts(rcEM8$n, rcEM8$m, 1)
s8$x <- v8

result8EIGEN <- binIRT(.rc = rcEM8,
                         .starts = s8,
                         .priors = p8,
                         .anchor_subject = 13,
                         .control = {list(threads = 8, checkfreq = 100)}
)  

legislators8eigen <- data.frame(
  EPG <- EPG8,
  coord1d <- result8EIGEN$means$x
)
legislators8eigen$y <- 0

p8EIGEN <- ggplot(legislators8eigen, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP8 EIGENSTARTS",
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
ggsave(filename = here("Results", "EP8", "emIRT_Plot_EP8_eigenstarts.png"), plot = p8EIGEN)


# Save the CSV
write.csv(legislators8eigen, file = here("Results", "EP8", "emIRT_EP8_eigenstarts.csv"), row.names = FALSE)

result8EIGENun <- binIRT(.rc = rcEM8,
                           .starts = s8,
                           .priors = p8,
                           .control = {list(threads = 8, checkfreq = 100)}
)  

legislators8eigenun <- data.frame(
  EPG <- EPG8,
  coord1d <- result8EIGENun$means$x
)
legislators8eigenun$y <- 0

p8EIGENun <- ggplot(legislators8eigenun, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP8 EIGENSTARTS unanchored",
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
ggsave(filename = here("Results", "EP8", "emIRT_Plot_EP8_unanchored_eigenstarts.png"), plot = p8EIGENun)

# Save the CSV
write.csv(legislators8eigen, file = here("Results", "EP8", "emIRT_EP8_unanchored_eigenstarts.csv"), row.names = FALSE)
v8 <- resultkpascore8$XDATA[,1]
v8 <- head(v8, 858)
v8 <- matrix(v8, nrow=858, ncol=1)
s8 <- getStarts(rcEM8$n, rcEM8$m, 1)
s8$x <- v8

result8EIGENXDATA1 <- binIRT(.rc = rcEM8,
                             .starts = s8,
                             .priors = p8,
                             .anchor_subject = 1,
                             .control = {list(threads = 8, checkfreq = 100)}
)  

legislators8eigenXDATA1 <- data.frame(
  EPG <- EPG8,
  coord1d <- result8EIGENXDATA1$means$x
)
legislators8eigenXDATA1$y <- 0

p8EIGENXDATA1 <- ggplot(legislators8eigenXDATA1, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP8 EIGENSTARTS XDATA 1",
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
ggsave(filename = here("Results", "EP8", "emIRT_Plot_EP8_eigenstartsXDATA1.png"), plot = p8EIGENXDATA1)


# Save the CSV
write.csv(legislators8eigenXDATA1, file = here("Results", "EP8", "emIRT_EP8_eigenstartsXDATA1.csv"), row.names = FALSE)

result8EIGENunXDATA1 <- binIRT(.rc = rcEM8,
                               .starts = s8,
                               .priors = p8,
                               .control = {list(threads = 8, checkfreq = 100)}
)  

legislators8eigenunXDATA1 <- data.frame(
  EPG <- EPG8,
  coord1d <- result8EIGENunXDATA1$means$x
)
legislators8eigenunXDATA1$y <- 0

p8EIGENunXDATA1 <- ggplot(legislators8eigenunXDATA1, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP8 EIGENSTARTS XDATA1 unanchored",
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
ggsave(filename = here("Results", "EP8", "emIRT_Plot_EP8_unanchored_XDATA1eigenstarts.png"), plot = p8EIGENunXDATA1)

# Save the CSV
write.csv(legislators8eigenXDATA1, file = here("Results", "EP8", "emIRT_EP8_unanchored_XDATA1eigenstarts.csv"), row.names = FALSE)


v8 <- resultkpascore8$XDATA[,2]
v8 <- head(v8, 858)
v8 <- matrix(v8, nrow=858, ncol=1)
s8 <- getStarts(rcEM8$n, rcEM8$m, 1)
s8$x <- v8

result8EIGENXDATA2 <- binIRT(.rc = rcEM8,
                             .starts = s8,
                             .priors = p8,
                             .anchor_subject = 1,
                             .control = {list(threads = 8, checkfreq = 100)}
)  

legislators8eigenXDATA2 <- data.frame(
  EPG <- EPG8,
  coord1d <- result8EIGENXDATA2$means$x
)
legislators8eigenXDATA2$y <- 0

p8EIGENXDATA2 <- ggplot(legislators8eigenXDATA2, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP8 EIGENSTARTS XDATA 2",
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
ggsave(filename = here("Results", "EP8", "emIRT_Plot_EP8_eigenstartsXDATA2.png"), plot = p8EIGENXDATA2)


# Save the CSV
write.csv(legislators8eigenXDATA2, file = here("Results", "EP8", "emIRT_EP8_eigenstartsXDATA2.csv"), row.names = FALSE)

result8EIGENunXDATA2 <- binIRT(.rc = rcEM8,
                               .starts = s8,
                               .priors = p8,
                               .control = {list(threads = 8, checkfreq = 100)}
)  

legislators8eigenunXDATA2 <- data.frame(
  EPG <- EPG8,
  coord1d <- result8EIGENunXDATA2$means$x
)
legislators8eigenunXDATA2$y <- 0

p8EIGENunXDATA2 <- ggplot(legislators8eigenunXDATA2, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "emIRT EP8 EIGENSTARTS XDATA2 unanchored",
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
ggsave(filename = here("Results", "EP8", "emIRT_Plot_EP8_unanchored_XDATA2eigenstarts.png"), plot = p8EIGENunXDATA1)

# Save the CSV
write.csv(legislators8eigenXDATA1, file = here("Results", "EP8", "emIRT_EP8_unanchored_XDATA2eigenstarts.csv"), row.names = FALSE)

