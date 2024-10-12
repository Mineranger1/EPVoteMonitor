pacman::p_load(wnominate,dplyr,pscl,ggplot2,here,remotes,pkgbuild,emIRT)
result6<-wnominate(rc6,polarity=c(1,1))
summary(result6) 

legislators6 <- data.frame(
  EPG = result6$legislators$EPG,
  coord1D = result6$legislators$coord1D,
  coord2D = result6$legislators$coord2D
)
p6 <- ggplot(legislators6, aes(x = coord1D, y = coord2D, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "W Nominate EP 6",
       x = "Coordinate 1D",
       y = "Coordinate 2D") +
  theme_minimal() +
  scale_color_discrete(name = "EPG Labels")

ggsave(filename = here("Results", "EP6", "W_Nominate_Plot_EP6.png"), plot = p6)

result61d <- wnominate(rc6,dims = 1, polarity = c(1))

summary(result61d) 

legislators61d <- data.frame(
  EPG = result61d$legislators$EPG,
  coord1D = result61d$legislators$coord1D
)
legislators61d$coord2D <- 0
p61d <- ggplot(legislators61d, aes(x = coord1D, y = coord2D, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "W Nominate EP 6 1d",
       x = "Coordinate 1D",
       y = "") +
  theme_minimal() +
  scale_color_discrete(name = "EPG Labels")

ggsave(filename = here("Results", "EP6", "W_Nominate_Plot_EP6_1d.png"), plot = p61d)
write.csv(legislators6, file = here("Results", "EP6", "wnominate6.csv"), row.names = FALSE)
write.csv(legislators61d, file = here("Results", "EP6", "wnominate61d.csv"), row.names = FALSE)
result7<-wnominate(rc7,polarity=c(1,1))
summary(result7) 

legislators7 <- data.frame(
  EPG = result7$legislators$EPG,
  coord1D = result7$legislators$coord1D,
  coord2D = result7$legislators$coord2D
)
p7 <- ggplot(legislators7, aes(x = coord1D, y = coord2D, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "W Nominate EP 7",
       x = "Coordinate 1D",
       y = "Coordinate 2D") +
  theme_minimal() +
  scale_color_discrete(name = "EPG Labels")

ggsave(filename = here("Results", "EP7", "W_Nominate_Plot_EP7.png"), plot = p7)

result71d <- wnominate(rc7,dims = 1, polarity = c(1))

summary(result71d) 

legislators71d <- data.frame(
  EPG = result71d$legislators$EPG,
  coord1D = result71d$legislators$coord1D
)
legislators71d$coord2D <- 0
p71d <- ggplot(legislators71d, aes(x = coord1D, y = coord2D, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "W Nominate EP 7 1d",
       x = "Coordinate 1D",
       y = "") +
  theme_minimal() +
  scale_color_discrete(name = "EPG Labels")
write.csv(legislators7, file = here("Results", "EP7", "wnominate71d.csv"), row.names = FALSE)
write.csv(legislators71d, file = here("Results", "EP7", "wnominate71d.csv"), row.names = FALSE)
ggsave(filename = here("Results", "EP7", "W_Nominate_Plot_EP7_1d.png"), plot = p71d)

votesEP8[] <- lapply(votesEP8, function(x) ifelse(x %in% 0:3, x, 3))
rc8 <- rollcall(votesEP8, yea = 1 , nay = 2 , missing = 3,notInLegis = 0,legis.names=names8,legis.data = legData8,desc="EP8")

result8<-wnominate(rc8,polarity=c(13,13))
summary(result8) 

legislators8 <- data.frame(
  EPG = result8$legislators$EPG,
  coord1D = result8$legislators$coord1D,
  coord2D = result8$legislators$coord2D
)
p8 <- ggplot(legislators8, aes(x = coord1D, y = coord2D, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "W Nominate EP 8",
       x = "Coordinate 1D",
       y = "Coordinate 2D") +
  theme_minimal() +
  scale_color_discrete(name = "EPG Labels")

ggsave(filename = here("Results", "EP8", "W_Nominate_Plot_EP8.png"), plot = p8)

result81d <- wnominate(rc8,dims = 1, polarity = c(1))

summary(result81d) 

legislators81d <- data.frame(
  EPG = result81d$legislators$EPG,
  coord1D = result81d$legislators$coord1D
)
legislators81d$coord2D <- 0
p81d <- ggplot(legislators81d, aes(x = coord1D, y = coord2D, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "W Nominate EP 8 1d",
       x = "Coordinate 1D",
       y = "") +
  theme_minimal() +
  scale_color_discrete(name = "EPG Labels")
write.csv(legislators8, file = here("Results", "EP8", "wnominate8.csv"), row.names = FALSE)
write.csv(legislators81d, file = here("Results", "EP8", "wnominate81d
                                      
                                      .csv"), row.names = FALSE)
ggsave(filename = here("Results", "EP8", "W_Nominate_Plot_EP8_1d.png"), plot = p81d)
votesEP9[] <- lapply(votesEP9, function(x) ifelse(x %in% 0:3, x, 3))
rc9 <- rollcall(votesEP9, yea = 1 , nay = 2 , missing = 3,notInLegis = 0,legis.names=names9,legis.data = legData9,desc="EP9")

result9<-wnominate(rc9,polarity=c(1,1))
summary(result9) 

legislators9 <- data.frame(
  EPG = result9$legislators$EPG,
  coord1D = result9$legislators$coord1D,
  coord2D = result9$legislators$coord2D
)
p9 <- ggplot(legislators9, aes(x = coord1D, y = coord2D, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "W Nominate EP 9",
       x = "Coordinate 1D",
       y = "Coordinate 2D") +
  theme_minimal() +
  scale_color_discrete(name = "EPG Labels")

ggsave(filename = here("Results", "EP9", "W_Nominate_Plot_EP9.png"), plot = p9)

result91d <- wnominate(rc9,dims = 1, polarity = c(1))

summary(result91d) 

legislators91d <- data.frame(
  EPG = result91d$legislators$EPG,
  coord1D = result91d$legislators$coord1D
)
legislators91d$coord2D <- 0
p91d <- ggplot(legislators91d, aes(x = coord1D, y = coord2D, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "W Nominate EP 9 1d",
       x = "Coordinate 1D",
       y = "") +
  theme_minimal() +
  scale_color_discrete(name = "EPG Labels")

ggsave(filename = here("Results", "EP9", "W_Nominate_Plot_EP9_1d.png"), plot = p91d)

