install.packages("pacman")
pacman::p_load(wnominate,dplyr,pscl,ggplot2,here,remotes,pkgbuild,emIRT)
mepsEP6 <- read.csv(here("Cleaned_data","EP6_clean_data","mep_info_for_wnominate.csv"),header=TRUE,strip.white=TRUE)
votesEP6 <- read.csv(here("Cleaned_data","EP6_clean_data","wnominate_ep6_votes.csv"),header = TRUE, strip.white = TRUE)

names6 <- mepsEP6[,1]
legData6 <- matrix(mepsEP6[,2],length(mepsEP6[,2]),1)
colnames(legData6) <- "EPG"
rc6 <- rollcall(votesEP6, yea = 1 , nay = 2 , missing = c(3,4,5),notInLegis = 0,legis.names=names6,legis.data = legData6,desc="EP6")
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
result63d<-wnominate(rc6,dims = 3, polarity=c(1,1,1))
summary(result63d)
legislators63d <- data.frame(
  EPG = result63d$legislators$EPG,
  coord1D = result63d$legislators$coord1D,
  coord2D = result63d$legislators$coord2D,
  coord3D = result63d$legislators$coord3D
)
write.csv(legislators6, file = here("Results", "EP6", "wnominate6.csv"), row.names = FALSE)
write.csv(legislators61d, file = here("Results", "EP6", "wnominate61d.csv"), row.names = FALSE)
write.csv(legislators63d, file = here("Results", "EP6", "wnominate63d.csv"), row.names = FALSE)
result64d<-wnominate(rc6,dims = 4, polarity=c(1,1,1,1))
summary(result64d)
legislators64d <- data.frame(
  EPG = result64d$legislators$EPG,
  coord1D = result64d$legislators$coord1D,
  coord2D = result64d$legislators$coord2D,
  coord3D = result64d$legislators$coord3D,
  coord4D = result64d$legislators$coord4D
)
write.csv(legislators64d, file = here("Results", "EP6", "wnominate64d.csv"), row.names = FALSE)
mepsEP7 <- read.csv(here("Cleaned_data","EP7_clean_data","mep_info_for_wnominate.csv"),header=TRUE,strip.white=TRUE)
votesEP7 <- read.csv(here("Cleaned_data","EP7_clean_data","matrix_ep7_votes.csv"),header = TRUE, strip.white = TRUE)

names7 <- mepsEP7[,1]
legData7 <- matrix(mepsEP7[,2],length(mepsEP7[,2]),1)
colnames(legData7) <- "EPG"
rc7 <- rollcall(votesEP7, yea = 1 , nay = 2 , missing = c(3,4,5),notInLegis = 0,legis.names=names7,legis.data = legData7,desc="EP7")
EPG7 <- mepsEP7$EPG
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

result73d<-wnominate(rc7,dims=3,polarity=c(1,1,1))
summary(result73d) 

legislators73d <- data.frame(
  EPG = result73d$legislators$EPG,
  coord1D = result73d$legislators$coord1D,
  coord2D = result73d$legislators$coord2D,
  coord3D = result73d$legislators$coord3D

)
result74d<-wnominate(rc7,dims=4,polarity=c(1,1,1,1))
summary(result74d) 

legislators74d <- data.frame(
  EPG = result74d$legislators$EPG,
  coord1D = result74d$legislators$coord1D,
  coord2D = result74d$legislators$coord2D,
  coord3D = result74d$legislators$coord3D,
  coord4D = result74d$legislators$coord3D

)

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


write.csv(legislators7, file = here("Results", "EP7", "wnominate7.csv"), row.names = FALSE)
write.csv(legislators71d, file = here("Results", "EP7", "wnominate71d.csv"), row.names = FALSE)
write.csv(legislators73d, file = here("Results", "EP7", "wnominate73d.csv"), row.names = FALSE)
write.csv(legislators74d, file = here("Results", "EP7", "wnominate74d.csv"), row.names = FALSE)


ggsave(filename = here("Results", "EP7", "W_Nominate_Plot_EP7_1d.png"), plot = p71d)
mepsEP8<-read.csv(here("Cleaned_data","EP8_clean_data","mep_info_for_wnominate.csv"),header=TRUE,strip.white=TRUE)
votesEP8 <- read.csv(here("Cleaned_data","EP8_clean_data","matrix_ep8_votes.csv"),header = TRUE, strip.white = TRUE)

names8 <- mepsEP8[,1]
legData8 <- matrix(mepsEP8[,2],length(mepsEP8[,2]),1)
colnames(legData8) <- "EPG"

rc8 <- rollcall(votesEP8, yea = 1 , nay = 2 , missing = c(3,4,5,6),notInLegis = 0,legis.names=names8,legis.data = legData8,desc="EP8")
EPG8 <- mepsEP8$EPG
votesEP8[] <- lapply(votesEP8, function(x) ifelse(x %in% 0:3, x, 3))
rc8 <- rollcall(votesEP8, yea = 1 , nay = 2 , missing = 3,notInLegis = 0,legis.names=names8,legis.data = legData8,desc="EP8")

result8<-wnominate(rc8,polarity=c(13,13))
summary(result8) 

legislators8 <- data.frame(
  EPG = result8$legislators$EPG,
  coord1D = result8$legislators$coord1D,
  coord2D = result8$legislators$coord2D
)


result81d <- wnominate(rc8,dims = 1, polarity = c(13))

summary(result81d) 

legislators81d <- data.frame(
  EPG = result81d$legislators$EPG,
  coord1D = result81d$legislators$coord1D
)

result83d <- wnominate(rc8,dims = 3, polarity = c(13,13,13))

summary(result83d) 

legislators83d <- data.frame(
  EPG = result83d$legislators$EPG,
  coord1D = result83d$legislators$coord1D,
  coord2D = result83d$legislators$coord2D,
  coord3D = result83d$legislators$coord3D
)

result84d <- wnominate(rc8,dims = 4, polarity = c(13,13,13,13))

summary(result83d) 

legislators84d <- data.frame(
  EPG = result84d$legislators$EPG,
  coord1D = result84d$legislators$coord1D,
  coord2D = result84d$legislators$coord2D,
  coord3D = result84d$legislators$coord3D,
  coord4D = result84d$legislators$coord4D
)

write.csv(legislators8, file = here("Results", "EP8", "wnominate8.csv"), row.names = FALSE)
write.csv(legislators81d, file = here("Results", "EP8", "wnominate81d.csv"), row.names = FALSE)
write.csv(legislators83d, file = here("Results", "EP8", "wnominate83d.csv"), row.names = FALSE)
write.csv(legislators84d, file = here("Results", "EP8", "wnominate84d.csv"), row.names = FALSE)

mepsEP9<-read.csv(here("Cleaned_data","EP9_clean_data","mep_info_for_wnominate.csv"),header=TRUE,strip.white=TRUE)
votesEP9 <- read.csv(here("Cleaned_data","EP9_clean_data","matrix_ep9_votes.csv"),header = TRUE, strip.white = TRUE)
votesEP9 <- votesEP9[,-1]
names9 <- mepsEP9[,1]
legData9 <- matrix(mepsEP9[,2],length(mepsEP9[,2]),1)
colnames(legData9) <- "EPG"

votesEP9[] <- lapply(votesEP9, function(x) ifelse(x %in% 0:3, x, 3))
rc9 <- rollcall(votesEP9, yea = 1 , nay = 2 , missing = 3,notInLegis = 0,legis.names=names9,legis.data = legData9,desc="EP9")

result9<-wnominate(rc9,polarity=c(1,1))
summary(result9) 

legislators9 <- data.frame(
  EPG = result9$legislators$EPG,
  coord1D = result9$legislators$coord1D,
  coord2D = result9$legislators$coord2D
)
result91d <- wnominate(rc9,dims = 1, polarity = c(1))

summary(result91d) 

legislators91d <- data.frame(
  EPG = result91d$legislators$EPG,
  coord1D = result91d$legislators$coord1D
)
result93d <- wnominate(rc9,dims = 3, polarity = c(13,13,13))

summary(result93d) 

legislators93d <- data.frame(
  EPG = result93d$legislators$EPG,
  coord1D = result93d$legislators$coord1D,
  coord2D = result93d$legislators$coord2D,
  coord3D = result93d$legislators$coord3D
)

result94d <- wnominate(rc9,dims = 4, polarity = c(13,13,13,13))

summary(result93d) 

legislators94d <- data.frame(
  EPG = result94d$legislators$EPG,
  coord1D = result94d$legislators$coord1D,
  coord2D = result94d$legislators$coord2D,
  coord3D = result94d$legislators$coord3D,
  coord4D = result94d$legislators$coord4D
)
write.csv(legislators9, file = here("Results", "EP9", "wnominate9.csv"), row.names = FALSE)
write.csv(legislators91d, file = here("Results", "EP9", "wnominate91d.csv"), row.names = FALSE)
write.csv(legislators93d, file = here("Results", "EP9", "wnominate93d.csv"), row.names = FALSE)
write.csv(legislators94d, file = here("Results", "EP9", "wnominate94d.csv"), row.names = FALSE)
