pacman::p_load(wnominate,dplyr,pscl,ggplot2,here,remotes,pkgbuild,emIRT)
mepsEP6<-read.csv(here("Cleaned_data","EP6_clean_data","mep_info_for_wnominate.csv"),header=TRUE,strip.white=TRUE)
votesEP6 <- read.csv("./Cleaned_data/EP6_clean_data/wnominate_ep6_votes.csv",header = TRUE, strip.white = TRUE)
names <- mepsEP6[,1]
legData <- matrix(mepsEP6[,2],length(mepsEP6[,2]),1)
colnames(legData) <- "EPG"
rc2 <- rollcall(votesEP6, yea = 1 , nay = 2 , missing = 3,notInLegis = 0,legis.names=names,legis.data = legData,desc="EP6")
kpascore <- function(LDATA) {
  # Determine dimensions
  NP <- nrow(LDATA)
  NRCALL <- ncol(LDATA)
  
  # Initialize arrays
  XCOL <- numeric(NRCALL)
  XROW <- numeric(NP)
  KROW <- integer(NP)
  ROWMEAN <- numeric(NP)
  XAGREE <- matrix(0.0, nrow=NP, ncol=NP)
  YCENTER <- matrix(0.0, nrow=NP, ncol=NP)
  DSTAR <- matrix(0.0, nrow=NP, ncol=NP)
  
  # Map values
  LDATA[LDATA == 2] <- 6
  LDATA[LDATA == 3] <- 9
  
  # Calculate roll call means
  for (J in 1:NRCALL) {
    non_zero <- !is.na(LDATA[, J]) & LDATA[, J] != 0
    KROW[non_zero] <- KROW[non_zero] + 1
    XROW[LDATA[, J] == 1 & !is.na(LDATA[, J])] <- XROW[LDATA[, J] == 1 & !is.na(LDATA[, J])] + 1
    SUM <- sum(LDATA[, J] == 1, na.rm = TRUE)
    KK <- sum(non_zero)
    XCOL[J] <- if (KK == 0) 0 else SUM / KK
  }
  
  # Calculate participant means
  XROW <- XROW / KROW
  
  # Compute Heckman-Snyder covariance matrix
  for (I in 1:NP) {
    for (J in 1:NP) {
      non_zero <- (LDATA[I, ] != 0) & (LDATA[J, ] != 0) & !is.na(LDATA[I, ]) & !is.na(LDATA[J, ])
      KK <- sum(non_zero)
      KKK <- sum(LDATA[I, non_zero] == LDATA[J, non_zero])
      if (KK == 0) {
        XAGREE[I, J] <- 0.25
        DSTAR[I, J] <- 1.0
      } else {
        XAGREE[I, J] <- (1.0 - (KKK / KK))^2
        DSTAR[I, J] <- (100.0 - (KKK / KK) * 100.0) / 50.0
      }
    }
    ROWMEAN[I] <- mean(XAGREE[I, ], na.rm = TRUE)
  }
  
  ALLMEAN <- mean(ROWMEAN, na.rm = TRUE)
  
  # Setup double-centered agreement score matrix
  for (I in 1:NP) {
    for (J in 1:NP) {
      YCENTER[I, J] <- (XAGREE[I, J] - ROWMEAN[I] - ROWMEAN[J] + ALLMEAN) / (-2.0)
    }
  }
  
  # Eigenvector-Eigenvalue decomposition
  eig_decomp <- eigen(YCENTER)
  WVEC2 <- eig_decomp$values
  ZMAT2 <- eig_decomp$vectors
  
  # Perform the matrix operation to calculate XDATA without nested loops
  # Create a matrix of the sqrt of the absolute values of WVEC2, repeated for each row
  sqrt_abs_WVEC2 <- matrix(sqrt(abs(WVEC2[NP:1])), nrow=NP, ncol=NP, byrow=TRUE)
  
  # Reverse the columns of ZMAT2 to match the order NP+1-K
  ZMAT2_reversed <- ZMAT2[, NP:1]
  
  # Calculate XDATA
  XDATA <- ZMAT2_reversed * sqrt_abs_WVEC2
  
  # Return values
  return(list(ZMAT2 = ZMAT2, WVEC2 = WVEC2, DSTAR = DSTAR, XDATA = XDATA))
}

# STATKP Subroutine
statkp <- function(NP, NS, DSTAR, ZZZ, XX) {
  SSE <- 0.0
  ASUM <- 0.0
  BSUM <- 0.0
  CSUM <- 0.0
  DSUM <- 0.0
  ESUM <- 0.0
  KK <- 0
  
  for (I in 1:NP) {
    for (J in 1:I) {
      if (I == J || DSTAR[I, J] == 99.0) next
      KK <- KK + 1
      AA <- if (NS == 1) abs(ZZZ[I] - ZZZ[J]) else sqrt(sum((XX[I, 1:NS] - XX[J, 1:NS])^2))
      ASUM <- ASUM + AA
      BSUM <- BSUM + DSTAR[I, J]
      CSUM <- CSUM + AA^2
      DSUM <- DSUM + DSTAR[I, J]^2
      ESUM <- ESUM + AA * DSTAR[I, J]
      SSE <- SSE + (DSTAR[I, J] - AA)^2
    }
  }
  
  AA <- KK * ESUM - ASUM * BSUM
  BB <- KK * CSUM - ASUM * ASUM
  CC <- KK * DSUM - BSUM * BSUM
  RRSQ <- (AA^2) / (BB * CC)
  
  return(list(SSE = SSE, RRSQ = RRSQ, KK = KK))
}

# FOCUSW Subroutine
focusw <- function(NP, NPQ, II, D, X, Z) {
  LL <- order(X[1:NPQ])
  Q <- X[LL]
  
  valid_indices <- which(D[LL] != 99.0)
  XX1 <- Q[valid_indices] - D[LL[valid_indices]]
  XX2 <- Q[valid_indices] + D[LL[valid_indices]]
  WWSUM <- length(valid_indices)
  ASUM <- sum(XX1)
  BSUM <- sum(XX1^2)
  
  AA <- WWSUM * BSUM - ASUM * ASUM
  KK <- 1
  
  for (I in valid_indices) {
    ASUM <- ASUM - XX1[I] + XX2[I]
    BSUM <- BSUM - XX1[I]^2 + XX2[I]^2
    BB <- WWSUM * BSUM - ASUM * ASUM
    CC <- min(AA, BB)
    if (abs(CC - AA) <= 0.00001 && KK > 1) break
    if (abs(CC - AA) <= 0.00001 && KK == 1) {
      Z[II] <- (ASUM + XX1[I] - XX2[I]) / WWSUM
    }
    if (abs(CC - BB) <= 0.00001) {
      Z[II] <- ASUM / WWSUM
    }
    AA <- CC
    KK <- KK + 1
  }
  
  return(Z)
}

# FOCUS Subroutine
focus <- function(NP, NPQ, NS, II, D, XX, XXXX) {
  ZZZ <- numeric(NS)
  KK <- 0
  valid_indices <- which(D[1:NPQ] != 99.0)
  
  for (J in valid_indices) {
    KK <- KK + 1
    SUM <- sum((XXXX[J, 1:NS] - XX[II, 1:NS])^2)
    XC <- if (SUM == 0.0) 1.0 else D[J] / sqrt(SUM)
    ZZZ <- ZZZ + XXXX[J, 1:NS] + XC * (XX[II, 1:NS] - XXXX[J, 1:NS])
  }
  
  if (KK == 0) {
    stop(paste("THIS IS YOUR PROBLEM STUPID!!!", II))
  }
  
  XX[II, 1:NS] <- ZZZ / KK
  return(XX)
}

# Process Result Function
process_result <- function(result) {
  ZMAT2 <- result$ZMAT2
  WVEC2 <- result$WVEC2
  DSTAR <- result$DSTAR
  XDATA <- result$XDATA
  
  NP <- nrow(XDATA)
  NS <- ncol(XDATA)
  NPQ <- NP - 1
  
  if (NS == 1) {
    ZZZ <- XDATA[, 1]
  } else {
    ZZZ <- ZMAT2[, 1] * sqrt(abs(WVEC2[1]))
  }
  
  statkp_result <- statkp(NP, NS, DSTAR, ZZZ, XDATA)
  SSE1 <- statkp_result$SSE
  RRSQ <- statkp_result$RRSQ
  KK <- statkp_result$KK
  
  DAT <- numeric(20)
  DAT[1] <- SSE1
  II <- 0
  AKKK <- 0.0
  if (SSE1 <= 0.001) {
    SSE2 <- 0.0
    return(list(XDATA = XDATA, SSE2 = SSE2, KTP = KTP))
  }
  
  for (II in 1:10) {
    KTP <- II
    for (J in 1:NP) {
      NPJ <- J
      rows_to_keep <- setdiff(1:NP, NPJ)
      XXXX <- XDATA[rows_to_keep, ]
      SAVEZ <- ZZZ[rows_to_keep]
      SAVED <- DSTAR[NPJ, rows_to_keep]
      
      if (NS == 1) {
        ZZZ <- focusw(NP, NPQ, NPJ, SAVED, SAVEZ, ZZZ)
      } else {
        XDATA <- focus(NP, NPQ, NS, NPJ, SAVED, XDATA, XXXX)
      }
    }
    statkp_result <- statkp(NP, NS, DSTAR, ZZZ, XDATA)
    SSE2 <- statkp_result$SSE
    RRSQ <- statkp_result$RRSQ
    KK <- statkp_result$KK
    
    DAT[II + 1] <- SSE2
    if (SSE2 == 0.0) break
    AKKK <- (DAT[II] - DAT[II + 1]) / DAT[II]
    if (AKKK <= 0.001) break
  }
  
  ZZZ <- ZZZ - mean(ZZZ)
  
  XDATA <- sweep(XDATA, 2, colSums(XDATA) / NP, "-")
  BB <- apply(XDATA, 2, function(col) max(abs(col)))
  XDATA <- sweep(XDATA, 2, BB, "/")
  
  XDATA <- sweep(XDATA, 2, colSums(XDATA) / NP, "-")
  BB <- max(rowSums(XDATA^2))
  XDATA <- XDATA / sqrt(BB)
  
  XDATA3 <- XDATA
  XMAT0 <- XDATA
  
  return(list(XDATA = XDATA, XDATA3 = XDATA3, XMAT0 = XMAT0, SSE2 = SSE2, KTP = KTP))
}





map_values <- function(matrix_input) {
  # Map values
  matrix_input[matrix_input == 0] <- 9
  matrix_input[matrix_input == 1] <- 1
  matrix_input[matrix_input == 2] <- 6
  matrix_input[matrix_input == 3] <- 9
  matrix_input[matrix_input == 4] <- 9
  matrix_input[matrix_input == 5] <- 9
  matrix_input[matrix_input == 6] <- 9
  
  return(matrix_input)
}
LDATA <- map_values(rc2$votes)
resultkpascore <-kpascore(LDATA)


v <- resultkpascore$XDATA[,1]
v <- head(v, 940)
v <- matrix(v, nrow=940, ncol=1)
s2 <- s
s2$x <- v
namevector <- c(names)
agreementmatrix <- resultkpascore$DSTAR
rownames(agreementmatrix) <- namevector
colnames(agreementmatrix) <- namevector

write.csv(agreementmatrix,file = here("Results","Agreement_matrix.csv"),row.names = TRUE)

resultEMeigenvectorstarts <- binIRT(.rc = rcEM6,
                   .starts = s2,
                   .priors = p6,
)                          
legislatorsem <- data.frame(
  EPG <- EPG6,
  coord1d <- (resultEMeigenvectorstarts$means$x)
)
legislatorsem$y <- 0

ggplot(legislatorsem, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "1 dimension emIRT",
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
processedresult <- process_result(resultkpascore)

v2 <- processedresult$XDATA[,1]
v2 <- head(v, 940)
v2 <- matrix(v, nrow=940, ncol=1)
s3 <- s
s3$x <- v2

resultEMeigenvectorstarts2 <- binIRT(.rc = rcEM6,
                                    .starts = s3,
                                    .priors = p6,
)                          
legislatorsem2 <- data.frame(
  EPG <- EPG6,
  coord1d <- (resultEMeigenvectorstarts2$means$x)
)
legislatorsem2$y <- 0

ggplot(legislatorsem2, aes(x = d1, y = y, color = EPG, label = EPG)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "1 dimension emIRT",
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
