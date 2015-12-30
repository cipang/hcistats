factor.freq <- function(input) {
  t <- table(input)
  p <- prop.table(t)
  result <- data.frame(names(t),
                       as.vector(t),
                       sprintf("%.1f%%", as.vector(p) * 100),
                       as.vector(p))
  colnames(result) <- c("Val", "N", "Percent", "RawPct")
  return(result)
}

basic.desc <- function(input) {
  m <- mean(input)
  stdev <- sd(input)
  med <- median(input)
  vmin <- min(input)
  vmax <- max(input)
  result <- c(m, stdev, med, vmin, vmax)
  names(result) <- c("Mean", "StDev", "Median", "Min", "Max")
  return(result)
}

library(coin)
wtest <- function(a, b) {
  t <- wilcoxsign_test(b ~ a, distribution="exact")
  z <- statistic(t)
  p <- pvalue(t)
  effect_size <- z / sqrt(length(a) + length(b))
  result <- c(z, p, effect_size)
  names(result) <- c("Z", "p-value", "EffectSize")
  return(result)
}

# Copy to Mac OS X clipboard.
xcopy <- function(table) {
  clip <- pipe("pbcopy", "w")
  write.table(table, file=clip, sep="\t", quote=FALSE)
  close(clip)
}

report <- function(d) {
  print.table <- function(obj) {
    write.table(obj, sep="\t", quote=F, col.names=NA)
  }
  
  cat(sprintf("N = %d\n", length(d[[1]])))
  cat("\nGender\n")
  print.table(factor.freq(d$Gender))
  cat("\nAge\n")
  print.table(basic.desc(d$Age))
  cat("\nOccupation\n")
  print.table(factor.freq(d$S3))
  
  cat("\nQ1")
  for (i in LETTERS[1:4]) {
    f <- factor.freq(d[[paste0("Q1_", i)]])
    cat("\nTask", i, "\n")
    print.table(f)
  }

  # Descriptive stats.
  q <- c("RealPage", "RealLink", "TimeinSec", "QuerySlider", "RecomClick", "Filter",
         "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10",
         "Q11", "Q12", "Q13", "Q14", "Q15")
  g <- LETTERS[1:4]
  m <- matrix(NA, nrow=length(q), ncol=length(g) * 3)
  x <- 1
  y <- 1
  for (i in 1:length(q)) {
    for (j in 1:length(g)) {
      vec <- d[[paste0(q[i], "_", g[j])]]
      m[x, y] <- mean(vec)
      m[x, y+1] <- sd(vec)
      m[x, y+2] <- median(vec)
      y <- y + 3
    }
    x <- x + 1
    y <- 1
  }
  rownames(m) <- q
  colnames(m) <- c("A_M", "A_SD", "A_Med",
                   "B_M", "B_SD", "B_Med",
                   "C_M", "C_SD", "C_Med",
                   "D_M", "D_SD", "D_Med")
  cat("\nDescriptive Statistics\n")
  print.table(m)
  
  # W-Test.
  q <- c("RealPage", "RealLink", "TimeinSec", "QuerySlider", "RecomClick", "Filter",
         "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10",
         "Q11", "Q12", "Q13", "Q14", "Q15")
  g <- LETTERS[1:4]
  m <- matrix(NA, nrow=length(q), ncol=2 * 3)
  x <- 1
  y <- 1
  test_func <- wtest
  for (i in 1:length(q)) {
    for (j in 1:2) {
      n1 <- paste0(q[i], "_", g[(j-1)*2 + 1])
      n2 <- paste0(q[i], "_", g[(j-1)*2 + 2])
      v1 <- d[[n1]]
      v2 <- d[[n2]]
      r = test_func(v1, v2)
      m[x, y] <- r[1]  # statistic
      m[x, y+1] <- r[2]   # p-value
      m[x, y+2] <- r[3]   # effect size
      y <- y + 3
    }
    x <- x + 1
    y <- 1
  }
  rownames(m) <- q
  colnames(m) <- c("AB_Z", "AB_p", "AB_ES",
                   "CD_Z", "CD_p", "CD_ES")
  cat("\nWilcoxon Test Results\n")
  print.table(m)
}

# Load data into a variable.
#setwd("~/Downloads")
#d = read.csv(file="24.csv")

# Run report function.
#sink("study_result_output.txt")     # Output to a file if you wish.
#report(d)
#sink()
