require("corrplot")

setwd("~/Downloads")
d = read.csv(file="31.csv")

data.b = data.frame(d$Q2_B, d$Q3_B, d$Q4_B, d$Q5_B,
                    d$Q6_B, d$Q7_B, d$Q8_B, d$Q9_B, d$Q10_B,
                    d$Q11_B, d$Q12_B, d$Q13_B, d$Q14_B, d$Q15_B)

data.d = data.frame(d$Q2_D, d$Q3_D, d$Q4_D, d$Q5_D,
                    d$Q6_D, d$Q7_D, d$Q8_D, d$Q9_D, d$Q10_D,
                    d$Q11_D, d$Q12_D, d$Q13_D, d$Q14_D, d$Q15_D)

data.cor = data.d
colnames(data.cor) <- c("Uncertain", "i Design.Helps", "o Knowledge", "o Successful",
                        "i Easy2Tell", "i Diverse", "i Serend.", "i E.Time", "i Enjoyable",
                        "o Engaged", "i Easy", "o Reuse", "o Useful", "o Satisfy")

# Modified from corrplot help docs.
cor.mtest <- function(mat, conf.level = 0.95, method) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level, method=method)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      #lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      #uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

cor.matrix = cor(data.cor, method="spearman")
res1 <- cor.mtest(cor.matrix, 0.95, "spearman")
corrplot(cor.matrix, p.mat=res1[[1]], sig.level = 0.05, insig = "p-value")

# Write correlation table.
# write.table(cor.matrix, sep="\t", quote=FALSE, file="~/Downloads/test.txt")
# write.table(res1[[1]], sep="\t", quote=FALSE, file="~/Downloads/test.txt")
