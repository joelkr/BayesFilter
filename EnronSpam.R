
# Load functions
source("EnronSpamFunctions.R")

filename <- paste(file_path, "spamDictionaryDF.Rdata", sep="/")
load(filename)


# Calculate Pspam and Pham
Pspam <- Nspam/(Nspam + Nham)
Pham <- (1 - Pspam)

# Will need to do cross-fold validation to set alpha and beta, so we will need a complete
# list and count of training files.
#
# Probably can't cross-fold without rebuilding dictionary every time. Can check accuracy
# with different alpha and beta against training data.
sfp <- paste(data_path, "spam", sep="/")
sfiles <- list.files(sfp)
hfp <- paste(data_path, "ham", sep="/")
hfiles <- list.files(hfp)
train_data <- sample(c(sfiles, hfiles))
rm(sfp, sfiles, hfp, hfiles)

# Alpha could range from 0-1 and beta from 0-2. Cartesian product might be possible.
a <- seq(0.01, 1, length=10)
b <- seq(0.01, 2, length=10)
# Dataframe of cartesian product
ab <- expand.grid(a, b)
n = 50
email_list <- sampleEmails(n, data_path)
# Original cutoff the obvious 0.5. This is the best value by testing.
cutoff <- 0.01
err <- rep(NA, dim(ab)[1])

for(i in 1:dim(ab)[1]) {
  ## Create vector of probabilities of a word being in a spam email
  alpha <- ab[i, 1]
  beta <- ab[i, 2]
  cat(i, " ", alpha, " ", beta, "\n")
  w <- list()
  w0 <- list()
  tr <- createThetaDF(SpamDictionary, Nspam, class=1, alpha=alpha, beta=beta)
  thetaSpam <- tr[["theta"]]
  w[["spam"]] <- tr[["w"]]
  w0[["spam"]] <- tr[["w0"]]

  ## Create vector of probabilities of being ham email
  tr <- createThetaDF(SpamDictionary, Nham, class=2, alpha=alpha, beta=beta)
  thetaHam <- tr[["theta"]]
  w[["ham"]] <- tr[["w"]]
  w0[["ham"]] <- tr[["w0"]]
  #rm(tr)

  #xr <- list()
  #xr <- rep(NA, length(rownames(SpamDictionary)))
  #  guess <- rep(NA, n)
  #spam_prob <- rep(NA, n)

  spam_prob <- spamProb(email_list$files, SpamDictionary, stop_words, w, w0)
  #print(probs)
  err[i] <- sum((spam_prob > cutoff) != email_list$c)
  cat(err[i], "\n")
}

#plot(1:dim(ab)[1], err, pch=19, xlab="ab row", ylab="Error")
minerr <- min(err)
opt_ab <- which(err == minerr)
print(ab[opt_ab,])


# At this point it seems that alpha should be 0.01 and beta can be anything 
# from 0.01 to 2.0. 500 sample emails might not be enough and there might be
# an interaction between the cutoff and alpha and beta.
#
#ctoff <- list()

for(o in opt_ab) {
  alpha <- ab[o, 1]
  beta <- ab[o, 2]
  cat(o, " ", alpha, " ", beta, "\n")
  w <- list()
  w0 <- list()
  tr <- createThetaDF(SpamDictionary, Nspam, class=1, alpha=alpha, beta=beta)
  thetaSpam <- tr[["theta"]]
  w[["spam"]] <- tr[["w"]]
  w0[["spam"]] <- tr[["w0"]]

  ## Create vector of probabilities of being ham email
  tr <- createThetaDF(SpamDictionary, Nham, class=2, alpha=alpha, beta=beta)
  thetaHam <- tr[["theta"]]
  w[["ham"]] <- tr[["w"]]
  w0[["ham"]] <- tr[["w0"]]

  spam_prob <- spamProb(email_list$files, SpamDictionary, stop_words, w, w0)
  ## Try to plot error at different cutoffs
  xx <- seq(0,1, length=100)
  err <- rep(NA, length(xx))
  for(i in 1:length(xx)) {
    err[i] <- sum((spam_prob > xx[i]) != email_list$c)
  }
  print("Probabilty cutoff for min error: ")
  print( xx[which.min(err)] )
#plot(xx, err, pch=19, xlab="Cutoff", ylab="Error")
}

