# This will work with the dataframe version of the dictionary
# Load dataframe dictionary
#data_path <- '../../doing_data_science/datasets/enron1/trial'
#save_path <- '../../doing_data_science/BayesSpamFilter/git'
#file_path <- '../../doing_data_science/BayesSpamFilter/git'

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
n = 500
email_list <- sampleEmails(n, data_path)
cutoff <- 0.5
err <- rep(NA, dim(ab)[1])

for(i in 1:dim(ab)[1]) {
  ## Create vector of probabilities of a word being in a spam email
  alpha <- ab[i, 1]
  beta <- ab[i, 2]
  print(ab[i,])
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
minerror <- min(err)
print(ab[c(which(err == minerror)),])

# At this point it seems that alpha should be 0.01 and beta can be anything 
# from 0.01 to 2.0. 500 sample emails might not be enough and there might be
# an interaction between the cutoff and alpha and beta.

# Work down email_list
#for(fp in email_list[["files"]]) {
#for(i in 1:length(email_list$files)) {
#  fp <- email_list$files[i]  
#
#
#  # No denominator here, just flipping 0's to 1's for words found in email.
#  # Need to use a consistent stop_words list to be sure the counts are the same.
#  # Save into the dictionary rdata file.
#  #xr[[i]] <- loadEmailDF(fp, SpamDictionary, stop_words)
#  xr <- loadEmailDF(fp, SpamDictionary, stop_words)
#
#  # xr is vector for this individual email, now need to do Bayes using it
#  # and our pre-calculated values above.
#
#  # p(spam|word) = (p(word|spam)*p(spam))/p(word)
#  # log(p(word|spam) = sum(x * w + w0) from above with x and w being vectors.
#
#  # Numbers are too big or too small when run through exp() to be 
#  # probabilities.
#  #log_word_spam <- (t(xr[[i]]) %*% w$spam + w0$spam)[1,1]
#  #log_word_ham <- (t(xr[[i]]) %*% w$ham + w0$ham)[1,1]
#  log_word_spam <- (t(xr) %*% w$spam + w0$spam)[1,1]
#  log_word_ham <- (t(xr) %*% w$ham + w0$ham)[1,1]
#  log_spam <- log(Pspam)
#  log_ham <- log(Pham)
#
#  print(fp)
#  #cat("log(p(word|spam)):",log_word_spam, "\n")
#  #cat("log(p(word|ham)):", log_word_ham, "\n")
#  # log identity: log(a + b) = log(a) + log(1 + exp(log(b) - log(a)))
#  # formula being solved:
#  # log(p(spam|word)) = log(p(word|spam)p(spam)/
#  #        (p(word|spam)p(spam) + p(word|ham)p(ham)))
#
## Work this through again to be sure you have it right.
#  logSpamGivenX <- -( log(1 + exp(log_word_ham + log_ham - log_word_spam - log_spam)) )
#  p_spam_word <- exp(logSpamGivenX)
#  cat("p(spam|word):", p_spam_word, "\n")
#  # Save probability so we can check model
#  spam_prob[i] <- p_spam_word
#  # What model guesses. Might be slicker way to do this.
#  if(p_spam_word >= cutoff){
#    guess[i] <- 1
#  } else {
#    guess[i] <- 0
#  }
#
#}


#print(email_list$c == guess)
#
## Try to plot error at different cutoffs
#xx <- seq(0,1, length=100)
#err <- rep(NA, length(xx))
#for(i in 1:length(xx)) {
#  err[i] <- sum((spam_prob > xx[i]) != email_list$c)
#}
##
#plot(xx, err, pch=19, xlab="Cutoff", ylab="Error")

