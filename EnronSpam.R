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

## Create vector of probabilities of a word being in a spam email
w <- list()
w0 <- list()
tr <- createThetaDF(SpamDictionary, Nspam, class=1)
thetaSpam <- tr[["theta"]]
w[["spam"]] <- tr[["w"]]
w0[["spam"]] <- tr[["w0"]]

## Create vector of probabilities of being ham email
tr <- createThetaDF(SpamDictionary, Nham, class=2)
thetaHam <- tr[["theta"]]
w[["ham"]] <- tr[["w"]]
w0[["ham"]] <- tr[["w0"]]
rm(tr)

# Need to:
#  *Open email

# Extract sample of emails
# Need to figure out why this does not output probabilities
n = 100 
email_list <- sampleEmails(n, data_path)

#xr <- list()
xr <- rep(NA, length(rownames(SpamDictionary)))
guess <- rep(NA, n)
spam_prob <- rep(NA, n)
cutoff <- 0.5

# Work down email_list
#for(fp in email_list[["files"]]) {
for(i in 1:length(email_list$files)) {
  fp <- email_list$files[i]  


  # No denominator here, just flipping 0's to 1's for words found in email.
  # Need to use a consistent stop_words list to be sure the counts are the same.
  # Save into the dictionary rdata file.
  #xr[[i]] <- loadEmailDF(fp, SpamDictionary, stop_words)
  xr <- loadEmailDF(fp, SpamDictionary, stop_words)

  # xr is vector for this individual email, now need to do Bayes using it
  # and our pre-calculated values above.

  # p(spam|word) = (p(word|spam)*p(spam))/p(word)
  # log(p(word|spam) = sum(x * w + w0) from above with x and w being vectors.

  # Numbers are too big or too small when run through exp() to be 
  # probabilities.
  #log_word_spam <- (t(xr[[i]]) %*% w$spam + w0$spam)[1,1]
  #log_word_ham <- (t(xr[[i]]) %*% w$ham + w0$ham)[1,1]
  log_word_spam <- (t(xr) %*% w$spam + w0$spam)[1,1]
  log_word_ham <- (t(xr) %*% w$ham + w0$ham)[1,1]
  log_spam <- log(Pspam)
  log_ham <- log(Pham)

  print(fp)
  #cat("log(p(word|spam)):",log_word_spam, "\n")
  #cat("log(p(word|ham)):", log_word_ham, "\n")
  # log identity: log(a + b) = log(a) + log(1 + exp(log(b) - log(a)))
  # formula being solved:
  # log(p(spam|word)) = log(p(word|spam)p(spam)/
  #        (p(word|spam)p(spam) + p(word|ham)p(ham)))

# Work this through again to be sure you have it right.
  logSpamGivenX <- -( log(1 + exp(log_word_ham + log_ham - log_word_spam - log_spam)) )
  p_spam_word <- exp(logSpamGivenX)
  cat("p(spam|word):", p_spam_word, "\n")
  # Save probability so we can check model
  spam_prob[i] <- p_spam_word
  # What model guesses. Might be slicker way to do this.
  if(p_spam_word >= cutoff){
    guess[i] <- 1
  } else {
    guess[i] <- 0
  }

}


print(email_list$c == guess)

# Try to plot error at different cutoffs
xx <- seq(0,1, length=100)
err <- rep(NA, length(xx))
for(i in 1:length(xx)) {
  err[i] <- sum((spam_prob > xx[i]) != email_list$c)
}

plot(xx, err, pch=19, xlab="Cutoff", ylab="Error")

