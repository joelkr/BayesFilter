# This will work with the dataframe version of the dictionary
# Load dataframe dictionary
data_path <- '../../doing_data_science/datasets/enron1/trial'
save_path <- '../../doing_data_science/BayesSpamFilter/git'
file_path <- '../../doing_data_science/BayesSpamFilter/git'

filename <- paste(file_path, "spamDictionaryDF.Rdata", sep="/")
load(filename)

# Load functions
source("EnronSpamFunctions.R")

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
n = 10 # Number of emails in trial directory
email_list <- sampleEmails(n, data_path)

xr <- list()

# Work down email_list
#for(fp in email_list[["files"]]) {
for(i in 1:length(email_list$files)) {
  fp <- email_list$files[i]  


  # No denominator here, just flipping 0's to 1's for words found in email.
  # Need to use a consistent stop_words list to be sure the counts are the same.
  # Save into the dictionary rdata file.
  xr[[i]] <- loadEmailDF(fp, SpamDictionary, stop_words)


  # xr is vector for this individual email, now need to do Bayes using it
  # and our pre-calculated values above.

  # p(spam|word) = (p(word|spam)*p(spam))/p(word)
  # log(p(word|spam) = sum(x * w + w0) from above with x and w being vectors.

  # Numbers are too big or too small when run through exp() to be 
  # probabilities.
  log_word_spam <- (t(xr[[i]]) %*% w$spam + w0$spam)[1,1]
  log_word_ham <- (t(xr[[i]]) %*% w$ham + w0$ham)[1,1]
  log_spam <- log(Pspam)
  log_ham <- log(Pham)

  print(fp)
  cat("log(p(word|spam)):",log_word_spam, "\n")
  cat("log(p(word|ham)):", log_word_ham, "\n")
  # log identity: log(a + b) = log(a) + log(1 + exp(log(b) - log(a)))
  # formula being solved:
  # log(p(spam|word)) = log(p(word|spam)p(spam)/
  #        (p(word|spam)p(spam) + p(word|ham)p(ham)))

# Work this through again to be sure you have it right.
  p_spam_word <- exp( log(1 + exp((log_word_ham+log_ham) - (log_word_spam+log_spam))) )
  cat("p(spam|word):", p_spam_word, "\n")
  cat("+++++++++++++++++++\n")
  cat("Try another way....\n")
  pxs <- pxGivenC(thetaSpam, xr[[i]])
  pxh <- pxGivenC(thetaHam, xr[[i]])
  pspam_given_x <- pxs * Pspam / (pxs * Pspam + pxh * Pham)
  cat("p(x|s): ", pxs, "\n")
  cat("p(x|h): ", pxh, "\n")
  cat("pspam_given_x: ", pspam_given_x, "\n")
  cat("-----------------\n")

  # p(spam) is Pspam from the dictionary script, but I did not save it.
  # p(word) should be (D[["word"]][2])/sum(all counts in D)

}

# Try summing probabilities. Some way or other this should sum to 1.
p <- rep(0, length(thetaSpam))
for(i in 1:length(email_list$c)) {
  p <- p + thetaSpam * xr[[i]] * email_list$c[i]
}
