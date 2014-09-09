#
# Paths for data and file save directories
#data_path <- '../datasets/enron1'
data_path <- '../../doing_data_science/datasets/enron1/'
save_path <- '../../doing_data_science/BayesSpamFilter/git'
file_path <- '../../doing_data_science/BayesSpamFilter/git'

# List of words to ignore. Add or remove as needed
stop_words <- c('the','and','a','in','on','is','of','subject',
                'to','at', ""
)

# Read file in as individual words.
scanFile <- function(fp) {
  words <- scan(fp, character(0), quote="", quiet=T)
  return(words)
}

# This function calculates p(x|c) by the initial formula rather than
# log(p(x|c))
pxGivenC <- function(theta, x) {
  not_x <- 1 - x
  not_theta <- 1 - theta
  pxc <- 1
  for(i in length(x)) {
    pxc <- theta[i]^x[i] * not_theta[i]^not_x[i]
  }
  return(pxc)
}

# We might be changing words in dictionary so need to remove everything
# not in dictionary. Only words that occur some min. times should be scored.
#loadEmail <- function(fp, x, dict_words) {
loadEmail <- function(fp, dict_words, stop_words) {
  x <- rep(0, length(names(dict_words)))
  # Assign names to x
  names(x) <- names(dict_words)
  words <- scan(fp, character(0), quote="", quiet=T)
  words <- removePunct(words, stop_words)
  words <- words[words %in% names(dict_words)]
  x[words] <-1
# Send this to dictionary eventually
#  words[!(words %in% names(x))]
  return(x)
}

loadEmailDF <- function(fp, SpamDictionary, stop_words) {
  # Names are first column of dataframe. Right now they are a factor.
  email_words <- rownames(SpamDictionary)
  x <- rep(0, length(email_words))
  # Assign names to x
  names(x) <- email_words
  words <- scan(fp, character(0), quote="", quiet=T)
  words <- removePunct(words, stop_words)
  words <- words[words %in% email_words]
  x[words] <-1
  # Send this to dictionary eventually
  #  words[!(words %in% names(x))]
  return(x)
}

# stop_words might change counts if the list is not the same as used for
# dictionary creation.
removePunct <- function(w,  stop_words) {
  w <- gsub("[\\W\\d]", " ", w, perl=T)
  w <- tolower(w)
  w <- strsplit(w, " +", perl=T)
  w <- gsub("\\W", "", w, perl=T)
  w <- unique(w)
  w <- w[!(w == "")]
  # Might want to check against stop words list.
  w <- w[!(w %in% stop_words)]
  # Would need to return list or write to global for word count.
  # wc <- wc + length(w)
  return(w)
}

# Sample emails: create a random list of emails to test
# Ratio should be about Nspam/Nham and should be random n emails.
sampleEmails <- function(n, data_path) {
  # This might be better to pass in so it could be centralized.
  #data_path <- '../datasets/enron1'
  #data_path <- '../../doing_data_science/datasets/enron1/trial'
  spam_path <- paste(data_path, "spam", sep="/")
  spam_files <- list.files(path=spam_path, pattern="*.txt", full.names=T)
  ham_path <- paste(data_path, "ham", sep="/")
  ham_files <- list.files(path=ham_path, pattern="*.txt", full.names=T)

  spam_list <- sample(spam_files, n)
  ham_list <- sample(ham_files, n)
  file_list <- c(spam_list, ham_list)
  file_list <- sample(file_list, size=length(file_list), replace=F)
  spam <- rep(0, length(file_list))
  spam[grepl("spam", file_list)] <- 1
  f <- list(files=file_list, c=spam)
  return(f)
}

## Need to remove words that don't occur more than 5 times to clean things
## up
removeLowCounts <- function(D,  low_count=5) {
  # Current dictionary format:
  # [spam_count, ham_count, total_count, in_english_wordlist]
  word_count <- 0
  for( w in names(D)) {
    # Total count is field 3
    if(D[[w]][3] < low_count) {
      word_count <- word_count + D[[w]][3]
      D[[w]] <- NULL
#      cat("removing ", w, "\n")
    }
  }
  r <- list(dictionary=D, words_removed=word_count) 
  return(r)
}
createTheta <- function(D, Nclass, class=1) {
  # D = list of spam words and counts, Nclass = count of spam or ham emails
  # class = spam 1 or ham 2
  # Need to change dictionary to keep separate counts for each class.
  alpha <- 1
  beta <- 2
  theta_c <- rep(0, length(D))
  for(i in 1:length(D)) {
    # This has a denominator. If the counts are bad, or the number of spam
    # and ham emails are bad, probabilities will not sum to 1.
    theta_c[i] <- (D[[i]][class] + alpha) / (Nclass + beta)
  }
  names(theta_c) <- names(D)
  # Precalculate independent parts of formula
  # This formula has a minor problem of having many values of Inf where the
  # probability of spam is 1.0 so it actually seems to need Laplace smoothing
  # to work at all.
  ## This also has a denominator, but we should have baked the error in by
  ## this point unless my notation is wrong.
  w <- log(theta_c/(1 - theta_c))
  w0 <- sum(log(1-theta_c))
  r <- list(theta=theta_c, w=w, w0=w0)
  return(r)
}

createThetaDF <- function(SpamDictionary, Nclass, class=1, alpha=1, beta=2) {
  # D = list of spam words and counts, Nclass = count of spam or ham emails
  # class = spam 1 or ham 2
  # Need to change dictionary to keep separate counts for each class.
  #alpha <- 1
  #beta <- 2
  class <- class + 1 # First column is the words
  email_words <- rownames(SpamDictionary)
  theta_c <- rep(0, length(email_words))
  # This should be done with sapply()
  for(i in 1:length(email_words)) {
    # This has a denominator. If the counts are bad, or the number of spam
    # and ham emails are bad, probabilities will not sum to 1.
    theta_c[i] <- (SpamDictionary[i, class] + alpha) / (Nclass + beta)
  }
  names(theta_c) <- names(email_words)
  # Precalculate independent parts of formula
  # This formula has a minor problem of having many values of Inf where the
  # probability of spam is 1.0 so it actually seems to need Laplace smoothing
  # to work at all.
  ## This also has a denominator, but we should have baked the error in by
  ## this point unless my notation is wrong.
  w <- log(theta_c/(1 - theta_c))
  w0 <- sum(log(1-theta_c))
  r <- list(theta=theta_c, w=w, w0=w0)
  return(r)
}

loadSetup <- function() {
  load("spamDict2.Rdata")
  dr <- removeLowCounts(D)
  D <- dr[["dictionary"]]
  WORDCOUNT <- WORDCOUNT - dr[["words_removed"]]
  tr <- createTheta(D, Nspam)
  w <- tr[["w"]]
  w0 <- tr[["w0"]]
  theta <- tr[["theta"]]
  save(D, WORDCOUNT, Nspam, Nham, Pspam, Pham, stop_words, file="spamShortDictionary.Rdata")
  r <- list(dictionary=D, word_count=WORDCOUNT, theta=theta, w=w, w0=w0,
            Nspam=Nspam, Nham=Nham, Pspam=Pspam, Pham=Pham)
  return(r)
}

# Pass in list of emails, dictionary, stop words, w, w0 and output vector
# of 1's and 0's email is spam = 1. Use to check accuracy and establish
# optimal values for Laplace smoothing and cutoff.
spamProb <- function(eml, SpamDictionary, stop_words, w, w0)  {
  spam_prob <- rep(NA, length(eml))
  for(i in 1:length(eml)) {
    fp <- eml[i]  
    
    xr <- loadEmailDF(fp, SpamDictionary, stop_words)
    
    # p(spam|word) = (p(word|spam)*p(spam))/p(word)
    # log(p(word|spam) = sum(x * w + w0) from above with x and w being vectors.
    
    log_word_spam <- (t(xr) %*% w$spam + w0$spam)[1,1]
    log_word_ham <- (t(xr) %*% w$ham + w0$ham)[1,1]
    log_spam <- log(Pspam)
    log_ham <- log(Pham)
    
    #print(fp)
    #cat("log(p(word|spam)):",log_word_spam, "\n")
    #cat("log(p(word|ham)):", log_word_ham, "\n")
    # log identity: log(a + b) = log(a) + log(1 + exp(log(b) - log(a)))
    # formula being solved:
    # log(p(spam|word)) = log(p(word|spam)p(spam)/
    #        (p(word|spam)p(spam) + p(word|ham)p(ham)))
    
    # Work this through again to be sure you have it right.
    logSpamGivenX <- -( log(1 + exp(log_word_ham + log_ham - log_word_spam - log_spam)) )
    p_spam_word <- exp(logSpamGivenX)
    #cat("p(spam|word):", p_spam_word, "\n")
    # Save probability so we can check model
    spam_prob[i] <- p_spam_word
  }
  return(spam_prob)
}
