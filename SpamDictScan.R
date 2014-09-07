# SpamDictScan.R
# Read in spam and ham emails record counts of emails contain each word and build a
# dictionary

# read in functions
source("EnronSpamFunctions.R")

#removePunct <- function(w, stop_words) {
#  w <- gsub("[\\W\\d\n\r]", " ", w, perl=T)
#  w <- tolower(w)
#  w <- strsplit(w, " +", perl=T)
#  w <- gsub("\\W", "", w, perl=T)
#  # Need to count word only once per email. Denominator is all spam emails
#  # rather than all words in spam emails.
#  w <- unique(w)
#  w <- w[!(w == "")]
#  # Might want to check against stop words list.
#  w <- w[!(w %in% stop_words)]
#  WORDCOUNT <<- WORDCOUNT + length(w)
#  return(w)
#}


WORDCOUNT <- 0

# Get counts for spam and ham
spam_path <- paste(data_path, "spam", sep="/")
spam_files <- list.files(path=spam_path, pattern="*.txt")
Nspam <- length(spam_files)
ham_path <- paste(data_path, "ham", sep="/")
ham_files <- list.files(path=ham_path, pattern="*.txt")
Nham <- length(ham_files)
Ntot <- Nspam + Nham

# Probabilities for emails being spam and ham
Pspam <- Nspam/(Nspam+Nham)
Pham <- (1-Pspam)


# Try starting from nothing to see if it is faster.
english_words <- vector()
spam_count <- vector()
ham_count <- vector()
total_count <- vector()

for(i in 1:length(spam_files)) {
#for(i in 1:5) {
  fp <- paste(spam_path, spam_files[i], sep="/")
  
  words <- scanFile(fp)
  words <- removePunct(words, stop_words)
  WORDCOUNT <- WORDCOUNT + length(words)
  d <- words[words %in% english_words]
  nd <- words[!(words %in% english_words)]
  spam_count[d] <- spam_count[d] + 1
  total_count[d] <- total_count[d] + 1

  for(w in nd) {
    english_words[length(english_words) + 1L] <- w
    spam_count[w] <- 1
    total_count[w] <- 1
    ham_count[w] <- 0
    # c(spamcount,hamcount,totalcount,indictionary)
    #D[[w]] <- c(1, 0, 1, 0)
    #cat(w, " NOT in dictionary\n")
  }
  print(fp)
}

for(i in 1:length(ham_files)) {
#for(i in 1:5) {
  fp <- paste(ham_path, ham_files[i], sep="/")
  
  words <- scanFile(fp)
  words <- removePunct(words, stop_words)
  WORDCOUNT <- WORDCOUNT + length(words)
  d <- words[words %in% english_words]
  nd <- words[!(words %in% english_words)]
  ham_count[d] <- ham_count[d] + 1
  total_count[d] <- total_count[d] + 1

  for(w in nd) {
    english_words[length(english_words) + 1L] <- w
    spam_count[w] <- 0
    total_count[w] <- 1
    ham_count[w] <- 1
  }
  print(fp)
}

# Need to alpha sort vectors and purge terms that have less than mincount.
# Maybe sort max to min use?
mincount <- 5  # Pass in or set at top eventually
spam_count <- spam_count[total_count >= mincount]
ham_count <- ham_count[total_count >= mincount]
english_words <- english_words[total_count >= mincount]
total_count <- total_count[total_count >= mincount]
# create a dataframe from the vectors
SpamDictionary <- data.frame(english_words, spam_count, ham_count, total_count, stringsAsFactors=FALSE)

filename <- paste(save_path, "spamDictionaryDF.Rdata", sep="/")
save(SpamDictionary, Nspam, Nham, stop_words, WORDCOUNT, file=filename)
