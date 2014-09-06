scanFile <- function(fp) {
  words <- scan(fp, character(0), quote="", quiet=T)
  return(words)
}

removePunct <- function(w, stop_words) {
  w <- gsub("[\\W\\d\n\r]", " ", w, perl=T)
  w <- tolower(w)
  w <- strsplit(w, " +", perl=T)
  w <- gsub("\\W", "", w, perl=T)
  # Need to count word only once per email. Denominator is all spam emails
  # rather than all words in spam emails.
  w <- unique(w)
  w <- w[!(w == "")]
  # Might want to check against stop words list.
  w <- w[!(w %in% stop_words)]
  WORDCOUNT <<- WORDCOUNT + length(w)
  return(w)
}

#data_path <- '../datasets/enron1'
data_path <- '../../doing_data_science/datasets/enron1/trial'
save_path <- '../../doing_data_science/BayesSpamFilter/git'
file_path <- '../../doing_data_science/BayesSpamFilter/git'
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

# List of words to ignore. Add or remove as needed
stop_words <- c('the','and','a','in','on','is','of','subject',
                'to','at', ""
               )
# Create vectors to store the counts and words for dictionary data frame.
#filename <- paste(file_path, "english_wordlist.txt", sep="/")
#english_words <- scanFile(filename)
#english_words <- tolower(english_words)
#spam_count <- rep(0, length(english_words))
#ham_count <- rep(0, length(english_words))
#total_count <- rep(0, length(english_words))
#names(spam_count) <- english_words
#names(ham_count) <- english_words
#names(total_count) <- english_words

# Try starting from nothing to see if it is faster.
english_words <- vector()
spam_count <- vector()
ham_count <- vector()
total_count <- vector()

#if(file.exists("EnglishDict.Rdata")) {
#  print("file present")
#  filename <- paste(file_path, "EnglishDict.Rdata", sep="/")
#  load(filename)
#  D <- EnglishDict
#  english_words <- names(EnglishDict)
#  rm(EnglishDict)
# } else {
# print("not present")
##  # Try building this from a dictionary of the english language
# filename <- paste(file_path, "english_wordlist.txt", sep="/")
# english_words <- scanFile(filename)
# english_words <- tolower(english_words)
# EnglishDict <- list()
# for(w in english_words) {
#   EnglishDict[[w]] <- c(0, 0, 0, 1)
#   cat(".")
# }
#  filename <- paste(save_path, "EnglishDict.Rdata", sep="/")
#  save(EnglishDict, file=filename)
#  D <- EnglishDict
#  rm(EnglishDict)
#}

for(i in 1:length(spam_files)) {
#for(i in 1:5) {
  fp <- paste(spam_path, spam_files[i], sep="/")
  
  words <- scanFile(fp)
  words <- removePunct(words, stop_words)
  d <- words[words %in% english_words]
  nd <- words[!(words %in% english_words)]
  spam_count[d] <- spam_count[d] + 1
  total_count[d] <- total_count[d] + 1
#  for(w in d) {
#    # Spam count
#    D[[w]][1] <- D[[w]][1] + 1
#    # Total count
#    D[[w]][3] <- D[[w]][3] + 1
#    #cat(w, " in dictionary\n")
#  }
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
  d <- words[words %in% english_words]
  nd <- words[!(words %in% english_words)]
  ham_count[d] <- ham_count[d] + 1
  total_count[d] <- total_count[d] + 1
#  for(w in d) {
#    # Ham count
#    D[[w]][2] <- D[[w]][2] + 1
#    # Total count
#    D[[w]][3] <- D[[w]][3] + 1
#  }
  for(w in nd) {
    english_words[length(english_words) + 1L] <- w
    spam_count[w] <- 0
    total_count[w] <- 1
    ham_count[w] <- 1
    # c(spamcount,hamcount,totalcount,indictionary)
    #D[[w]] <- c(0, 1, 1, 0)
  }
  print(fp)
}
# Save everything
#filename <- paste(save_path, "spamDict2.Rdata", sep="/")
#save(D, WORDCOUNT, Nspam, Nham, Pspam, Pham, stop_words, file=filename)
## Try creating a sorted data frame
## Save variables and dictionary
#sdwords <- sort(names(D))
## Only save tokens found in mincount emails
#mincount <- 5
##mincount <- 2
## Create vectors for data frame columns.
#email_words <- vector()
#spam_count <- vector()
#ham_count <- vector()
#total_count <- vector()
#
#for(i in 1:length(sdwords)) {
##for(i in 1:10000) {
#  #print(D[[sdwords[i]]][3])
#  if(D[[sdwords[i]]][3] >= mincount){
#    print("found one")
#    #email_words[length(email_words)+1] <- sdwords[i]
#    email_words <- c(email_words, sdwords[i])
#    #spam_count[length(spam_count)+1] <- D[[sdwords[i]]][1]
#    spam_count <- c(spam_count, D[[sdwords[i]]][1])
#    #ham_count[length(ham_count)+1] <- D[[sdwords[i]]][2]
#    ham_count <- c(ham_count, D[[sdwords[i]]][2])
#    #total_count[length(total_count)+1] <- D[[sdwords[i]]][3]
#    total_count <- c(total_count, D[[sdwords[i]]][3])
#  }  
#}

# Need to alpha sort vectors and purge terms that have less than mincount.
# Maybe sort max to min use?
mincount <- 5  # Pass in or set at top eventually
spam_count <- spam_count[total_count >= mincount]
ham_count <- ham_count[total_count >= mincount]
english_words <- english_words[total_count >= mincount]
total_count <- total_count[total_count >= mincount]
# create a dataframe from the vectors
SpamDictionary <- data.frame(english_words, spam_count, ham_count, total_count, stringsAsFactors=FALSE)

#filename <- paste(save_path, "spamDictionaryDF.Rdata", sep="/")
#save(SpamDictionary, Nspam, Nham, stop_words, file=filename)
