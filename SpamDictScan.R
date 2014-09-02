# If the dictionary has been constructed with correct counts, then it will sum to 1
# when the counts are divided by the correct denominators since the dictionary contains
# all possible outcomes and so must sum to 1. I evidently must figure out how this works
# to have the program output probabilities.

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
save_path <- '../../doing_data_science/BayesSpamFilter/'
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

# Going to need a structure 
# Try:
# List[[word]][spam count][total count][training set: 0 or 1]
# Change to:
# List[[word][spam count][ham count][total count][word in dictionary T=1]
#if(file.exists("EnglishDict.Rdata")) {
#  load("EnglishDict.Rdata")
#  D <- EnglishDict
#  english_words <- names(EnglishDict)
#  rm(EnglishDict)
#}
#else {
#  # Try building this from a dictionary of the english language
#  english_words <- scanFile("./english_wordlist.txt")
#  EnglishDict <- list()
#  for(w in english_words) {
#    D[[w]] <- c(0, 0, 0, 1)
#    #print(w)
#  }
#  save(EnglishDict, file="EnglishDict.Rdata")
#}

if(file.exists("EnglishDict.Rdata")) {
  print("file present")
  load("EnglishDict.Rdata")
  D <- EnglishDict
  english_words <- names(EnglishDict)
  rm(EnglishDict)
 } else {
 print("not present")
#  # Try building this from a dictionary of the english language
 english_words <- scanFile("./english_wordlist.txt")
 english_words <- tolower(english_words)
 EnglishDict <- list()
 for(w in english_words) {
   EnglishDict[[w]] <- c(0, 0, 0, 1)
   cat(".")
 }
  filename <- paste(save_path, "EnglishDict.Rdata", sep="/")
  save(EnglishDict, file=filename)
  D <- EnglishDict
  rm(EnglishDict)
}

for(i in 1:length(spam_files)) {
#for(i in 1:5) {
  fp <- paste(spam_path, spam_files[i], sep="/")
  
  words <- scanFile(fp)
  words <- removePunct(words, stop_words)
  d <- words[words %in% english_words]
  nd <- words[!(words %in% english_words)]
  for(w in d) {
    # Spam count
    D[[w]][1] <- D[[w]][1] + 1
    # Total count
    D[[w]][3] <- D[[w]][3] + 1
    #cat(w, " in dictionary\n")
  }
  for(w in nd) {
    # c(spamcount,hamcount,totalcount,indictionary)
    D[[w]] <- c(1, 0, 1, 0)
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
  for(w in d) {
    # Ham count
    D[[w]][2] <- D[[w]][2] + 1
    # Total count
    D[[w]][3] <- D[[w]][3] + 1
  }
  for(w in nd) {
    # c(spamcount,hamcount,totalcount,indictionary)
    D[[w]] <- c(0, 1, 1, 0)
  }
  print(fp)
}
# Save everything
filename <- paste(save_path, "spamDict2.Rdata", sep="/")
save(D, WORDCOUNT, Nspam, Nham, Pspam, Pham, stop_words, file=filename)
# Try creating a sorted data frame
# Save variables and dictionary
sdwords <- sort(names(D))
# Only save tokens found in mincount emails
mincount <- 5
#mincount <- 2
# Create vectors for data frame columns.
email_words <- vector()
spam_count <- vector()
ham_count <- vector()
total_count <- vector()

for(i in 1:length(sdwords)) {
#for(i in 1:10000) {
  #print(D[[sdwords[i]]][3])
  if(D[[sdwords[i]]][3] >= mincount){
    print("found one")
    #email_words[length(email_words)+1] <- sdwords[i]
    email_words <- c(email_words, sdwords[i])
    #spam_count[length(spam_count)+1] <- D[[sdwords[i]]][1]
    spam_count <- c(spam_count, D[[sdwords[i]]][1])
    #ham_count[length(ham_count)+1] <- D[[sdwords[i]]][2]
    ham_count <- c(ham_count, D[[sdwords[i]]][2])
    #total_count[length(total_count)+1] <- D[[sdwords[i]]][3]
    total_count <- c(total_count, D[[sdwords[i]]][3])
  }  
}

# create a dataframe from the vectors
SpamDictionary <- data.frame(email_words, spam_count, ham_count, total_count, stringsAsFactors=FALSE)

filename <- paste(save_path, "spamDictionaryDF.Rdata", sep="/")
save(SpamDictionary, Nspam, Nham, stop_words, file=filename)
