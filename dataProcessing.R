
setwd("C:/Work/DS/Capstone")
#setwd("c:/Users/SB/Documents/Data Sceince/10. Capstone")
library(data.table); library(data.table); library(ggplot2); library(tidyr); library(tidytext); library(dplyr); 

# functions 
# remove all simbols that not english letters, whitespace or apostrophe
rmSpec <- function(x) gsub("[^a-zA-Z ']", "", x) 
# return list of profanity words
ProfanityFilter <- function(){
    file_dest <- "data/full-list-of-bad-words.zip"
    if ( !file.exists(file_dest)) {
        file_link <- "https://www.freewebheaders.com/download/files/full-list-of-bad-words_csv-file_2018_07_30.zip"
        download.file(file_link, file_dest)
    }
    
    file <- unz(file_dest, filename= "full-list-of-bad-words_csv-file_2018_07_30.csv")
    filter <- read.csv(file, sep = ";", header = F, stringsAsFactors = F)
    
    tibble(word = filter[,1])
}
# read all data, or some proprotion from files with text rows
readText <- function(files, prop= NULL) {
    text <- NULL;     rn <- 0
    if(!is.null(prop)) {
        for (i in files) {
            file <- file(description = i, "r")
            n <- 0
            while(TRUE){
                ln <- readLines(file, 1, skipNul = T); if (length(ln) == 0 ) break
                if (rbinom(1,1,0.03) == 1)
                {
                    src <- i
                    if(grepl(".blogs.", i, fixed = T)) src <- "blogs"
                    if(grepl(".news.", i, fixed = T)) src <- "news"
                    if(grepl(".twitter.", i, fixed = T)) src <- "twitter"
                    
                    n <- n+1; rn <- rn + 1
                    text <- rbind(text, tibble(doc_id= rn, text = ln, line= n, source = src ))
                }
            }
            close(file);
        }
    }
    else {
        for (i in files) {
            file <- file(description = i, "r")
            ln <- readLines(file, skipNul = T) 
            src <- i
            if(grepl(".blogs.", i, fixed = T)) src <- "blogs"
            if(grepl(".news.", i, fixed = T)) src <- "news"
            if(grepl(".twitter.", i, fixed = T)) src <- "twitter"
            print(src)
            text <- rbind(text, tibble(doc_id= (rn+1):(rn+length(ln)), text = ln, line= 1:length(ln), 
                                       source = src ))
            rn <- rn + length(ln)
            close(file);
        }
        
    }
    text
}


# Model development


# read a text sample from file
text <- as.tbl(readText(c( "data/en_US.news.txt" , "data/en_US.blogs.txt",  "data/en_US.twitter.txt") ))
# clear data from non-textual simbols and extra whitespaces
clearText <- text
clearText$text <- text$text %>% tolower() %>% rmSpec() %>% stripWhitespace() %>% trimws()


# create train and test datasets
tmp <- clearText
set.seed(123)

train_ind <- sample(seq_len(nrow(tmp)), size = floor(0.15 * nrow(tmp)))
train <- tmp[train_ind, ]
train <- train[sample(seq_len(nrow(train)), size = floor(1*nrow(train))), ]

test <- tmp[-train_ind, ]

rm("tmp")


# create unigram dictionary
UD <- train[, "text"] %>%
    # split sentences on words
    unnest_tokens(word, text) %>%
    # filter profanty words
    anti_join( ProfanityFilter(), by= "word") %>%
    # count words and sort starting from most frequent
    count(word, sort = T) %>% 
    # estimate probability as portion word count from all inctance of words
    mutate(p1= n / sum(n)) %>%
    # code each word with integer number
    mutate(wc= row_number()) %>%
    ungroup()


# create bigram dictionary and calc probabilities using Kneser-Ney Smoothing
# set discount for bigram level
d2<-0.75
BD <- train[, "text"] %>% 
    # split sentences on bigrams
    unnest_tokens(bigram, text, token = "ngrams", n = 2, collapse = FALSE) %>%
    filter(!is.na(bigram)) %>%
    # split bigrams on words
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    # filter profanty words
    filter(!word1 %in% ProfanityFilter()) %>% filter(!word2 %in% ProfanityFilter()) %>%
    # count word pairs and sort it from most frequent
    count(word1, word2, sort = T) %>%
    # count how often word2 is a novel continuation 
    group_by(word2) %>% 
    mutate(cont_n2=n()) %>% 
    ungroup() %>%
    # add bigrams dictionary length
    mutate(dl2= n() ) %>%
    group_by(word1) %>%
    # calc lambda for lower order n-grams
    mutate( lamb2 = (d2 / dl2) * n() ) %>%
    # and use it to spread discounted probability mass in proportion to words novel continuation frequency 
    # this use only if we don't find trigram when predicting
    mutate( p2 = pmax(n-d2, 0) / sum(n) + lamb2 * (cont_n2 / dl2 ) ) %>% 
    # calc probability with continuation counts to use it according to Kneser-Ney recursive function in trigramms
    mutate( p_kn2 = pmax(cont_n2-d2, 0) / dl2 + lamb2 * (cont_n2 / dl2 ) ) %>%
    # select data that we need in prediction
    select(-cont_n2, -lamb2, -dl2) %>%
    ungroup()


# create trigram dictionary and calc probabilities using Kneser-Ney Smoothing
# set discount for trigram level
d3 <- 0.75
TD <- train[, "text"] %>% 
    # split sentences on trigrams
    unnest_tokens(trigram, text, token = "ngrams", n = 3, collapse = FALSE) %>%
    filter(!is.na(trigram)) %>%
    # split trigrams on words
    separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
    # filter rows with profanty words
    filter(!word1 %in% ProfanityFilter()) %>% filter(!word2 %in% ProfanityFilter()) %>% 
    filter(!word3 %in% ProfanityFilter()) %>%
    # count trigrms and sort it from most frequent
    count(word1, word2, word3, sort = T) %>%
    # add Kneser-Ney probability for lower order bigrams
    left_join((BD %>% select(word3 = word2, word2 = word1, p_kn2)), by= c("word2", "word3")) %>%
    # count how often word3 is a novel continuation 
    group_by(word3) %>% 
    mutate(cont_n3=n()) %>% 
    ungroup() %>%
    # add trigrams dictionary length
    mutate(dl3= n() ) %>%
    group_by(word1, word2) %>%
    # calc lambda for lower order n-grams
    mutate( lamb3 = (d3 / sum(n)) * n() ) %>%
    # and use it to spread discounted probability mass in proportion to lower order probabiliry that based on continuation count
    mutate( p_kn3 = pmax(n-d3, 0) / sum(n) + lamb3 * p_kn2 ) %>%
    # select data that we need in prediction
    select(-cont_n3, -lamb3, -dl3) %>%
    ungroup()


# create new dictionaries were all words replaced with integer codes from ungrams
BDc <- BD %>%
    # order bigrams starting from most likely
    arrange(word1, -p2) %>%
    group_by(word1) %>%
    # take 3 most likely bigrams 
    filter(row_number() < 4) %>%
    ungroup() %>%
    # and codes from unigrams
    left_join(UD %>% select(word1= word, wc1= wc), by="word1") %>%
    left_join(UD %>% select(word2= word, wc2= wc), by="word2") %>%
    # remove text
    select(-word1, -word2, -n, -p_kn2)
    
TDc <- TD %>% 
    # order trigrams starting from most likely
    arrange(word1, word2, -p_kn3) %>%
    group_by(word1, word2) %>%
    # take 3 most likely bigrams 
    filter(row_number() < 4) %>%
    ungroup() %>%
    # and codes from unigrams
    left_join(UD %>% select(word1= word, wc1= wc), by="word1") %>%
    left_join(UD %>% select(word2= word, wc2= wc), by="word2") %>%
    left_join(UD %>% select(word3= word, wc3= wc), by="word3") %>%
    # remove text
    select(-word1, -word2, -word3, -n, -p_kn2)

# save dicts for forward use in application
UD <- as.data.table(UD)
BDc <- as.data.table(BDc)
TDc <- as.data.table(TDc)
save(file = "txt.Rdata", list = c("UD", "BDc", "TDc"))   


# Model performance assessment


# funcation that produce accuracy report table for input test dataset
textPredAccNgram <- function(txt){
    bigrams <-  txt %>% 
        unnest_tokens(bigram, text, token = "ngrams", n = 2, collapse = FALSE) %>% filter(!is.na(bigram)) %>%
        # take only first bigrams in sentence, cause for the next we use trigrms with backoff process
        group_by(line) %>%
        filter(row_number() == 1) %>%
        # split bigrams to words
        separate(bigram, c("word1", "w2"), sep = " ") %>%
        # filter rows with profanity words
        filter(!word1 %in% ProfanityFilter()) %>% filter(!w2 %in% ProfanityFilter()) %>%
        # join the most expected beginnings of bigrams
        left_join( (BD %>% 
                        select(word1, word2, p2) %>% 
                        group_by(word1) %>% arrange(-p2) %>% 
                        filter(row_number() == 1) ), 
                   by= "word1") %>%
        # and check did we make correct guess
        mutate(pred = w2 == word2)%>%
        # calc performance measures 
        ungroup() %>%
        summarise(nn = n(), nna =  sum(!is.na(pred)),
                  accuracy = round( 100*sum(pred, na.rm = T) / sum(!is.na(pred)), 1 ), 
                  confusion = round( 100*sum(is.na(pred)) / n(), 2)
        )
    
    trigrams <- txt %>% 
        unnest_tokens(trigram, text, token = "ngrams", n = 3, collapse = FALSE) %>% filter(!is.na(trigram)) %>%
        # split bigrams to words
        separate(trigram, c("w1", "w2", "w3"), sep = " ") %>%
        group_by(line) %>%
        # filter rows with profanity words
        filter(!w1 %in% ProfanityFilter()) %>% filter(!w2 %in% ProfanityFilter()) %>% filter(!w3 %in% ProfanityFilter()) %>%
        # join the most expected beginnings of trigrams
        left_join( (TD %>% select(word1, word2, word3, p_kn3) %>%
                        group_by(word1, word2) %>% 
                        arrange(-p_kn3) %>% 
                        filter(row_number() == 1) %>% 
                        ungroup() ), 
                   by= c("w1" = "word1", "w2" = "word2") ) %>%
        # join the most expected beginnings of bigrams to second word (backoff process)
        left_join( (BD %>% select(word1, word2, p2) %>%
                        group_by(word1) %>% 
                        arrange(-p2) %>% 
                        filter(row_number() == 1) ) , 
                   by= c("w2" = "word1")) %>%
        # select most expected word based on simple backoff model
        mutate(p_word = if_else(is.na(word3), word2, word3) ) %>%
        # and check did we make correct guess
        mutate(pred = w3 == p_word) %>%
        # calc performance measures 
        ungroup() %>%
        summarise(nn = n(), nna =  sum(!is.na(pred)),
                  accuracy = round( 100*sum(pred, na.rm = T) / sum(!is.na(pred)), 1 ), 
                  confusion = round( 100*sum(is.na(pred)) / n(), 2)
                 )
        rbind(bigrams %>% select( -nna) %>% rename(n = nn) %>% mutate(type= "bigrams"), 
              trigrams %>% select( -nna) %>% rename(n = nn) %>% mutate(type= "trigrams"),
              rbind(bigrams, trigrams) %>% 
                    summarise(n=sum(nn), 
                              accuracy =  sum(accuracy * nna) / sum(nna), 
                              confusion = sum(confusion * nn) / sum(nn)) %>% 
                    mutate(type= "total")
        ) %>% select(type, accuracy, confusion, n) %>% 
            rename(`Accuracy, %` = accuracy, `Confusion, %` = confusion)
 } 

# run test to get accuracy report
report <- textPredAccNgram (test[1:10000, ])
# and save it for forward use in application
save(file = "stat.Rdata", list = c("report"))   

