# rm(list = ls())

library(tidyverse)
library(tidytext)
library(stringr)

#--------------------------------------------------------------#
# 1 - Reticulate python setence tokenizer with python NLTK
#--------------------------------------------------------------#

py.sent_tokenize = function(text) {
  
  require(reticulate)
  require(dplyr)
  nltk = import("nltk")
  
  sent_list = vector(mode="list", length=length(text))  
  counter = 0
  
  for (i in 1:length(text)){  
    if (!grepl('[:alnum:]',text[i])) {text[i] = "NO sentecne"}
    sents = nltk$tokenize$sent_tokenize(text[i])
    sent_list[[i]] = data.frame(docID = i, 
                                sentID = counter + seq(1:length(sents)), 
                                text = sents, 
                                stringsAsFactors=FALSE)
    
    counter = max(sent_list[[i]]$sentID)   }    # i ends
  
  sent_df = bind_rows(sent_list)   
  return(sent_df)  }   # func ends


#--------------------------------------------------------------#
# 2 - basic text cleaning
#--------------------------------------------------------------#

clean_text <- function(text, lower=T, alphanum=T, drop_num=T){
  text  =  str_replace_all(text, "<.*?>", " ")   # drop html junk
  
  if (lower=="TRUE") {text = text %>% str_to_lower()}
  if (alphanum=="TRUE") {text = text %>% str_replace_all("[^[:alnum:]]", " ")}
  if (drop_num=="TRUE") {text = text %>% str_replace_all("[:digit:]", "")}
  
  text = text %>% str_replace_all("\\\\s+", " ")   # collapse multiple spaces
  return(text) 
  
} # clean_text() ends

#--------------------------------------------------------------#
# 3 - Removing Stop Words
#--------------------------------------------------------------#

c_remove_words =  function (x, words) {
      gsub(sprintf("(*UCP)\\b(%s)\\b", paste(sort(words, decreasing = TRUE), 
                                           collapse = "|")), "", x, perl = TRUE) }
  
replace_ngram <- function(text, 
                          bi_gram_pct = 0.05,
                          min_freq = 5, 
                          filter = 'pct', 
                          py.sent_tknzr = T, 
                          textcleaning = T,
                          lower=T,
                          alphanum=T, 
                          drop_num=T,
                          rm_stop_words=T,
                          stop_custom = c('will','was','can'),
                          smart_stop_words = T
                          ){

  text_orig = text
  # Cleaning text
  if( textcleaning == T) {
  text = clean_text(text,lower = lower, alphanum = alphanum, drop_num = drop_num)
  }
  
  # defining smart stop words
  smart_stop = c("i","me","my","myself","we","our","ours","ourselves","you","your","yours","yourself","yourselves","he","him","his","himself","she","her","hers","herself",
                 "it","its","itself","they","them","their","theirs","themselves","what","which","who","whom","this","that","these","those","am","is","are","was","were","be",
                 "been","being","have","has","had","having","do","does","did","doing","would","should","could","ought","i'm","you're","he's","she's","it's","we're","they're",
                 "i've","you've","we've","they've","i'd","you'd","he'd","she'd","we'd","they'd","i'll","you'll","he'll","she'll","we'll","they'll","isn't","aren't","wasn't",
                 "weren't","hasn't","haven't","hadn't","doesn't","don't","didn't","won't","wouldn't","shan't","shouldn't","can't","cannot","couldn't","mustn't","let's","that's",
                 "who's","what's","here's","there's","when's","where's","why's","how's","a","an","the","and","but","if","or","because","as","until","while","of","at","by","for",
                 "with","about","against","between","into","through","during","before","after","above","below","to","from","up","down","in","out","on","off","over","under",
                 "again","further","then","once","here","there","when","where","why","how","all","any","both","each","few","more","most","other","some","such","no","nor","not",
                 "only","own","same","so","than","too","very","a's","able","according","accordingly","across","actually","afterwards","ain't","allow","allows","almost","alone",
                 "along","already","also","although","always","among","amongst","another","anybody","anyhow","anyone","anything","anyway","anyways","anywhere","apart","appear",
                 "appreciate","appropriate","around","aside","ask","asking","associated","available","away","awfully","b","became","become","becomes","becoming","beforehand",
                 "behind","believe","beside","besides","best","better","beyond","brief","c","c'mon","c's","came","can","cant","cause","causes","certain","certainly","changes",
                 "clearly","co","com","come","comes","concerning","consequently","consider","considering","contain","containing","contains","corresponding","course","currently",
                 "d","definitely","described","despite","different","done","downwards","e","edu","eg","eight","either","else","elsewhere","enough","entirely","especially","et",
                 "etc","even","ever","every","everybody","everyone","everything","everywhere","ex","exactly","example","except","f","far","fifth","first","five","followed",
                 "following","follows","former","formerly","forth","four","furthermore","g","get","gets","getting","given","gives","go","goes","going","gone","got","gotten",
                 "greetings","h","happens","hardly","hello","help","hence","hereafter","hereby","herein","hereupon","hi","hither","hopefully","howbeit","however","ie",
                 "ignored","immediate","inasmuch","inc","indeed","indicate","indicated","indicates","inner","insofar","instead","inward","it'd","it'll","j","just","k",
                 "keep","keeps","kept","know","knows","known","l","last","lately","later","latter","latterly","least","less","lest","let","like","liked","likely",
                 "little","look","looking","looks","ltd","m","mainly","many","may","maybe","mean","meanwhile","merely","might","moreover","mostly","much","must",
                 "n","name","namely","nd","near","nearly","necessary","need","needs","neither","never","nevertheless","new","next","nine","nobody","non","none",
                 "noone","normally","nothing","novel","now","nowhere","o","obviously","often","oh","ok","okay","old","one","ones","onto","others","otherwise",
                 "outside","overall","p","particular","particularly","per","perhaps","placed","please","plus","possible","presumably","probably","provides",
                 "q","que","quite","qv","r","rather","rd","re","really","reasonably","regarding","regardless","regards","relatively","respectively","right",
                 "s","said","saw","say","saying","says","second","secondly","see","seeing","seem","seemed","seeming","seems","seen","self","selves","sensible",
                 "sent","serious","seriously","seven","several","shall","since","six","somebody","somehow","someone","something","sometime","sometimes","somewhat",
                 "somewhere","soon","sorry","specified","specify","specifying","still","sub","sup","sure","t","t's","take","taken","tell","tends","th","thank","thanks",
                 "thanx","thats","thence","thereafter","thereby","therefore","therein","theres","thereupon","think","third","thorough","thoroughly","though","three","throughout",
                 "thru","thus","together","took","toward","towards","tried","tries","truly","try","trying","twice","two","u","un","unfortunately","unless","unlikely","unto","upon",
                 "us","use","used","useful","uses","using","usually","uucp","v","value","various","via","viz","vs","w","want","wants","way","welcome","well","went","whatever","whence",
                 "whenever","whereafter","whereas","whereby","wherein","whereupon","wherever","whether","whither","whoever","whole","whose","will","willing","wish","within","without",
                 "wonder","x","y","yes","yet","z")
  
  # removing stopwords
  
  if (rm_stop_words == T) {
    if(smart_stop_words == T){
      stop = unique(tolower(c(stop_custom,smart_stop)))
    } else {
      stop = unique(tolower(stop_custom))
    }
    text = c_remove_words(text,stop)
  }
  
  
  textdf = data_frame(text = text, docID = 1:length(text))
  
  # Build All possible bigram list
  bigram_df_orig = textdf %>% group_by(docID) %>% unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>% 
    unite(bigram1, word1, word2, sep = " ", remove = FALSE) %>%
    ungroup()
  
  if (py.sent_tknzr == T) {
  # now first split corpus in sentences to get the vaalid bi-grams
  sentdf = py.sent_tokenize(text_orig)
  if( textcleaning == T) {
    sentdf$text = clean_text(sentdf$text,lower = lower, alphanum = alphanum, drop_num = drop_num)
  }
  
  if (rm_stop_words == T) {
    if(smart_stop_words == T){
      stop = unique(tolower(c(stop_custom,smart_stop)))
    } else {
      stop = unique(tolower(stop_custom))
    }
    sentdf$text = c_remove_words(sentdf$text,stop)
  }
  
  
  } else {
    sentdf = data_frame(text = text, docID = 1:length(text), sentID = 1:length(text))
    }
  
  # get the valid bi-grams based on sentence level corpus for replacement
  bigram_df <- sentdf %>% unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    count(word1, word2, sort = T) %>%
    filter(n >= 2) %>%     # valid bi-grams
    unite(bigram1, word1, word2, sep = " ", remove = FALSE) %>%
    unite(bigram2, word1, word2, sep = "_")
  
  # if (filter = 'pct') {
    bigram_df1 = bigram_df[1:round(nrow(bigram_df)*bi_gram_pct),]  
  # } else {
    bigram_df2 = bigram_df %>% filter(n >= min_freq)
  # }
  
  # Print counts of bi-grams
  print(paste0("Toatl first ",bi_gram_pct*100, "% bi-grams identified is ", (nrow(bigram_df1))))
  print(paste0(nrow(bigram_df2)," bi-grams identified with minimum frequency ", (min_freq)))
  
  if (filter == 'pct') {
    bigram_df = bigram_df1 }
  else {
    bigram_df = bigram_df2
  }
  
  # merge the two bi-grams set
  merged_df1 <- left_join(bigram_df_orig, bigram_df, by=c("bigram1"="bigram1"))
  
  # first, replace NAs with word1
  a0 = (is.na(merged_df1$bigram2))
  merged_df1$bigram2[a0] = merged_df1$word1[a0]
  
  # and NAs count with n = 1
  merged_df1$n[a0] = 1
  
  # create vectors for each column of merged df for faster loop operations
  docID = merged_df1$docID
  bigram1 = merged_df1$bigram1
  bigram2 = merged_df1$bigram2
  nvec = merged_df1$n
  word1 = merged_df1$word1
  word2 = merged_df1$word2
  
  # system.time({
  i = 1
  while(i < (nrow(merged_df1))){             
    
    if (docID[i] == docID[i+1]){
      
    if (grepl("_",bigram2[i]) & grepl("_",bigram2[i+1]) ) {
      if (nvec[i] >= nvec[i+1]){ 
        bigram2[i+1] = ""
          } else { bigram2[i] = word1[i]} 
    }
    
    if (grepl("_",bigram2[i])){
      bigram2[i+1] = ""
    }    
      
    } else if (!(docID[i] == docID[i+1]) ) {
      if (grepl("_",bigram2[i-1])) {
        bigram2[i] = word2[i]
        } else if (!grepl("_",bigram2[i])) {
        bigram2[i] = bigram1[i]
                                                  }
                                        }
          i = i + 1  
  }
  if (!grepl("_",bigram2[i])) {bigram2[i] = bigram1[i]}
    
      # })

  merged_df1$bigram2 = bigram2

  # Now, finally, roll-back the tokens into a doc string
  cleaned_corpus = data.frame(docID = numeric(), text = character(), stringsAsFactors=FALSE) # define empty df to populate
  tokens_only = merged_df1 %>% select(docID, bigram2)
  
  for (i1 in 1:nrow(textdf)){
    a100 = tokens_only[tokens_only$docID == i1,]   # for each doc, collect all cleaned tokens
    
    if (nrow(a100) == 0){
      cleaned_corpus[i1, 1] =  i1
      cleaned_corpus[i1, 2] = "NA_BLANK"
    } else {
      cleaned_corpus[i1, 1] =  i1
      cleaned_corpus[i1, 2] = str_c(a100$bigram2, collapse=" ")   # using str_c()
    }
    
    if (i1 %% 100 == 0) {cat(i1, " docs processed\n")}   # counter for docs processed. Run '?%%'
  } # i1 loop ends
  
  return(cleaned_corpus)   }    # func ends  

#---------------------------------------------------------------------#
# 4 - Create DTM
#---------------------------------------------------------------------#

create_DTM = function(text, 
                      docID = NULL,
                      replace_ngrm = T,
                      rm_stop_words=T,
                      textcleaning = T,
                      lower=T,
                      alphanum=T, 
                      drop_num=T,
                      stop_custom = c('will','was','can'),
                      smart_stop_words = T,
                      tfidf = F,
                      bi_gram_pct = 0.05,
                      min_freq = 5, 
                      filter = 'pct', 
                      py.sent_tknzr = T){
  
  if (is.null(docID)) {docID = 1:length(text)}
  if (replace_ngrm == T) {
    replaced_text = replace_ngram(text = text, 
                                  bi_gram_pct = bi_gram_pct,
                                  min_freq = min_freq, 
                                  filter = filter, 
                                  py.sent_tknzr = py.sent_tknzr, 
                                  textcleaning = textcleaning,
                                  lower=lower,
                                  alphanum=alphanum, 
                                  drop_num=drop_num,
                                  rm_stop_words=rm_stop_words,
                                  stop_custom = stop_custom,
                                  smart_stop_words = smart_stop_words)
    replaced_text$docID = docID
  } else {
    if( textcleaning == T) {
      text = clean_text(text,lower = lower, alphanum = alphanum, drop_num = drop_num)
    }
    
    if (rm_stop_words == T) {
      if(smart_stop_words == T){
        stop = unique(tolower(c(stop_custom,smart_stop)))
      } else {
        stop = unique(tolower(stop_custom))
      }
      text = c_remove_words(text,stop)
    }
    
    replaced_text = data_frame(text = text, docID = docID)
  }
  
  
  ## tokenizing the corpus
  textdf1 = replaced_text %>% 
    # mutate(docIDs = row_number()) %>%    # row_number() is v useful.
    group_by(docID) %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) %>% ungroup()
  
  ## cast into a Matrix object
  if (tfidf == "TRUE") {
    textdf2 = textdf1 %>% group_by(docID) %>% 
      count(word, sort=TRUE) %>% ungroup() %>%
      bind_tf_idf(word, docID, nn) %>% 
      rename(value = tf_idf)} else { textdf2 = textdf1 %>% rename(value = n)  }
  
  m <- textdf2 %>% cast_sparse(docID, word, value)
  
  # reorder dtm to have sorted rows by doc_num and cols by colsums	
  # m = m[order(as.numeric(rownames(m))),]    # reorder rows	
  b0 = apply(m, 2, sum) %>% order(decreasing = TRUE)
  dtm = m[, b0]
  
  return(dtm)
  
}

#--------------------------------------------------------#
#   
#--------------------------------------------------------#


# replaced_text = replace_ngram(text = mydata, 
#                               bi_gram_pct = 0.02,
#                               min_freq = 50, 
#                               filter = 'pct', 
#                               py.sent_tknzr = T, 
#                               textcleaning = T,
#                               lower=T,
#                               alphanum=T, 
#                               drop_num=T,
#                               rm_stop_words=T,
#                               stop_custom = c('will','was','can'),
#                               smart_stop_words = T)
# replaced_text$docID = bd.df$file
