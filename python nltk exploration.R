
# tokens = []
# for doc in corpus:
#   tkns = nltk.word_tokenize(doc)
#   tokens = tokens + tkns
#   
#   text = nltk.Text(word.lower() for word in tokens)
# 
# 
#   text.similar('word',20)

#--------------------------------------------#
bd.df = readRDS("D:\\31127 Aashish\\Projects\\10-K Processed\\data files\\Information Technology\\bd.df.2014.Rds")  
require(reticulate)
require(dplyr)
nltk = import("nltk")
corpus = bd.df$bd.text  

tokens = NULL
counter = 0
for (doc in corpus){
  tkns = nltk$word_tokenize(doc)  
  tokens = c(tokens, tkns)
  counter = counter+1
  print(paste(counter, length(tokens), length(tkns), sep = ", "))
  }

text = nltk$Text(tokens)
text$similar('customer')

py.sent_tokenize = function(text) {

require(reticulate)
require(dplyr)
nltk = import("nltk")

sent_list = vector(mode="list", length=length(text))  
counter = 0

for (i in 1:length(text)){  
  sents = nltk$tokenize$sent_tokenize(text[i])
  sent_list[[i]] = data.frame(docID = i, 
                              sentID = counter + seq(1:length(sents)), 
                              text = sents, 
                              stringsAsFactors=FALSE)
  
  counter = max(sent_list[[i]]$sentID)   }    # i ends

sent_df = bind_rows(sent_list)   
return(sent_df)  }   # func ends





