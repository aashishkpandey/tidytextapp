source('https://raw.githubusercontent.com/aashishkpandey/tidytextapp/master/tidytextapp_functions.R')

#--------------------------------------------------------#
#   Create DTM of BD
#--------------------------------------------------------#
system.time({ 
  years = 2015:2016
  
  for (year in years) {
    bd.df = readRDS(paste0("D:\\31127 Aashish\\10KTech\\clean_data\\bd.df.",year,".Rds"))
    bdtext = gsub('table of contents|table of content',' ',bd.df$bd.text)
    
    bd.dtm = create_DTM( text = bdtext,
                         docID = bd.df$file,
                         replace_ngrm = T,
                         rm_stop_words=T,
                         textcleaning = T,
                         lower=T,
                         alphanum=T, 
                         drop_num=T,
                         stop_custom = c('will','was','can'),
                         smart_stop_words = T,
                         tfidf = F,
                         bi_gram_pct = 0.02,
                         min_freq = 5, 
                         filter = 'pct', 
                         py.sent_tknzr = T)
    
    saveRDS(bd.dtm,paste0('D:\\31127 Aashish\\10KTech\\clean_data\\dtm.bd.',year,'.Rds'))
    
  }
})

#--------------------------------------------------------#
#   Create DTM of RF
#--------------------------------------------------------#

system.time({ 
  years = 2015:2016
  
  for (year in years) {
    rf.df = readRDS(paste0("D:\\31127 Aashish\\10KTech\\clean_data\\rf.df.",year,".Rds"))
    rftext = gsub('table of contents|table of content',' ',rf.df$rf.text)
    
    rf.dtm = create_DTM( text = rftext,
                         docID = rf.df$file,
                         replace_ngrm = T,
                         rm_stop_words=T,
                         textcleaning = T,
                         lower=T,
                         alphanum=T, 
                         drop_num=T,
                         stop_custom = c('will','was','can'),
                         smart_stop_words = T,
                         tfidf = F,
                         bi_gram_pct = 0.02,
                         min_freq = 5, 
                         filter = 'pct', 
                         py.sent_tknzr = T)
    
    saveRDS(rf.dtm,paste0('D:\\31127 Aashish\\10KTech\\clean_data\\dtm.rf.',year,'.Rds'))
    
  }
})
