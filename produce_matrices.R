# USE THIS TO PRODUCE UNTRANSPOSED VERSIONS
setwd("C:/Users/gaoan/Desktop/step8_narrow/matrices")
library(quanteda)

word_type_options = c("wordtype_all", "wordtype_wm", "wordtype_world", 
                      "wordtype_mech")
word_freq_options = c("pct_full", "pct_99p", "pct_95p")
avg_score_options = c("mc_allscore", "mc_90score", "mc_75score")

# df = read.csv("merged_data_w_dummies.csv")

df$genre_narrow = gsub("/", "-", df$genre_narrow)                               # filename needs and underscore not space     
df$genre_narrow = gsub(" ", "-", df$genre_narrow)                               # "exercise/fitness" -> "exercise_fitness"

narrow_genre_options = unique(df$genre_narrow)
year_options = unique(df$release_year)

counter = 0

for(type in word_type_options){
  for(freq in word_freq_options){
    for(score in avg_score_options){
      for(genre in narrow_genre_options){
        for(year in year_options){
          counter = counter + 1
          print(sprintf("%s / 19440", counter))
          
          cs = df[df[[type]] == 1 & df[[freq]] == 1 & df[[score]] & 
                  df$genre_narrow == genre & df$release_year == year,]
      
          filename = paste(type,freq,score,genre,year, sep="_")
          
          if(nrow(cs) > 0){                                                     # some subsets are empty
            temp = cs[,c("id","std_word")]
            strings_df = data.frame()
      
            for(current_id in unique(temp$id)){                                 # create this for quanteda to make a corpus from
              current_vec = unique(temp$std_word[temp$id == current_id])
              current_string = paste(current_vec,collapse=" ")
              line_to_add = data.frame(id = current_id,
                                       std_string = current_string)
      
              strings_df = rbind(strings_df, line_to_add)
            }
      
            temp = merge(temp, strings_df, by="id")
            temp$std_word = NULL
            temp = temp[!duplicated(temp),]
            
            corp = corpus(temp, text_field = 'std_string')
            toks <- corp %>% tokens()
            dtm = dfm(toks)
            docnames(dtm) <- dtm$id                                             # reassign the document names
            
            mat = as.matrix(dtm)
            write.csv(mat, sprintf("%s.csv",filename))
            
          }
          
          if(nrow(cs) == 0){
            empty_df = data.frame(empty_column = NA)
            rownames(empty_df)[1] = "999999"
            write.csv(empty_df, sprintf("%s.csv",filename))                     # blank file for empty subsets
          }
          
        }
      }
    }
  }
}

library(beepr)
beep(4)