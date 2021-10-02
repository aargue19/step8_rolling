
df = read.csv("C:/Users/gaoan/Desktop/step8_rolling/merged_data_w_dummies.csv")

# USE THIS TO CALCULATE ALL COS SIMS BASED ON PROTOTYPES USING 1 AND 2 YEAR ROLLING WINDOWS PRIOR TO RELEASE DATE
setwd("C:/Users/gaoan/Desktop/step8_rolling/matrices")

library(dplyr)
library(lsa)
library(quanteda)
library(lubridate)

'%!in%' <- Negate('%in%')

# word_type_options = c("wordtype_all", "wordtype_wm", "wordtype_world",
#                       "wordtype_mech")
# word_freq_options = c("pct_full")
# avg_score_options = c("mc_allscore")

word_type_options = c("wordtype_world","wordtype_mech")
word_freq_options = c("pct_99p", "pct_95p")
avg_score_options = c("mc_90score", "mc_75score")

# 
# word_type_options = c("wordtype_all", "wordtype_wm", "wordtype_world",
#                       "wordtype_mech")
# word_freq_options = c("pct_full", "pct_99p", "pct_95p")
# avg_score_options = c("mc_allscore", "mc_90score", "mc_75score")

df$genre_wide = gsub(" ", "-", df$genre_wide)                               # filename needs and underscore not space
df$genre_wide = gsub("/", "-", df$genre_wide)                               # "exercise/fitness" -> "exercise_fitness"

wide_genre_options = unique(df$genre_wide)

####################################
# TEST ONE SET ONLY

# word_type_options = c("wordtype_all")
# word_type_options = c("wordtype_all")
# word_freq_options = c("pct_95p")
# avg_score_options = c("mc_allscore","mc_75score")
# wide_genre_options = c("sports","strategy")
####################################

# final_df = data.frame(game_id = unique(df$id))

for(type in word_type_options){
  for(freq in word_freq_options){
    for(score in avg_score_options){
      
      tfs_df = data.frame()
      
      for(genre in wide_genre_options){
        
        print(sprintf("calculating for %s, %s, %s, %s", type, freq, score, genre))
        
        current_group_proto = df[df[[type]] == 1 & df[[freq]] == 1 & df[[score]] == 1 & df$genre_wide == genre,]
        current_group_games = df[df[[type]] == 1 & df$pct_full == 1 & df$mc_allscore == 1 & df$genre_wide == genre,]
        
        current_group_ids = unique(current_group_games$id)
        
        all_1yr_rows_df = data.frame()
        all_2yr_rows_df = data.frame()
        
        if(nrow(current_group_proto) > 0 & nrow(current_group_games) > 0){
          
          # LOOP THROUGH EACH GAME IN THE GROUP
          for(check_id in current_group_ids){
  
            test_df = current_group_games[current_group_games$id == check_id,]
            current_game_date = as.Date(unique(test_df$release_date))
            current_game_month = month(current_game_date)
            
            
            
            
            # create a dtf matrix for current game
            temp = test_df[,c("id","std_word")]
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
            docnames(dtm) <- dtm$id                                               # reassign the document names
            
            mat1 = as.matrix(dtm)
            
            # games released btw current game release and one year ago

            cs = current_group_games[interval(rollforward(current_game_date), rollback(as.Date(current_group_games$release_date))) %/% months(1) < -1 &
                      interval(rollforward(current_game_date), rollback(as.Date(current_group_games$release_date))) %/% months(1) > -14,]
            
            if(nrow(cs)>0){
              
              # create a dtf matrix for one year prototype
              
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
              docnames(dtm) <- dtm$id                                               # reassign the document names
              
              mat2 = as.matrix(dtm)
              # write.csv(mat, sprintf("test.csv"))
            
              #CREATE TWO DFS WITH SAME COLUMNS FOR 1yr SCORES
              
              proto_df = mat2                                                          
              proto_avgs_1yr = colSums(proto_df) / nrow(proto_df)
              
              all_df = as.data.frame(mat1)
              
              proto_avgs_1yr_df = as.data.frame(t(proto_avgs_1yr))
              colnames_to_add = colnames(all_df)[colnames(all_df) %!in% names(proto_avgs_1yr)]
              proto_avgs_1yr_df[colnames_to_add] = 0
              proto_avgs_1yr_df = proto_avgs_1yr_df[ , order(names(proto_avgs_1yr_df))]
              
              colnames_to_add = names(proto_avgs_1yr)[names(proto_avgs_1yr) %!in% colnames(all_df)]
              all_df[colnames_to_add] = 0
              all_df = all_df[ , order(names(all_df))]
              
              #CALCULATE COSINE SIM SCORE
              
              v1 = as.numeric(c(t(all_df[1,])))
              v2 = as.numeric(c(t(proto_avgs_1yr_df[1,])))
              current_cos_sim_val = cosine(v1, v2)
              #print(sprintf("#%s 1yr: %s",check_id, as.character(current_cos_sim_val[1])))
              
              row_to_add = c(check_id, current_cos_sim_val[1])
              
              all_1yr_rows_df = rbind(all_1yr_rows_df, data.frame(game_id = check_id, 
                                                                  cos_sim_1yr = current_cos_sim_val[1]))
            }
            
            if(nrow(cs)==0){
              all_1yr_rows_df = rbind(all_1yr_rows_df, data.frame(game_id = check_id, 
                                                                  cos_sim_1yr = NA))
            }
            
            ######################################################################
            # DO IT ALL AGAIN BUT FOR 2 YEAR WINDOW
            
            # games released btw current game release and 24 months ago
            
            cs = current_group_games[interval(rollforward(current_game_date), rollback(as.Date(current_group_games$release_date))) %/% months(1) < -1 &
                                       interval(rollforward(current_game_date), rollback(as.Date(current_group_games$release_date))) %/% months(1) > -26,]
            
            if(nrow(cs)>0){
              
              # create a dtf matrix for two year prototype
              
    
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
              docnames(dtm) <- dtm$id                                               # reassign the document names
              
              mat2 = as.matrix(dtm)
              # write.csv(mat, sprintf("test.csv"))
              
              #CREATE TWO DFS WITH SAME COLUMNS FOR 2yr SCORES
              
              proto_df = mat2                                                          
              proto_avgs_1yr = colSums(proto_df) / nrow(proto_df)
              
              all_df = as.data.frame(mat1)
              
              proto_avgs_1yr_df = as.data.frame(t(proto_avgs_1yr))
              colnames_to_add = colnames(all_df)[colnames(all_df) %!in% names(proto_avgs_1yr)]
              proto_avgs_1yr_df[colnames_to_add] = 0
              proto_avgs_1yr_df = proto_avgs_1yr_df[ , order(names(proto_avgs_1yr_df))]
              
              colnames_to_add = names(proto_avgs_1yr)[names(proto_avgs_1yr) %!in% colnames(all_df)]
              all_df[colnames_to_add] = 0
              all_df = all_df[ , order(names(all_df))]
              
              #CALCULATE COSINE SIM SCORE
              
              v1 = as.numeric(c(t(all_df[1,])))
              v2 = as.numeric(c(t(proto_avgs_1yr_df[1,])))
              current_cos_sim_val = cosine(v1, v2)
              #print(sprintf("#%s 2yr: %s",check_id, as.character(current_cos_sim_val[1])))
              
              row_to_add = c(check_id, current_cos_sim_val[1])
              
              all_2yr_rows_df = rbind(all_2yr_rows_df, data.frame(game_id = check_id, 
                                                                  cos_sim_2yr = current_cos_sim_val[1]))
            }
            
            if(nrow(cs)==0){
              all_2yr_rows_df = rbind(all_2yr_rows_df, data.frame(game_id = check_id, 
                                                                  cos_sim_2yr = NA))
            }
        
          }
          
        }
          
        if(nrow(current_group_proto) == 0 | nrow(current_group_games) == 0){
          all_1yr_rows_df = rbind(all_1yr_rows_df, data.frame(game_id = 999999, cos_sim_1yr = NA))       
          all_2yr_rows_df = rbind(all_2yr_rows_df, data.frame(game_id = 999999, cos_sim_2yr = NA))                 
        }

        ######################################################################
        # MERGE THE DATASETS WITH 1yr and 2yr COS SIMS
        
        both_years_df = merge(all_1yr_rows_df,all_2yr_rows_df, by="game_id")
          
        colnames(both_years_df) = c("game_id", sprintf("%s_%s_%s_1yr", type, freq, score), sprintf("%s_%s_%s_2yr", type, freq, score))
        
        tfs_df = rbind(tfs_df, both_years_df)
        tfs_df = tfs_df[tfs_df$game_id != 999999,]
        
        write.csv(tfs_df, sprintf("%s_%s_%s.csv", type, freq, score), row.names = F)
        
      }
        
      # final_df = merge(final_df, tfs_df, by = "game_id", all.x = T)
      
    }
  }
}


