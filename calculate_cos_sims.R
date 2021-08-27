# setwd("C:/Users/gaoan/Desktop/step8_narrow")
# df = read.csv("merged_data_w_dummies.csv")

# USE THIS TO CALCULATE ALL COS SIMS BASED ON PROTOTYPES USING 1 AND 2 YEARS
setwd("C:/Users/gaoan/Desktop/step8_narrow/matrices")

library(dplyr)
library(lsa)

'%!in%' <- Negate('%in%')


word_type_options = c("wordtype_all", "wordtype_wm", "wordtype_world",
                      "wordtype_mech")
word_freq_options = c("pct_full", "pct_99p", "pct_95p")
avg_score_options = c("mc_allscore", "mc_90score", "mc_75score")

# df$genre_narrow = gsub("action adventure", "action_adventure", df$genre_narrow) 

df$genre_narrow = gsub(" ", "-", df$genre_narrow)                               # filename needs and underscore not space
df$genre_narrow = gsub("/", "-", df$genre_narrow)                               # "exercise/fitness" -> "exercise_fitness"

narrow_genre_options = unique(df$genre_narrow)

year_options = as.numeric(unique(df$release_year))                            
year_options = sort(year_options)[3:length(year_options)]                       # EXCLUDE 2005/2006 b/c no prototypes 


for(type in word_type_options){
  for(freq in word_freq_options){
    for(score in avg_score_options){
      
      tfs_df = data.frame()

      for(genre in narrow_genre_options){
        for(year in year_options){
          
          print(paste(type,freq,score,genre,year, sep="_"))
          
          current_cos_sim_df = data.frame()
                                                                            
          # IF EITHER CURRENT OR PREVIOUS YEAR IS EMPTY FILL WITH NA
          
          f4 = paste0(paste(type,"pct_full_mc_allscore", 
                            genre, year, sep = "_"), ".csv")
          
          f1 = paste0(paste(type,freq,score,genre,year-1, sep="_"), ".csv")
          f2 = paste0(paste(type,freq,score,genre,year-2, sep="_"), ".csv")
          
          print(sprintf("Checking if %f or %f is empty", year-1, year-2))
          
          all_df = read.csv(f4)
          
          if(file.size(f1) <= 32 | file.size(f2) <= 32 | file.size(f4) <= 32){

            print("at least one empty")
            current_cos_sim_df = data.frame(id = all_df$X, cos_sim_1yr = NA, cos_sim_2yr = NA)

          }
          
          if(file.size(f1) > 32 &                                                #USING & MEANS YOU WONT GET 1 yr for 2006 games
             file.size(f2) > 32 &
             file.size(f4) > 32){
            
            print("both not empty")
            file.size("wordtype_all_pct_full_mc_allscore_role-playing-general_2013.csv")
            
            #CALCULATE VECTOR FOR 1YR PROTOTYPE
            print("1")
            
            f = paste0(paste(type,freq,score,genre,year-1, sep="_"), ".csv")
            proto_df = read.csv(f)
            rownames(proto_df) = proto_df$X                                     #NOT SURE HOW TO LABEL THE ROWNAMES COLUMN WHEN WRITING CSV SO ID COLUMN IS BLANK ("X" WHEN READ)
            proto_df = subset(proto_df, select = -X)
            proto_avgs_1yr = colSums(proto_df) / nrow(proto_df)
            
            #CALCULATE VECTOR FOR 2YRS PROTOTYPE
            print("2")
            
            f1 = paste0(paste(type,freq,score,genre,year-1, sep="_"), ".csv")
            f2 = paste0(paste(type,freq,score,genre,year-2, sep="_"), ".csv")
            
            proto_df1 = read.csv(f1)                                            # REMEMBER CHECK FOR EMPTY FILES
            proto_df2 = read.csv(f2)
            
            proto_df_combo = bind_rows(proto_df1, proto_df2)                    # DFS CONTAIN DIFFERENT ROWS AND COLUMNS
            proto_df_combo[is.na(proto_df_combo)] <- 0
            
            rownames(proto_df_combo) = proto_df_combo$X
            proto_df_combo = subset(proto_df_combo, select = -X)
            proto_avgs_2yr = colSums(proto_df_combo) / nrow(proto_df_combo)
            
            #DF OF ALL GENRES ALL YEARS FOR current (type/freq/score) COMBO
            print("3")
            
            
            f4 = paste0(paste(type,"pct_full_mc_allscore", 
                              genre, year, sep = "_"), ".csv")

            
            #CREATE TWO DFS WITH SAME COLUMNS FOR 1yr SCORES
            print("4")
            
            all_df = read.csv(f4)
            rownames(all_df) = all_df$X                                         #NOT SURE HOW TO LABEL THE ROWNAMES COLUMN WHEN WRITING CSV SO ID COLUMN IS BLANK ("X" WHEN READ)
            all_df = subset(all_df, select = -X)
            
            proto_avgs_1yr_df = as.data.frame(t(proto_avgs_1yr))
            colnames_to_add = colnames(all_df)[colnames(all_df) %!in% names(proto_avgs_1yr)]
            proto_avgs_1yr_df[colnames_to_add] = 0
            proto_avgs_1yr_df = proto_avgs_1yr_df[ , order(names(proto_avgs_1yr_df))]
            
            colnames_to_add = names(proto_avgs_1yr)[names(proto_avgs_1yr) %!in% colnames(all_df)]
            all_df[colnames_to_add] = 0
            all_df = all_df[ , order(names(all_df))]
            
            #FOR EACH ROW IN ALL_DF CALCULATE 1yr COS SIM TO PROTOTYPE
            print("5")
            
            cosine_1yr_vals = vector()
            cosine_2yr_vals = vector()
            
            for(r in 1:nrow(all_df)){
              
              v1 = as.numeric(c(t(all_df[r,])))
              v2 = as.numeric(c(t(proto_avgs_1yr_df[1,])))
              cos_sim_mat = cosine(v1, v2)
              
              cosine_1yr_vals = c(cosine_1yr_vals, (cos_sim_mat[1]))
              
            }
            
            #CREATE TWO DFS WITH SAME COLUMNS FOR 2yr SCORES
            print("6")
            
            all_df = read.csv(f4)
            rownames(all_df) = all_df$X                                         #NOT SURE HOW TO LABEL THE ROWNAMES COLUMN WHEN WRITING CSV SO ID COLUMN IS BLANK ("X" WHEN READ)
            all_df = subset(all_df, select = -X)
            
            proto_avgs_2yr_df = as.data.frame(t(proto_avgs_2yr))
            colnames_to_add = colnames(all_df)[colnames(all_df) %!in% names(proto_avgs_2yr)]
            proto_avgs_2yr_df[colnames_to_add] = 0
            proto_avgs_2yr_df = proto_avgs_2yr_df[ , order(names(proto_avgs_2yr_df))]
            
            colnames_to_add = names(proto_avgs_2yr)[names(proto_avgs_2yr) %!in% colnames(all_df)]
            all_df[colnames_to_add] = 0
            all_df = all_df[ , order(names(all_df))]
            
            #FOR EACH ROW IN ALL_DF CALCULATE 1yr COS SIM TO PROTOTYPE
            print("7")
            
            for(r in 1:nrow(all_df)){
              
              v1 = as.numeric(c(t(all_df[r,])))
              v2 = as.numeric(c(t(proto_avgs_2yr_df[1,])))
              cos_sim_mat = cosine(v1, v2)
              
              cosine_2yr_vals = c(cosine_2yr_vals, (cos_sim_mat[1]))
              
            }          
            
            current_cos_sim_df = data.frame(id = rownames(all_df),
                                            cos_sim_1yr = cosine_1yr_vals,
                                            cos_sim_2yr = cosine_2yr_vals)
          }                                                                     #END OF IF STATEMENT
          
          tfs_df = rbind(tfs_df, current_cos_sim_df)
          print(nrow(tfs_df))
        }
      }

      tfs_df = tfs_df[tfs_df$id != "999999",]
      write.csv(tfs_df, paste0("C:/Users/gaoan/Desktop/step8_narrow/cos_sim/", paste(type,freq,score, sep="_"), ".csv"), row.names = F)
      
      
    }
  }
}

library(beepr)
beep(4)





















# for (fil in list.files("C:/Users/gaoan/Desktop/step8/matrices")){
#   print(fil)
#   print(file.size(fil))
# 
# }
































# 
# 
# 
# # rm(list=ls())
# # setwd("C:/Users/gaoan/Desktop/step8/subset_wordtype_all_pct_full_mc_allscore")
# setwd(sprintf("C:/Users/gaoan/Desktop/step8/subset_%s", current_dir))
# 
# '%!in%' <- Negate('%in%')
# 
# list_of_all_gameids = all_ids
# 
# list_of_genres = c("action adventure","action","role-playing","puzzle",
#                    "racing","miscellaneous","simulation","adventure",
#                    "sports","strategy")
# 
# # list_of_genres = c("action")
# 
# list_of_years = rev(as.numeric(c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")))
# 
# final_cos_sim_df = data.frame()
# 
# ###### ADD FOR LOOP HERE TO GO THROUGH GENRES 1 BY 1
# for(current_genre in list_of_genres){
#   
#   ################################################################################
#   # CREATE DF OF SIMILARITY SCORES FOR ALL GAME IN SUBSET BASED ON PREV. 1 YEAR PROTOTYPE
#   
#   full_cos_sim_df = data.frame()
#   
#   for (year in list_of_years[1:(length(list_of_years)-1)]){
#     
#     # CHECK IF CURRENT YEAR OR PREVIOUS YEAR IS EMPTY
#     
#     if(file.size(sprintf("%s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year)) > 4 &
#        file.size(sprintf("%s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year-1)) > 4){
#     
#       print(sprintf("calculating 1yr: %s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year))
#       
#       
#       # CURRENTLY YOURE CALCULATING THE SCORES FOR ONLY THE SAME GAMES AS THE PROTOTYPE
#       # PROTOTYPE IS BASED ON PARTICULAR COMBO OF: genre/yr/wordtype/frequency/score
#       # BUT THEN YOU NEED TO CALCULATE COS SIM FOR ALL GAMES FOR EACH genre/yr/wordtype COMBOS, DESPITE THEIR frequency/score
#       
#       
#       
#       df1 = read.csv(sprintf("%s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year))              
#       df2 = read.csv(sprintf("%s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year-1))
#       
#       # print(sprintf("df1: %s obs.",nrow(df1)))
#       
#       #NOT SURE HOW TO LABEL THE ROWNAMES COLUMN WHEN WRITING CSV SO ID COLUMN IS BLANK ("X" WHEN READ)
#       colnames(df1)[1] = "id"
#       df1$id = as.character(df1$id)
#       
#       colnames(df2)[1] = "id"
#       df2$id = as.character(df2$id)
#       
#       ################################################################################
#       # CALCULATE AVERAGES FOR PROTOTYPE
#       df2 = rbind(df2, c("Total",colSums(df2[,2:ncol(df2)])))
#       
#       library(dplyr)
#       df2[,2:ncol(df2)] <- mutate_all(df2[,2:ncol(df2)], function(x) as.numeric(as.character(x)))
#       
#       df2 = rbind(df2, c("avg", colSums(df2[,2:ncol(df2)]) / (nrow(df2) - 1)))                    # IS THIS RIGHT????
#       
#       df2 = df2[-c(nrow(df2)-1),]
#       
#       rownames(df2) <- NULL
#       
#       # df1 = rbind(df1, as.data.frame(df2[nrow(df2),1:ncol(df2)]))
#       
#       #####################################################
#       # CALCULATE COSINE SIMILARITY
#       
#       library(lsa)
#       
#       rownames(df1) = df1$id
#       
#       prototype = as.numeric(df2[nrow(df2), 2:ncol(df2)])
#       
#       cosine_vals = vector()
#       
#       for(i in 1:nrow(df1)){
#         # print(i)
#         v1 = as.numeric(df1[i,2:ncol(df1)])
#         c_mat = cosine(v1, prototype)
#         cosine_vals = c(cosine_vals, (c_mat[1]))
#       }
#       
#       current_cos_sim_df = data.frame(year = year, id = df1$id, cos_sim = cosine_vals)
#     
#       # missing_ids_df = data.frame()                                              #DONT NEED THIS ANYMORE
#       # 
#       # list_of_all_gameids[1] %!in% df1$id
#       #   
#       # for(gameid in list_of_all_gameids){
#       #   if(gameid %!in% df1$id){
#       #     missing_ids_df = rbind(missing_ids_df, data.frame(year = year, id = gameid, cos_sim = NA))
#       #   }
#       # }
#       # 
#       # current_cos_sim_df = rbind(current_cos_sim_df, missing_ids_df)
#     
#       print(sprintf("appending: %s obs.",nrow(current_cos_sim_df)))
#       
#     }
#   
#     # IF EITHER THE CURRENT YEAR OR THE PREVIOUS YEAR IS EMPTY DO THIS
#     
#     if(file.size(sprintf("%s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year)) <= 4 |
#        file.size(sprintf("%s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year-1)) <= 4){
#     
#       print(sprintf("empty: %s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year))
#       
#       current_cos_sim_df = data.frame(year = year, id = 999999, cos_sim = NA)
#   
#     }
#   
#     
#     full_cos_sim_df = rbind(full_cos_sim_df, current_cos_sim_df)
# 
#   }
#   
#   full_cos_sim_df_1yr = full_cos_sim_df
#   # write.csv(full_cos_sim_df_1yr, "simlilarities_1yr_action_adventure_wordtype_all_pct_full_mc_allscore.csv", row.names = F)
#   
#   
#   ################################################################################
#   
#   ################################################################################
#   # CREATE DF OF SIMILARITY SCORES FOR ALL GAME IN SUBSET BASED ON PREV. 2 YEARs PROTOTYPE
#   
#   full_cos_sim_df = data.frame()
#   
# 
#   
#   for (year in list_of_years[1:(length(list_of_years)-2)]){
#     
#     # CHECK IF CURRENT YEAR OR EITHER OF 2 PREVIOUS YEARs ARE EMPTY
#     
#     if(file.size(sprintf("%s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year)) > 4 &
#        file.size(sprintf("%s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year-1)) > 4 &
#        file.size(sprintf("%s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year-2)) > 4){
#       
#       print(sprintf("calculating 2yrs: %s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year))
#       
#       df1 = read.csv(sprintf("%s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year))
#       
#       print(sprintf("df1: %s obs.",nrow(df1)))
#       
#       yr_minus_1 = read.csv(sprintf("%s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year-1))
#       yr_minus_2 = read.csv(sprintf("%s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year-2))
#        
#       df2 = rbind(yr_minus_1, yr_minus_2)
#       # df2 = df2[!duplicated(df2),]
#       
#       #NOT SURE HOW TO LABEL THE ROWNAMES COLUMN WHEN WRITING CSV SO ID COLUMN IS BLANK ("X" WHEN READ)
#       colnames(df1)[1] = "id"
#       df1$id = as.character(df1$id)
#       
#       colnames(df2)[1] = "id"
#       df2$id = as.character(df2$id)
#       
#       ################################################################################
#       # CALCULATE AVERAGES FOR PROTOTYPE
#       
#       df2 = rbind(df2, c("Total",colSums(df2[,2:ncol(df2)])))
#       
#       library(dplyr)
#       df2[,2:ncol(df2)] <- mutate_all(df2[,2:ncol(df2)], function(x) as.numeric(as.character(x)))
#       
#       df2 = rbind(df2, c("avg", colSums(df2[,2:ncol(df2)]) / (nrow(df2) - 1)))                    # IS THIS RIGHT????
#       
#       df2 = df2[-c(nrow(df2)-1),]
#       
#       rownames(df2) <- NULL
#       
#       # df1 = rbind(df1, as.data.frame(df2[nrow(df2),1:ncol(df2)]))
#       
#       #####################################################
#       # CALCULATE COSINE SIMILARITY
#       
#       library(lsa)
#       
#       rownames(df1) = df1$id
#       
#       prototype = as.numeric(df2[nrow(df2), 2:ncol(df2)])
#       
#       cosine_vals = vector()                                                    # length(prototype)
#       
#       for(i in 1:nrow(df1)){
#         # print(i)
#         v1 = as.numeric(df1[i,2:ncol(df1)])    
#         c_mat = cosine(v1, prototype)                                            #   length(v1)
#         cosine_vals = c(cosine_vals, (c_mat[1]))
#       }
#       
#       current_cos_sim_df = data.frame(year = year, id = df1$id, cos_sim = cosine_vals)
#       
#       # missing_ids_df = data.frame()                                             #DONT NEED THIS ANYMORE
#       # 
#       # for(gameid in list_of_all_gameids){
#       #   if(gameid %!in% df1$id){
#       #     missing_ids_df = rbind(missing_ids_df, data.frame(year = year, id = gameid, cos_sim = NA))
#       #   }
#       # }
#       # 
#       # current_cos_sim_df = rbind(current_cos_sim_df, missing_ids_df)
#       
#       
#       
#       print(sprintf("appending: %s obs.",nrow(current_cos_sim_df)))
# 
#     }
#     
#     # IF EITHER THE CURRENT YEAR OR THE PREVIOUS YEAR IS EMPTY DO THIS
#     
#     if(file.size(sprintf("%s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year)) <= 4 |
#        file.size(sprintf("%s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year-1)) <= 4 |
#        file.size(sprintf("%s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year-2)) <= 4){
#       
#       print(sprintf("empty: %s_%s_%s.csv", sub(" ", "_", current_genre), current_dir, year))
#       
#       current_cos_sim_df = data.frame(year = year, id = 999999, cos_sim = NA)
#       
#     }
#     
#     full_cos_sim_df = rbind(full_cos_sim_df, current_cos_sim_df)
#     
#     
#   }
# 
#   full_cos_sim_df_2yr = full_cos_sim_df
#   # write.csv(full_cos_sim_df_2yr, "simlilarities_2yr_action_adventure_wordtype_all_pct_full_mc_allscore.csv", row.names = F) 
#   
# 
#   
#   ################################################################################
#   # MERGE THE TWO TOGETHER
#   
#   together_full_cos_sim_df = merge(full_cos_sim_df_1yr, full_cos_sim_df_2yr, by=c("year","id"), all = T)
#   colnames(together_full_cos_sim_df) = c("year","id","sim_prev1yr","sim_prev2yr")
#   
#   # write.csv(together_full_cos_sim_df, "simlilarities_1yr_and_2yr_action_adventure_wordtype_all_pct_full_mc_allscore.csv", row.names = F)
#   
#   final_cos_sim_df = rbind(final_cos_sim_df, together_full_cos_sim_df)
#   
# } 
# 
# 
# ################################################################################
# # WRITE THE RESULT TO FILE
# 
# 
# final_cos_sim_df = final_cos_sim_df[, c("id", "sim_prev1yr", "sim_prev2yr")]
# 
# final_cos_sim_df = final_cos_sim_df[final_cos_sim_df$id != "999999",]
# 
# not_in_final = vector()
# 
# for(i in all_ids){
#   if(as.integer(i) %!in% final_cos_sim_df$id){
#     not_in_final = c(not_in_final,i)
#   }
# }
# 
# for(j in not_in_final){
#   temp_df = data.frame(id = as.numeric(j), sim_prev1yr = NA, sim_prev2yr = NA)
#   final_cos_sim_df = rbind(final_cos_sim_df, temp_df)
# }
# 
# write.csv(final_cos_sim_df, "sim_scores.csv", row.names = F)
# 
# library(beepr)
# beep(4)
# 
# 






















# 
# 
# test_df2 = info[info$game_id %in% all_ids, ]
# nrow(test_df2)
# test_df2 = test_df2[test_df2$release_year != "2005",]
# nrow(test_df2)
# 
# 
# test_df2 = test_df2[test_df2$game_id %in% not_in_final, ]
# 
# 
# 
# 
# 
# 
# test_df2 = test_df2[test_df2$release_year != 2005,]
# str(test_df2)
# length(unique(test_df2$game_id))
# 
# 
# unique(test_df2$game_id[test_df2$genre_wide == "simulation" & test_df2$release_year != 2005])


# JUST CHECKING THAT ALL THE GAMES ARE ACCOUNTED FOR (E.G., 2007-2014 THERE WERE 260 AA GAMES)
# test_df = together_full_cos_sim_df[, c("id", "sim_previous1year", "sim_previous2years")]
# test_df = na.omit(test_df)
# test_df = test_df[test_df$id %in% all_ids, ]
# test_df2 = info[info$game_id %in% all_ids, ]
# test_df2 =test_df2[test_df2$genre_wide == "action adventure", ]
# test_df2 = test_df2[test_df2$release_year > 2006, ]                   #260

# test_df = together_full_cos_sim_df[, c("id", "sim_previous1year", "sim_previous2years")]
# test_df = na.omit(test_df)
# test_df = test_df[test_df$id %in% all_ids, ]
# test_df2 = info[info$game_id %in% all_ids, ]
# test_df2 =test_df2[test_df2$genre_wide == "action", ]
# test_df2 = test_df2[test_df2$release_year > 2006, ]                   #1060
















################################################################################
################################################################################
# TEST ONE OFF CASE HERE
# 
# df1 = read.csv("action_adventure_wordtype_all_pct_full_mc_allscore_2006.csv")
# df2 = read.csv("action_adventure_wordtype_all_pct_full_mc_allscore_2005.csv")
# 
# #NOT SURE HOW TO LABEL THE ROWNAMES COLUMN WHEN WRITING CSV SO ID COLUMN IS BLANK ("X" WHEN READ)
# colnames(df1)[1] = "id"
# df1$id = as.character(df1$id)
# 
# colnames(df2)[1] = "id"
# df2$id = as.character(df2$id)
# 
# ################################################################################
# # CALCULATE AVERAGES FOR PROTOTYPE
# 
# df2 = rbind(df2, c("Total",colSums(df2[,2:ncol(df2)])))
# 
# library(dplyr)
# df2[,2:ncol(df2)] <- mutate_all(df2[,2:ncol(df2)], function(x) as.numeric(as.character(x)))
# 
# df2 = rbind(df2, c("avg", colSums(df2[,2:ncol(df2)]) / (nrow(df2) - 1)))                    # IS THIS RIGHT????
# 
# df2 = df2[-c(nrow(df2)-1),]
# 
# rownames(df2) <- NULL
# 
# # df1 = rbind(df1, as.data.frame(df2[nrow(df2),1:ncol(df2)]))
# 
# #####################################################
# # CALCULATE COSINE SIMILARITY
# 
# library(lsa)
# 
# rownames(df1) = df1$id
# 
# prototype = as.numeric(df2[nrow(df2), 2:ncol(df2)])
# 
# cosine_vals = vector()
# 
# for(i in 1:nrow(df1)){
#   print(i)
#   v1 = as.numeric(df1[i,2:ncol(df1)])
#   c_mat = cosine(v1, prototype)
#   cosine_vals = c(cosine_vals, (c_mat[1]))
# }
# 
# cos_sim_df = data.frame(id = df1$id, cos_sim = cosine_vals)
