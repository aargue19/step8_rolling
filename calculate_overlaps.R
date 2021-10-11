game_world_words = df[df$wordtype_world == 1,]
nrow(game_world_words) #25289
game_world_words = game_world_words[game_world_words$mc_75score == 1,]
nrow(game_world_words) #7735
game_world_words = unique(game_world_words$std_word)
length(game_world_words) #824


game_mech_words = df[df$wordtype_mech == 1,]
nrow(game_mech_words) #10,304
game_mech_words = game_mech_words[game_mech_words$mc_75score == 1,]
nrow(game_mech_words) #2927
game_mech_words = unique(game_mech_words$std_word)
length(game_mech_words) #309

# calculate overlaps

gw_df = as.data.frame(game_world_words)
colnames(gw_df) = "word"
gw_df$overlap = 0
gw_df$overlap[gw_df$word %in% gm_df$word] = 1
write.csv(as.data.frame(gw_df), "all_world_words.csv", row.names = F)

gm_df = as.data.frame(game_mech_words)
colnames(gm_df) = "word"
gm_df$overlap = 0
gm_df$overlap[gm_df$word %in% gw_df$word] = 1
write.csv(as.data.frame(gm_df), "all_mech_words.csv", row.names = F)

# calculate percentages

sum(gw_df$overlap) / nrow(gw_df)
nrow(gw_df[gw_df$overlap == 1,])

sum(gm_df$overlap) / nrow(gm_df)
nrow(gm_df[gm_df$overlap == 1,])

overlap_vec = intersect(gw_df$word,gm_df$word)

full_vec = unique(c(gw_df$word, gm_df$word))

length(overlap_vec) / length(full_vec)

sum(gw_df$overlap)
sum(gm_df$overlap)
