# USE THIS TO CALCULATE ALL COS SIMS BASED ON PROTOTYPES USING 1 AND 2 YEARS
df = read.csv("C:/Users/gaoan/Desktop/step8_narrow/merged_data_w_dummies.csv")

setwd("C:/Users/gaoan/Desktop/step8_narrow/cos_sim")
ids_no_2005_2006 = unique(df$id[df$release_year != "2005" & df$release_year != "2006"])
big_df = data.frame(id = ids_no_2005_2006)

for (fil in list.files("C:/Users/gaoan/Desktop/step8_narrow/cos_sim")){
  
  cname = sub(".csv", "", fil)
  print(cname)
  
  current_df = read.csv(fil)
  current_df$X = NULL
  
  cname1yr = paste(cname, "1yr", sep="_")
  cname2yr = paste(cname, "2yr", sep="_")
  
  colnames(current_df) = c("id", cname1yr, cname2yr)
  
  big_df = merge(big_df, current_df, by= "id", all.x = T)
  print(ncol(big_df))
}

write.csv(big_df, "all_cos_sims_narrow.csv", row.names = F)