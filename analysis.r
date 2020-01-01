library(tidyverse)
library(ggplot2)

meta <- read_csv("language metadata.csv")
childes <- read_csv("childes_brown-results.csv")
breton <- read_csv("ud_bre-results.csv")
welsh <- read_csv("ud_cym-results.csv")
german <- read_csv("ud_deu-results.csv")
english <- read_csv("ud_eng-results.csv")
basque <- read_csv("ud_eus-results.csv")
irish <- read_csv("ud_gae-results.csv")
scottish_gaelic <- read_csv("ud_gai-results.csv")
hindi <- read_csv("ud_hin-results.csv")
hungarian <- read_csv("ud_hun-results.csv")
indonesian <- read_csv("ud_ind-results.csv")
japanese <- read_csv("ud_ja-results.csv")
korean <- read_csv("ud_kor-results.csv")
mbya_guarani <- read_csv("ud_mgu-results.csv")
dutch <- read_csv("ud_ned-results.csv")
tagalog <- read_csv("ud_tag-results.csv")
turkish <- read_csv("ud_tur-results.csv")
vietnamese <- read_csv("ud_vie-results.csv")
warlpiri <- read_csv("ud_war-results.csv")
wolof <- read_csv("ud_wol-results.csv")
mandarin <- read_csv("ud_zho-results.csv")

calcEntRankings <- function(df) {
  svo <- filter(df, Word_Order == "SVO")
  sov <- filter(df, Word_Order == "SOV")
  vso <- filter(df, Word_Order == "VSO")
  vos <- filter(df, Word_Order == "VOS")
  ovs <- filter(df, Word_Order == "OVS")
  osv <- filter(df, Word_Order == "OSV")
  
  svo_ent_mean <- weighted.mean(svo$Entropy_Deviation_Score, svo$Probability)
  sov_ent_mean <- weighted.mean(sov$Entropy_Deviation_Score, sov$Probability)
  vso_ent_mean <- weighted.mean(vso$Entropy_Deviation_Score, vso$Probability)
  vos_ent_mean <- weighted.mean(vos$Entropy_Deviation_Score, vos$Probability)
  ovs_ent_mean <- weighted.mean(ovs$Entropy_Deviation_Score, ovs$Probability)
  osv_ent_mean <- weighted.mean(osv$Entropy_Deviation_Score, osv$Probability)
  
  tribble(
    ~word_order, ~score,
    "OVS", ovs_ent_mean,
    "SOV", sov_ent_mean,
    "OSV", osv_ent_mean,
    "VOS", vos_ent_mean,
    "VSO", vso_ent_mean,
    "SVO", svo_ent_mean
  ) %>% arrange(score)
}

calcSurpRankings <- function(df) {
  svo <- filter(df, Word_Order == "SVO")
  sov <- filter(df, Word_Order == "SOV")
  vso <- filter(df, Word_Order == "VSO")
  vos <- filter(df, Word_Order == "VOS")
  ovs <- filter(df, Word_Order == "OVS")
  osv <- filter(df, Word_Order == "OSV")
  
  svo_surp_mean <- weighted.mean(svo$Surprisal_Deviation_Score, svo$Probability)
  sov_surp_mean <- weighted.mean(sov$Surprisal_Deviation_Score, sov$Probability)
  vso_surp_mean <- weighted.mean(vso$Surprisal_Deviation_Score, vso$Probability)
  vos_surp_mean <- weighted.mean(vos$Surprisal_Deviation_Score, vos$Probability)
  ovs_surp_mean <- weighted.mean(ovs$Surprisal_Deviation_Score, ovs$Probability)
  osv_surp_mean <- weighted.mean(osv$Surprisal_Deviation_Score, osv$Probability)
  
  tribble(
    ~word_order, ~score,
    "OVS", ovs_surp_mean,
    "SOV", sov_surp_mean,
    "OSV", osv_surp_mean,
    "VOS", vos_surp_mean,
    "VSO", vso_surp_mean,
    "SVO", svo_surp_mean
  ) %>% arrange(score)
}

calcMIRankings <- function(df) {
  svo <- filter(df, Word_Order == "SVO")
  sov <- filter(df, Word_Order == "SOV")
  vso <- filter(df, Word_Order == "VSO")
  vos <- filter(df, Word_Order == "VOS")
  ovs <- filter(df, Word_Order == "OVS")
  osv <- filter(df, Word_Order == "OSV")
  
  svo_mi_mean <- weighted.mean(svo$Mutual_Information, svo$Probability)
  sov_mi_mean <- weighted.mean(sov$Mutual_Information, sov$Probability)
  vso_mi_mean <- weighted.mean(vso$Mutual_Information, vso$Probability)
  vos_mi_mean <- weighted.mean(vos$Mutual_Information, vos$Probability)
  ovs_mi_mean <- weighted.mean(ovs$Mutual_Information, ovs$Probability)
  osv_mi_mean <- weighted.mean(osv$Mutual_Information, osv$Probability)
  
tribble(
    ~word_order, ~score,
    "OVS", ovs_mi_mean,
    "SOV", sov_mi_mean,
    "OSV", osv_mi_mean,
    "VOS", vos_mi_mean,
    "VSO", vso_mi_mean,
    "SVO", svo_mi_mean
  ) %>% arrange(desc(score))
}

calcZScoreRankings <- function(df) {
  svo <- filter(df, Word_Order == "SVO")
  sov <- filter(df, Word_Order == "SOV")
  vso <- filter(df, Word_Order == "VSO")
  vos <- filter(df, Word_Order == "VOS")
  ovs <- filter(df, Word_Order == "OVS")
  osv <- filter(df, Word_Order == "OSV")
  
  svo_z_mean <- weighted.mean(svo$mi_z_score, svo$Probability)
  sov_z_mean <- weighted.mean(sov$mi_z_score, sov$Probability)
  vso_z_mean <- weighted.mean(vso$mi_z_score, vso$Probability)
  vos_z_mean <- weighted.mean(vos$mi_z_score, vos$Probability)
  ovs_z_mean <- weighted.mean(ovs$mi_z_score, ovs$Probability)
  osv_z_mean <- weighted.mean(osv$mi_z_score, osv$Probability)
  
  tribble(
    ~word_order, ~mi_z_score,
    "OVS", ovs_z_mean,
    "SOV", sov_z_mean,
    "OSV", osv_z_mean,
    "VOS", vos_z_mean,
    "VSO", vso_z_mean,
    "SVO", svo_z_mean
  ) %>% arrange(desc(mi_z_score))
}

childes_meta <- filter(meta, Language == "childes-brown-eng") 
breton_meta <- filter(meta, Language == "ud-breton")
welsh_meta <- filter(meta, Language == "ud-welsh")
german_meta <- filter(meta, Language == "ud-german")
english_meta <- filter(meta, Language == "ud-english")
basque_meta <- filter(meta, Language == "ud-basque")
irish_meta <- filter(meta, Language == "ud-irish")
scottish_gaelic_meta <- filter(meta, Language == "ud-scottish_gaelic")
hindi_meta <- filter(meta, Language == "ud-hindi")
hungarian_meta <- filter(meta, Language == "ud-hungarian")
indonesian_meta <- filter(meta, Language == "ud-indonesian")
japanese_meta <- filter(meta, Language == "ud-japanese")
korean_meta <- filter(meta, Language == "ud-korean")
mbya_guarani_meta <- filter(meta, Language == "ud-mbya_guarani")
dutch_meta <- filter(meta, Language == "ud-dutch")
tagalog_meta <- filter(meta, Language == "ud-tagalog")
turkish_meta <- filter(meta, Language == "ud-turkish")
vietnamese_meta <- filter(meta, Language == "ud-vietnamese")
warlpiri_meta <- filter(meta, Language == "ud-warlpiri")
wolof_meta <- filter(meta, Language == "ud-wolof")
mandarin_meta <- filter(meta, Language == "ud-mandarin")

childes_ent_Rankings <- childes %>% calcEntRankings
breton_ent_Rankings <- breton %>% calcEntRankings
welsh_ent_Rankings <- welsh %>% calcEntRankings
german_ent_Rankings <- german %>% calcEntRankings
english_ent_Rankings <- english %>% calcEntRankings
basque_ent_Rankings <- basque %>% calcEntRankings
irish_ent_Rankings <- irish %>% calcEntRankings
scottish_gaelic_ent_Rankings <- scottish_gaelic %>% calcEntRankings
hindi_ent_Rankings <- hindi %>% calcEntRankings
hungarian_ent_Rankings <- hungarian %>% calcEntRankings
indonesian_ent_Rankings <- indonesian %>% calcEntRankings
japanese_ent_Rankings <- japanese %>% calcEntRankings
korean_ent_Rankings <- korean %>% calcEntRankings
mbya_guarani_ent_Rankings <- mbya_guarani %>% calcEntRankings
dutch_ent_Rankings <- dutch %>% calcEntRankings
tagalog_ent_Rankings <- tagalog %>% calcEntRankings
turkish_ent_Rankings <- turkish %>% calcEntRankings
vietnamese_ent_Rankings <- vietnamese %>% calcEntRankings
warlpiri_ent_Rankings <- warlpiri %>% calcEntRankings
wolof_ent_Rankings <- wolof %>% calcEntRankings
mandarin_ent_Rankings <- mandarin %>% calcEntRankings

childes_surp_Rankings <- childes %>% calcSurpRankings
breton_surp_Rankings <- breton %>% calcSurpRankings
welsh_surp_Rankings <- welsh %>% calcSurpRankings
german_surp_Rankings <- german %>% calcSurpRankings
english_surp_Rankings <- english %>% calcSurpRankings
basque_surp_Rankings <- basque %>% calcSurpRankings
irish_surp_Rankings <- irish %>% calcSurpRankings
scottish_gaelic_surp_Rankings <- scottish_gaelic %>% calcSurpRankings
hindi_surp_Rankings <- hindi %>% calcSurpRankings
hungarian_surp_Rankings <- hungarian %>% calcSurpRankings
indonesian_surp_Rankings <- indonesian %>% calcSurpRankings
japanese_surp_Rankings <- japanese %>% calcSurpRankings
korean_surp_Rankings <- korean %>% calcSurpRankings
mbya_guarani_surp_Rankings <- mbya_guarani %>% calcSurpRankings
dutch_surp_Rankings <- dutch %>% calcSurpRankings
tagalog_surp_Rankings <- tagalog %>% calcSurpRankings
turkish_surp_Rankings <- turkish %>% calcSurpRankings
vietnamese_surp_Rankings <- vietnamese %>% calcSurpRankings
warlpiri_surp_Rankings <- warlpiri %>% calcSurpRankings
wolof_surp_Rankings <- wolof %>% calcSurpRankings
mandarin_surp_Rankings <- mandarin %>% calcSurpRankings

childes_mi_Rankings <- childes %>% calcMIRankings
breton_mi_Rankings <- breton %>% calcMIRankings
welsh_mi_Rankings <- welsh %>% calcMIRankings
german_mi_Rankings <- german %>% calcMIRankings
english_mi_Rankings <- english %>% calcMIRankings
basque_mi_Rankings <- basque %>% calcMIRankings
irish_mi_Rankings <- irish %>% calcMIRankings
scottish_gaelic_mi_Rankings <- scottish_gaelic %>% calcMIRankings
hindi_mi_Rankings <- hindi %>% calcMIRankings
hungarian_mi_Rankings <- hungarian %>% calcMIRankings
indonesian_mi_Rankings <- indonesian %>% calcMIRankings
japanese_mi_Rankings <- japanese %>% calcMIRankings
korean_mi_Rankings <- korean %>% calcMIRankings
mbya_guarani_mi_Rankings <- mbya_guarani %>% calcMIRankings
dutch_mi_Rankings <- dutch %>% calcMIRankings
tagalog_mi_Rankings <- tagalog %>% calcMIRankings
turkish_mi_Rankings <- turkish %>% calcMIRankings
vietnamese_mi_Rankings <- vietnamese %>% calcMIRankings
warlpiri_mi_Rankings <- warlpiri %>% calcMIRankings
wolof_mi_Rankings <- wolof %>% calcMIRankings
mandarin_mi_Rankings <- mandarin %>% calcMIRankings

big_table_o_data <- tribble(
  ~language, ~measure, ~word_order, ~score,
  "English CHILDES", "entropy UID", "SVO", filter(childes_ent_Rankings, word_order == "SVO")$score,
  "English CHILDES", "entropy UID", "SOV", filter(childes_ent_Rankings, word_order == "SOV")$score,
  "English CHILDES", "entropy UID", "VSO", filter(childes_ent_Rankings, word_order == "VSO")$score,
  "English CHILDES", "entropy UID", "VOS", filter(childes_ent_Rankings, word_order == "VOS")$score,
  "English CHILDES", "entropy UID", "OVS", filter(childes_ent_Rankings, word_order == "OVS")$score,
  "English CHILDES", "entropy UID", "OSV", filter(childes_ent_Rankings, word_order == "OSV")$score,
  "English CHILDES", "surprisal UID", "SVO", filter(childes_surp_Rankings, word_order == "SVO")$score,
  "English CHILDES", "surprisal UID", "SOV", filter(childes_surp_Rankings, word_order == "SOV")$score,
  "English CHILDES", "surprisal UID", "VSO", filter(childes_surp_Rankings, word_order == "VSO")$score,
  "English CHILDES", "surprisal UID", "VOS", filter(childes_surp_Rankings, word_order == "VOS")$score,
  "English CHILDES", "surprisal UID", "OVS", filter(childes_surp_Rankings, word_order == "OVS")$score,
  "English CHILDES", "surprisal UID", "OSV", filter(childes_surp_Rankings, word_order == "OSV")$score,
  "English CHILDES", "mi", "SVO", filter(childes_mi_Rankings, word_order == "SVO")$score,
  "English CHILDES", "mi", "SOV", filter(childes_mi_Rankings, word_order == "SOV")$score,
  "English CHILDES", "mi", "VSO", filter(childes_mi_Rankings, word_order == "VSO")$score,
  "English CHILDES", "mi", "VOS", filter(childes_mi_Rankings, word_order == "VOS")$score,
  "English CHILDES", "mi", "OVS", filter(childes_mi_Rankings, word_order == "OVS")$score,
  "English CHILDES", "mi", "OSV", filter(childes_mi_Rankings, word_order == "OSV")$score,
  "Breton", "entropy UID", "SVO", filter(breton_ent_Rankings, word_order == "SVO")$score,
  "Breton", "entropy UID", "SOV", filter(breton_ent_Rankings, word_order == "SOV")$score,
  "Breton", "entropy UID", "VSO", filter(breton_ent_Rankings, word_order == "VSO")$score,
  "Breton", "entropy UID", "VOS", filter(breton_ent_Rankings, word_order == "VOS")$score,
  "Breton", "entropy UID", "OVS", filter(breton_ent_Rankings, word_order == "OVS")$score,
  "Breton", "entropy UID", "OSV", filter(breton_ent_Rankings, word_order == "OSV")$score,
  "Breton", "surprisal UID", "SVO", filter(breton_surp_Rankings, word_order == "SVO")$score,
  "Breton", "surprisal UID", "SOV", filter(breton_surp_Rankings, word_order == "SOV")$score,
  "Breton", "surprisal UID", "VSO", filter(breton_surp_Rankings, word_order == "VSO")$score,
  "Breton", "surprisal UID", "VOS", filter(breton_surp_Rankings, word_order == "VOS")$score,
  "Breton", "surprisal UID", "OVS", filter(breton_surp_Rankings, word_order == "OVS")$score,
  "Breton", "surprisal UID", "OSV", filter(breton_surp_Rankings, word_order == "OSV")$score,
  "Breton", "mi", "SVO", filter(breton_mi_Rankings, word_order == "SVO")$score,
  "Breton", "mi", "SOV", filter(breton_mi_Rankings, word_order == "SOV")$score,
  "Breton", "mi", "VSO", filter(breton_mi_Rankings, word_order == "VSO")$score,
  "Breton", "mi", "VOS", filter(breton_mi_Rankings, word_order == "VOS")$score,
  "Breton", "mi", "OVS", filter(breton_mi_Rankings, word_order == "OVS")$score,
  "Breton", "mi", "OSV", filter(breton_mi_Rankings, word_order == "OSV")$score,
  "Welsh", "entropy UID", "SVO", filter(welsh_ent_Rankings, word_order == "SVO")$score,
  "Welsh", "entropy UID", "SOV", filter(welsh_ent_Rankings, word_order == "SOV")$score,
  "Welsh", "entropy UID", "VSO", filter(welsh_ent_Rankings, word_order == "VSO")$score,
  "Welsh", "entropy UID", "VOS", filter(welsh_ent_Rankings, word_order == "VOS")$score,
  "Welsh", "entropy UID", "OVS", filter(welsh_ent_Rankings, word_order == "OVS")$score,
  "Welsh", "entropy UID", "OSV", filter(welsh_ent_Rankings, word_order == "OSV")$score,
  "Welsh", "surprisal UID", "SVO", filter(welsh_surp_Rankings, word_order == "SVO")$score,
  "Welsh", "surprisal UID", "SOV", filter(welsh_surp_Rankings, word_order == "SOV")$score,
  "Welsh", "surprisal UID", "VSO", filter(welsh_surp_Rankings, word_order == "VSO")$score,
  "Welsh", "surprisal UID", "VOS", filter(welsh_surp_Rankings, word_order == "VOS")$score,
  "Welsh", "surprisal UID", "OVS", filter(welsh_surp_Rankings, word_order == "OVS")$score,
  "Welsh", "surprisal UID", "OSV", filter(welsh_surp_Rankings, word_order == "OSV")$score,
  "Welsh", "mi", "SVO", filter(welsh_mi_Rankings, word_order == "SVO")$score,
  "Welsh", "mi", "SOV", filter(welsh_mi_Rankings, word_order == "SOV")$score,
  "Welsh", "mi", "VSO", filter(welsh_mi_Rankings, word_order == "VSO")$score,
  "Welsh", "mi", "VOS", filter(welsh_mi_Rankings, word_order == "VOS")$score,
  "Welsh", "mi", "OVS", filter(welsh_mi_Rankings, word_order == "OVS")$score,
  "Welsh", "mi", "OSV", filter(welsh_mi_Rankings, word_order == "OSV")$score,
  "German", "entropy UID", "SVO", filter(german_ent_Rankings, word_order == "SVO")$score,
  "German", "entropy UID", "SOV", filter(german_ent_Rankings, word_order == "SOV")$score,
  "German", "entropy UID", "VSO", filter(german_ent_Rankings, word_order == "VSO")$score,
  "German", "entropy UID", "VOS", filter(german_ent_Rankings, word_order == "VOS")$score,
  "German", "entropy UID", "OVS", filter(german_ent_Rankings, word_order == "OVS")$score,
  "German", "entropy UID", "OSV", filter(german_ent_Rankings, word_order == "OSV")$score,
  "German", "surprisal UID", "SVO", filter(german_surp_Rankings, word_order == "SVO")$score,
  "German", "surprisal UID", "SOV", filter(german_surp_Rankings, word_order == "SOV")$score,
  "German", "surprisal UID", "VSO", filter(german_surp_Rankings, word_order == "VSO")$score,
  "German", "surprisal UID", "VOS", filter(german_surp_Rankings, word_order == "VOS")$score,
  "German", "surprisal UID", "OVS", filter(german_surp_Rankings, word_order == "OVS")$score,
  "German", "surprisal UID", "OSV", filter(german_surp_Rankings, word_order == "OSV")$score,
  "German", "mi", "SVO", filter(german_mi_Rankings, word_order == "SVO")$score,
  "German", "mi", "SOV", filter(german_mi_Rankings, word_order == "SOV")$score,
  "German", "mi", "VSO", filter(german_mi_Rankings, word_order == "VSO")$score,
  "German", "mi", "VOS", filter(german_mi_Rankings, word_order == "VOS")$score,
  "German", "mi", "OVS", filter(german_mi_Rankings, word_order == "OVS")$score,
  "German", "mi", "OSV", filter(german_mi_Rankings, word_order == "OSV")$score,
  "English", "entropy UID", "SVO", filter(english_ent_Rankings, word_order == "SVO")$score,
  "English", "entropy UID", "SOV", filter(english_ent_Rankings, word_order == "SOV")$score,
  "English", "entropy UID", "VSO", filter(english_ent_Rankings, word_order == "VSO")$score,
  "English", "entropy UID", "VOS", filter(english_ent_Rankings, word_order == "VOS")$score,
  "English", "entropy UID", "OVS", filter(english_ent_Rankings, word_order == "OVS")$score,
  "English", "entropy UID", "OSV", filter(english_ent_Rankings, word_order == "OSV")$score,
  "English", "surprisal UID", "SVO", filter(english_surp_Rankings, word_order == "SVO")$score,
  "English", "surprisal UID", "SOV", filter(english_surp_Rankings, word_order == "SOV")$score,
  "English", "surprisal UID", "VSO", filter(english_surp_Rankings, word_order == "VSO")$score,
  "English", "surprisal UID", "VOS", filter(english_surp_Rankings, word_order == "VOS")$score,
  "English", "surprisal UID", "OVS", filter(english_surp_Rankings, word_order == "OVS")$score,
  "English", "surprisal UID", "OSV", filter(english_surp_Rankings, word_order == "OSV")$score,
  "English", "mi", "SVO", filter(english_mi_Rankings, word_order == "SVO")$score,
  "English", "mi", "SOV", filter(english_mi_Rankings, word_order == "SOV")$score,
  "English", "mi", "VSO", filter(english_mi_Rankings, word_order == "VSO")$score,
  "English", "mi", "VOS", filter(english_mi_Rankings, word_order == "VOS")$score,
  "English", "mi", "OVS", filter(english_mi_Rankings, word_order == "OVS")$score,
  "English", "mi", "OSV", filter(english_mi_Rankings, word_order == "OSV")$score,
  "Basque", "entropy UID", "SVO", filter(basque_ent_Rankings, word_order == "SVO")$score,
  "Basque", "entropy UID", "SOV", filter(basque_ent_Rankings, word_order == "SOV")$score,
  "Basque", "entropy UID", "VSO", filter(basque_ent_Rankings, word_order == "VSO")$score,
  "Basque", "entropy UID", "VOS", filter(basque_ent_Rankings, word_order == "VOS")$score,
  "Basque", "entropy UID", "OVS", filter(basque_ent_Rankings, word_order == "OVS")$score,
  "Basque", "entropy UID", "OSV", filter(basque_ent_Rankings, word_order == "OSV")$score,
  "Basque", "surprisal UID", "SVO", filter(basque_surp_Rankings, word_order == "SVO")$score,
  "Basque", "surprisal UID", "SOV", filter(basque_surp_Rankings, word_order == "SOV")$score,
  "Basque", "surprisal UID", "VSO", filter(basque_surp_Rankings, word_order == "VSO")$score,
  "Basque", "surprisal UID", "VOS", filter(basque_surp_Rankings, word_order == "VOS")$score,
  "Basque", "surprisal UID", "OVS", filter(basque_surp_Rankings, word_order == "OVS")$score,
  "Basque", "surprisal UID", "OSV", filter(basque_surp_Rankings, word_order == "OSV")$score,
  "Basque", "mi", "SVO", filter(basque_mi_Rankings, word_order == "SVO")$score,
  "Basque", "mi", "SOV", filter(basque_mi_Rankings, word_order == "SOV")$score,
  "Basque", "mi", "VSO", filter(basque_mi_Rankings, word_order == "VSO")$score,
  "Basque", "mi", "VOS", filter(basque_mi_Rankings, word_order == "VOS")$score,
  "Basque", "mi", "OVS", filter(basque_mi_Rankings, word_order == "OVS")$score,
  "Basque", "mi", "OSV", filter(basque_mi_Rankings, word_order == "OSV")$score,
  "Irish", "entropy UID", "SVO", filter(irish_ent_Rankings, word_order == "SVO")$score,
  "Irish", "entropy UID", "SOV", filter(irish_ent_Rankings, word_order == "SOV")$score,
  "Irish", "entropy UID", "VSO", filter(irish_ent_Rankings, word_order == "VSO")$score,
  "Irish", "entropy UID", "VOS", filter(irish_ent_Rankings, word_order == "VOS")$score,
  "Irish", "entropy UID", "OVS", filter(irish_ent_Rankings, word_order == "OVS")$score,
  "Irish", "entropy UID", "OSV", filter(irish_ent_Rankings, word_order == "OSV")$score,
  "Irish", "surprisal UID", "SVO", filter(irish_surp_Rankings, word_order == "SVO")$score,
  "Irish", "surprisal UID", "SOV", filter(irish_surp_Rankings, word_order == "SOV")$score,
  "Irish", "surprisal UID", "VSO", filter(irish_surp_Rankings, word_order == "VSO")$score,
  "Irish", "surprisal UID", "VOS", filter(irish_surp_Rankings, word_order == "VOS")$score,
  "Irish", "surprisal UID", "OVS", filter(irish_surp_Rankings, word_order == "OVS")$score,
  "Irish", "surprisal UID", "OSV", filter(irish_surp_Rankings, word_order == "OSV")$score,
  "Irish", "mi", "SVO", filter(irish_mi_Rankings, word_order == "SVO")$score,
  "Irish", "mi", "SOV", filter(irish_mi_Rankings, word_order == "SOV")$score,
  "Irish", "mi", "VSO", filter(irish_mi_Rankings, word_order == "VSO")$score,
  "Irish", "mi", "VOS", filter(irish_mi_Rankings, word_order == "VOS")$score,
  "Irish", "mi", "OVS", filter(irish_mi_Rankings, word_order == "OVS")$score,
  "Irish", "mi", "OSV", filter(irish_mi_Rankings, word_order == "OSV")$score,
  "Scottish Gaelic", "entropy UID", "SVO", filter(scottish_gaelic_ent_Rankings, word_order == "SVO")$score,
  "Scottish Gaelic", "entropy UID", "SOV", filter(scottish_gaelic_ent_Rankings, word_order == "SOV")$score,
  "Scottish Gaelic", "entropy UID", "VSO", filter(scottish_gaelic_ent_Rankings, word_order == "VSO")$score,
  "Scottish Gaelic", "entropy UID", "VOS", filter(scottish_gaelic_ent_Rankings, word_order == "VOS")$score,
  "Scottish Gaelic", "entropy UID", "OVS", filter(scottish_gaelic_ent_Rankings, word_order == "OVS")$score,
  "Scottish Gaelic", "entropy UID", "OSV", filter(scottish_gaelic_ent_Rankings, word_order == "OSV")$score,
  "Scottish Gaelic", "surprisal UID", "SVO", filter(scottish_gaelic_surp_Rankings, word_order == "SVO")$score,
  "Scottish Gaelic", "surprisal UID", "SOV", filter(scottish_gaelic_surp_Rankings, word_order == "SOV")$score,
  "Scottish Gaelic", "surprisal UID", "VSO", filter(scottish_gaelic_surp_Rankings, word_order == "VSO")$score,
  "Scottish Gaelic", "surprisal UID", "VOS", filter(scottish_gaelic_surp_Rankings, word_order == "VOS")$score,
  "Scottish Gaelic", "surprisal UID", "OVS", filter(scottish_gaelic_surp_Rankings, word_order == "OVS")$score,
  "Scottish Gaelic", "surprisal UID", "OSV", filter(scottish_gaelic_surp_Rankings, word_order == "OSV")$score,
  "Scottish Gaelic", "mi", "SVO", filter(scottish_gaelic_mi_Rankings, word_order == "SVO")$score,
  "Scottish Gaelic", "mi", "SOV", filter(scottish_gaelic_mi_Rankings, word_order == "SOV")$score,
  "Scottish Gaelic", "mi", "VSO", filter(scottish_gaelic_mi_Rankings, word_order == "VSO")$score,
  "Scottish Gaelic", "mi", "VOS", filter(scottish_gaelic_mi_Rankings, word_order == "VOS")$score,
  "Scottish Gaelic", "mi", "OVS", filter(scottish_gaelic_mi_Rankings, word_order == "OVS")$score,
  "Scottish Gaelic", "mi", "OSV", filter(scottish_gaelic_mi_Rankings, word_order == "OSV")$score,
  "Hindi", "entropy UID", "SVO", filter(hindi_ent_Rankings, word_order == "SVO")$score,
  "Hindi", "entropy UID", "SOV", filter(hindi_ent_Rankings, word_order == "SOV")$score,
  "Hindi", "entropy UID", "VSO", filter(hindi_ent_Rankings, word_order == "VSO")$score,
  "Hindi", "entropy UID", "VOS", filter(hindi_ent_Rankings, word_order == "VOS")$score,
  "Hindi", "entropy UID", "OVS", filter(hindi_ent_Rankings, word_order == "OVS")$score,
  "Hindi", "entropy UID", "OSV", filter(hindi_ent_Rankings, word_order == "OSV")$score,
  "Hindi", "surprisal UID", "SVO", filter(hindi_surp_Rankings, word_order == "SVO")$score,
  "Hindi", "surprisal UID", "SOV", filter(hindi_surp_Rankings, word_order == "SOV")$score,
  "Hindi", "surprisal UID", "VSO", filter(hindi_surp_Rankings, word_order == "VSO")$score,
  "Hindi", "surprisal UID", "VOS", filter(hindi_surp_Rankings, word_order == "VOS")$score,
  "Hindi", "surprisal UID", "OVS", filter(hindi_surp_Rankings, word_order == "OVS")$score,
  "Hindi", "surprisal UID", "OSV", filter(hindi_surp_Rankings, word_order == "OSV")$score,
  "Hindi", "mi", "SVO", filter(hindi_mi_Rankings, word_order == "SVO")$score,
  "Hindi", "mi", "SOV", filter(hindi_mi_Rankings, word_order == "SOV")$score,
  "Hindi", "mi", "VSO", filter(hindi_mi_Rankings, word_order == "VSO")$score,
  "Hindi", "mi", "VOS", filter(hindi_mi_Rankings, word_order == "VOS")$score,
  "Hindi", "mi", "OVS", filter(hindi_mi_Rankings, word_order == "OVS")$score,
  "Hindi", "mi", "OSV", filter(hindi_mi_Rankings, word_order == "OSV")$score,
  "Hungarian", "entropy UID", "SVO", filter(hungarian_ent_Rankings, word_order == "SVO")$score,
  "Hungarian", "entropy UID", "SOV", filter(hungarian_ent_Rankings, word_order == "SOV")$score,
  "Hungarian", "entropy UID", "VSO", filter(hungarian_ent_Rankings, word_order == "VSO")$score,
  "Hungarian", "entropy UID", "VOS", filter(hungarian_ent_Rankings, word_order == "VOS")$score,
  "Hungarian", "entropy UID", "OVS", filter(hungarian_ent_Rankings, word_order == "OVS")$score,
  "Hungarian", "entropy UID", "OSV", filter(hungarian_ent_Rankings, word_order == "OSV")$score,
  "Hungarian", "surprisal UID", "SVO", filter(hungarian_surp_Rankings, word_order == "SVO")$score,
  "Hungarian", "surprisal UID", "SOV", filter(hungarian_surp_Rankings, word_order == "SOV")$score,
  "Hungarian", "surprisal UID", "VSO", filter(hungarian_surp_Rankings, word_order == "VSO")$score,
  "Hungarian", "surprisal UID", "VOS", filter(hungarian_surp_Rankings, word_order == "VOS")$score,
  "Hungarian", "surprisal UID", "OVS", filter(hungarian_surp_Rankings, word_order == "OVS")$score,
  "Hungarian", "surprisal UID", "OSV", filter(hungarian_surp_Rankings, word_order == "OSV")$score,
  "Hungarian", "mi", "SVO", filter(hungarian_mi_Rankings, word_order == "SVO")$score,
  "Hungarian", "mi", "SOV", filter(hungarian_mi_Rankings, word_order == "SOV")$score,
  "Hungarian", "mi", "VSO", filter(hungarian_mi_Rankings, word_order == "VSO")$score,
  "Hungarian", "mi", "VOS", filter(hungarian_mi_Rankings, word_order == "VOS")$score,
  "Hungarian", "mi", "OVS", filter(hungarian_mi_Rankings, word_order == "OVS")$score,
  "Hungarian", "mi", "OSV", filter(hungarian_mi_Rankings, word_order == "OSV")$score,
  "Indonesian", "entropy UID", "SVO", filter(indonesian_ent_Rankings, word_order == "SVO")$score,
  "Indonesian", "entropy UID", "SOV", filter(indonesian_ent_Rankings, word_order == "SOV")$score,
  "Indonesian", "entropy UID", "VSO", filter(indonesian_ent_Rankings, word_order == "VSO")$score,
  "Indonesian", "entropy UID", "VOS", filter(indonesian_ent_Rankings, word_order == "VOS")$score,
  "Indonesian", "entropy UID", "OVS", filter(indonesian_ent_Rankings, word_order == "OVS")$score,
  "Indonesian", "entropy UID", "OSV", filter(indonesian_ent_Rankings, word_order == "OSV")$score,
  "Indonesian", "surprisal UID", "SVO", filter(indonesian_surp_Rankings, word_order == "SVO")$score,
  "Indonesian", "surprisal UID", "SOV", filter(indonesian_surp_Rankings, word_order == "SOV")$score,
  "Indonesian", "surprisal UID", "VSO", filter(indonesian_surp_Rankings, word_order == "VSO")$score,
  "Indonesian", "surprisal UID", "VOS", filter(indonesian_surp_Rankings, word_order == "VOS")$score,
  "Indonesian", "surprisal UID", "OVS", filter(indonesian_surp_Rankings, word_order == "OVS")$score,
  "Indonesian", "surprisal UID", "OSV", filter(indonesian_surp_Rankings, word_order == "OSV")$score,
  "Indonesian", "mi", "SVO", filter(indonesian_mi_Rankings, word_order == "SVO")$score,
  "Indonesian", "mi", "SOV", filter(indonesian_mi_Rankings, word_order == "SOV")$score,
  "Indonesian", "mi", "VSO", filter(indonesian_mi_Rankings, word_order == "VSO")$score,
  "Indonesian", "mi", "VOS", filter(indonesian_mi_Rankings, word_order == "VOS")$score,
  "Indonesian", "mi", "OVS", filter(indonesian_mi_Rankings, word_order == "OVS")$score,
  "Indonesian", "mi", "OSV", filter(indonesian_mi_Rankings, word_order == "OSV")$score,
  "Japanese", "entropy UID", "SVO", filter(japanese_ent_Rankings, word_order == "SVO")$score,
  "Japanese", "entropy UID", "SOV", filter(japanese_ent_Rankings, word_order == "SOV")$score,
  "Japanese", "entropy UID", "VSO", filter(japanese_ent_Rankings, word_order == "VSO")$score,
  "Japanese", "entropy UID", "VOS", filter(japanese_ent_Rankings, word_order == "VOS")$score,
  "Japanese", "entropy UID", "OVS", filter(japanese_ent_Rankings, word_order == "OVS")$score,
  "Japanese", "entropy UID", "OSV", filter(japanese_ent_Rankings, word_order == "OSV")$score,
  "Japanese", "surprisal UID", "SVO", filter(japanese_surp_Rankings, word_order == "SVO")$score,
  "Japanese", "surprisal UID", "SOV", filter(japanese_surp_Rankings, word_order == "SOV")$score,
  "Japanese", "surprisal UID", "VSO", filter(japanese_surp_Rankings, word_order == "VSO")$score,
  "Japanese", "surprisal UID", "VOS", filter(japanese_surp_Rankings, word_order == "VOS")$score,
  "Japanese", "surprisal UID", "OVS", filter(japanese_surp_Rankings, word_order == "OVS")$score,
  "Japanese", "surprisal UID", "OSV", filter(japanese_surp_Rankings, word_order == "OSV")$score,
  "Japanese", "mi", "SVO", filter(japanese_mi_Rankings, word_order == "SVO")$score,
  "Japanese", "mi", "SOV", filter(japanese_mi_Rankings, word_order == "SOV")$score,
  "Japanese", "mi", "VSO", filter(japanese_mi_Rankings, word_order == "VSO")$score,
  "Japanese", "mi", "VOS", filter(japanese_mi_Rankings, word_order == "VOS")$score,
  "Japanese", "mi", "OVS", filter(japanese_mi_Rankings, word_order == "OVS")$score,
  "Japanese", "mi", "OSV", filter(japanese_mi_Rankings, word_order == "OSV")$score,
  "Korean", "entropy UID", "SVO", filter(korean_ent_Rankings, word_order == "SVO")$score,
  "Korean", "entropy UID", "SOV", filter(korean_ent_Rankings, word_order == "SOV")$score,
  "Korean", "entropy UID", "VSO", filter(korean_ent_Rankings, word_order == "VSO")$score,
  "Korean", "entropy UID", "VOS", filter(korean_ent_Rankings, word_order == "VOS")$score,
  "Korean", "entropy UID", "OVS", filter(korean_ent_Rankings, word_order == "OVS")$score,
  "Korean", "entropy UID", "OSV", filter(korean_ent_Rankings, word_order == "OSV")$score,
  "Korean", "surprisal UID", "SVO", filter(korean_surp_Rankings, word_order == "SVO")$score,
  "Korean", "surprisal UID", "SOV", filter(korean_surp_Rankings, word_order == "SOV")$score,
  "Korean", "surprisal UID", "VSO", filter(korean_surp_Rankings, word_order == "VSO")$score,
  "Korean", "surprisal UID", "VOS", filter(korean_surp_Rankings, word_order == "VOS")$score,
  "Korean", "surprisal UID", "OVS", filter(korean_surp_Rankings, word_order == "OVS")$score,
  "Korean", "surprisal UID", "OSV", filter(korean_surp_Rankings, word_order == "OSV")$score,
  "Korean", "mi", "SVO", filter(korean_mi_Rankings, word_order == "SVO")$score,
  "Korean", "mi", "SOV", filter(korean_mi_Rankings, word_order == "SOV")$score,
  "Korean", "mi", "VSO", filter(korean_mi_Rankings, word_order == "VSO")$score,
  "Korean", "mi", "VOS", filter(korean_mi_Rankings, word_order == "VOS")$score,
  "Korean", "mi", "OVS", filter(korean_mi_Rankings, word_order == "OVS")$score,
  "Korean", "mi", "OSV", filter(korean_mi_Rankings, word_order == "OSV")$score,
  "Mbya Guarani", "entropy UID", "SVO", filter(mbya_guarani_ent_Rankings, word_order == "SVO")$score,
  "Mbya Guarani", "entropy UID", "SOV", filter(mbya_guarani_ent_Rankings, word_order == "SOV")$score,
  "Mbya Guarani", "entropy UID", "VSO", filter(mbya_guarani_ent_Rankings, word_order == "VSO")$score,
  "Mbya Guarani", "entropy UID", "VOS", filter(mbya_guarani_ent_Rankings, word_order == "VOS")$score,
  "Mbya Guarani", "entropy UID", "OVS", filter(mbya_guarani_ent_Rankings, word_order == "OVS")$score,
  "Mbya Guarani", "entropy UID", "OSV", filter(mbya_guarani_ent_Rankings, word_order == "OSV")$score,
  "Mbya Guarani", "surprisal UID", "SVO", filter(mbya_guarani_surp_Rankings, word_order == "SVO")$score,
  "Mbya Guarani", "surprisal UID", "SOV", filter(mbya_guarani_surp_Rankings, word_order == "SOV")$score,
  "Mbya Guarani", "surprisal UID", "VSO", filter(mbya_guarani_surp_Rankings, word_order == "VSO")$score,
  "Mbya Guarani", "surprisal UID", "VOS", filter(mbya_guarani_surp_Rankings, word_order == "VOS")$score,
  "Mbya Guarani", "surprisal UID", "OVS", filter(mbya_guarani_surp_Rankings, word_order == "OVS")$score,
  "Mbya Guarani", "surprisal UID", "OSV", filter(mbya_guarani_surp_Rankings, word_order == "OSV")$score,
  "Mbya Guarani", "mi", "SVO", filter(mbya_guarani_mi_Rankings, word_order == "SVO")$score,
  "Mbya Guarani", "mi", "SOV", filter(mbya_guarani_mi_Rankings, word_order == "SOV")$score,
  "Mbya Guarani", "mi", "VSO", filter(mbya_guarani_mi_Rankings, word_order == "VSO")$score,
  "Mbya Guarani", "mi", "VOS", filter(mbya_guarani_mi_Rankings, word_order == "VOS")$score,
  "Mbya Guarani", "mi", "OVS", filter(mbya_guarani_mi_Rankings, word_order == "OVS")$score,
  "Mbya Guarani", "mi", "OSV", filter(mbya_guarani_mi_Rankings, word_order == "OSV")$score,
  "Dutch", "entropy UID", "SVO", filter(dutch_ent_Rankings, word_order == "SVO")$score,
  "Dutch", "entropy UID", "SOV", filter(dutch_ent_Rankings, word_order == "SOV")$score,
  "Dutch", "entropy UID", "VSO", filter(dutch_ent_Rankings, word_order == "VSO")$score,
  "Dutch", "entropy UID", "VOS", filter(dutch_ent_Rankings, word_order == "VOS")$score,
  "Dutch", "entropy UID", "OVS", filter(dutch_ent_Rankings, word_order == "OVS")$score,
  "Dutch", "entropy UID", "OSV", filter(dutch_ent_Rankings, word_order == "OSV")$score,
  "Dutch", "surprisal UID", "SVO", filter(dutch_surp_Rankings, word_order == "SVO")$score,
  "Dutch", "surprisal UID", "SOV", filter(dutch_surp_Rankings, word_order == "SOV")$score,
  "Dutch", "surprisal UID", "VSO", filter(dutch_surp_Rankings, word_order == "VSO")$score,
  "Dutch", "surprisal UID", "VOS", filter(dutch_surp_Rankings, word_order == "VOS")$score,
  "Dutch", "surprisal UID", "OVS", filter(dutch_surp_Rankings, word_order == "OVS")$score,
  "Dutch", "surprisal UID", "OSV", filter(dutch_surp_Rankings, word_order == "OSV")$score,
  "Dutch", "mi", "SVO", filter(dutch_mi_Rankings, word_order == "SVO")$score,
  "Dutch", "mi", "SOV", filter(dutch_mi_Rankings, word_order == "SOV")$score,
  "Dutch", "mi", "VSO", filter(dutch_mi_Rankings, word_order == "VSO")$score,
  "Dutch", "mi", "VOS", filter(dutch_mi_Rankings, word_order == "VOS")$score,
  "Dutch", "mi", "OVS", filter(dutch_mi_Rankings, word_order == "OVS")$score,
  "Dutch", "mi", "OSV", filter(dutch_mi_Rankings, word_order == "OSV")$score,
  "Tagalog", "entropy UID", "SVO", filter(tagalog_ent_Rankings, word_order == "SVO")$score,
  "Tagalog", "entropy UID", "SOV", filter(tagalog_ent_Rankings, word_order == "SOV")$score,
  "Tagalog", "entropy UID", "VSO", filter(tagalog_ent_Rankings, word_order == "VSO")$score,
  "Tagalog", "entropy UID", "VOS", filter(tagalog_ent_Rankings, word_order == "VOS")$score,
  "Tagalog", "entropy UID", "OVS", filter(tagalog_ent_Rankings, word_order == "OVS")$score,
  "Tagalog", "entropy UID", "OSV", filter(tagalog_ent_Rankings, word_order == "OSV")$score,
  "Tagalog", "surprisal UID", "SVO", filter(tagalog_surp_Rankings, word_order == "SVO")$score,
  "Tagalog", "surprisal UID", "SOV", filter(tagalog_surp_Rankings, word_order == "SOV")$score,
  "Tagalog", "surprisal UID", "VSO", filter(tagalog_surp_Rankings, word_order == "VSO")$score,
  "Tagalog", "surprisal UID", "VOS", filter(tagalog_surp_Rankings, word_order == "VOS")$score,
  "Tagalog", "surprisal UID", "OVS", filter(tagalog_surp_Rankings, word_order == "OVS")$score,
  "Tagalog", "surprisal UID", "OSV", filter(tagalog_surp_Rankings, word_order == "OSV")$score,
  "Tagalog", "mi", "SVO", filter(tagalog_mi_Rankings, word_order == "SVO")$score,
  "Tagalog", "mi", "SOV", filter(tagalog_mi_Rankings, word_order == "SOV")$score,
  "Tagalog", "mi", "VSO", filter(tagalog_mi_Rankings, word_order == "VSO")$score,
  "Tagalog", "mi", "VOS", filter(tagalog_mi_Rankings, word_order == "VOS")$score,
  "Tagalog", "mi", "OVS", filter(tagalog_mi_Rankings, word_order == "OVS")$score,
  "Tagalog", "mi", "OSV", filter(tagalog_mi_Rankings, word_order == "OSV")$score,
  "Turkish", "entropy UID", "SVO", filter(turkish_ent_Rankings, word_order == "SVO")$score,
  "Turkish", "entropy UID", "SOV", filter(turkish_ent_Rankings, word_order == "SOV")$score,
  "Turkish", "entropy UID", "VSO", filter(turkish_ent_Rankings, word_order == "VSO")$score,
  "Turkish", "entropy UID", "VOS", filter(turkish_ent_Rankings, word_order == "VOS")$score,
  "Turkish", "entropy UID", "OVS", filter(turkish_ent_Rankings, word_order == "OVS")$score,
  "Turkish", "entropy UID", "OSV", filter(turkish_ent_Rankings, word_order == "OSV")$score,
  "Turkish", "surprisal UID", "SVO", filter(turkish_surp_Rankings, word_order == "SVO")$score,
  "Turkish", "surprisal UID", "SOV", filter(turkish_surp_Rankings, word_order == "SOV")$score,
  "Turkish", "surprisal UID", "VSO", filter(turkish_surp_Rankings, word_order == "VSO")$score,
  "Turkish", "surprisal UID", "VOS", filter(turkish_surp_Rankings, word_order == "VOS")$score,
  "Turkish", "surprisal UID", "OVS", filter(turkish_surp_Rankings, word_order == "OVS")$score,
  "Turkish", "surprisal UID", "OSV", filter(turkish_surp_Rankings, word_order == "OSV")$score,
  "Turkish", "mi", "SVO", filter(turkish_mi_Rankings, word_order == "SVO")$score,
  "Turkish", "mi", "SOV", filter(turkish_mi_Rankings, word_order == "SOV")$score,
  "Turkish", "mi", "VSO", filter(turkish_mi_Rankings, word_order == "VSO")$score,
  "Turkish", "mi", "VOS", filter(turkish_mi_Rankings, word_order == "VOS")$score,
  "Turkish", "mi", "OVS", filter(turkish_mi_Rankings, word_order == "OVS")$score,
  "Turkish", "mi", "OSV", filter(turkish_mi_Rankings, word_order == "OSV")$score,
  "Vietnamese", "entropy UID", "SVO", filter(vietnamese_ent_Rankings, word_order == "SVO")$score,
  "Vietnamese", "entropy UID", "SOV", filter(vietnamese_ent_Rankings, word_order == "SOV")$score,
  "Vietnamese", "entropy UID", "VSO", filter(vietnamese_ent_Rankings, word_order == "VSO")$score,
  "Vietnamese", "entropy UID", "VOS", filter(vietnamese_ent_Rankings, word_order == "VOS")$score,
  "Vietnamese", "entropy UID", "OVS", filter(vietnamese_ent_Rankings, word_order == "OVS")$score,
  "Vietnamese", "entropy UID", "OSV", filter(vietnamese_ent_Rankings, word_order == "OSV")$score,
  "Vietnamese", "surprisal UID", "SVO", filter(vietnamese_surp_Rankings, word_order == "SVO")$score,
  "Vietnamese", "surprisal UID", "SOV", filter(vietnamese_surp_Rankings, word_order == "SOV")$score,
  "Vietnamese", "surprisal UID", "VSO", filter(vietnamese_surp_Rankings, word_order == "VSO")$score,
  "Vietnamese", "surprisal UID", "VOS", filter(vietnamese_surp_Rankings, word_order == "VOS")$score,
  "Vietnamese", "surprisal UID", "OVS", filter(vietnamese_surp_Rankings, word_order == "OVS")$score,
  "Vietnamese", "surprisal UID", "OSV", filter(vietnamese_surp_Rankings, word_order == "OSV")$score,
  "Vietnamese", "mi", "SVO", filter(vietnamese_mi_Rankings, word_order == "SVO")$score,
  "Vietnamese", "mi", "SOV", filter(vietnamese_mi_Rankings, word_order == "SOV")$score,
  "Vietnamese", "mi", "VSO", filter(vietnamese_mi_Rankings, word_order == "VSO")$score,
  "Vietnamese", "mi", "VOS", filter(vietnamese_mi_Rankings, word_order == "VOS")$score,
  "Vietnamese", "mi", "OVS", filter(vietnamese_mi_Rankings, word_order == "OVS")$score,
  "Vietnamese", "mi", "OSV", filter(vietnamese_mi_Rankings, word_order == "OSV")$score,
  "Warlpiri", "entropy UID", "SVO", filter(warlpiri_ent_Rankings, word_order == "SVO")$score,
  "Warlpiri", "entropy UID", "SOV", filter(warlpiri_ent_Rankings, word_order == "SOV")$score,
  "Warlpiri", "entropy UID", "VSO", filter(warlpiri_ent_Rankings, word_order == "VSO")$score,
  "Warlpiri", "entropy UID", "VOS", filter(warlpiri_ent_Rankings, word_order == "VOS")$score,
  "Warlpiri", "entropy UID", "OVS", filter(warlpiri_ent_Rankings, word_order == "OVS")$score,
  "Warlpiri", "entropy UID", "OSV", filter(warlpiri_ent_Rankings, word_order == "OSV")$score,
  "Warlpiri", "surprisal UID", "SVO", filter(warlpiri_surp_Rankings, word_order == "SVO")$score,
  "Warlpiri", "surprisal UID", "SOV", filter(warlpiri_surp_Rankings, word_order == "SOV")$score,
  "Warlpiri", "surprisal UID", "VSO", filter(warlpiri_surp_Rankings, word_order == "VSO")$score,
  "Warlpiri", "surprisal UID", "VOS", filter(warlpiri_surp_Rankings, word_order == "VOS")$score,
  "Warlpiri", "surprisal UID", "OVS", filter(warlpiri_surp_Rankings, word_order == "OVS")$score,
  "Warlpiri", "surprisal UID", "OSV", filter(warlpiri_surp_Rankings, word_order == "OSV")$score,
  "Warlpiri", "mi", "SVO", filter(warlpiri_mi_Rankings, word_order == "SVO")$score,
  "Warlpiri", "mi", "SOV", filter(warlpiri_mi_Rankings, word_order == "SOV")$score,
  "Warlpiri", "mi", "VSO", filter(warlpiri_mi_Rankings, word_order == "VSO")$score,
  "Warlpiri", "mi", "VOS", filter(warlpiri_mi_Rankings, word_order == "VOS")$score,
  "Warlpiri", "mi", "OVS", filter(warlpiri_mi_Rankings, word_order == "OVS")$score,
  "Warlpiri", "mi", "OSV", filter(warlpiri_mi_Rankings, word_order == "OSV")$score,
  "Wolof", "entropy UID", "SVO", filter(wolof_ent_Rankings, word_order == "SVO")$score,
  "Wolof", "entropy UID", "SOV", filter(wolof_ent_Rankings, word_order == "SOV")$score,
  "Wolof", "entropy UID", "VSO", filter(wolof_ent_Rankings, word_order == "VSO")$score,
  "Wolof", "entropy UID", "VOS", filter(wolof_ent_Rankings, word_order == "VOS")$score,
  "Wolof", "entropy UID", "OVS", filter(wolof_ent_Rankings, word_order == "OVS")$score,
  "Wolof", "entropy UID", "OSV", filter(wolof_ent_Rankings, word_order == "OSV")$score,
  "Wolof", "surprisal UID", "SVO", filter(wolof_surp_Rankings, word_order == "SVO")$score,
  "Wolof", "surprisal UID", "SOV", filter(wolof_surp_Rankings, word_order == "SOV")$score,
  "Wolof", "surprisal UID", "VSO", filter(wolof_surp_Rankings, word_order == "VSO")$score,
  "Wolof", "surprisal UID", "VOS", filter(wolof_surp_Rankings, word_order == "VOS")$score,
  "Wolof", "surprisal UID", "OVS", filter(wolof_surp_Rankings, word_order == "OVS")$score,
  "Wolof", "surprisal UID", "OSV", filter(wolof_surp_Rankings, word_order == "OSV")$score,
  "Wolof", "mi", "SVO", filter(wolof_mi_Rankings, word_order == "SVO")$score,
  "Wolof", "mi", "SOV", filter(wolof_mi_Rankings, word_order == "SOV")$score,
  "Wolof", "mi", "VSO", filter(wolof_mi_Rankings, word_order == "VSO")$score,
  "Wolof", "mi", "VOS", filter(wolof_mi_Rankings, word_order == "VOS")$score,
  "Wolof", "mi", "OVS", filter(wolof_mi_Rankings, word_order == "OVS")$score,
  "Wolof", "mi", "OSV", filter(wolof_mi_Rankings, word_order == "OSV")$score,
  "Mandarin", "entropy UID", "SVO", filter(mandarin_ent_Rankings, word_order == "SVO")$score,
  "Mandarin", "entropy UID", "SOV", filter(mandarin_ent_Rankings, word_order == "SOV")$score,
  "Mandarin", "entropy UID", "VSO", filter(mandarin_ent_Rankings, word_order == "VSO")$score,
  "Mandarin", "entropy UID", "VOS", filter(mandarin_ent_Rankings, word_order == "VOS")$score,
  "Mandarin", "entropy UID", "OVS", filter(mandarin_ent_Rankings, word_order == "OVS")$score,
  "Mandarin", "entropy UID", "OSV", filter(mandarin_ent_Rankings, word_order == "OSV")$score,
  "Mandarin", "surprisal UID", "SVO", filter(mandarin_surp_Rankings, word_order == "SVO")$score,
  "Mandarin", "surprisal UID", "SOV", filter(mandarin_surp_Rankings, word_order == "SOV")$score,
  "Mandarin", "surprisal UID", "VSO", filter(mandarin_surp_Rankings, word_order == "VSO")$score,
  "Mandarin", "surprisal UID", "VOS", filter(mandarin_surp_Rankings, word_order == "VOS")$score,
  "Mandarin", "surprisal UID", "OVS", filter(mandarin_surp_Rankings, word_order == "OVS")$score,
  "Mandarin", "surprisal UID", "OSV", filter(mandarin_surp_Rankings, word_order == "OSV")$score,
  "Mandarin", "mi", "SVO", filter(mandarin_mi_Rankings, word_order == "SVO")$score,
  "Mandarin", "mi", "SOV", filter(mandarin_mi_Rankings, word_order == "SOV")$score,
  "Mandarin", "mi", "VSO", filter(mandarin_mi_Rankings, word_order == "VSO")$score,
  "Mandarin", "mi", "VOS", filter(mandarin_mi_Rankings, word_order == "VOS")$score,
  "Mandarin", "mi", "OVS", filter(mandarin_mi_Rankings, word_order == "OVS")$score,
  "Mandarin", "mi", "OSV", filter(mandarin_mi_Rankings, word_order == "OSV")$score
)

prediction_accuracies_wals <- tribble(
  ~ent_uid, ~surp_uid, ~mi, ~lang
)

calc_wals_accuracy <- function(rankings, lang_meta) {
  i <- 0
  if(rankings[1,]$score == rankings[2,]$score) {
    if(rankings[1,1]$word_order == select(lang_meta, WALS_classification)$WALS_classification |
       rankings[2,1]$word_order == select(lang_meta, WALS_classification)$WALS_classification) {
      i <- 1
    }
  } else {
    if(rankings[1,1]$word_order == select(lang_meta, WALS_classification)$WALS_classification) {
      i <- 1
    }
  }
  returnValue(i)
}

ent <- calc_wals_accuracy(childes_ent_Rankings, childes_meta)
surp <- calc_wals_accuracy(childes_surp_Rankings, childes_meta)
mi <- calc_wals_accuracy(childes_mi_Rankings, childes_meta)
prediction_accuracies_wals <- add_row(prediction_accuracies_wals, ent_uid = ent, surp_uid = surp, mi = mi, lang = childes_meta$Language)

ent <- calc_wals_accuracy(breton_ent_Rankings, breton_meta)
surp <- calc_wals_accuracy(breton_surp_Rankings, breton_meta)
mi <- calc_wals_accuracy(breton_mi_Rankings, breton_meta)
prediction_accuracies_wals <- add_row(prediction_accuracies_wals, ent_uid = ent, surp_uid = surp, mi = mi, lang = breton_meta$Language)

ent <- calc_wals_accuracy(welsh_ent_Rankings, welsh_meta)
surp <- calc_wals_accuracy(welsh_surp_Rankings, welsh_meta)
mi <- calc_wals_accuracy(welsh_mi_Rankings, welsh_meta)
prediction_accuracies_wals <- add_row(prediction_accuracies_wals, ent_uid = ent, surp_uid = surp, mi = mi, lang = welsh_meta$Language)

ent <- calc_wals_accuracy(english_ent_Rankings, english_meta)
surp <- calc_wals_accuracy(english_surp_Rankings, english_meta)
mi <- calc_wals_accuracy(english_mi_Rankings, english_meta)
prediction_accuracies_wals <- add_row(prediction_accuracies_wals, ent_uid = ent, surp_uid = surp, mi = mi, lang = english_meta$Language)

ent <- calc_wals_accuracy(basque_ent_Rankings, basque_meta)
surp <- calc_wals_accuracy(basque_surp_Rankings, basque_meta)
mi <- calc_wals_accuracy(basque_mi_Rankings, basque_meta)
prediction_accuracies_wals <- add_row(prediction_accuracies_wals, ent_uid = ent, surp_uid = surp, mi = mi, lang = basque_meta$Language)

ent <- calc_wals_accuracy(irish_ent_Rankings, irish_meta)
surp <- calc_wals_accuracy(irish_surp_Rankings, irish_meta)
mi <- calc_wals_accuracy(irish_mi_Rankings, irish_meta)
prediction_accuracies_wals <- add_row(prediction_accuracies_wals, ent_uid = ent, surp_uid = surp, mi = mi, lang = irish_meta$Language)

ent <- calc_wals_accuracy(scottish_gaelic_ent_Rankings, scottish_gaelic_meta)
surp <- calc_wals_accuracy(scottish_gaelic_surp_Rankings, scottish_gaelic_meta)
mi <- calc_wals_accuracy(scottish_gaelic_mi_Rankings, scottish_gaelic_meta)
prediction_accuracies_wals <- add_row(prediction_accuracies_wals, ent_uid = ent, surp_uid = surp, mi = mi, lang = scottish_gaelic_meta$Language)

ent <- calc_wals_accuracy(hindi_ent_Rankings, hindi_meta)
surp <- calc_wals_accuracy(hindi_surp_Rankings, hindi_meta)
mi <- calc_wals_accuracy(hindi_mi_Rankings, hindi_meta)
prediction_accuracies_wals <- add_row(prediction_accuracies_wals, ent_uid = ent, surp_uid = surp, mi = mi, lang = hindi_meta$Language)

ent <- calc_wals_accuracy(indonesian_ent_Rankings, indonesian_meta)
surp <- calc_wals_accuracy(indonesian_surp_Rankings, indonesian_meta)
mi <- calc_wals_accuracy(indonesian_mi_Rankings, indonesian_meta)
prediction_accuracies_wals <- add_row(prediction_accuracies_wals, ent_uid = ent, surp_uid = surp, mi = mi, lang = indonesian_meta$Language)

ent <- calc_wals_accuracy(japanese_ent_Rankings, japanese_meta)
surp <- calc_wals_accuracy(japanese_surp_Rankings, japanese_meta)
mi <- calc_wals_accuracy(japanese_mi_Rankings, japanese_meta)
prediction_accuracies_wals <- add_row(prediction_accuracies_wals, ent_uid = ent, surp_uid = surp, mi = mi, lang = japanese_meta$Language)

ent <- calc_wals_accuracy(korean_ent_Rankings, korean_meta)
surp <- calc_wals_accuracy(korean_surp_Rankings, korean_meta)
mi <- calc_wals_accuracy(korean_mi_Rankings, korean_meta)
prediction_accuracies_wals <- add_row(prediction_accuracies_wals, ent_uid = ent, surp_uid = surp, mi = mi, lang = korean_meta$Language)

ent <- calc_wals_accuracy(mbya_guarani_ent_Rankings, mbya_guarani_meta)
surp <- calc_wals_accuracy(mbya_guarani_surp_Rankings, mbya_guarani_meta)
mi <- calc_wals_accuracy(mbya_guarani_mi_Rankings, mbya_guarani_meta)
prediction_accuracies_wals <- add_row(prediction_accuracies_wals, ent_uid = ent, surp_uid = surp, mi = mi, lang = mbya_guarani_meta$Language)

ent <- calc_wals_accuracy(tagalog_ent_Rankings, tagalog_meta)
surp <- calc_wals_accuracy(tagalog_surp_Rankings, tagalog_meta)
mi <- calc_wals_accuracy(tagalog_mi_Rankings, tagalog_meta)
prediction_accuracies_wals <- add_row(prediction_accuracies_wals, ent_uid = ent, surp_uid = surp, mi = mi, lang = tagalog_meta$Language)

ent <- calc_wals_accuracy(turkish_ent_Rankings, turkish_meta)
surp <- calc_wals_accuracy(turkish_surp_Rankings, turkish_meta)
mi <- calc_wals_accuracy(turkish_mi_Rankings, turkish_meta)
prediction_accuracies_wals <- add_row(prediction_accuracies_wals, ent_uid = ent, surp_uid = surp, mi = mi, lang = turkish_meta$Language)

ent <- calc_wals_accuracy(vietnamese_ent_Rankings, vietnamese_meta)
surp <- calc_wals_accuracy(vietnamese_surp_Rankings, vietnamese_meta)
mi <- calc_wals_accuracy(vietnamese_mi_Rankings, vietnamese_meta)
prediction_accuracies_wals <- add_row(prediction_accuracies_wals, ent_uid = ent, surp_uid = surp, mi = mi, lang = vietnamese_meta$Language)

ent <- calc_wals_accuracy(wolof_ent_Rankings, wolof_meta)
surp <- calc_wals_accuracy(wolof_surp_Rankings, wolof_meta)
mi <- calc_wals_accuracy(wolof_mi_Rankings, wolof_meta)
prediction_accuracies_wals <- add_row(prediction_accuracies_wals, ent_uid = ent, surp_uid = surp, mi = mi, lang = wolof_meta$Language)

ent <- calc_wals_accuracy(mandarin_ent_Rankings, mandarin_meta)
surp <- calc_wals_accuracy(mandarin_surp_Rankings, mandarin_meta)
mi <- calc_wals_accuracy(mandarin_mi_Rankings, mandarin_meta)
prediction_accuracies_wals <- add_row(prediction_accuracies_wals, ent_uid = ent, surp_uid = surp, mi = mi, lang = mandarin_meta$Language)

most_common_word_order <- function(df) {
  x <- select(df, SVO, SOV, VSO, VOS, OVS, OSV)
  colnames(x)[apply(x,1,which.max)]
}

prediction_accuracies_counts <- tribble(
  ~ent_uid, ~surp_uid, ~mi, ~lang
)

calc_counts_accuracy <- function(rankings, lang_meta) {
  i <- 0
  word_order <- most_common_word_order(lang_meta)
  if(rankings[1,]$score == rankings[2,]$score) {
    if(rankings[1,1]$word_order == word_order |
       rankings[2,1]$word_order == word_order) {
      i <- 1
    }
  } else {
    if(rankings[1,1]$word_order == word_order) {
      i <- 1
    }
  }
  returnValue(i)
}

ent <- calc_counts_accuracy(childes_ent_Rankings, childes_meta)
surp <- calc_counts_accuracy(childes_surp_Rankings, childes_meta)
mi <- calc_counts_accuracy(childes_mi_Rankings, childes_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = childes_meta$Language)

ent <- calc_counts_accuracy(breton_ent_Rankings, breton_meta)
surp <- calc_counts_accuracy(breton_surp_Rankings, breton_meta)
mi <- calc_counts_accuracy(breton_mi_Rankings, breton_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = breton_meta$Language)

ent <- calc_counts_accuracy(welsh_ent_Rankings, welsh_meta)
surp <- calc_counts_accuracy(welsh_surp_Rankings, welsh_meta)
mi <- calc_counts_accuracy(welsh_mi_Rankings, welsh_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = welsh_meta$Language)

ent <- calc_counts_accuracy(german_ent_Rankings, german_meta)
surp <- calc_counts_accuracy(german_surp_Rankings, german_meta)
mi <- calc_counts_accuracy(german_mi_Rankings, german_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = german_meta$Language)

ent <- calc_counts_accuracy(english_ent_Rankings, english_meta)
surp <- calc_counts_accuracy(english_surp_Rankings, english_meta)
mi <- calc_counts_accuracy(english_mi_Rankings, english_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = english_meta$Language)

ent <- calc_counts_accuracy(basque_ent_Rankings, basque_meta)
surp <- calc_counts_accuracy(basque_surp_Rankings, basque_meta)
mi <- calc_counts_accuracy(basque_mi_Rankings, basque_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = basque_meta$Language)

ent <- calc_counts_accuracy(irish_ent_Rankings, irish_meta)
surp <- calc_counts_accuracy(irish_surp_Rankings, irish_meta)
mi <- calc_counts_accuracy(irish_mi_Rankings, irish_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = irish_meta$Language)

ent <- calc_counts_accuracy(scottish_gaelic_ent_Rankings, scottish_gaelic_meta)
surp <- calc_counts_accuracy(scottish_gaelic_surp_Rankings, scottish_gaelic_meta)
mi <- calc_counts_accuracy(scottish_gaelic_mi_Rankings, scottish_gaelic_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = scottish_gaelic_meta$Language)

ent <- calc_counts_accuracy(hindi_ent_Rankings, hindi_meta)
surp <- calc_counts_accuracy(hindi_surp_Rankings, hindi_meta)
mi <- calc_counts_accuracy(hindi_mi_Rankings, hindi_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = hindi_meta$Language)

ent <- calc_counts_accuracy(hungarian_ent_Rankings, hungarian_meta)
surp <- calc_counts_accuracy(hungarian_surp_Rankings, hungarian_meta)
mi <- calc_counts_accuracy(hungarian_mi_Rankings, hungarian_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = hungarian_meta$Language)

ent <- calc_counts_accuracy(indonesian_ent_Rankings, indonesian_meta)
surp <- calc_counts_accuracy(indonesian_surp_Rankings, indonesian_meta)
mi <- calc_counts_accuracy(indonesian_mi_Rankings, indonesian_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = indonesian_meta$Language)

ent <- calc_counts_accuracy(japanese_ent_Rankings, japanese_meta)
surp <- calc_counts_accuracy(japanese_surp_Rankings, japanese_meta)
mi <- calc_counts_accuracy(japanese_mi_Rankings, japanese_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = japanese_meta$Language)

ent <- calc_counts_accuracy(korean_ent_Rankings, korean_meta)
surp <- calc_counts_accuracy(korean_surp_Rankings, korean_meta)
mi <- calc_counts_accuracy(korean_mi_Rankings, korean_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = korean_meta$Language)

ent <- calc_counts_accuracy(mbya_guarani_ent_Rankings, mbya_guarani_meta)
surp <- calc_counts_accuracy(mbya_guarani_surp_Rankings, mbya_guarani_meta)
mi <- calc_counts_accuracy(mbya_guarani_mi_Rankings, mbya_guarani_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = mbya_guarani_meta$Language)

ent <- calc_counts_accuracy(dutch_ent_Rankings, dutch_meta)
surp <- calc_counts_accuracy(dutch_surp_Rankings, dutch_meta)
mi <- calc_counts_accuracy(dutch_mi_Rankings, dutch_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = dutch_meta$Language)

ent <- calc_counts_accuracy(tagalog_ent_Rankings, tagalog_meta)
surp <- calc_counts_accuracy(tagalog_surp_Rankings, tagalog_meta)
mi <- calc_counts_accuracy(tagalog_mi_Rankings, tagalog_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = tagalog_meta$Language)

ent <- calc_counts_accuracy(turkish_ent_Rankings, turkish_meta)
surp <- calc_counts_accuracy(turkish_surp_Rankings, turkish_meta)
mi <- calc_counts_accuracy(turkish_mi_Rankings, turkish_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = turkish_meta$Language)

ent <- calc_counts_accuracy(vietnamese_ent_Rankings, vietnamese_meta)
surp <- calc_counts_accuracy(vietnamese_surp_Rankings, vietnamese_meta)
mi <- calc_counts_accuracy(vietnamese_mi_Rankings, vietnamese_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = vietnamese_meta$Language)

ent <- calc_counts_accuracy(warlpiri_ent_Rankings, warlpiri_meta)
surp <- calc_counts_accuracy(warlpiri_surp_Rankings, warlpiri_meta)
mi <- calc_counts_accuracy(warlpiri_mi_Rankings, warlpiri_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = warlpiri_meta$Language)

ent <- calc_counts_accuracy(wolof_ent_Rankings, wolof_meta)
surp <- calc_counts_accuracy(wolof_surp_Rankings, wolof_meta)
mi <- calc_counts_accuracy(wolof_mi_Rankings, wolof_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = wolof_meta$Language)

ent <- calc_counts_accuracy(mandarin_ent_Rankings, mandarin_meta)
surp <- calc_counts_accuracy(mandarin_surp_Rankings, mandarin_meta)
mi <- calc_counts_accuracy(mandarin_mi_Rankings, mandarin_meta)
prediction_accuracies_counts <- add_row(prediction_accuracies_counts, ent_uid = ent, surp_uid = surp, mi = mi, lang = mandarin_meta$Language)

(sum(prediction_accuracies_wals$ent_uid))
(sum(prediction_accuracies_wals$surp_uid))
(sum(prediction_accuracies_wals$mi))

(sum(prediction_accuracies_counts$ent_uid))
(sum(prediction_accuracies_counts$surp_uid))
(sum(prediction_accuracies_counts$mi))

svo_agg_ent <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "entropy UID") %>% filter(word_order == "SVO") %>% select(score) %>% sum/20
sov_agg_ent <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "entropy UID") %>% filter(word_order == "SOV") %>% select(score) %>% sum/20
vso_agg_ent <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "entropy UID") %>% filter(word_order == "VSO") %>% select(score) %>% sum/20
vos_agg_ent <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "entropy UID") %>% filter(word_order == "VOS") %>% select(score) %>% sum/20
ovs_agg_ent <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "entropy UID") %>% filter(word_order == "OVS") %>% select(score) %>% sum/20
osv_agg_ent <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "entropy UID") %>% filter(word_order == "OSV") %>% select(score) %>% sum/20

(tribble(
  ~word_order, ~score,
  "OVS", ovs_agg_ent,
  "SOV", sov_agg_ent,
  "OSV", osv_agg_ent,
  "VOS", vos_agg_ent,
  "VSO", vso_agg_ent,
  "SVO", svo_agg_ent
) %>% arrange(score))

svo_agg_surp <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "surprisal UID") %>% filter(word_order == "SVO") %>% select(score) %>% sum/20
sov_agg_surp <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "surprisal UID") %>% filter(word_order == "SOV") %>% select(score) %>% sum/20
vso_agg_surp <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "surprisal UID") %>% filter(word_order == "VSO") %>% select(score) %>% sum/20
vos_agg_surp <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "surprisal UID") %>% filter(word_order == "VOS") %>% select(score) %>% sum/20
ovs_agg_surp <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "surprisal UID") %>% filter(word_order == "OVS") %>% select(score) %>% sum/20
osv_agg_surp <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "surprisal UID") %>% filter(word_order == "OSV") %>% select(score) %>% sum/20

(tribble(
  ~word_order, ~score,
  "OVS", ovs_agg_surp,
  "SOV", sov_agg_surp,
  "OSV", osv_agg_surp,
  "VOS", vos_agg_surp,
  "VSO", vso_agg_surp,
  "SVO", svo_agg_surp
) %>% arrange(score))

z_scores <- tribble(
  ~language, ~word_order, ~score
)
z_score <- function(df) {
  mean_mi <- mean(df$Mutual_Information)
  sd_mi <- sd(df$Mutual_Information)
  df %>% mutate(mi_z_score = (Mutual_Information-mean_mi)/sd_mi)
}

z <- english %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "English", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "English", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "English", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "English", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "English", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "English", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- breton %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Breton", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Breton", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Breton", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Breton", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Breton", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Breton", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- welsh %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Welsh", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Welsh", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Welsh", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Welsh", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Welsh", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Welsh", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- german %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "German", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "German", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "German", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "German", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "German", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "German", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- basque %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Basque", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Basque", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Basque", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Basque", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Basque", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Basque", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- irish %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Irish", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Irish", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Irish", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Irish", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Irish", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Irish", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- scottish_gaelic %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Scottish Gaelic", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Scottish Gaelic", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Scottish Gaelic", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Scottish Gaelic", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Scottish Gaelic", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Scottish Gaelic", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- hindi %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Hindi", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Hindi", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Hindi", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Hindi", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Hindi", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Hindi", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- hungarian %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Hungarian", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Hungarian", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Hungarian", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Hungarian", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Hungarian", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Hungarian", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- indonesian %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Indonesian", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Indonesian", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Indonesian", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Indonesian", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Indonesian", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Indonesian", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- japanese %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Japanese", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Japanese", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Japanese", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Japanese", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Japanese", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Japanese", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- korean %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Korean", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Korean", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Korean", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Korean", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Korean", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Korean", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- mbya_guarani %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Mbya Guarani", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Mbya Guarani", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Mbya Guarani", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Mbya Guarani", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Mbya Guarani", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Mbya Guarani", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- dutch %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Dutch", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Dutch", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Dutch", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Dutch", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Dutch", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Dutch", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- tagalog %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Tagalog", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Tagalog", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Tagalog", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Tagalog", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Tagalog", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Tagalog", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- turkish %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Turkish", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Turkish", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Turkish", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Turkish", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Turkish", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Turkish", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- vietnamese %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Vietnamese", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Vietnamese", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Vietnamese", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Vietnamese", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Vietnamese", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Vietnamese", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- warlpiri %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Warlpiri", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Warlpiri", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Warlpiri", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Warlpiri", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Warlpiri", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Warlpiri", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- wolof %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Wolof", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Wolof", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Wolof", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Wolof", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Wolof", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Wolof", word_order = z$word_order[6], score = z$mi_z_score[6])

z <- mandarin %>% z_score %>% calcZScoreRankings
z_scores <- z_scores %>% add_row(language = "Mandarin", word_order = z$word_order[1], score = z$mi_z_score[1])
z_scores <- z_scores %>% add_row(language = "Mandarin", word_order = z$word_order[2], score = z$mi_z_score[2])
z_scores <- z_scores %>% add_row(language = "Mandarin", word_order = z$word_order[3], score = z$mi_z_score[3])
z_scores <- z_scores %>% add_row(language = "Mandarin", word_order = z$word_order[4], score = z$mi_z_score[4])
z_scores <- z_scores %>% add_row(language = "Mandarin", word_order = z$word_order[5], score = z$mi_z_score[5])
z_scores <- z_scores %>% add_row(language = "Mandarin", word_order = z$word_order[6], score = z$mi_z_score[6])

(svo_agg_mi <- z_scores %>% filter(word_order == "SVO") %>% select(score) %>% sum(na.rm = TRUE)/20)
(sov_agg_mi <- z_scores %>% filter(word_order == "SOV") %>% select(score) %>% sum(na.rm = TRUE)/20)
(vso_agg_mi <- z_scores %>% filter(word_order == "VSO") %>% select(score) %>% sum(na.rm = TRUE)/20)
(vos_agg_mi <- z_scores %>% filter(word_order == "VOS") %>% select(score) %>% sum(na.rm = TRUE)/20)
(ovs_agg_mi <- z_scores %>% filter(word_order == "OVS") %>% select(score) %>% sum(na.rm = TRUE)/20)
(osv_agg_mi <- z_scores %>% filter(word_order == "OSV") %>% select(score) %>% sum(na.rm = TRUE)/20)

(tribble(
  ~word_order, ~score,
  "OVS", ovs_agg_mi,
  "SOV", sov_agg_mi,
  "OSV", osv_agg_mi,
  "VOS", vos_agg_mi,
  "VSO", vso_agg_mi,
  "SVO", svo_agg_mi
) %>% arrange(score))

big_table_o_data <- add_column(big_table_o_data, proportion = c(childes_meta$SVO/childes_meta$Tokens, childes_meta$SOV/childes_meta$Tokens, childes_meta$VSO/childes_meta$Tokens, childes_meta$VOS/childes_meta$Tokens, childes_meta$OVS/childes_meta$Tokens, childes_meta$OSV/childes_meta$Tokens,
                                                          childes_meta$SVO/childes_meta$Tokens, childes_meta$SOV/childes_meta$Tokens, childes_meta$VSO/childes_meta$Tokens, childes_meta$VOS/childes_meta$Tokens, childes_meta$OVS/childes_meta$Tokens, childes_meta$OSV/childes_meta$Tokens,
                                                          childes_meta$SVO/childes_meta$Tokens, childes_meta$SOV/childes_meta$Tokens, childes_meta$VSO/childes_meta$Tokens, childes_meta$VOS/childes_meta$Tokens, childes_meta$OVS/childes_meta$Tokens, childes_meta$OSV/childes_meta$Tokens,
                                                          breton_meta$SVO/breton_meta$Tokens, breton_meta$SOV/breton_meta$Tokens, breton_meta$VSO/breton_meta$Tokens, breton_meta$VOS/breton_meta$Tokens, breton_meta$OVS/breton_meta$Tokens, breton_meta$OSV/breton_meta$Tokens,
                                                          breton_meta$SVO/breton_meta$Tokens, breton_meta$SOV/breton_meta$Tokens, breton_meta$VSO/breton_meta$Tokens, breton_meta$VOS/breton_meta$Tokens, breton_meta$OVS/breton_meta$Tokens, breton_meta$OSV/breton_meta$Tokens,
                                                          breton_meta$SVO/breton_meta$Tokens, breton_meta$SOV/breton_meta$Tokens, breton_meta$VSO/breton_meta$Tokens, breton_meta$VOS/breton_meta$Tokens, breton_meta$OVS/breton_meta$Tokens, breton_meta$OSV/breton_meta$Tokens,
                                                          welsh_meta$SVO/welsh_meta$Tokens, welsh_meta$SOV/welsh_meta$Tokens, welsh_meta$VSO/welsh_meta$Tokens, welsh_meta$VOS/welsh_meta$Tokens, welsh_meta$OVS/welsh_meta$Tokens, welsh_meta$OSV/welsh_meta$Tokens,
                                                          welsh_meta$SVO/welsh_meta$Tokens, welsh_meta$SOV/welsh_meta$Tokens, welsh_meta$VSO/welsh_meta$Tokens, welsh_meta$VOS/welsh_meta$Tokens, welsh_meta$OVS/welsh_meta$Tokens, welsh_meta$OSV/welsh_meta$Tokens,
                                                          welsh_meta$SVO/welsh_meta$Tokens, welsh_meta$SOV/welsh_meta$Tokens, welsh_meta$VSO/welsh_meta$Tokens, welsh_meta$VOS/welsh_meta$Tokens, welsh_meta$OVS/welsh_meta$Tokens, welsh_meta$OSV/welsh_meta$Tokens,
                                                          german_meta$SVO/german_meta$Tokens, german_meta$SOV/german_meta$Tokens, german_meta$VSO/german_meta$Tokens, german_meta$VOS/german_meta$Tokens, german_meta$OVS/german_meta$Tokens, german_meta$OSV/german_meta$Tokens,
                                                          german_meta$SVO/german_meta$Tokens, german_meta$SOV/german_meta$Tokens, german_meta$VSO/german_meta$Tokens, german_meta$VOS/german_meta$Tokens, german_meta$OVS/german_meta$Tokens, german_meta$OSV/german_meta$Tokens,
                                                          german_meta$SVO/german_meta$Tokens, german_meta$SOV/german_meta$Tokens, german_meta$VSO/german_meta$Tokens, german_meta$VOS/german_meta$Tokens, german_meta$OVS/german_meta$Tokens, german_meta$OSV/german_meta$Tokens,
                                                          english_meta$SVO/english_meta$Tokens, english_meta$SOV/english_meta$Tokens, english_meta$VSO/english_meta$Tokens, english_meta$VOS/english_meta$Tokens, english_meta$OVS/english_meta$Tokens, english_meta$OSV/english_meta$Tokens,
                                                          english_meta$SVO/english_meta$Tokens, english_meta$SOV/english_meta$Tokens, english_meta$VSO/english_meta$Tokens, english_meta$VOS/english_meta$Tokens, english_meta$OVS/english_meta$Tokens, english_meta$OSV/english_meta$Tokens,
                                                          english_meta$SVO/english_meta$Tokens, english_meta$SOV/english_meta$Tokens, english_meta$VSO/english_meta$Tokens, english_meta$VOS/english_meta$Tokens, english_meta$OVS/english_meta$Tokens, english_meta$OSV/english_meta$Tokens,
                                                          basque_meta$SVO/basque_meta$Tokens, basque_meta$SOV/basque_meta$Tokens, basque_meta$VSO/basque_meta$Tokens, basque_meta$VOS/basque_meta$Tokens, basque_meta$OVS/basque_meta$Tokens, basque_meta$OSV/basque_meta$Tokens,
                                                          basque_meta$SVO/basque_meta$Tokens, basque_meta$SOV/basque_meta$Tokens, basque_meta$VSO/basque_meta$Tokens, basque_meta$VOS/basque_meta$Tokens, basque_meta$OVS/basque_meta$Tokens, basque_meta$OSV/basque_meta$Tokens,
                                                          basque_meta$SVO/basque_meta$Tokens, basque_meta$SOV/basque_meta$Tokens, basque_meta$VSO/basque_meta$Tokens, basque_meta$VOS/basque_meta$Tokens, basque_meta$OVS/basque_meta$Tokens, basque_meta$OSV/basque_meta$Tokens,
                                                          irish_meta$SVO/irish_meta$Tokens, irish_meta$SOV/irish_meta$Tokens, irish_meta$VSO/irish_meta$Tokens, irish_meta$VOS/irish_meta$Tokens, irish_meta$OVS/irish_meta$Tokens, irish_meta$OSV/irish_meta$Tokens,
                                                          irish_meta$SVO/irish_meta$Tokens, irish_meta$SOV/irish_meta$Tokens, irish_meta$VSO/irish_meta$Tokens, irish_meta$VOS/irish_meta$Tokens, irish_meta$OVS/irish_meta$Tokens, irish_meta$OSV/irish_meta$Tokens,
                                                          irish_meta$SVO/irish_meta$Tokens, irish_meta$SOV/irish_meta$Tokens, irish_meta$VSO/irish_meta$Tokens, irish_meta$VOS/irish_meta$Tokens, irish_meta$OVS/irish_meta$Tokens, irish_meta$OSV/irish_meta$Tokens,
                                                          scottish_gaelic_meta$SVO/scottish_gaelic_meta$Tokens, scottish_gaelic_meta$SOV/scottish_gaelic_meta$Tokens, scottish_gaelic_meta$VSO/scottish_gaelic_meta$Tokens, scottish_gaelic_meta$VOS/scottish_gaelic_meta$Tokens, scottish_gaelic_meta$OVS/scottish_gaelic_meta$Tokens, scottish_gaelic_meta$OSV/scottish_gaelic_meta$Tokens,
                                                          scottish_gaelic_meta$SVO/scottish_gaelic_meta$Tokens, scottish_gaelic_meta$SOV/scottish_gaelic_meta$Tokens, scottish_gaelic_meta$VSO/scottish_gaelic_meta$Tokens, scottish_gaelic_meta$VOS/scottish_gaelic_meta$Tokens, scottish_gaelic_meta$OVS/scottish_gaelic_meta$Tokens, scottish_gaelic_meta$OSV/scottish_gaelic_meta$Tokens,
                                                          scottish_gaelic_meta$SVO/scottish_gaelic_meta$Tokens, scottish_gaelic_meta$SOV/scottish_gaelic_meta$Tokens, scottish_gaelic_meta$VSO/scottish_gaelic_meta$Tokens, scottish_gaelic_meta$VOS/scottish_gaelic_meta$Tokens, scottish_gaelic_meta$OVS/scottish_gaelic_meta$Tokens, scottish_gaelic_meta$OSV/scottish_gaelic_meta$Tokens,
                                                          hindi_meta$SVO/hindi_meta$Tokens, hindi_meta$SOV/hindi_meta$Tokens, hindi_meta$VSO/hindi_meta$Tokens, hindi_meta$VOS/hindi_meta$Tokens, hindi_meta$OVS/hindi_meta$Tokens, hindi_meta$OSV/hindi_meta$Tokens,
                                                          hindi_meta$SVO/hindi_meta$Tokens, hindi_meta$SOV/hindi_meta$Tokens, hindi_meta$VSO/hindi_meta$Tokens, hindi_meta$VOS/hindi_meta$Tokens, hindi_meta$OVS/hindi_meta$Tokens, hindi_meta$OSV/hindi_meta$Tokens,
                                                          hindi_meta$SVO/hindi_meta$Tokens, hindi_meta$SOV/hindi_meta$Tokens, hindi_meta$VSO/hindi_meta$Tokens, hindi_meta$VOS/hindi_meta$Tokens, hindi_meta$OVS/hindi_meta$Tokens, hindi_meta$OSV/hindi_meta$Tokens,
                                                          hungarian_meta$SVO/hungarian_meta$Tokens, hungarian_meta$SOV/hungarian_meta$Tokens, hungarian_meta$VSO/hungarian_meta$Tokens, hungarian_meta$VOS/hungarian_meta$Tokens, hungarian_meta$OVS/hungarian_meta$Tokens, hungarian_meta$OSV/hungarian_meta$Tokens,
                                                          hungarian_meta$SVO/hungarian_meta$Tokens, hungarian_meta$SOV/hungarian_meta$Tokens, hungarian_meta$VSO/hungarian_meta$Tokens, hungarian_meta$VOS/hungarian_meta$Tokens, hungarian_meta$OVS/hungarian_meta$Tokens, hungarian_meta$OSV/hungarian_meta$Tokens,
                                                          hungarian_meta$SVO/hungarian_meta$Tokens, hungarian_meta$SOV/hungarian_meta$Tokens, hungarian_meta$VSO/hungarian_meta$Tokens, hungarian_meta$VOS/hungarian_meta$Tokens, hungarian_meta$OVS/hungarian_meta$Tokens, hungarian_meta$OSV/hungarian_meta$Tokens,
                                                          indonesian_meta$SVO/indonesian_meta$Tokens, indonesian_meta$SOV/indonesian_meta$Tokens, indonesian_meta$VSO/indonesian_meta$Tokens, indonesian_meta$VOS/indonesian_meta$Tokens, indonesian_meta$OVS/indonesian_meta$Tokens, indonesian_meta$OSV/indonesian_meta$Tokens,
                                                          indonesian_meta$SVO/indonesian_meta$Tokens, indonesian_meta$SOV/indonesian_meta$Tokens, indonesian_meta$VSO/indonesian_meta$Tokens, indonesian_meta$VOS/indonesian_meta$Tokens, indonesian_meta$OVS/indonesian_meta$Tokens, indonesian_meta$OSV/indonesian_meta$Tokens,
                                                          indonesian_meta$SVO/indonesian_meta$Tokens, indonesian_meta$SOV/indonesian_meta$Tokens, indonesian_meta$VSO/indonesian_meta$Tokens, indonesian_meta$VOS/indonesian_meta$Tokens, indonesian_meta$OVS/indonesian_meta$Tokens, indonesian_meta$OSV/indonesian_meta$Tokens,
                                                          japanese_meta$SVO/japanese_meta$Tokens, japanese_meta$SOV/japanese_meta$Tokens, japanese_meta$VSO/japanese_meta$Tokens, japanese_meta$VOS/japanese_meta$Tokens, japanese_meta$OVS/japanese_meta$Tokens, japanese_meta$OSV/japanese_meta$Tokens,
                                                          japanese_meta$SVO/japanese_meta$Tokens, japanese_meta$SOV/japanese_meta$Tokens, japanese_meta$VSO/japanese_meta$Tokens, japanese_meta$VOS/japanese_meta$Tokens, japanese_meta$OVS/japanese_meta$Tokens, japanese_meta$OSV/japanese_meta$Tokens,
                                                          japanese_meta$SVO/japanese_meta$Tokens, japanese_meta$SOV/japanese_meta$Tokens, japanese_meta$VSO/japanese_meta$Tokens, japanese_meta$VOS/japanese_meta$Tokens, japanese_meta$OVS/japanese_meta$Tokens, japanese_meta$OSV/japanese_meta$Tokens,
                                                          korean_meta$SVO/korean_meta$Tokens, korean_meta$SOV/korean_meta$Tokens, korean_meta$VSO/korean_meta$Tokens, korean_meta$VOS/korean_meta$Tokens, korean_meta$OVS/korean_meta$Tokens, korean_meta$OSV/korean_meta$Tokens,
                                                          korean_meta$SVO/korean_meta$Tokens, korean_meta$SOV/korean_meta$Tokens, korean_meta$VSO/korean_meta$Tokens, korean_meta$VOS/korean_meta$Tokens, korean_meta$OVS/korean_meta$Tokens, korean_meta$OSV/korean_meta$Tokens,
                                                          korean_meta$SVO/korean_meta$Tokens, korean_meta$SOV/korean_meta$Tokens, korean_meta$VSO/korean_meta$Tokens, korean_meta$VOS/korean_meta$Tokens, korean_meta$OVS/korean_meta$Tokens, korean_meta$OSV/korean_meta$Tokens,
                                                          mbya_guarani_meta$SVO/mbya_guarani_meta$Tokens, mbya_guarani_meta$SOV/mbya_guarani_meta$Tokens, mbya_guarani_meta$VSO/mbya_guarani_meta$Tokens, mbya_guarani_meta$VOS/mbya_guarani_meta$Tokens, mbya_guarani_meta$OVS/mbya_guarani_meta$Tokens, mbya_guarani_meta$OSV/mbya_guarani_meta$Tokens,
                                                          mbya_guarani_meta$SVO/mbya_guarani_meta$Tokens, mbya_guarani_meta$SOV/mbya_guarani_meta$Tokens, mbya_guarani_meta$VSO/mbya_guarani_meta$Tokens, mbya_guarani_meta$VOS/mbya_guarani_meta$Tokens, mbya_guarani_meta$OVS/mbya_guarani_meta$Tokens, mbya_guarani_meta$OSV/mbya_guarani_meta$Tokens,
                                                          mbya_guarani_meta$SVO/mbya_guarani_meta$Tokens, mbya_guarani_meta$SOV/mbya_guarani_meta$Tokens, mbya_guarani_meta$VSO/mbya_guarani_meta$Tokens, mbya_guarani_meta$VOS/mbya_guarani_meta$Tokens, mbya_guarani_meta$OVS/mbya_guarani_meta$Tokens, mbya_guarani_meta$OSV/mbya_guarani_meta$Tokens,
                                                          dutch_meta$SVO/dutch_meta$Tokens, dutch_meta$SOV/dutch_meta$Tokens, dutch_meta$VSO/dutch_meta$Tokens, dutch_meta$VOS/dutch_meta$Tokens, dutch_meta$OVS/dutch_meta$Tokens, dutch_meta$OSV/dutch_meta$Tokens,
                                                          dutch_meta$SVO/dutch_meta$Tokens, dutch_meta$SOV/dutch_meta$Tokens, dutch_meta$VSO/dutch_meta$Tokens, dutch_meta$VOS/dutch_meta$Tokens, dutch_meta$OVS/dutch_meta$Tokens, dutch_meta$OSV/dutch_meta$Tokens,
                                                          dutch_meta$SVO/dutch_meta$Tokens, dutch_meta$SOV/dutch_meta$Tokens, dutch_meta$VSO/dutch_meta$Tokens, dutch_meta$VOS/dutch_meta$Tokens, dutch_meta$OVS/dutch_meta$Tokens, dutch_meta$OSV/dutch_meta$Tokens,
                                                          tagalog_meta$SVO/tagalog_meta$Tokens, tagalog_meta$SOV/tagalog_meta$Tokens, tagalog_meta$VSO/tagalog_meta$Tokens, tagalog_meta$VOS/tagalog_meta$Tokens, tagalog_meta$OVS/tagalog_meta$Tokens, tagalog_meta$OSV/tagalog_meta$Tokens,
                                                          tagalog_meta$SVO/tagalog_meta$Tokens, tagalog_meta$SOV/tagalog_meta$Tokens, tagalog_meta$VSO/tagalog_meta$Tokens, tagalog_meta$VOS/tagalog_meta$Tokens, tagalog_meta$OVS/tagalog_meta$Tokens, tagalog_meta$OSV/tagalog_meta$Tokens,
                                                          tagalog_meta$SVO/tagalog_meta$Tokens, tagalog_meta$SOV/tagalog_meta$Tokens, tagalog_meta$VSO/tagalog_meta$Tokens, tagalog_meta$VOS/tagalog_meta$Tokens, tagalog_meta$OVS/tagalog_meta$Tokens, tagalog_meta$OSV/tagalog_meta$Tokens,
                                                          turkish_meta$SVO/turkish_meta$Tokens, turkish_meta$SOV/turkish_meta$Tokens, turkish_meta$VSO/turkish_meta$Tokens, turkish_meta$VOS/turkish_meta$Tokens, turkish_meta$OVS/turkish_meta$Tokens, turkish_meta$OSV/turkish_meta$Tokens,
                                                          turkish_meta$SVO/turkish_meta$Tokens, turkish_meta$SOV/turkish_meta$Tokens, turkish_meta$VSO/turkish_meta$Tokens, turkish_meta$VOS/turkish_meta$Tokens, turkish_meta$OVS/turkish_meta$Tokens, turkish_meta$OSV/turkish_meta$Tokens,
                                                          turkish_meta$SVO/turkish_meta$Tokens, turkish_meta$SOV/turkish_meta$Tokens, turkish_meta$VSO/turkish_meta$Tokens, turkish_meta$VOS/turkish_meta$Tokens, turkish_meta$OVS/turkish_meta$Tokens, turkish_meta$OSV/turkish_meta$Tokens,
                                                          vietnamese_meta$SVO/vietnamese_meta$Tokens, vietnamese_meta$SOV/vietnamese_meta$Tokens, vietnamese_meta$VSO/vietnamese_meta$Tokens, vietnamese_meta$VOS/vietnamese_meta$Tokens, vietnamese_meta$OVS/vietnamese_meta$Tokens, vietnamese_meta$OSV/vietnamese_meta$Tokens,
                                                          vietnamese_meta$SVO/vietnamese_meta$Tokens, vietnamese_meta$SOV/vietnamese_meta$Tokens, vietnamese_meta$VSO/vietnamese_meta$Tokens, vietnamese_meta$VOS/vietnamese_meta$Tokens, vietnamese_meta$OVS/vietnamese_meta$Tokens, vietnamese_meta$OSV/vietnamese_meta$Tokens,
                                                          vietnamese_meta$SVO/vietnamese_meta$Tokens, vietnamese_meta$SOV/vietnamese_meta$Tokens, vietnamese_meta$VSO/vietnamese_meta$Tokens, vietnamese_meta$VOS/vietnamese_meta$Tokens, vietnamese_meta$OVS/vietnamese_meta$Tokens, vietnamese_meta$OSV/vietnamese_meta$Tokens,
                                                          warlpiri_meta$SVO/warlpiri_meta$Tokens, warlpiri_meta$SOV/warlpiri_meta$Tokens, warlpiri_meta$VSO/warlpiri_meta$Tokens, warlpiri_meta$VOS/warlpiri_meta$Tokens, warlpiri_meta$OVS/warlpiri_meta$Tokens, warlpiri_meta$OSV/warlpiri_meta$Tokens,
                                                          warlpiri_meta$SVO/warlpiri_meta$Tokens, warlpiri_meta$SOV/warlpiri_meta$Tokens, warlpiri_meta$VSO/warlpiri_meta$Tokens, warlpiri_meta$VOS/warlpiri_meta$Tokens, warlpiri_meta$OVS/warlpiri_meta$Tokens, warlpiri_meta$OSV/warlpiri_meta$Tokens,
                                                          warlpiri_meta$SVO/warlpiri_meta$Tokens, warlpiri_meta$SOV/warlpiri_meta$Tokens, warlpiri_meta$VSO/warlpiri_meta$Tokens, warlpiri_meta$VOS/warlpiri_meta$Tokens, warlpiri_meta$OVS/warlpiri_meta$Tokens, warlpiri_meta$OSV/warlpiri_meta$Tokens,
                                                          wolof_meta$SVO/wolof_meta$Tokens, wolof_meta$SOV/wolof_meta$Tokens, wolof_meta$VSO/wolof_meta$Tokens, wolof_meta$VOS/wolof_meta$Tokens, wolof_meta$OVS/wolof_meta$Tokens, wolof_meta$OSV/wolof_meta$Tokens,
                                                          wolof_meta$SVO/wolof_meta$Tokens, wolof_meta$SOV/wolof_meta$Tokens, wolof_meta$VSO/wolof_meta$Tokens, wolof_meta$VOS/wolof_meta$Tokens, wolof_meta$OVS/wolof_meta$Tokens, wolof_meta$OSV/wolof_meta$Tokens,
                                                          wolof_meta$SVO/wolof_meta$Tokens, wolof_meta$SOV/wolof_meta$Tokens, wolof_meta$VSO/wolof_meta$Tokens, wolof_meta$VOS/wolof_meta$Tokens, wolof_meta$OVS/wolof_meta$Tokens, wolof_meta$OSV/wolof_meta$Tokens,
                                                          mandarin_meta$SVO/mandarin_meta$Tokens, mandarin_meta$SOV/mandarin_meta$Tokens, mandarin_meta$VSO/mandarin_meta$Tokens, mandarin_meta$VOS/mandarin_meta$Tokens, mandarin_meta$OVS/mandarin_meta$Tokens, mandarin_meta$OSV/mandarin_meta$Tokens,
                                                          mandarin_meta$SVO/mandarin_meta$Tokens, mandarin_meta$SOV/mandarin_meta$Tokens, mandarin_meta$VSO/mandarin_meta$Tokens, mandarin_meta$VOS/mandarin_meta$Tokens, mandarin_meta$OVS/mandarin_meta$Tokens, mandarin_meta$OSV/mandarin_meta$Tokens,
                                                          mandarin_meta$SVO/mandarin_meta$Tokens, mandarin_meta$SOV/mandarin_meta$Tokens, mandarin_meta$VSO/mandarin_meta$Tokens, mandarin_meta$VOS/mandarin_meta$Tokens, mandarin_meta$OVS/mandarin_meta$Tokens, mandarin_meta$OSV/mandarin_meta$Tokens)) %>%
    mutate(proportion = proportion*100)
