library(tidyverse)
library(ggplot2)

meta <- read_csv("language metadata.csv")
childes <- read_csv("childes_brown-results.csv") %>%
  mutate(language = "English CHILDES")
ancient_greek <- read_csv("ud_ael-results.csv") %>%
  mutate(language = "Ancient Greek") %>%
  bind_rows(read_csv("ud_ael-subset1-results.csv") %>% mutate(language = "Ancient Greek subset 1")) %>%
  bind_rows(read_csv("ud_ael-subset2-results.csv") %>% mutate(language = "Ancient Greek subset 2")) %>%
  bind_rows(read_csv("ud_ael-subset3-results.csv") %>% mutate(language = "Ancient Greek subset 3")) %>%
  bind_rows(read_csv("ud_ael-subset4-results.csv") %>% mutate(language = "Ancient Greek subset 4")) %>%
  bind_rows(read_csv("ud_ael-subset5-results.csv") %>% mutate(language = "Ancient Greek subset 5")) %>%
  bind_rows(read_csv("ud_ael-subset6-results.csv") %>% mutate(language = "Ancient Greek subset 6")) %>%
  bind_rows(read_csv("ud_ael-subset7-results.csv") %>% mutate(language = "Ancient Greek subset 7")) %>%
  bind_rows(read_csv("ud_ael-subset8-results.csv") %>% mutate(language = "Ancient Greek subset 8")) %>%
  bind_rows(read_csv("ud_ael-subset9-results.csv") %>% mutate(language = "Ancient Greek subset 9")) %>%
  bind_rows(read_csv("ud_ael-subset10-results.csv") %>% mutate(language = "Ancient Greek subset 10"))

afrikaans <- read_csv("ud_afr-results.csv") %>%
  mutate(language = "Afrikaans") %>%
  bind_rows(read_csv("ud_afr-subset1-results.csv") %>% mutate(language = "Afrikaans subset 1")) %>%
  bind_rows(read_csv("ud_afr-subset2-results.csv") %>% mutate(language = "Afrikaans subset 2")) %>%
  bind_rows(read_csv("ud_afr-subset3-results.csv") %>% mutate(language = "Afrikaans subset 3")) %>%
  bind_rows(read_csv("ud_afr-subset4-results.csv") %>% mutate(language = "Afrikaans subset 4")) %>%
  bind_rows(read_csv("ud_afr-subset5-results.csv") %>% mutate(language = "Afrikaans subset 5")) %>%
  bind_rows(read_csv("ud_afr-subset6-results.csv") %>% mutate(language = "Afrikaans subset 6")) %>%
  bind_rows(read_csv("ud_afr-subset7-results.csv") %>% mutate(language = "Afrikaans subset 7")) %>%
  bind_rows(read_csv("ud_afr-subset8-results.csv") %>% mutate(language = "Afrikaans subset 8")) %>%
  bind_rows(read_csv("ud_afr-subset9-results.csv") %>% mutate(language = "Afrikaans subset 9")) %>%
  bind_rows(read_csv("ud_afr-subset10-results.csv") %>% mutate(language = "Afrikaans subset 10"))

akkadian <- read_csv("ud_akk-results.csv") %>%
  mutate(language = "Akkadian")

amharic <- read_csv("ud_amh-results.csv") %>%
  mutate(I2 = as.double(I2), language = "Amharic") %>%
  bind_rows(read_csv("ud_amh-subset1-results.csv") %>% mutate(language = "Amharic subset 1")) %>%
  bind_rows(read_csv("ud_amh-subset2-results.csv") %>% mutate(language = "Amharic subset 2")) %>%
  bind_rows(read_csv("ud_amh-subset3-results.csv") %>% mutate(language = "Amharic subset 3")) %>%
  bind_rows(read_csv("ud_amh-subset4-results.csv") %>% mutate(language = "Amharic subset 4")) %>%
  bind_rows(read_csv("ud_amh-subset5-results.csv") %>% mutate(language = "Amharic subset 5")) %>%
  bind_rows(read_csv("ud_amh-subset6-results.csv") %>% mutate(language = "Amharic subset 6")) %>%
  bind_rows(read_csv("ud_amh-subset7-results.csv") %>% mutate(language = "Amharic subset 7")) %>%
  bind_rows(read_csv("ud_amh-subset8-results.csv") %>% mutate(I2 = as.double(I2),language = "Amharic subset 8")) %>%
  bind_rows(read_csv("ud_amh-subset9-results.csv") %>% mutate(language = "Amharic subset 9")) %>%
  bind_rows(read_csv("ud_amh-subset10-results.csv") %>% mutate(I2 = as.double(I2), language = "Amharic subset 10"))

arabic <- read_csv("ud_ara-results.csv") %>%
  mutate(language = "Arabic") %>%
  bind_rows(read_csv("ud_ara-subset1-results.csv") %>% mutate(language = "Arabic subset 1")) %>%
  bind_rows(read_csv("ud_ara-subset2-results.csv") %>% mutate(language = "Arabic subset 2")) %>%
  bind_rows(read_csv("ud_ara-subset3-results.csv") %>% mutate(language = "Arabic subset 3")) %>%
  bind_rows(read_csv("ud_ara-subset4-results.csv") %>% mutate(language = "Arabic subset 4")) %>%
  bind_rows(read_csv("ud_ara-subset5-results.csv") %>% mutate(language = "Arabic subset 5")) %>%
  bind_rows(read_csv("ud_ara-subset6-results.csv") %>% mutate(language = "Arabic subset 6")) %>%
  bind_rows(read_csv("ud_ara-subset7-results.csv") %>% mutate(language = "Arabic subset 7")) %>%
  bind_rows(read_csv("ud_ara-subset8-results.csv") %>% mutate(language = "Arabic subset 8")) %>%
  bind_rows(read_csv("ud_ara-subset9-results.csv") %>% mutate(language = "Arabic subset 9")) %>%
  bind_rows(read_csv("ud_ara-subset10-results.csv") %>% mutate(language = "Arabic subset 10"))

armenian <- read_csv("ud_arm-results.csv") %>%
  mutate(I2 = as.double(I2), language = "Armenian") %>%
  bind_rows(read_csv("ud_arm-subset1-results.csv") %>% mutate(language = "Armenian subset 1")) %>%
  bind_rows(read_csv("ud_arm-subset2-results.csv") %>% mutate(language = "Armenian subset 2")) %>%
  bind_rows(read_csv("ud_arm-subset3-results.csv") %>% mutate(language = "Armenian subset 3")) %>%
  bind_rows(read_csv("ud_arm-subset4-results.csv") %>% mutate(language = "Armenian subset 4")) %>%
  bind_rows(read_csv("ud_arm-subset5-results.csv") %>% mutate(language = "Armenian subset 5")) %>%
  bind_rows(read_csv("ud_arm-subset6-results.csv") %>% mutate(language = "Armenian subset 6")) %>%
  bind_rows(read_csv("ud_arm-subset7-results.csv") %>% mutate(language = "Armenian subset 7")) %>%
  bind_rows(read_csv("ud_arm-subset8-results.csv") %>% mutate(language = "Armenian subset 8")) %>%
  bind_rows(read_csv("ud_arm-subset9-results.csv") %>% mutate(language = "Armenian subset 9")) %>%
  bind_rows(read_csv("ud_arm-subset10-results.csv") %>% mutate(language = "Armenian subset 10"))

bambara <- read_csv("ud_bam-results.csv") %>%
  mutate(language = "Bambara")

bhojpuri <- read_csv("ud_bho-results.csv") %>%
  mutate(language = "Bhojpuri")

breton <- read_csv("ud_bre-results.csv") %>%
  mutate(language = "Breton")

belarusian <- read_csv("ud_bru-results.csv") %>%
  mutate(language = "Belarusian")

bulgarian <- read_csv("ud_bul-results.csv") %>%
  mutate(language = "Bulgarian") %>%
  bind_rows(read_csv("ud_bul-subset1-results.csv") %>% mutate(language = "Bulgarian subset 1")) %>%
  bind_rows(read_csv("ud_bul-subset2-results.csv") %>% mutate(language = "Bulgarian subset 2")) %>%
  bind_rows(read_csv("ud_bul-subset3-results.csv") %>% mutate(language = "Bulgarian subset 3")) %>%
  bind_rows(read_csv("ud_bul-subset4-results.csv") %>% mutate(language = "Bulgarian subset 4")) %>%
  bind_rows(read_csv("ud_bul-subset5-results.csv") %>% mutate(language = "Bulgarian subset 5")) %>%
  bind_rows(read_csv("ud_bul-subset6-results.csv") %>% mutate(language = "Bulgarian subset 6")) %>%
  bind_rows(read_csv("ud_bul-subset7-results.csv") %>% mutate(language = "Bulgarian subset 7")) %>%
  bind_rows(read_csv("ud_bul-subset8-results.csv") %>% mutate(language = "Bulgarian subset 8")) %>%
  bind_rows(read_csv("ud_bul-subset9-results.csv") %>% mutate(language = "Bulgarian subset 9")) %>%
  bind_rows(read_csv("ud_bul-subset10-results.csv") %>% mutate(language = "Bulgarian subset 10"))

buryat <- read_csv("ud_bur-results.csv") %>%
  mutate(language = "Buryat")

cantonese <- read_csv("ud_can-results.csv") %>%
  mutate(language = "Cantonese")

catalan <- read_csv("ud_cat-results.csv") %>%
  mutate(language = "Catalan") %>%
  bind_rows(read_csv("ud_cat-subset1-results.csv") %>% mutate(language = "Catalan subset 1")) %>%
  bind_rows(read_csv("ud_cat-subset2-results.csv") %>% mutate(language = "Catalan subset 2")) %>%
  bind_rows(read_csv("ud_cat-subset3-results.csv") %>% mutate(language = "Catalan subset 3")) %>%
  bind_rows(read_csv("ud_cat-subset4-results.csv") %>% mutate(language = "Catalan subset 4")) %>%
  bind_rows(read_csv("ud_cat-subset5-results.csv") %>% mutate(language = "Catalan subset 5")) %>%
  bind_rows(read_csv("ud_cat-subset6-results.csv") %>% mutate(language = "Catalan subset 6")) %>%
  bind_rows(read_csv("ud_cat-subset7-results.csv") %>% mutate(language = "Catalan subset 7")) %>%
  bind_rows(read_csv("ud_cat-subset8-results.csv") %>% mutate(language = "Catalan subset 8")) %>%
  bind_rows(read_csv("ud_cat-subset9-results.csv") %>% mutate(language = "Catalan subset 9")) %>%
  bind_rows(read_csv("ud_cat-subset10-results.csv") %>% mutate(language = "Catalan subset 10"))

classical_chinese <- read_csv("ud_cch-results.csv") %>%
  mutate(language = "Classical Chinese") %>%
  bind_rows(read_csv("ud_cch-subset1-results.csv") %>% mutate(language = "Classical Chinese subset 1")) %>%
  bind_rows(read_csv("ud_cch-subset2-results.csv") %>% mutate(language = "Classical Chinese subset 2")) %>%
  bind_rows(read_csv("ud_cch-subset3-results.csv") %>% mutate(language = "Classical Chinese subset 3")) %>%
  bind_rows(read_csv("ud_cch-subset4-results.csv") %>% mutate(language = "Classical Chinese subset 4")) %>%
  bind_rows(read_csv("ud_cch-subset5-results.csv") %>% mutate(language = "Classical Chinese subset 5")) %>%
  bind_rows(read_csv("ud_cch-subset6-results.csv") %>% mutate(language = "Classical Chinese subset 6")) %>%
  bind_rows(read_csv("ud_cch-subset7-results.csv") %>% mutate(language = "Classical Chinese subset 7")) %>%
  bind_rows(read_csv("ud_cch-subset8-results.csv") %>% mutate(language = "Classical Chinese subset 8")) %>%
  bind_rows(read_csv("ud_cch-subset9-results.csv") %>% mutate(language = "Classical Chinese subset 9")) %>%
  bind_rows(read_csv("ud_cch-subset10-results.csv") %>% mutate(language = "Classical Chinese subset 10"))

coptic <- read_csv("ud_cop-results.csv") %>%
  mutate(language = "Coptic") %>%
  bind_rows(read_csv("ud_cop-subset1-results.csv") %>% mutate(language = "Coptic subset 1")) %>%
  bind_rows(read_csv("ud_cop-subset2-results.csv") %>% mutate(language = "Coptic subset 2")) %>%
  bind_rows(read_csv("ud_cop-subset3-results.csv") %>% mutate(language = "Coptic subset 3")) %>%
  bind_rows(read_csv("ud_cop-subset4-results.csv") %>% mutate(language = "Coptic subset 4")) %>%
  bind_rows(read_csv("ud_cop-subset5-results.csv") %>% mutate(language = "Coptic subset 5")) %>%
  bind_rows(read_csv("ud_cop-subset6-results.csv") %>% mutate(language = "Coptic subset 6")) %>%
  bind_rows(read_csv("ud_cop-subset7-results.csv") %>% mutate(language = "Coptic subset 7")) %>%
  bind_rows(read_csv("ud_cop-subset8-results.csv") %>% mutate(language = "Coptic subset 8")) %>%
  bind_rows(read_csv("ud_cop-subset9-results.csv") %>% mutate(language = "Coptic subset 9")) %>%
  bind_rows(read_csv("ud_cop-subset10-results.csv") %>% mutate(language = "Coptic subset 10"))

croatian <- read_csv("ud_cro-results.csv") %>%
  mutate(language = "Croatian") %>%
  bind_rows(read_csv("ud_cro-subset1-results.csv") %>% mutate(language = "Croatian subset 1")) %>%
  bind_rows(read_csv("ud_cro-subset2-results.csv") %>% mutate(language = "Croatian subset 2")) %>%
  bind_rows(read_csv("ud_cro-subset3-results.csv") %>% mutate(language = "Croatian subset 3")) %>%
  bind_rows(read_csv("ud_cro-subset4-results.csv") %>% mutate(language = "Croatian subset 4")) %>%
  bind_rows(read_csv("ud_cro-subset5-results.csv") %>% mutate(language = "Croatian subset 5")) %>%
  bind_rows(read_csv("ud_cro-subset6-results.csv") %>% mutate(language = "Croatian subset 6")) %>%
  bind_rows(read_csv("ud_cro-subset7-results.csv") %>% mutate(language = "Croatian subset 7")) %>%
  bind_rows(read_csv("ud_cro-subset8-results.csv") %>% mutate(language = "Croatian subset 8")) %>%
  bind_rows(read_csv("ud_cro-subset9-results.csv") %>% mutate(language = "Croatian subset 9")) %>%
  bind_rows(read_csv("ud_cro-subset10-results.csv") %>% mutate(language = "Croatian subset 10"))

welsh <- read_csv("ud_cym-results.csv") %>%
  mutate(language = "Welsh")

czech <- read_csv("ud_cze-results.csv") %>%
  mutate(language = "Czech") %>%
  bind_rows(read_csv("ud_cze-subset1-results.csv") %>% mutate(language = "Czech subset 1")) %>%
  bind_rows(read_csv("ud_cze-subset2-results.csv") %>% mutate(language = "Czech subset 2")) %>%
  bind_rows(read_csv("ud_cze-subset3-results.csv") %>% mutate(language = "Czech subset 3")) %>%
  bind_rows(read_csv("ud_cze-subset4-results.csv") %>% mutate(language = "Czech subset 4")) %>%
  bind_rows(read_csv("ud_cze-subset5-results.csv") %>% mutate(language = "Czech subset 5")) %>%
  bind_rows(read_csv("ud_cze-subset6-results.csv") %>% mutate(language = "Czech subset 6")) %>%
  bind_rows(read_csv("ud_cze-subset7-results.csv") %>% mutate(language = "Czech subset 7")) %>%
  bind_rows(read_csv("ud_cze-subset8-results.csv") %>% mutate(language = "Czech subset 8")) %>%
  bind_rows(read_csv("ud_cze-subset9-results.csv") %>% mutate(language = "Czech subset 9")) %>%
  bind_rows(read_csv("ud_cze-subset10-results.csv") %>% mutate(language = "Czech subset 10"))

danish <- read_csv("ud_dan-results.csv") %>%
  mutate(language = "Danish") %>%
  bind_rows(read_csv("ud_dan-subset1-results.csv") %>% mutate(language = "Danish subset 1")) %>%
  bind_rows(read_csv("ud_dan-subset2-results.csv") %>% mutate(language = "Danish subset 2")) %>%
  bind_rows(read_csv("ud_dan-subset3-results.csv") %>% mutate(language = "Danish subset 3")) %>%
  bind_rows(read_csv("ud_dan-subset4-results.csv") %>% mutate(language = "Danish subset 4")) %>%
  bind_rows(read_csv("ud_dan-subset5-results.csv") %>% mutate(language = "Danish subset 5")) %>%
  bind_rows(read_csv("ud_dan-subset6-results.csv") %>% mutate(language = "Danish subset 6")) %>%
  bind_rows(read_csv("ud_dan-subset7-results.csv") %>% mutate(language = "Danish subset 7")) %>%
  bind_rows(read_csv("ud_dan-subset8-results.csv") %>% mutate(language = "Danish subset 8")) %>%
  bind_rows(read_csv("ud_dan-subset9-results.csv") %>% mutate(language = "Danish subset 9")) %>%
  bind_rows(read_csv("ud_dan-subset10-results.csv") %>% mutate(language = "Danish subset 10"))

german <- read_csv("ud_deu-results.csv") %>%
  mutate(language = "German") %>%
  bind_rows(read_csv("ud_deu-subset1-results.csv") %>% mutate(language = "German subset 1")) %>%
  bind_rows(read_csv("ud_deu-subset2-results.csv") %>% mutate(language = "German subset 2")) %>%
  bind_rows(read_csv("ud_deu-subset3-results.csv") %>% mutate(language = "German subset 3")) %>%
  bind_rows(read_csv("ud_deu-subset4-results.csv") %>% mutate(language = "German subset 4")) %>%
  bind_rows(read_csv("ud_deu-subset5-results.csv") %>% mutate(language = "German subset 5")) %>%
  bind_rows(read_csv("ud_deu-subset6-results.csv") %>% mutate(language = "German subset 6")) %>%
  bind_rows(read_csv("ud_deu-subset7-results.csv") %>% mutate(language = "German subset 7")) %>%
  bind_rows(read_csv("ud_deu-subset8-results.csv") %>% mutate(language = "German subset 8")) %>%
  bind_rows(read_csv("ud_deu-subset9-results.csv") %>% mutate(language = "German subset 9")) %>%
  bind_rows(read_csv("ud_deu-subset10-results.csv") %>% mutate(language = "German subset 10"))

greek <- read_csv("ud_ell-results.csv") %>%
  mutate(I2 = as.double(I2), language = "Modern Greek") %>%
  bind_rows(read_csv("ud_ell-subset1-results.csv") %>% mutate(I2 = as.double(I2), language = "Modern Greek subset 1")) %>%
  bind_rows(read_csv("ud_ell-subset2-results.csv") %>% mutate(I2 = as.double(I2), language = "Modern Greek subset 2")) %>%
  bind_rows(read_csv("ud_ell-subset3-results.csv") %>% mutate(language = "Greek subset 3")) %>%
  bind_rows(read_csv("ud_ell-subset4-results.csv") %>% mutate(I2 = as.double(I2), language = "Modern Greek subset 4")) %>%
  bind_rows(read_csv("ud_ell-subset5-results.csv") %>% mutate(I2 = as.double(I2), language = "Modern Greek subset 5")) %>%
  bind_rows(read_csv("ud_ell-subset6-results.csv") %>% mutate(I2 = as.double(I2), language = "Modern Greek subset 6")) %>%
  bind_rows(read_csv("ud_ell-subset7-results.csv") %>% mutate(I2 = as.double(I2), language = "Modern Greek subset 7")) %>%
  bind_rows(read_csv("ud_ell-subset8-results.csv") %>% mutate(I2 = as.double(I2), language = "Modern Greek subset 8")) %>%
  bind_rows(read_csv("ud_ell-subset9-results.csv") %>% mutate(I2 = as.double(I2), language = "Modern Greek subset 9")) %>%
  bind_rows(read_csv("ud_ell-subset10-results.csv") %>% mutate(I2 = as.double(I2), language = "Modern Greek subset 10"))

english <- read_csv("ud_eng-results.csv") %>%
  mutate(language = "English") %>%
  bind_rows(read_csv("ud_eng-subset1-results.csv") %>% mutate(language = "English subset 1")) %>%
  bind_rows(read_csv("ud_eng-subset2-results.csv") %>% mutate(language = "English subset 2")) %>%
  bind_rows(read_csv("ud_eng-subset3-results.csv") %>% mutate(language = "English subset 3")) %>%
  bind_rows(read_csv("ud_eng-subset4-results.csv") %>% mutate(language = "English subset 4")) %>%
  bind_rows(read_csv("ud_eng-subset5-results.csv") %>% mutate(language = "English subset 5")) %>%
  bind_rows(read_csv("ud_eng-subset6-results.csv") %>% mutate(language = "English subset 6")) %>%
  bind_rows(read_csv("ud_eng-subset7-results.csv") %>% mutate(language = "English subset 7")) %>%
  bind_rows(read_csv("ud_eng-subset8-results.csv") %>% mutate(language = "English subset 8")) %>%
  bind_rows(read_csv("ud_eng-subset9-results.csv") %>% mutate(language = "English subset 9")) %>%
  bind_rows(read_csv("ud_eng-subset10-results.csv") %>% mutate(language = "English subset 10"))

erzya <- read_csv("ud_erz-results.csv") %>%
  mutate(language = "Erzya")

estonian <- read_csv("ud_est-results.csv") %>%
  mutate(language = "Estonian") %>%
  bind_rows(read_csv("ud_est-subset1-results.csv") %>% mutate(language = "Estonian subset 1")) %>%
  bind_rows(read_csv("ud_est-subset2-results.csv") %>% mutate(language = "Estonian subset 2")) %>%
  bind_rows(read_csv("ud_est-subset3-results.csv") %>% mutate(language = "Estonian subset 3")) %>%
  bind_rows(read_csv("ud_est-subset4-results.csv") %>% mutate(language = "Estonian subset 4")) %>%
  bind_rows(read_csv("ud_est-subset5-results.csv") %>% mutate(language = "Estonian subset 5")) %>%
  bind_rows(read_csv("ud_est-subset6-results.csv") %>% mutate(language = "Estonian subset 6")) %>%
  bind_rows(read_csv("ud_est-subset7-results.csv") %>% mutate(language = "Estonian subset 7")) %>%
  bind_rows(read_csv("ud_est-subset8-results.csv") %>% mutate(language = "Estonian subset 8")) %>%
  bind_rows(read_csv("ud_est-subset9-results.csv") %>% mutate(language = "Estonian subset 9")) %>%
  bind_rows(read_csv("ud_est-subset10-results.csv") %>% mutate(language = "Estonian subset 10"))

basque <- read_csv("ud_eus-results.csv") %>%
  mutate(language = "Basque") %>%
  bind_rows(read_csv("ud_eus-subset1-results.csv") %>% mutate(language = "Basque subset 1")) %>%
  bind_rows(read_csv("ud_eus-subset2-results.csv") %>% mutate(language = "Basque subset 2")) %>%
  bind_rows(read_csv("ud_eus-subset3-results.csv") %>% mutate(language = "Basque subset 3")) %>%
  bind_rows(read_csv("ud_eus-subset4-results.csv") %>% mutate(language = "Basque subset 4")) %>%
  bind_rows(read_csv("ud_eus-subset5-results.csv") %>% mutate(language = "Basque subset 5")) %>%
  bind_rows(read_csv("ud_eus-subset6-results.csv") %>% mutate(language = "Basque subset 6")) %>%
  bind_rows(read_csv("ud_eus-subset7-results.csv") %>% mutate(language = "Basque subset 7")) %>%
  bind_rows(read_csv("ud_eus-subset8-results.csv") %>% mutate(language = "Basque subset 8")) %>%
  bind_rows(read_csv("ud_eus-subset9-results.csv") %>% mutate(language = "Basque subset 9")) %>%
  bind_rows(read_csv("ud_eus-subset10-results.csv") %>% mutate(language = "Basque subset 10"))

farsi <- read_csv("ud_far-results.csv") %>%
  mutate(language = "Farsi")

finnish <- read_csv("ud_fin-results.csv") %>%
  mutate(language = "Finnish") %>%
  bind_rows(read_csv("ud_fin-subset1-results.csv") %>% mutate(language = "Finnish subset 1")) %>%
  bind_rows(read_csv("ud_fin-subset2-results.csv") %>% mutate(language = "Finnish subset 2")) %>%
  bind_rows(read_csv("ud_fin-subset3-results.csv") %>% mutate(language = "Finnish subset 3")) %>%
  bind_rows(read_csv("ud_fin-subset4-results.csv") %>% mutate(language = "Finnish subset 4")) %>%
  bind_rows(read_csv("ud_fin-subset5-results.csv") %>% mutate(language = "Finnish subset 5")) %>%
  bind_rows(read_csv("ud_fin-subset6-results.csv") %>% mutate(language = "Finnish subset 6")) %>%
  bind_rows(read_csv("ud_fin-subset7-results.csv") %>% mutate(language = "Finnish subset 7")) %>%
  bind_rows(read_csv("ud_fin-subset8-results.csv") %>% mutate(language = "Finnish subset 8")) %>%
  bind_rows(read_csv("ud_fin-subset9-results.csv") %>% mutate(language = "Finnish subset 9")) %>%
  bind_rows(read_csv("ud_fin-subset10-results.csv") %>% mutate(language = "Finnish subset 10"))

french <- read_csv("ud_fre-results.csv") %>%
  mutate(language = "French") %>%
  bind_rows(read_csv("ud_fre-subset1-results.csv") %>% mutate(language = "French subset 1")) %>%
  bind_rows(read_csv("ud_fre-subset2-results.csv") %>% mutate(language = "French subset 2")) %>%
  bind_rows(read_csv("ud_fre-subset3-results.csv") %>% mutate(language = "French subset 3")) %>%
  bind_rows(read_csv("ud_fre-subset4-results.csv") %>% mutate(language = "French subset 4")) %>%
  bind_rows(read_csv("ud_fre-subset5-results.csv") %>% mutate(language = "French subset 5")) %>%
  bind_rows(read_csv("ud_fre-subset6-results.csv") %>% mutate(language = "French subset 6")) %>%
  bind_rows(read_csv("ud_fre-subset7-results.csv") %>% mutate(language = "French subset 7")) %>%
  bind_rows(read_csv("ud_fre-subset8-results.csv") %>% mutate(language = "French subset 8")) %>%
  bind_rows(read_csv("ud_fre-subset9-results.csv") %>% mutate(language = "French subset 9")) %>%
  bind_rows(read_csv("ud_fre-subset10-results.csv") %>% mutate(language = "French subset 10"))

faroese <- read_csv("ud_fro-results.csv") %>%
  mutate(language = "Faroese")

irish <- read_csv("ud_gae-results.csv") %>%
  mutate(language = "Irish")

scottish_gaelic <- read_csv("ud_gai-results.csv") %>%
  mutate(language = "Scottish Gaelic")

galician <- read_csv("ud_gal-results.csv") %>%
  mutate(language = "Galician") %>%
  bind_rows(read_csv("ud_gal-subset1-results.csv") %>% mutate(language = "Galician subset 1")) %>%
  bind_rows(read_csv("ud_gal-subset2-results.csv") %>% mutate(language = "Galician subset 2")) %>%
  bind_rows(read_csv("ud_gal-subset3-results.csv") %>% mutate(language = "Galician subset 3")) %>%
  bind_rows(read_csv("ud_gal-subset4-results.csv") %>% mutate(language = "Galician subset 4")) %>%
  bind_rows(read_csv("ud_gal-subset5-results.csv") %>% mutate(language = "Galician subset 5")) %>%
  bind_rows(read_csv("ud_gal-subset6-results.csv") %>% mutate(language = "Galician subset 6")) %>%
  bind_rows(read_csv("ud_gal-subset7-results.csv") %>% mutate(language = "Galician subset 7")) %>%
  bind_rows(read_csv("ud_gal-subset8-results.csv") %>% mutate(language = "Galician subset 8")) %>%
  bind_rows(read_csv("ud_gal-subset9-results.csv") %>% mutate(language = "Galician subset 9")) %>%
  bind_rows(read_csv("ud_gal-subset10-results.csv") %>% mutate(language = "Galician subset 10"))

gothic <- read_csv("ud_got-results.csv") %>%
  mutate(language = "Gothic") %>%
  bind_rows(read_csv("ud_got-subset1-results.csv") %>% mutate(language = "Gothic subset 1")) %>%
  bind_rows(read_csv("ud_got-subset2-results.csv") %>% mutate(language = "Gothic subset 2")) %>%
  bind_rows(read_csv("ud_got-subset3-results.csv") %>% mutate(language = "Gothic subset 3")) %>%
  bind_rows(read_csv("ud_got-subset4-results.csv") %>% mutate(language = "Gothic subset 4")) %>%
  bind_rows(read_csv("ud_got-subset5-results.csv") %>% mutate(language = "Gothic subset 5")) %>%
  bind_rows(read_csv("ud_got-subset6-results.csv") %>% mutate(language = "Gothic subset 6")) %>%
  bind_rows(read_csv("ud_got-subset7-results.csv") %>% mutate(language = "Gothic subset 7")) %>%
  bind_rows(read_csv("ud_got-subset8-results.csv") %>% mutate(language = "Gothic subset 8")) %>%
  bind_rows(read_csv("ud_got-subset9-results.csv") %>% mutate(language = "Gothic subset 9")) %>%
  bind_rows(read_csv("ud_got-subset10-results.csv") %>% mutate(language = "Gothic subset 10"))

hebrew <- read_csv("ud_heb-results.csv") %>%
  mutate(I2 = as.double(I2), language = "Hebrew") %>%
  bind_rows(read_csv("ud_heb-subset1-results.csv") %>% mutate(I2 = as.double(I2), language = "Hebrew subset 1")) %>%
  bind_rows(read_csv("ud_heb-subset2-results.csv") %>% mutate(I2 = as.double(I2), language = "Hebrew subset 2")) %>%
  bind_rows(read_csv("ud_heb-subset3-results.csv") %>% mutate(I2 = as.double(I2), language = "Hebrew subset 3")) %>%
  bind_rows(read_csv("ud_heb-subset4-results.csv") %>% mutate(I2 = as.double(I2), language = "Hebrew subset 4")) %>%
  bind_rows(read_csv("ud_heb-subset5-results.csv") %>% mutate(I2 = as.double(I2), language = "Hebrew subset 5")) %>%
  bind_rows(read_csv("ud_heb-subset6-results.csv") %>% mutate(I2 = as.double(I2), language = "Hebrew subset 6")) %>%
  bind_rows(read_csv("ud_heb-subset7-results.csv") %>% mutate(I2 = as.double(I2), language = "Hebrew subset 7")) %>%
  bind_rows(read_csv("ud_heb-subset8-results.csv") %>% mutate(language = "Hebrew subset 8")) %>%
  bind_rows(read_csv("ud_heb-subset9-results.csv") %>% mutate(I2 = as.double(I2), language = "Hebrew subset 9")) %>%
  bind_rows(read_csv("ud_heb-subset10-results.csv") %>% mutate(I2 = as.double(I2), language = "Hebrew subset 10"))

hindi <- read_csv("ud_hin-results.csv") %>%
  mutate(language = "Hindi") %>%
  bind_rows(read_csv("ud_hin-subset1-results.csv") %>% mutate(language = "Hindi subset 1")) %>%
  bind_rows(read_csv("ud_hin-subset2-results.csv") %>% mutate(language = "Hindi subset 2")) %>%
  bind_rows(read_csv("ud_hin-subset3-results.csv") %>% mutate(language = "Hindi subset 3")) %>%
  bind_rows(read_csv("ud_hin-subset4-results.csv") %>% mutate(language = "Hindi subset 4")) %>%
  bind_rows(read_csv("ud_hin-subset5-results.csv") %>% mutate(language = "Hindi subset 5")) %>%
  bind_rows(read_csv("ud_hin-subset6-results.csv") %>% mutate(language = "Hindi subset 6")) %>%
  bind_rows(read_csv("ud_hin-subset7-results.csv") %>% mutate(language = "Hindi subset 7")) %>%
  bind_rows(read_csv("ud_hin-subset8-results.csv") %>% mutate(language = "Hindi subset 8")) %>%
  bind_rows(read_csv("ud_hin-subset9-results.csv") %>% mutate(language = "Hindi subset 9")) %>%
  bind_rows(read_csv("ud_hin-subset10-results.csv") %>% mutate(language = "Hindi subset 10"))

hungarian <- read_csv("ud_hun-results.csv") %>%
  mutate(I2 = as.double(I2), language = "Hungarian") %>%
  bind_rows(read_csv("ud_hun-subset1-results.csv") %>% mutate(I2 = as.double(I2), language = "Hungarian subset 1")) %>%
  bind_rows(read_csv("ud_hun-subset2-results.csv") %>% mutate(I2 = as.double(I2), language = "Hungarian subset 2")) %>%
  bind_rows(read_csv("ud_hun-subset3-results.csv") %>% mutate(I2 = as.double(I2), language = "Hungarian subset 3")) %>%
  bind_rows(read_csv("ud_hun-subset4-results.csv") %>% mutate(I2 = as.double(I2), language = "Hungarian subset 4")) %>%
  bind_rows(read_csv("ud_hun-subset5-results.csv") %>% mutate(I2 = as.double(I2), language = "Hungarian subset 5")) %>%
  bind_rows(read_csv("ud_hun-subset6-results.csv") %>% mutate(I2 = as.double(I2), language = "Hungarian subset 6")) %>%
  bind_rows(read_csv("ud_hun-subset7-results.csv") %>% mutate(I2 = as.double(I2), language = "Hungarian subset 7")) %>%
  bind_rows(read_csv("ud_hun-subset8-results.csv") %>% mutate(I2 = as.double(I2), language = "Hungarian subset 8")) %>%
  bind_rows(read_csv("ud_hun-subset9-results.csv") %>% mutate(I2 = as.double(I2), language = "Hungarian subset 9")) %>%
  bind_rows(read_csv("ud_hun-subset10-results.csv") %>% mutate(I2 = as.double(I2), language = "Hungarian subset 10"))


indonesian <- read_csv("ud_ind-results.csv") %>%
  mutate(language = "Indonesian") %>%
  bind_rows(read_csv("ud_ind-subset1-results.csv") %>% mutate(language = "Indonesian subset 1")) %>%
  bind_rows(read_csv("ud_ind-subset2-results.csv") %>% mutate(language = "Indonesian subset 2")) %>%
  bind_rows(read_csv("ud_ind-subset3-results.csv") %>% mutate(language = "Indonesian subset 3")) %>%
  bind_rows(read_csv("ud_ind-subset4-results.csv") %>% mutate(language = "Indonesian subset 4")) %>%
  bind_rows(read_csv("ud_ind-subset5-results.csv") %>% mutate(language = "Indonesian subset 5")) %>%
  bind_rows(read_csv("ud_ind-subset6-results.csv") %>% mutate(language = "Indonesian subset 6")) %>%
  bind_rows(read_csv("ud_ind-subset7-results.csv") %>% mutate(language = "Indonesian subset 7")) %>%
  bind_rows(read_csv("ud_ind-subset8-results.csv") %>% mutate(language = "Indonesian subset 8")) %>%
  bind_rows(read_csv("ud_ind-subset9-results.csv") %>% mutate(language = "Indonesian subset 9")) %>%
  bind_rows(read_csv("ud_ind-subset10-results.csv") %>% mutate(language = "Indonesian subset 10"))

italian <- read_csv("ud_ita-results.csv") %>%
  mutate(language = "Italian") %>%
  bind_rows(read_csv("ud_ita-subset1-results.csv") %>% mutate(language = "Italian subset 1")) %>%
  bind_rows(read_csv("ud_ita-subset2-results.csv") %>% mutate(language = "Italian subset 2")) %>%
  bind_rows(read_csv("ud_ita-subset3-results.csv") %>% mutate(language = "Italian subset 3")) %>%
  bind_rows(read_csv("ud_ita-subset4-results.csv") %>% mutate(language = "Italian subset 4")) %>%
  bind_rows(read_csv("ud_ita-subset5-results.csv") %>% mutate(language = "Italian subset 5")) %>%
  bind_rows(read_csv("ud_ita-subset6-results.csv") %>% mutate(language = "Italian subset 6")) %>%
  bind_rows(read_csv("ud_ita-subset7-results.csv") %>% mutate(language = "Italian subset 7")) %>%
  bind_rows(read_csv("ud_ita-subset8-results.csv") %>% mutate(language = "Italian subset 8")) %>%
  bind_rows(read_csv("ud_ita-subset9-results.csv") %>% mutate(language = "Italian subset 9")) %>%
  bind_rows(read_csv("ud_ita-subset10-results.csv") %>% mutate(language = "Italian subset 10"))

japanese <- read_csv("ud_ja-results.csv") %>%
  mutate(I2 = as.double(I2), language = "Japanese") %>%
  bind_rows(read_csv("ud_ja-subset1-results.csv") %>% mutate(I2 = as.double(I2), language = "Japanese subset 1")) %>%
  bind_rows(read_csv("ud_ja-subset2-results.csv") %>% mutate(I2 = as.double(I2), language = "Japanese subset 2")) %>%
  bind_rows(read_csv("ud_ja-subset3-results.csv") %>% mutate(I2 = as.double(I2), language = "Japanese subset 3")) %>%
  bind_rows(read_csv("ud_ja-subset4-results.csv") %>% mutate(I2 = as.double(I2), language = "Japanese subset 4")) %>%
  bind_rows(read_csv("ud_ja-subset5-results.csv") %>% mutate(I2 = as.double(I2), language = "Japanese subset 5")) %>%
  bind_rows(read_csv("ud_ja-subset6-results.csv") %>% mutate(I2 = as.double(I2), language = "Japanese subset 6")) %>%
  bind_rows(read_csv("ud_ja-subset7-results.csv") %>% mutate(I2 = as.double(I2), language = "Japanese subset 7")) %>%
  bind_rows(read_csv("ud_ja-subset8-results.csv") %>% mutate(I2 = as.double(I2), language = "Japanese subset 8")) %>%
  bind_rows(read_csv("ud_ja-subset9-results.csv") %>% mutate(I2 = as.double(I2), language = "Japanese subset 9")) %>%
  bind_rows(read_csv("ud_ja-subset10-results.csv") %>% mutate(I2 = as.double(I2), language = "Japanese subset 10"))

karelian <- read_csv("ud_kar-results.csv") %>%
  mutate(language = "Karelian")

kazakh <- read_csv("ud_kaz-results.csv") %>%
  mutate(language = "Kazakh")

korean <- read_csv("ud_kor-results.csv") %>%
  mutate(language = "Korean") %>%
  bind_rows(read_csv("ud_kor-subset1-results.csv") %>% mutate(language = "Korean subset 1")) %>%
  bind_rows(read_csv("ud_kor-subset2-results.csv") %>% mutate(language = "Korean subset 2")) %>%
  bind_rows(read_csv("ud_kor-subset3-results.csv") %>% mutate(language = "Korean subset 3")) %>%
  bind_rows(read_csv("ud_kor-subset4-results.csv") %>% mutate(language = "Korean subset 4")) %>%
  bind_rows(read_csv("ud_kor-subset5-results.csv") %>% mutate(language = "Korean subset 5")) %>%
  bind_rows(read_csv("ud_kor-subset6-results.csv") %>% mutate(language = "Korean subset 6")) %>%
  bind_rows(read_csv("ud_kor-subset7-results.csv") %>% mutate(language = "Korean subset 7")) %>%
  bind_rows(read_csv("ud_kor-subset8-results.csv") %>% mutate(language = "Korean subset 8")) %>%
  bind_rows(read_csv("ud_kor-subset9-results.csv") %>% mutate(language = "Korean subset 9")) %>%
  bind_rows(read_csv("ud_kor-subset10-results.csv") %>% mutate(language = "Korean subset 10"))

komi_permyak <- read_csv("ud_kpe-results.csv") %>%
  mutate(language = "Komi Permyak")

kurmanji <- read_csv("ud_kur-results.csv") %>%
  mutate(language = "Kurmanji")

komi_zyrian <- read_csv("ud_kzy-results.csv") %>%
  mutate(language = "Komi Zyrian")

latin <- read_csv("ud_lat-results.csv") %>%
  mutate(I2 = as.double(I2), language = "Latin") %>%
  bind_rows(read_csv("ud_lat-subset1-results.csv") %>% mutate(I2 = as.double(I2), language = "Latin subset 1")) %>%
  bind_rows(read_csv("ud_lat-subset2-results.csv") %>% mutate(I2 = as.double(I2), language = "Latin subset 2")) %>%
  bind_rows(read_csv("ud_lat-subset3-results.csv") %>% mutate(I2 = as.double(I2), language = "Latin subset 3")) %>%
  bind_rows(read_csv("ud_lat-subset4-results.csv") %>% mutate(I2 = as.double(I2), language = "Latin subset 4")) %>%
  bind_rows(read_csv("ud_lat-subset5-results.csv") %>% mutate(I2 = as.double(I2), language = "Latin subset 5")) %>%
  bind_rows(read_csv("ud_lat-subset6-results.csv") %>% mutate(I2 = as.double(I2), language = "Latin subset 6")) %>%
  bind_rows(read_csv("ud_lat-subset7-results.csv") %>% mutate(I2 = as.double(I2), language = "Latin subset 7")) %>%
  bind_rows(read_csv("ud_lat-subset8-results.csv") %>% mutate(I2 = as.double(I2), language = "Latin subset 8")) %>%
  bind_rows(read_csv("ud_lat-subset9-results.csv") %>% mutate(I2 = as.double(I2), language = "Latin subset 9")) %>%
  bind_rows(read_csv("ud_lat-subset10-results.csv") %>% mutate(I2 = as.double(I2), language = "Latin subset 10"))

lithuanian <- read_csv("ud_lit-results.csv") %>%
  mutate(language = "Lithuanian") %>%
  bind_rows(read_csv("ud_lit-subset1-results.csv") %>% mutate(language = "Lithuanian subset 1")) %>%
  bind_rows(read_csv("ud_lit-subset2-results.csv") %>% mutate(language = "Lithuanian subset 2")) %>%
  bind_rows(read_csv("ud_lit-subset3-results.csv") %>% mutate(language = "Lithuanian subset 3")) %>%
  bind_rows(read_csv("ud_lit-subset4-results.csv") %>% mutate(language = "Lithuanian subset 4")) %>%
  bind_rows(read_csv("ud_lit-subset5-results.csv") %>% mutate(language = "Lithuanian subset 5")) %>%
  bind_rows(read_csv("ud_lit-subset6-results.csv") %>% mutate(language = "Lithuanian subset 6")) %>%
  bind_rows(read_csv("ud_lit-subset7-results.csv") %>% mutate(language = "Lithuanian subset 7")) %>%
  bind_rows(read_csv("ud_lit-subset8-results.csv") %>% mutate(language = "Lithuanian subset 8")) %>%
  bind_rows(read_csv("ud_lit-subset9-results.csv") %>% mutate(language = "Lithuanian subset 9")) %>%
  bind_rows(read_csv("ud_lit-subset10-results.csv") %>% mutate(language = "Lithuanian subset 10"))

livvi <- read_csv("ud_liv-results.csv") %>%
  mutate(language = "Livvi")

latvian <- read_csv("ud_ltv-results.csv") %>%
  mutate(I2 = as.double(I2), language = "Latvian") %>%
  bind_rows(read_csv("ud_ltv-subset1-results.csv") %>% mutate(I2 = as.double(I2), language = "Latvian subset 1")) %>%
  bind_rows(read_csv("ud_ltv-subset2-results.csv") %>% mutate(I2 = as.double(I2), language = "Latvian subset 2")) %>%
  bind_rows(read_csv("ud_ltv-subset3-results.csv") %>% mutate(I2 = as.double(I2), language = "Latvian subset 3")) %>%
  bind_rows(read_csv("ud_ltv-subset4-results.csv") %>% mutate(I2 = as.double(I2), language = "Latvian subset 4")) %>%
  bind_rows(read_csv("ud_ltv-subset5-results.csv") %>% mutate(I2 = as.double(I2), language = "Latvian subset 5")) %>%
  bind_rows(read_csv("ud_ltv-subset6-results.csv") %>% mutate(I2 = as.double(I2), language = "Latvian subset 6")) %>%
  bind_rows(read_csv("ud_ltv-subset7-results.csv") %>% mutate(I2 = as.double(I2), language = "Latvian subset 7")) %>%
  bind_rows(read_csv("ud_ltv-subset8-results.csv") %>% mutate(I2 = as.double(I2), language = "Latvian subset 8")) %>%
  bind_rows(read_csv("ud_ltv-subset9-results.csv") %>% mutate(I2 = as.double(I2), language = "Latvian subset 9")) %>%
  bind_rows(read_csv("ud_ltv-subset10-results.csv") %>% mutate(I2 = as.double(I2), language = "Latvian subset 10"))

maltese <- read_csv("ud_mal-results.csv") %>%
  mutate(language = "Maltese")

marathi <- read_csv("ud_mar-results.csv") %>%
  mutate(language = "Marathi")

mbya_guarani <- read_csv("ud_mgu-results.csv") %>%
  mutate(language = "Mbya Guarani")

moksha <- read_csv("ud_mok-results.csv") %>%
  mutate(language = "Moksha")

dutch <- read_csv("ud_ned-results.csv") %>%
  mutate(language = "Dutch") %>%
  bind_rows(read_csv("ud_ned-subset1-results.csv") %>% mutate(language = "Dutch subset 1")) %>%
  bind_rows(read_csv("ud_ned-subset2-results.csv") %>% mutate(language = "Dutch subset 2")) %>%
  bind_rows(read_csv("ud_ned-subset3-results.csv") %>% mutate(language = "Dutch subset 3")) %>%
  bind_rows(read_csv("ud_ned-subset4-results.csv") %>% mutate(language = "Dutch subset 4")) %>%
  bind_rows(read_csv("ud_ned-subset5-results.csv") %>% mutate(language = "Dutch subset 5")) %>%
  bind_rows(read_csv("ud_ned-subset6-results.csv") %>% mutate(language = "Dutch subset 6")) %>%
  bind_rows(read_csv("ud_ned-subset7-results.csv") %>% mutate(language = "Dutch subset 7")) %>%
  bind_rows(read_csv("ud_ned-subset8-results.csv") %>% mutate(language = "Dutch subset 8")) %>%
  bind_rows(read_csv("ud_ned-subset9-results.csv") %>% mutate(language = "Dutch subset 9")) %>%
  bind_rows(read_csv("ud_ned-subset10-results.csv") %>% mutate(language = "Dutch subset 10"))

norwegian <- read_csv("ud_nor-results.csv") %>%
  mutate(language = "Norwegian") %>%
  bind_rows(read_csv("ud_nor-subset1-results.csv") %>% mutate(language = "Norwegian subset 1")) %>%
  bind_rows(read_csv("ud_nor-subset2-results.csv") %>% mutate(language = "Norwegian subset 2")) %>%
  bind_rows(read_csv("ud_nor-subset3-results.csv") %>% mutate(language = "Norwegian subset 3")) %>%
  bind_rows(read_csv("ud_nor-subset4-results.csv") %>% mutate(language = "Norwegian subset 4")) %>%
  bind_rows(read_csv("ud_nor-subset5-results.csv") %>% mutate(language = "Norwegian subset 5")) %>%
  bind_rows(read_csv("ud_nor-subset6-results.csv") %>% mutate(language = "Norwegian subset 6")) %>%
  bind_rows(read_csv("ud_nor-subset7-results.csv") %>% mutate(language = "Norwegian subset 7")) %>%
  bind_rows(read_csv("ud_nor-subset8-results.csv") %>% mutate(language = "Norwegian subset 8")) %>%
  bind_rows(read_csv("ud_nor-subset9-results.csv") %>% mutate(language = "Norwegian subset 9")) %>%
  bind_rows(read_csv("ud_nor-subset10-results.csv") %>% mutate(language = "Norwegian subset 10"))

nigerian_pidgin <- read_csv("ud_npe-results.csv") %>%
  mutate(language = "Nigerian Pidgin")

north_sami <- read_csv("ud_nsa-results.csv") %>%
  mutate(I2 = as.double(I2), language = "North Sami") %>%
  bind_rows(read_csv("ud_nsa-subset1-results.csv") %>% mutate(I2 = as.double(I2), language = "North Sami subset 1")) %>%
  bind_rows(read_csv("ud_nsa-subset2-results.csv") %>% mutate(I2 = as.double(I2), language = "North Sami subset 2")) %>%
  bind_rows(read_csv("ud_nsa-subset3-results.csv") %>% mutate(I2 = as.double(I2), language = "North Sami subset 3")) %>%
  bind_rows(read_csv("ud_nsa-subset4-results.csv") %>% mutate(I2 = as.double(I2), language = "North Sami subset 4")) %>%
  bind_rows(read_csv("ud_nsa-subset5-results.csv") %>% mutate(I2 = as.double(I2), language = "North Sami subset 5")) %>%
  bind_rows(read_csv("ud_nsa-subset6-results.csv") %>% mutate(I2 = as.double(I2), language = "North Sami subset 6")) %>%
  bind_rows(read_csv("ud_nsa-subset7-results.csv") %>% mutate(I2 = as.double(I2), language = "North Sami subset 7")) %>%
  bind_rows(read_csv("ud_nsa-subset8-results.csv") %>% mutate(I2 = as.double(I2), language = "North Sami subset 8")) %>%
  bind_rows(read_csv("ud_nsa-subset9-results.csv") %>% mutate(I2 = as.double(I2), language = "North Sami subset 9")) %>%
  bind_rows(read_csv("ud_nsa-subset10-results.csv") %>% mutate(I2 = as.double(I2), language = "North Sami subset 10"))

old_church_slavonic <- read_csv("ud_ocs-results.csv") %>%
  mutate(I2 = as.double(I2), language = "Old Church Slavonic") %>%
  bind_rows(read_csv("ud_ocs-subset1-results.csv") %>% mutate(I2 = as.double(I2), language = "Old Church Slavonic subset 1")) %>%
  bind_rows(read_csv("ud_ocs-subset2-results.csv") %>% mutate(I2 = as.double(I2), language = "Old Church Slavonic subset 2")) %>%
  bind_rows(read_csv("ud_ocs-subset3-results.csv") %>% mutate(I2 = as.double(I2), language = "Old Church Slavonic subset 3")) %>%
  bind_rows(read_csv("ud_ocs-subset4-results.csv") %>% mutate(I2 = as.double(I2), language = "Old Church Slavonic subset 4")) %>%
  bind_rows(read_csv("ud_ocs-subset5-results.csv") %>% mutate(I2 = as.double(I2), language = "Old Church Slavonic subset 5")) %>%
  bind_rows(read_csv("ud_ocs-subset6-results.csv") %>% mutate(I2 = as.double(I2), language = "Old Church Slavonic subset 6")) %>%
  bind_rows(read_csv("ud_ocs-subset7-results.csv") %>% mutate(I2 = as.double(I2), language = "Old Church Slavonic subset 7")) %>%
  bind_rows(read_csv("ud_ocs-subset8-results.csv") %>% mutate(I2 = as.double(I2), language = "Old Church Slavonic subset 8")) %>%
  bind_rows(read_csv("ud_ocs-subset9-results.csv") %>% mutate(I2 = as.double(I2), language = "Old Church Slavonic subset 9")) %>%
  bind_rows(read_csv("ud_ocs-subset10-results.csv") %>% mutate(I2 = as.double(I2), language = "Old Church Slavonic subset 10"))

old_french <- read_csv("ud_ofr-results.csv") %>%
  mutate(language = "Old French") %>%
  bind_rows(read_csv("ud_ofr-subset1-results.csv") %>% mutate(language = "Old French subset 1")) %>%
  bind_rows(read_csv("ud_ofr-subset2-results.csv") %>% mutate(language = "Old French subset 2")) %>%
  bind_rows(read_csv("ud_ofr-subset3-results.csv") %>% mutate(language = "Old French subset 3")) %>%
  bind_rows(read_csv("ud_ofr-subset4-results.csv") %>% mutate(language = "Old French subset 4")) %>%
  bind_rows(read_csv("ud_ofr-subset5-results.csv") %>% mutate(language = "Old French subset 5")) %>%
  bind_rows(read_csv("ud_ofr-subset6-results.csv") %>% mutate(language = "Old French subset 6")) %>%
  bind_rows(read_csv("ud_ofr-subset7-results.csv") %>% mutate(language = "Old French subset 7")) %>%
  bind_rows(read_csv("ud_ofr-subset8-results.csv") %>% mutate(language = "Old French subset 8")) %>%
  bind_rows(read_csv("ud_ofr-subset9-results.csv") %>% mutate(language = "Old French subset 9")) %>%
  bind_rows(read_csv("ud_ofr-subset10-results.csv") %>% mutate(language = "Old French subset 10"))

old_russian <- read_csv("ud_oru-results.csv") %>%
  mutate(language = "Old Russian") %>%
  bind_rows(read_csv("ud_oru-subset1-results.csv") %>% mutate(language = "Old Russian subset 1")) %>%
  bind_rows(read_csv("ud_oru-subset2-results.csv") %>% mutate(language = "Old Russian subset 2")) %>%
  bind_rows(read_csv("ud_oru-subset3-results.csv") %>% mutate(language = "Old Russian subset 3")) %>%
  bind_rows(read_csv("ud_oru-subset4-results.csv") %>% mutate(language = "Old Russian subset 4")) %>%
  bind_rows(read_csv("ud_oru-subset5-results.csv") %>% mutate(language = "Old Russian subset 5")) %>%
  bind_rows(read_csv("ud_oru-subset6-results.csv") %>% mutate(language = "Old Russian subset 6")) %>%
  bind_rows(read_csv("ud_oru-subset7-results.csv") %>% mutate(language = "Old Russian subset 7")) %>%
  bind_rows(read_csv("ud_oru-subset8-results.csv") %>% mutate(language = "Old Russian subset 8")) %>%
  bind_rows(read_csv("ud_oru-subset9-results.csv") %>% mutate(language = "Old Russian subset 9")) %>%
  bind_rows(read_csv("ud_oru-subset10-results.csv") %>% mutate(language = "Old Russian subset 10"))

polish <- read_csv("ud_pol-results.csv") %>%
  mutate(language = "Polish") %>%
  bind_rows(read_csv("ud_pol-subset1-results.csv") %>% mutate(language = "Polish subset 1")) %>%
  bind_rows(read_csv("ud_pol-subset2-results.csv") %>% mutate(language = "Polish subset 2")) %>%
  bind_rows(read_csv("ud_pol-subset3-results.csv") %>% mutate(language = "Polish subset 3")) %>%
  bind_rows(read_csv("ud_pol-subset4-results.csv") %>% mutate(language = "Polish subset 4")) %>%
  bind_rows(read_csv("ud_pol-subset5-results.csv") %>% mutate(language = "Polish subset 5")) %>%
  bind_rows(read_csv("ud_pol-subset6-results.csv") %>% mutate(language = "Polish subset 6")) %>%
  bind_rows(read_csv("ud_pol-subset7-results.csv") %>% mutate(language = "Polish subset 7")) %>%
  bind_rows(read_csv("ud_pol-subset8-results.csv") %>% mutate(language = "Polish subset 8")) %>%
  bind_rows(read_csv("ud_pol-subset9-results.csv") %>% mutate(language = "Polish subset 9")) %>%
  bind_rows(read_csv("ud_pol-subset10-results.csv") %>% mutate(language = "Polish subset 10"))

portuguese <- read_csv("ud_por-results.csv") %>%
  mutate(I2 = as.double(I2), language = "Portuguese") %>%
  bind_rows(read_csv("ud_por-subset1-results.csv") %>% mutate(I2 = as.double(I2), language = "Portuguese subset 1")) %>%
  bind_rows(read_csv("ud_por-subset2-results.csv") %>% mutate(I2 = as.double(I2), language = "Portuguese subset 2")) %>%
  bind_rows(read_csv("ud_por-subset3-results.csv") %>% mutate(I2 = as.double(I2), language = "Portuguese subset 3")) %>%
  bind_rows(read_csv("ud_por-subset4-results.csv") %>% mutate(I2 = as.double(I2), language = "Portuguese subset 4")) %>%
  bind_rows(read_csv("ud_por-subset5-results.csv") %>% mutate(I2 = as.double(I2), language = "Portuguese subset 5")) %>%
  bind_rows(read_csv("ud_por-subset6-results.csv") %>% mutate(I2 = as.double(I2), language = "Portuguese subset 6")) %>%
  bind_rows(read_csv("ud_por-subset7-results.csv") %>% mutate(I2 = as.double(I2), language = "Portuguese subset 7")) %>%
  bind_rows(read_csv("ud_por-subset8-results.csv") %>% mutate(I2 = as.double(I2), language = "Portuguese subset 8")) %>%
  bind_rows(read_csv("ud_por-subset9-results.csv") %>% mutate(I2 = as.double(I2), language = "Portuguese subset 9")) %>%
  bind_rows(read_csv("ud_por-subset10-results.csv") %>% mutate(I2 = as.double(I2), language = "Portuguese subset 10"))

romanian <- read_csv("ud_rom-results.csv") %>%
  mutate(language = "Romanian") %>%
  bind_rows(read_csv("ud_rom-subset1-results.csv") %>% mutate(language = "Romanian subset 1")) %>%
  bind_rows(read_csv("ud_rom-subset2-results.csv") %>% mutate(language = "Romanian subset 2")) %>%
  bind_rows(read_csv("ud_rom-subset3-results.csv") %>% mutate(language = "Romanian subset 3")) %>%
  bind_rows(read_csv("ud_rom-subset4-results.csv") %>% mutate(language = "Romanian subset 4")) %>%
  bind_rows(read_csv("ud_rom-subset5-results.csv") %>% mutate(language = "Romanian subset 5")) %>%
  bind_rows(read_csv("ud_rom-subset6-results.csv") %>% mutate(language = "Romanian subset 6")) %>%
  bind_rows(read_csv("ud_rom-subset7-results.csv") %>% mutate(language = "Romanian subset 7")) %>%
  bind_rows(read_csv("ud_rom-subset8-results.csv") %>% mutate(language = "Romanian subset 8")) %>%
  bind_rows(read_csv("ud_rom-subset9-results.csv") %>% mutate(language = "Romanian subset 9")) %>%
  bind_rows(read_csv("ud_rom-subset10-results.csv") %>% mutate(language = "Romanian subset 10"))

russian <- read_csv("ud_rus-results.csv") %>%
  mutate(language = "Russian") %>%
  bind_rows(read_csv("ud_rus-subset1-results.csv") %>% mutate(language = "Russian subset 1")) %>%
  bind_rows(read_csv("ud_rus-subset2-results.csv") %>% mutate(language = "Russian subset 2")) %>%
  bind_rows(read_csv("ud_rus-subset3-results.csv") %>% mutate(language = "Russian subset 3")) %>%
  bind_rows(read_csv("ud_rus-subset4-results.csv") %>% mutate(language = "Russian subset 4")) %>%
  bind_rows(read_csv("ud_rus-subset5-results.csv") %>% mutate(language = "Russian subset 5")) %>%
  bind_rows(read_csv("ud_rus-subset6-results.csv") %>% mutate(language = "Russian subset 6")) %>%
  bind_rows(read_csv("ud_rus-subset7-results.csv") %>% mutate(language = "Russian subset 7")) %>%
  bind_rows(read_csv("ud_rus-subset8-results.csv") %>% mutate(language = "Russian subset 8")) %>%
  bind_rows(read_csv("ud_rus-subset9-results.csv") %>% mutate(language = "Russian subset 9")) %>%
  bind_rows(read_csv("ud_rus-subset10-results.csv") %>% mutate(language = "Russian subset 10"))

sanskrit <- read_csv("ud_san-results.csv") %>%
  mutate(language = "Sanskrit")

serbian <- read_csv("ud_ser-results.csv") %>%
  mutate(I2 = as.double(I2), language = "Serbian") %>%
  bind_rows(read_csv("ud_ser-subset1-results.csv") %>% mutate(I2 = as.double(I2), language = "Serbian subset 1")) %>%
  bind_rows(read_csv("ud_ser-subset2-results.csv") %>% mutate(I2 = as.double(I2), language = "Serbian subset 2")) %>%
  bind_rows(read_csv("ud_ser-subset3-results.csv") %>% mutate(I2 = as.double(I2), language = "Serbian subset 3")) %>%
  bind_rows(read_csv("ud_ser-subset4-results.csv") %>% mutate(I2 = as.double(I2), language = "Serbian subset 4")) %>%
  bind_rows(read_csv("ud_ser-subset5-results.csv") %>% mutate(I2 = as.double(I2), language = "Serbian subset 5")) %>%
  bind_rows(read_csv("ud_ser-subset6-results.csv") %>% mutate(I2 = as.double(I2), language = "Serbian subset 6")) %>%
  bind_rows(read_csv("ud_ser-subset7-results.csv") %>% mutate(I2 = as.double(I2), language = "Serbian subset 7")) %>%
  bind_rows(read_csv("ud_ser-subset8-results.csv") %>% mutate(I2 = as.double(I2), language = "Serbian subset 8")) %>%
  bind_rows(read_csv("ud_ser-subset9-results.csv") %>% mutate(I2 = as.double(I2), language = "Serbian subset 9")) %>%
  bind_rows(read_csv("ud_ser-subset10-results.csv") %>% mutate(I2 = as.double(I2), language = "Serbian subset 10"))

slovakian <- read_csv("ud_slvk-results.csv") %>%
  mutate(language = "Slovak") %>%
  bind_rows(read_csv("ud_slvk-subset1-results.csv") %>% mutate(language = "Slovak subset 1")) %>%
  bind_rows(read_csv("ud_slvk-subset2-results.csv") %>% mutate(language = "Slovak subset 2")) %>%
  bind_rows(read_csv("ud_slvk-subset3-results.csv") %>% mutate(language = "Slovak subset 3")) %>%
  bind_rows(read_csv("ud_slvk-subset4-results.csv") %>% mutate(language = "Slovak subset 4")) %>%
  bind_rows(read_csv("ud_slvk-subset5-results.csv") %>% mutate(language = "Slovak subset 5")) %>%
  bind_rows(read_csv("ud_slvk-subset6-results.csv") %>% mutate(language = "Slovak subset 6")) %>%
  bind_rows(read_csv("ud_slvk-subset7-results.csv") %>% mutate(language = "Slovak subset 7")) %>%
  bind_rows(read_csv("ud_slvk-subset8-results.csv") %>% mutate(language = "Slovak subset 8")) %>%
  bind_rows(read_csv("ud_slvk-subset9-results.csv") %>% mutate(language = "Slovak subset 9")) %>%
  bind_rows(read_csv("ud_slvk-subset10-results.csv") %>% mutate(language = "Slovak subset 10"))

slovenian <- read_csv("ud_slvk-results.csv") %>%
  mutate(language = "Slovenian") %>%
  bind_rows(read_csv("ud_slvn-subset1-results.csv") %>% mutate(language = "Slovenian subset 1")) %>%
  bind_rows(read_csv("ud_slvn-subset2-results.csv") %>% mutate(language = "Slovenian subset 2")) %>%
  bind_rows(read_csv("ud_slvn-subset3-results.csv") %>% mutate(language = "Slovenian subset 3")) %>%
  bind_rows(read_csv("ud_slvn-subset4-results.csv") %>% mutate(language = "Slovenian subset 4")) %>%
  bind_rows(read_csv("ud_slvn-subset5-results.csv") %>% mutate(language = "Slovenian subset 5")) %>%
  bind_rows(read_csv("ud_slvn-subset6-results.csv") %>% mutate(language = "Slovenian subset 6")) %>%
  bind_rows(read_csv("ud_slvn-subset7-results.csv") %>% mutate(language = "Slovenian subset 7")) %>%
  bind_rows(read_csv("ud_slvn-subset8-results.csv") %>% mutate(language = "Slovenian subset 8")) %>%
  bind_rows(read_csv("ud_slvn-subset9-results.csv") %>% mutate(language = "Slovenian subset 9")) %>%
  bind_rows(read_csv("ud_slvn-subset10-results.csv") %>% mutate(language = "Slovenian subset 10"))

spanish <- read_csv("ud_slvn-results.csv") %>%
  mutate(language = "Spanish") %>%
  bind_rows(read_csv("ud_spa-subset1-results.csv") %>% mutate(language = "Spanish subset 1")) %>%
  bind_rows(read_csv("ud_spa-subset2-results.csv") %>% mutate(language = "Spanish subset 2")) %>%
  bind_rows(read_csv("ud_spa-subset3-results.csv") %>% mutate(language = "Spanish subset 3")) %>%
  bind_rows(read_csv("ud_spa-subset4-results.csv") %>% mutate(language = "Spanish subset 4")) %>%
  bind_rows(read_csv("ud_spa-subset5-results.csv") %>% mutate(language = "Spanish subset 5")) %>%
  bind_rows(read_csv("ud_spa-subset6-results.csv") %>% mutate(language = "Spanish subset 6")) %>%
  bind_rows(read_csv("ud_spa-subset7-results.csv") %>% mutate(language = "Spanish subset 7")) %>%
  bind_rows(read_csv("ud_spa-subset8-results.csv") %>% mutate(language = "Spanish subset 8")) %>%
  bind_rows(read_csv("ud_spa-subset9-results.csv") %>% mutate(language = "Spanish subset 9")) %>%
  bind_rows(read_csv("ud_spa-subset10-results.csv") %>% mutate(language = "Spanish subset 10"))

skolt_sami <- read_csv("ud_ssa-results.csv") %>%
  mutate(language = "Skolt Sami")

swedish_sign <- read_csv("ud_ssl-results.csv") %>%
  mutate(language = "Swedish Sign Language")

assyrian <- read_csv("ud_sur-results.csv") %>%
  mutate(language = "Assyrian")

swedish <- read_csv("ud_swe-results.csv") %>%
  mutate(language = "Swedish") %>%
  bind_rows(read_csv("ud_swe-subset1-results.csv") %>% mutate(language = "Swedish subset 1")) %>%
  bind_rows(read_csv("ud_swe-subset2-results.csv") %>% mutate(language = "Swedish subset 2")) %>%
  bind_rows(read_csv("ud_swe-subset3-results.csv") %>% mutate(language = "Swedish subset 3")) %>%
  bind_rows(read_csv("ud_swe-subset4-results.csv") %>% mutate(language = "Swedish subset 4")) %>%
  bind_rows(read_csv("ud_swe-subset5-results.csv") %>% mutate(language = "Swedish subset 5")) %>%
  bind_rows(read_csv("ud_swe-subset6-results.csv") %>% mutate(language = "Swedish subset 6")) %>%
  bind_rows(read_csv("ud_swe-subset7-results.csv") %>% mutate(language = "Swedish subset 7")) %>%
  bind_rows(read_csv("ud_swe-subset8-results.csv") %>% mutate(language = "Swedish subset 8")) %>%
  bind_rows(read_csv("ud_swe-subset9-results.csv") %>% mutate(language = "Swedish subset 9")) %>%
  bind_rows(read_csv("ud_swe-subset10-results.csv") %>% mutate(language = "Swedish subset 10"))

swiss_german <- read_csv("ud_swg-results.csv") %>%
  mutate(language = "Swiss German")

tagalog <- read_csv("ud_tag-results.csv") %>%
  mutate(language = "Tagalog")

tamil <- read_csv("ud_tam-results.csv") %>%
  mutate(language = "Tamil")

telugu <- read_csv("ud_tel-results.csv") %>%
  mutate(language = "Telugu") %>%
  bind_rows(read_csv("ud_tel-subset1-results.csv") %>% mutate(language = "Telugu subset 1")) %>%
  bind_rows(read_csv("ud_tel-subset2-results.csv") %>% mutate(language = "Telugu subset 2")) %>%
  bind_rows(read_csv("ud_tel-subset3-results.csv") %>% mutate(language = "Telugu subset 3")) %>%
  bind_rows(read_csv("ud_tel-subset4-results.csv") %>% mutate(language = "Telugu subset 4")) %>%
  bind_rows(read_csv("ud_tel-subset5-results.csv") %>% mutate(language = "Telugu subset 5")) %>%
  bind_rows(read_csv("ud_tel-subset6-results.csv") %>% mutate(language = "Telugu subset 6")) %>%
  bind_rows(read_csv("ud_tel-subset7-results.csv") %>% mutate(language = "Telugu subset 7")) %>%
  bind_rows(read_csv("ud_tel-subset8-results.csv") %>% mutate(language = "Telugu subset 8")) %>%
  bind_rows(read_csv("ud_tel-subset9-results.csv") %>% mutate(language = "Telugu subset 9")) %>%
  bind_rows(read_csv("ud_tel-subset10-results.csv") %>% mutate(language = "Telugu subset 10"))

thai <- read_csv("ud_tha-results.csv") %>%
  mutate(language = "Thai")

turkish <- read_csv("ud_tur-results.csv") %>%
  mutate(language = "Turkish") %>%
  bind_rows(read_csv("ud_tur-subset1-results.csv") %>% mutate(language = "Turkish subset 1")) %>%
  bind_rows(read_csv("ud_tur-subset2-results.csv") %>% mutate(language = "Turkish subset 2")) %>%
  bind_rows(read_csv("ud_tur-subset3-results.csv") %>% mutate(language = "Turkish subset 3")) %>%
  bind_rows(read_csv("ud_tur-subset4-results.csv") %>% mutate(language = "Turkish subset 4")) %>%
  bind_rows(read_csv("ud_tur-subset5-results.csv") %>% mutate(language = "Turkish subset 5")) %>%
  bind_rows(read_csv("ud_tur-subset6-results.csv") %>% mutate(language = "Turkish subset 6")) %>%
  bind_rows(read_csv("ud_tur-subset7-results.csv") %>% mutate(language = "Turkish subset 7")) %>%
  bind_rows(read_csv("ud_tur-subset8-results.csv") %>% mutate(language = "Turkish subset 8")) %>%
  bind_rows(read_csv("ud_tur-subset9-results.csv") %>% mutate(language = "Turkish subset 9")) %>%
  bind_rows(read_csv("ud_tur-subset10-results.csv") %>% mutate(language = "Turkish subset 10"))

ukrainian <- read_csv("ud_ukr-results.csv") %>%
  mutate(I2 = as.double(I2), language = "Ukrainian") %>%
  bind_rows(read_csv("ud_ukr-subset1-results.csv") %>% mutate(I2 = as.double(I2), language = "Ukrainian subset 1")) %>%
  bind_rows(read_csv("ud_ukr-subset2-results.csv") %>% mutate(I2 = as.double(I2), language = "Ukrainian subset 2")) %>%
  bind_rows(read_csv("ud_ukr-subset3-results.csv") %>% mutate(I2 = as.double(I2), language = "Ukrainian subset 3")) %>%
  bind_rows(read_csv("ud_ukr-subset4-results.csv") %>% mutate(I2 = as.double(I2), language = "Ukrainian subset 4")) %>%
  bind_rows(read_csv("ud_ukr-subset5-results.csv") %>% mutate(I2 = as.double(I2), language = "Ukrainian subset 5")) %>%
  bind_rows(read_csv("ud_ukr-subset6-results.csv") %>% mutate(I2 = as.double(I2), language = "Ukrainian subset 6")) %>%
  bind_rows(read_csv("ud_ukr-subset7-results.csv") %>% mutate(I2 = as.double(I2), language = "Ukrainian subset 7")) %>%
  bind_rows(read_csv("ud_ukr-subset8-results.csv") %>% mutate(I2 = as.double(I2), language = "Ukrainian subset 8")) %>%
  bind_rows(read_csv("ud_ukr-subset9-results.csv") %>% mutate(I2 = as.double(I2), language = "Ukrainian subset 9")) %>%
  bind_rows(read_csv("ud_ukr-subset10-results.csv") %>% mutate(I2 = as.double(I2), language = "Ukrainian subset 10"))

urdu <- read_csv("ud_urd-results.csv") %>%
  mutate(language = "Urdu") %>%
  bind_rows(read_csv("ud_urd-subset1-results.csv") %>% mutate(language = "Urdu subset 1")) %>%
  bind_rows(read_csv("ud_urd-subset2-results.csv") %>% mutate(language = "Urdu subset 2")) %>%
  bind_rows(read_csv("ud_urd-subset3-results.csv") %>% mutate(language = "Urdu subset 3")) %>%
  bind_rows(read_csv("ud_urd-subset4-results.csv") %>% mutate(language = "Urdu subset 4")) %>%
  bind_rows(read_csv("ud_urd-subset5-results.csv") %>% mutate(language = "Urdu subset 5")) %>%
  bind_rows(read_csv("ud_urd-subset6-results.csv") %>% mutate(language = "Urdu subset 6")) %>%
  bind_rows(read_csv("ud_urd-subset7-results.csv") %>% mutate(language = "Urdu subset 7")) %>%
  bind_rows(read_csv("ud_urd-subset8-results.csv") %>% mutate(language = "Urdu subset 8")) %>%
  bind_rows(read_csv("ud_urd-subset9-results.csv") %>% mutate(language = "Urdu subset 9")) %>%
  bind_rows(read_csv("ud_urd-subset10-results.csv") %>% mutate(language = "Urdu subset 10"))

upper_sorbian <- read_csv("ud_uso-results.csv") %>%
  mutate(language = "Upper Sorbian")

uyghur <- read_csv("ud_uyg-results.csv") %>%
  mutate(I2 = as.double(I2), language = "Uyghur") %>%
  bind_rows(read_csv("ud_uyg-subset1-results.csv") %>% mutate(I2 = as.double(I2), language = "Uyghur subset 1")) %>%
  bind_rows(read_csv("ud_uyg-subset2-results.csv") %>% mutate(I2 = as.double(I2), language = "Uyghur subset 2")) %>%
  bind_rows(read_csv("ud_uyg-subset3-results.csv") %>% mutate(I2 = as.double(I2), language = "Uyghur subset 3")) %>%
  bind_rows(read_csv("ud_uyg-subset4-results.csv") %>% mutate(I2 = as.double(I2), language = "Uyghur subset 4")) %>%
  bind_rows(read_csv("ud_uyg-subset5-results.csv") %>% mutate(I2 = as.double(I2), language = "Uyghur subset 5")) %>%
  bind_rows(read_csv("ud_uyg-subset6-results.csv") %>% mutate(I2 = as.double(I2), language = "Uyghur subset 6")) %>%
  bind_rows(read_csv("ud_uyg-subset7-results.csv") %>% mutate(I2 = as.double(I2), language = "Uyghur subset 7")) %>%
  bind_rows(read_csv("ud_uyg-subset8-results.csv") %>% mutate(I2 = as.double(I2), language = "Uyghur subset 8")) %>%
  bind_rows(read_csv("ud_uyg-subset9-results.csv") %>% mutate(I2 = as.double(I2), language = "Uyghur subset 9")) %>%
  bind_rows(read_csv("ud_uyg-subset10-results.csv") %>% mutate(I2 = as.double(I2), language = "Uyghur subset 10"))

vietnamese <- read_csv("ud_vie-results.csv") %>%
  mutate(I2 = as.double(I2), language = "Vietnamese") %>%
  bind_rows(read_csv("ud_vie-subset1-results.csv") %>% mutate(I2 = as.double(I2), language = "Vietnamese subset 1")) %>%
  bind_rows(read_csv("ud_vie-subset2-results.csv") %>% mutate(I2 = as.double(I2), language = "Vietnamese subset 2")) %>%
  bind_rows(read_csv("ud_vie-subset3-results.csv") %>% mutate(I2 = as.double(I2), language = "Vietnamese subset 3")) %>%
  bind_rows(read_csv("ud_vie-subset4-results.csv") %>% mutate(I2 = as.double(I2), language = "Vietnamese subset 4")) %>%
  bind_rows(read_csv("ud_vie-subset5-results.csv") %>% mutate(I2 = as.double(I2), language = "Vietnamese subset 5")) %>%
  bind_rows(read_csv("ud_vie-subset6-results.csv") %>% mutate(I2 = as.double(I2), language = "Vietnamese subset 6")) %>%
  bind_rows(read_csv("ud_vie-subset7-results.csv") %>% mutate(I2 = as.double(I2), language = "Vietnamese subset 7")) %>%
  bind_rows(read_csv("ud_vie-subset8-results.csv") %>% mutate(I2 = as.double(I2), language = "Vietnamese subset 8")) %>%
  bind_rows(read_csv("ud_vie-subset9-results.csv") %>% mutate(I2 = as.double(I2), language = "Vietnamese subset 9")) %>%
  bind_rows(read_csv("ud_vie-subset10-results.csv") %>% mutate(I2 = as.double(I2), language = "Vietnamese subset 10"))

warlpiri <- read_csv("ud_war-results.csv") %>%
  mutate(language = "Warlpiri")

wolof <- read_csv("ud_wol-results.csv") %>%
  mutate(language = "Wolof") %>%
  bind_rows(read_csv("ud_wol-subset1-results.csv") %>% mutate(language = "Wolof subset 1")) %>%
  bind_rows(read_csv("ud_wol-subset2-results.csv") %>% mutate(language = "Wolof subset 2")) %>%
  bind_rows(read_csv("ud_wol-subset3-results.csv") %>% mutate(language = "Wolof subset 3")) %>%
  bind_rows(read_csv("ud_wol-subset4-results.csv") %>% mutate(language = "Wolof subset 4")) %>%
  bind_rows(read_csv("ud_wol-subset5-results.csv") %>% mutate(language = "Wolof subset 5")) %>%
  bind_rows(read_csv("ud_wol-subset6-results.csv") %>% mutate(language = "Wolof subset 6")) %>%
  bind_rows(read_csv("ud_wol-subset7-results.csv") %>% mutate(language = "Wolof subset 7")) %>%
  bind_rows(read_csv("ud_wol-subset8-results.csv") %>% mutate(language = "Wolof subset 8")) %>%
  bind_rows(read_csv("ud_wol-subset9-results.csv") %>% mutate(language = "Wolof subset 9")) %>%
  bind_rows(read_csv("ud_wol-subset10-results.csv") %>% mutate(language = "Wolof subset 10"))

yoruba <- read_csv("ud_yor-results.csv") %>%
  mutate(language = "Yoruba")

mandarin <- read_csv("ud_zho-results.csv") %>%
  mutate(language = "Mandarin") %>%
  bind_rows(read_csv("ud_zho-subset1-results.csv") %>% mutate(language = "Mandarin subset 1")) %>%
  bind_rows(read_csv("ud_zho-subset2-results.csv") %>% mutate(language = "Mandarin subset 2")) %>%
  bind_rows(read_csv("ud_zho-subset3-results.csv") %>% mutate(language = "Mandarin subset 3")) %>%
  bind_rows(read_csv("ud_zho-subset4-results.csv") %>% mutate(language = "Mandarin subset 4")) %>%
  bind_rows(read_csv("ud_zho-subset5-results.csv") %>% mutate(language = "Mandarin subset 5")) %>%
  bind_rows(read_csv("ud_zho-subset6-results.csv") %>% mutate(language = "Mandarin subset 6")) %>%
  bind_rows(read_csv("ud_zho-subset7-results.csv") %>% mutate(language = "Mandarin subset 7")) %>%
  bind_rows(read_csv("ud_zho-subset8-results.csv") %>% mutate(language = "Mandarin subset 8")) %>%
  bind_rows(read_csv("ud_zho-subset9-results.csv") %>% mutate(language = "Mandarin subset 9")) %>%
  bind_rows(read_csv("ud_zho-subset10-results.csv") %>% mutate(language = "Mandarin subset 10"))

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
  ) %>% arrange(score) %>%
    mutate(language = df$language[[1]]) %>%
    mutate(measure = "entropy UID") %>%
    mutate(rank = min_rank(score))
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
  ) %>% arrange(score) %>%
    mutate(language = df$language[[1]]) %>%
    mutate(measure = "surprisal UID") %>%
    mutate(rank = min_rank(score))
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
  ) %>% arrange(desc(score)) %>%
  mutate(language = df$language[[1]]) %>%
  mutate(measure = "mi") %>%
  mutate(rank = min_rank(desc(score)))
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
  ) %>% arrange(desc(mi_z_score)) %>%
    mutate(language = df$language[[1]])
}

big_table_o_data <- tribble(
  ~language, ~measure, ~word_order, ~score, ~rank
) %>% 
  bind_rows(calcEntRankings(filter(afrikaans, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(afrikaans, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(afrikaans, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(akkadian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(akkadian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(akkadian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(amharic, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(amharic, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(amharic, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(ancient_greek, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(ancient_greek, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(ancient_greek, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(arabic, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(arabic, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(arabic, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(armenian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(armenian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(armenian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(assyrian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(assyrian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(assyrian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(bambara, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(bambara, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(bambara, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(basque, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(basque, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(basque, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(belarusian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(belarusian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(belarusian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(bhojpuri, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(bhojpuri, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(bhojpuri, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(breton, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(breton, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(breton, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(bulgarian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(bulgarian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(bulgarian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(buryat, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(buryat, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(buryat, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(cantonese, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(cantonese, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(cantonese, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(catalan, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(catalan, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(catalan, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(classical_chinese, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(classical_chinese, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(classical_chinese, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(coptic, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(coptic, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(coptic, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(croatian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(croatian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(croatian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(czech, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(czech, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(czech, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(danish, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(danish, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(danish, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(dutch, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(dutch, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(dutch, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(english, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(english, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(english, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(childes, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(childes, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(childes, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(erzya, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(erzya, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(erzya, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(estonian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(estonian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(estonian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(faroese, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(faroese, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(faroese, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(farsi, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(farsi, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(farsi, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(finnish, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(finnish, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(finnish, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(french, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(french, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(french, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(galician, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(galician, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(galician, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(german, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(german, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(german, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(gothic, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(gothic, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(gothic, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(hebrew, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(hebrew, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(hebrew, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(hindi, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(hindi, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(hindi, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(hungarian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(hungarian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(hungarian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(indonesian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(indonesian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(indonesian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(irish, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(irish, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(irish, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(italian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(italian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(italian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(japanese, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(japanese, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(japanese, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(karelian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(karelian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(karelian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(kazakh, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(kazakh, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(kazakh, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(komi_permyak, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(komi_permyak, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(komi_permyak, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(komi_zyrian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(komi_zyrian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(komi_zyrian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(korean, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(korean, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(korean, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(kurmanji, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(kurmanji, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(kurmanji, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(latin, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(latin, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(latin, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(latvian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(latvian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(latvian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(lithuanian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(lithuanian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(lithuanian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(livvi, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(livvi, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(livvi, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(maltese, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(maltese, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(maltese, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(mandarin, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(mandarin, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(mandarin, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(marathi, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(marathi, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(marathi, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(mbya_guarani, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(mbya_guarani, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(mbya_guarani, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(greek, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(greek, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(greek, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(moksha, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(moksha, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(moksha, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(nigerian_pidgin, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(nigerian_pidgin, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(nigerian_pidgin, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(north_sami, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(north_sami, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(north_sami, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(norwegian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(norwegian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(norwegian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(old_church_slavonic, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(old_church_slavonic, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(old_church_slavonic, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(old_french, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(old_french, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(old_french, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(old_russian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(old_russian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(old_russian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(polish, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(polish, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(polish, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(portuguese, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(portuguese, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(portuguese, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(romanian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(romanian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(romanian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(russian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(russian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(russian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(sanskrit, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(sanskrit, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(sanskrit, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(scottish_gaelic, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(scottish_gaelic, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(scottish_gaelic, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(serbian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(serbian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(serbian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(skolt_sami, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(skolt_sami, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(skolt_sami, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(slovakian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(slovakian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(slovakian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(slovenian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(slovenian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(slovenian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(spanish, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(spanish, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(spanish, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(swedish, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(swedish, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(swedish, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(swedish_sign, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(swedish_sign, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(swedish_sign, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(swiss_german, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(swiss_german, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(swiss_german, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(tagalog, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(tagalog, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(tagalog, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(tamil, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(tamil, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(tamil, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(telugu, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(telugu, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(telugu, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(thai, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(thai, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(thai, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(turkish, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(turkish, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(turkish, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(ukrainian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(ukrainian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(ukrainian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(upper_sorbian, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(upper_sorbian, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(upper_sorbian, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(urdu, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(urdu, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(urdu, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(uyghur, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(uyghur, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(uyghur, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(vietnamese, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(vietnamese, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(vietnamese, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(warlpiri, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(warlpiri, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(warlpiri, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(welsh, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(welsh, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(welsh, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(wolof, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(wolof, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(wolof, !language %in% c("subset")))) %>%
  bind_rows(calcEntRankings(filter(yoruba, !language %in% c("subset")))) %>%
  bind_rows(calcSurpRankings(filter(yoruba, !language %in% c("subset")))) %>%
  bind_rows(calcMIRankings(filter(yoruba, !language %in% c("subset"))))

class_accs <- left_join(filter(big_table_o_data, rank == 1), rename(meta, language = Language), big_table_o_data, by = "language") %>%
  mutate(acc = as.integer(word_order == Classification))

prediction_accuracies_class <- tribble(
  ~ent_uid, ~surp_uid, ~mi,
  sum(filter(class_accs, measure == "entropy UID")$acc), sum(filter(class_accs, measure == "surprisal UID")$acc), sum(filter(class_accs, measure == "mi")$acc)
)

most_common_word_order <- function(df) {
  x <- select(df, SVO, SOV, VSO, VOS, OVS, OSV)
  colnames(x)[apply(x,1,which.max)]
}

count_accs <- left_join(filter(big_table_o_data, rank == 1), rename(mutate(meta, most_common = most_common_word_order(meta)), language = Language), big_table_o_data, by = "language") %>%
  mutate(acc = as.integer(word_order == most_common))


prediction_accuracies_counts <- tribble(
  ~ent_uid, ~surp_uid, ~mi,
  sum(filter(count_accs, measure == "entropy UID")$acc), sum(filter(count_accs, measure == "surprisal UID")$acc), sum(filter(count_accs, measure == "mi")$acc)
)

svo_agg_ent <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "entropy UID") %>% filter(word_order == "SVO") %>% select(score) %>% sum/89
sov_agg_ent <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "entropy UID") %>% filter(word_order == "SOV") %>% select(score) %>% sum/89
vso_agg_ent <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "entropy UID") %>% filter(word_order == "VSO") %>% select(score) %>% sum/89
vos_agg_ent <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "entropy UID") %>% filter(word_order == "VOS") %>% select(score) %>% sum/89
ovs_agg_ent <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "entropy UID") %>% filter(word_order == "OVS") %>% select(score) %>% sum/89
osv_agg_ent <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "entropy UID") %>% filter(word_order == "OSV") %>% select(score) %>% sum/89

(tribble(
  ~word_order, ~score,
  "OVS", ovs_agg_ent,
  "SOV", sov_agg_ent,
  "OSV", osv_agg_ent,
  "VOS", vos_agg_ent,
  "VSO", vso_agg_ent,
  "SVO", svo_agg_ent
) %>% arrange(score) %>%
    mutate(rank = min_rank(score)))

svo_agg_surp <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "surprisal UID") %>% filter(word_order == "SVO") %>% select(score) %>% sum/89
sov_agg_surp <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "surprisal UID") %>% filter(word_order == "SOV") %>% select(score) %>% sum/89
vso_agg_surp <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "surprisal UID") %>% filter(word_order == "VSO") %>% select(score) %>% sum/89
vos_agg_surp <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "surprisal UID") %>% filter(word_order == "VOS") %>% select(score) %>% sum/89
ovs_agg_surp <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "surprisal UID") %>% filter(word_order == "OVS") %>% select(score) %>% sum/89
osv_agg_surp <- big_table_o_data %>% filter(language != "English CHILDES") %>% filter(measure == "surprisal UID") %>% filter(word_order == "OSV") %>% select(score) %>% sum/89

(tribble(
  ~word_order, ~score,
  "OVS", ovs_agg_surp,
  "SOV", sov_agg_surp,
  "OSV", osv_agg_surp,
  "VOS", vos_agg_surp,
  "VSO", vso_agg_surp,
  "SVO", svo_agg_surp
) %>% arrange(score) %>%
    mutate(rank = min_rank(score)))

z_score <- function(df) {
  mean_mi <- mean(df$Mutual_Information)
  sd_mi <- sd(df$Mutual_Information)
  df %>% mutate(mi_z_score = (Mutual_Information-mean_mi)/sd_mi)
}

z_scores <- tribble(
  ~language, ~word_order, ~score
) %>% bind_rows(rename(filter(afrikaans, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(akkadian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(amharic, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(ancient_greek, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(arabic, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(armenian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(assyrian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(bambara, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(basque, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(belarusian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(bhojpuri, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(breton, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(bulgarian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(buryat, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(cantonese, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(catalan, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(classical_chinese, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(coptic, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(croatian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(czech, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(danish, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(dutch, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(english, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(erzya, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(estonian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(faroese, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(farsi, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(finnish, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(french, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(galician, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(german, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(gothic, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(greek, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(hebrew, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(hindi, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(hungarian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(indonesian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(irish, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(italian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(japanese, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(karelian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(komi_permyak, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(komi_zyrian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(korean, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(kurmanji, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(latin, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(latvian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(lithuanian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(livvi, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(maltese, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(mandarin, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(marathi, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(mbya_guarani, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(moksha, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(nigerian_pidgin, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(north_sami, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(norwegian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(old_church_slavonic, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(old_french, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(old_russian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(polish, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(portuguese, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(romanian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(russian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(sanskrit, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(scottish_gaelic, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(serbian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(skolt_sami, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(slovakian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(slovenian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(spanish, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(swedish, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(swedish_sign, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(swiss_german, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(tagalog, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(tamil, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(telugu, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(thai, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(turkish, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(ukrainian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(upper_sorbian, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(urdu, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(vietnamese, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(uyghur, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(warlpiri, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(welsh, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(wolof, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(yoruba, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score)) %>%
  bind_rows(rename(filter(kazakh, !language %in% c("subset")) %>% z_score %>% calcZScoreRankings, score = mi_z_score))

(svo_agg_mi <- z_scores %>% filter(word_order == "SVO") %>% select(score) %>% sum(na.rm = TRUE)/89)
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

(big_table_o_data <- add_column(big_table_o_data, proportion = c(childes_meta$SVO/childes_meta$Tokens, childes_meta$SOV/childes_meta$Tokens, childes_meta$VSO/childes_meta$Tokens, childes_meta$VOS/childes_meta$Tokens, childes_meta$OVS/childes_meta$Tokens, childes_meta$OSV/childes_meta$Tokens,
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
                                                          mandarin_meta$SVO/mandarin_meta$Tokens, mandarin_meta$SOV/mandarin_meta$Tokens, mandarin_meta$VSO/mandarin_meta$Tokens, mandarin_meta$VOS/mandarin_meta$Tokens, mandarin_meta$OVS/mandarin_meta$Tokens, mandarin_meta$OSV/mandarin_meta$Tokens)))

cor(mutate(filter(big_table_o_data, measure == "entropy UID"), inv_score = 1/score)$inv_score, filter(big_table_o_data, measure == "entropy UID")$proportion)
cor(mutate(filter(big_table_o_data, measure == "surprisal UID"), inv_score = 1/score)$inv_score, filter(big_table_o_data, measure == "surprisal UID")$proportion)
cor(filter(big_table_o_data, measure == "mi")$score, filter(big_table_o_data, measure == "mi")$proportion)

cor.test(mutate(filter(big_table_o_data, measure == "entropy UID"), inv_score = 1/score)$inv_score, filter(big_table_o_data, measure == "entropy UID")$proportion, method = "spearman")
cor.test(mutate(filter(big_table_o_data, measure == "surprisal UID"), inv_score = 1/score)$inv_score, filter(big_table_o_data, measure == "surprisal UID")$proportion, method = "spearman")
cor.test(filter(big_table_o_data, measure == "mi")$score, filter(big_table_o_data, measure == "mi")$proportion, method = "spearman")

c("English CHILDES", "Breton", "Welsh", "German", "English", "Basque", "Irish", "Scottish Gaelic", "Hindi", "Hungarian", "Indonesian", "Japanese", "Korean", "Mbya Guarani", "Dutch", "Tagalog", "Turkish", "Vietnamese", "Warlpiri", "Wolof", "Mandarin") %>% map(function(lang) {
  c("entropy UID", "surprisal UID", "mi") %>% map(function(m, l) {
    if(m == "mi") {
    #  cor(filter(big_table_o_data, language == l, measure == m)$score, filter(big_table_o_data, language == l, measure == m)$proportion)
      cor.test(filter(big_table_o_data, language == l, measure == m)$score, filter(big_table_o_data, language == l, measure == m)$proportion, method = "spearman")
    } else {
    #  cor(mutate(filter(big_table_o_data, language == l, measure == m), inv_score = 1/score)$inv_score, filter(big_table_o_data, language == l, measure == m)$proportion)
      cor.test(mutate(filter(big_table_o_data, language == l, measure == m), inv_score = 1/score)$inv_score, filter(big_table_o_data, language == l, measure == m)$proportion, method = "spearman")
    }
  }, l = lang)
})

calc_order_predictions <- function(meas, order1) {
  rankings <- big_table_o_data
  rankings <- filter(rankings, measure == meas) %>%
      group_by(language)
  if(meas == "mi") {
    rankings <- rankings %>%
      arrange(desc(score), .by_group = TRUE)
  } else {
    rankings <- rankings %>%
      arrange(score, .by_group = TRUE)
  }
  tb <- c("English CHILDES", "Breton", "Welsh", "English", "Basque", "Irish", "Scottish Gaelic", "Hindi", "Indonesian", "Japanese", "Korean", "Mbya Guarani", "Tagalog", "Turkish", "Vietnamese", "Wolof", "Mandarin") %>%
    sapply(function(l) {
      first <- filter(rankings, language == l)[1,]
      second <- filter(rankings, language == l)[2,]
      if(first$score == second$score) {
        if(first$word_order == order1 |
           second$word_order == order1) {
          i <- 1
        } else {
          i <- 0
        }
      } else {
        if(first$word_order == order1) {
          i <- 1
        } else {
          i <- 0
        }
      }
    })
  print(tb)
  returnValue(sum(tb))
}

entR <- sum(filter(prediction_accuracies_wals, lang == "childes-brown-eng" | lang == "ud-english" | lang == "ud-mandarin" | lang == "ud-indonesian" | lang == "ud-vietnamese" | lang == "ud-mbya_guarani" | lang == "ud-wolof")$ent_uid)
surpR <- sum(filter(prediction_accuracies_wals, lang == "childes-brown-eng" | lang == "ud-english" | lang == "ud-mandarin" | lang == "ud-indonesian" | lang == "ud-vietnamese" | lang == "ud-mbya_guarani" | lang == "ud-wolof")$surp_uid)
miR <- sum(filter(prediction_accuracies_wals, lang == "childes-brown-eng" | lang == "ud-english" | lang == "ud-mandarin" | lang == "ud-indonesian" | lang == "ud-vietnamese" | lang == "ud-mbya_guarani" | lang == "ud-wolof")$mi)
(svo_conf_matrix <- tribble(
  ~measure, ~true_pos, ~recall, ~precision,
  "entropy UID", entR, entR/7, entR/calc_order_predictions("entropy UID", "SVO"),
  "surprisal UID", surpR, surpR/7, surpR/calc_order_predictions("surprisal UID", "SVO"),
  "mutual info", miR, miR/7, miR/calc_order_predictions("mi", "SVO")
))

entR <- sum(filter(prediction_accuracies_wals, lang == "ud-japanese" | lang == "ud-korean" | lang == "ud-hindi" | lang == "ud-turkish" | lang == "ud-basque")$ent_uid)
surpR <- sum(filter(prediction_accuracies_wals, lang == "ud-japanese" | lang == "ud-korean" | lang == "ud-hindi" | lang == "ud-turkish" | lang == "ud-basque")$surp_uid)
miR <- sum(filter(prediction_accuracies_wals, lang == "ud-japanese" | lang == "ud-korean" | lang == "ud-hindi" | lang == "ud-turkish" | lang == "ud-basque")$mi)
(sov_conf_matrix <- tribble(
  ~measure, ~true_pos, ~recall, ~precision,
  "entropy UID", entR, entR/5, entR/calc_order_predictions("entropy UID", "SOV"),
  "surprisal UID", surpR, surpR/5, surpR/calc_order_predictions("surprisal UID", "SOV"),
  "mutual info", miR, miR/5, miR/calc_order_predictions("mi", "SOV")
))

entR <- sum(filter(prediction_accuracies_wals, lang == "ud-breton" | lang == "ud-irish" | lang == "ud-scottish_gaelic" | lang == "ud-tagalog" | lang == "ud-welsh")$ent_uid)
surpR <- sum(filter(prediction_accuracies_wals, lang == "ud-breton" | lang == "ud-irish" | lang == "ud-scottish_gaelic" | lang == "ud-tagalog" | lang == "ud-welsh")$surp_uid)
miR <- sum(filter(prediction_accuracies_wals, lang == "ud-breton" | lang == "ud-irish" | lang == "ud-scottish_gaelic" | lang == "ud-tagalog" | lang == "ud-welsh")$mi)
(vso_conf_matrix <- tribble(
  ~measure, ~true_pos, ~recall, ~precision,
  "entropy UID", entR, entR/5, entR/calc_order_predictions("entropy UID", "VSO"),
  "surprisal UID", surpR, surpR/5, surpR/calc_order_predictions("surprisal UID", "VSO"),
  "mutual info", miR, miR/5, miR/calc_order_predictions("mi", "VSO")
))

correls <- tribble(
  ~language, ~measure, ~r, ~dom_order,
  "English CHILDES", "entropy UID", 0.6324682, "SVO",
  "Breton", "entropy UID", 0.3845503, "VSO",
  "Welsh", "entropy UID", 0.4459809, "VSO",
  "German", "entropy UID", -0.3007341, "None",
  "English", "entropy UID", 0.4547007, "SVO",
  "Basque", "entropy UID", -0.5775454, "SOV",
  "Irish", "entropy UID", 0.3329745, "VSO",
  "Scottish Gaelic", "entropy UID", 0.1625653, "VSO",
  "Hindi", "entropy UID", -0.3473683, "SOV",
  "Hungarian", "entropy UID", -0.582591, "None",
  "Indonesian", "entropy UID", 0.1793664, "SVO",
  "Japanese", "entropy UID", -0.4598236, "SOV",
  "Korean", "entropy UID", 0.5851102, "SOV",
  "Mbya Guarani", "entropy UID", -0.1709232, "SVO",
  "Dutch", "entropy UID", 0.2856145, "None",
  "Tagalog", "entropy UID", 0.1505849, "VSO",
  "Turkish", "entropy UID", -0.2680761, "SOV",
  "Vietnamese", "entropy UID", 0.3060113, "SVO",
  "Warlpiri", "entropy UID", 0.6680841, "None",
  "Wolof", "entropy UID", 0.7386079, "SVO",
  "Mandarin", "entropy UID", -0.04177955, "SVO",
  "English CHILDES", "surprisal UID", -0.03464062, "SVO",
  "Breton", "surprisal UID", 0.3663543, "VSO",
  "Welsh", "surprisal UID", 0.4459809, "VSO",
  "German", "surprisal UID", -0.8078796, "None",
  "English", "surprisal UID", -0.485443, "SVO",
  "Basque", "surprisal UID", -0.6199609, "SOV",
  "Irish", "surprisal UID", 0.7228481, "VSO",
  "Scottish Gaelic", "surprisal UID", 0.6934322, "VSO",
  "Hindi", "surprisal UID", -0.3608037, "SOV",
  "Hungarian", "surprisal UID", -0.5824678, "None",
  "Indonesian", "surprisal UID", -0.2601404, "SVO",
  "Japanese", "surprisal UID", -0.4456583, "SOV",
  "Korean", "surprisal UID", 0.5815809, "SOV",
  "Mbya Guarani", "surprisal UID", -0.1709232, "SVO",
  "Dutch", "surprisal UID", -0.8701938, "None",
  "Tagalog", "surprisal UID", 0.1895209, "VSO",
  "Turkish", "surprisal UID", -0.3258963, "SOV",
  "Vietnamese", "surprisal UID", 0.3730164, "SVO",
  "Warlpiri", "surprisal UID", 0.6526612, "None",
  "Wolof", "surprisal UID", -0.6759722, "SVO",
  "Mandarin", "surprisal UID", -0.2288886, "SVO",
  "English CHILDES", "mutual info", 0.1739146, "SVO",
  "Breton", "mutual info", -0.5861659, "VSO",
  "Welsh", "mutual info", -0.4710567, "VSO",
  "German", "mutual info", 0.1677536, "None",
  "English", "mutual info", -0.09812509, "SVO",
  "Basque", "mutual info", -0.07265581, "SOV",
  "Irish", "mutual info", -0.4303142, "VSO",
  "Scottish Gaelic", "mutual info", -0.4531177, "VSO",
  "Hindi", "mutual info", 0.009769402, "SOV",
  "Hungarian", "mutual info", -0.4654166, "None",
  "Indonesian", "mutual info", -0.4160634, "SVO",
  "Japanese", "mutual info", 0.3751736, "SOV",
  "Korean", "mutual info", 0.08761783, "SOV",
  "Mbya Guarani", "mutual info", -0.1135553, "SVO",
  "Dutch", "mutual info", 0.1347135, "None",
  "Tagalog", "mutual info", -0.6032506, "VSO",
  "Turkish", "mutual info", 0.4514237, "SOV",
  "Vietnamese", "mutual info", -0.2892723, "SVO",
  "Warlpiri", "mutual info", 0.2070285, "None",
  "Wolof", "mutual info", 0.3350923, "SVO",
  "Mandarin", "mutual info", -0.5600294, "SVO"
) %>% arrange(dom_order)

  ggplot(data = filter(correls, dom_order == "SVO")) +
    geom_bar(mapping = aes(x = language, y = r, fill = dom_order), stat = "identity") +
    ylim(-1, 1) +
    facet_wrap(~ measure) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  ggplot(data = filter(correls, dom_order == "SOV")) +
    geom_bar(mapping = aes(x = language, y = r, fill = dom_order), stat = "identity") +
    ylim(-1, 1) +
    facet_wrap(~ measure) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    scale_fill_manual(values = "#7CAE00")
  
  ggplot(data = filter(correls, dom_order == "VSO")) +
    geom_bar(mapping = aes(x = language, y = r, fill = dom_order), stat = "identity") +
    ylim(-1, 1) +
    facet_wrap(~ measure) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    scale_fill_manual(values = "#C77CFF")
  
  ggplot(data = filter(correls, dom_order == "None")) +
    geom_bar(mapping = aes(x = language, y = r, fill = dom_order), stat = "identity") +
    ylim(-1, 1) +
    facet_wrap(~ measure) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    scale_fill_manual(values = "#00BFC4")
  
  ggplot(data = filter(correls, measure == "mi")) +
    geom_bar(mapping = aes(x = language, y = r, fill = dom_order), stat = "identity") +
    ylim(-1, 1) +
    facet_wrap(~ dom_order) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(data = big_table_o_data) +
  geom_point(mapping = aes(x = score, y = proportion, color = word_order), position = "jitter") +
  facet_grid(measure ~ language)

(tb <- tribble(
  ~order, ~count,
  "SOV", 565,
  "SVO", 488,
  "VSO", 95,
  "VOS", 25,
  "OVS", 11,
  "OSV", 4) %>% mutate(prop = count/sum(count)) %>% 
    add_column(entropy_uid = c(sov_agg_ent, svo_agg_ent, vso_agg_ent, vos_agg_ent, ovs_agg_ent, osv_agg_ent)) %>%
    mutate(entropy_uid = entropy_uid/sum(entropy_uid)) %>%
    add_column(surprisal_uid = c(sov_agg_surp, svo_agg_surp, vso_agg_surp, vos_agg_surp, ovs_agg_surp, osv_agg_surp)) %>%
    mutate(surprisal_uid = surprisal_uid/sum(surprisal_uid)) %>%
    add_column(mutual_info = c(sov_agg_mi, svo_agg_mi, vso_agg_mi, vos_agg_mi, ovs_agg_mi, osv_agg_mi)) %>%
    mutate(mutual_info = mutual_info/sum(mutual_info)) %>%
    mutate(mutual_info = mutual_info-min(mutual_info))
  )
tb$order <- factor(tb$order, levels = c("SOV", "SVO", "VSO", "VOS", "OVS", "OSV"))

(plot <- qplot(x = order, y = prop, data = tb, group = 1, color = "WALS calculated proportion") +
  geom_line(mapping = aes(x = order, y = prop, color = "WALS calculated proportion"), data = tb) +
  geom_point(data = tb, mapping = aes(x = order, y = entropy_uid, color = "entropy-based UID"), group = 1) +
  geom_line(mapping = aes(x = order, y = entropy_uid, color = "entropy-based UID"), data = tb) +
  geom_point(mapping = aes(x = order, y = surprisal_uid, color = "surprisal-based UID"), group = 1) +
  geom_line(mapping = aes(x = order, y = surprisal_uid, color = "surprisal-based UID")) +
  geom_point(mapping = aes(x = order, y = mutual_info, color = "mutual information"), group = 1) +
  geom_line(mapping = aes(x = order, y = mutual_info, color = "mutual information")) +
  labs(x = "Word order", y = "Proportion (%)", color = "Measure")
)


filter(big_table_o_data, measure == "entropy UID" | measure == "surprisal UID") %>%
  group_by(language, measure) %>%
  mutate(rank = min_rank(score))

filter(big_table_o_data, measure == "entropy UID") %>%
  group_by(language, measure) %>%
  mutate(rank = min_rank(score)) %>%
ggplot() +
  geom_jitter(mapping = aes(x = language, y = rank, color = word_order), size = 2, width = 1, height = 0) +
  geom_bar(mapping = aes(x = language, y = rank, fill = word_order), stat = "identity", position = "stack") +
  scale_y_reverse()
  