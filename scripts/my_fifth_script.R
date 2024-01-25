# Create a "data" directory
dir.create("data_rnaseq")

# Download the data provided by your collaborator using a for loop to automate this step

for(i in c("counts_raw.csv", "counts_transformed.csv", "sample_info.csv", "test_result.csv")){
  download.file(
    url = paste0("https://github.com/tavareshugo/data-carpentry-rnaseq/blob/master/data/", i, "?raw=true"),
    destfile = paste0("data_rnaseq/", i)
  )
}

library(tidyverse)

raw_cts <- read_csv("data_rnaseq/counts_raw.csv")
trans_cts <- read_csv("data_rnaseq/counts_transformed.csv")
sample_info <- read_csv("data_rnaseq/sample_info.csv")
test_result <- read_csv("data_rnaseq/test_result.csv")

trans_cts_long <- trans_cts %>% 
  pivot_longer(names_to = "sample", values_to = "cts", cols = wt_0_r1:mut_180_r3)

trans_cts_long <- full_join(trans_cts_long, sample_info, by = "sample")

trans_cts_long %>% 
  ggplot(aes(x = cts)) +
  geom_freqpoly()

trans_cts_long %>% 
  ggplot(aes(x = cts, colour = replicate)) +
  geom_freqpoly()

trans_cts_long %>% 
  ggplot(aes(x = cts, colour = replicate)) +
  geom_freqpoly(binwidth = 1)

trans_cts_long %>% 
  ggplot(aes(x = cts, colour = replicate)) +
  geom_freqpoly(binwidth = 1) +
  facet_grid(rows = vars(strain), cols = vars(minute))

raw_cts_long <- raw_cts %>% 
  pivot_longer(names_to = "sample", values_to = "cts", cols = wt_0_r1:mut_180_r3)

raw_cts_long <- full_join(raw_cts_long, sample_info, by = "sample")

raw_cts_long %>% 
  ggplot(aes(x = cts)) +
  geom_freqpoly()

raw_cts_long %>% 
  ggplot(aes(x = cts, colour = replicate)) +
  geom_freqpoly()

raw_cts_long %>% 
  ggplot(aes(x = cts, colour = replicate)) +
  geom_freqpoly(binwidth = 1)

raw_cts_long %>% 
  ggplot(aes(x = cts, colour = replicate)) +
  geom_freqpoly(binwidth = 1) +
  facet_grid(rows = vars(strain), cols = vars(minute)) +
  scale_x_log10()

raw_cts_long %>% 
  ggplot(aes(x = log10(cts), colour = replicate)) +
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols = vars(minute))

raw_cts_long %>% 
  ggplot(aes(x = log10(cts+1), colour = replicate)) +
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols = vars(minute))

raw_cts_long %>% 
  ggplot(aes(x = log10(cts+1), colour = replicate)) +
  geom_freqpoly(binwidth = 1) +
  facet_grid(rows = vars(strain), cols = vars(minute))

#Instead of the freqpoly, let's try a boxplot
#Minute is a category in this example so we factor it!

raw_cts_long %>% 
  ggplot(aes(x = factor(minute), y = log10(cts + 1), fill = strain)) +
  geom_boxplot()

raw_cts_long %>% 
  ggplot(aes(x = factor(minute), y = log10(cts + 1), fill = strain)) +
  geom_boxplot() +
  facet_grid(cols = vars(replicate))

raw_cts_long %>% 
  ggplot(aes(x = factor(minute), y = log10(cts + 1), fill = replicate)) +
  geom_boxplot() +
  facet_grid(cols = vars(strain))

# Correlation between wt sample at T0 AND T30

trans_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_30_r1)) +
  geom_point() +
  geom_abline(colour = "brown")

trans_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_0_r2)) +
  geom_point() +
  geom_abline(colour = "brown")

#To look at the correlation of count data across all samples in our experiment

trans_cts_corr <- trans_cts %>% 
  select(-gene) %>% #removing gene column
  cor(method = "spearman")

#install.packages("corrr")
library(corrr)

rplot(trans_cts_corr) + #heat map
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Compare trans_cts and raw_cts

summary(raw_cts_long$cts)
summary(trans_cts_long$cts)

raw_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_0_r2)) +
  geom_point()

raw_cts %>% 
  ggplot(aes(x = wt_0_r1 + 1, y = wt_0_r2 + 1 )) +
  geom_point() +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")

raw_cts_long %>% 
  group_by(gene) %>% 
  summarise(mean_cts = mean(cts), var_cts = var(cts)) %>% 
  ggplot(aes(x = mean_cts, y = var_cts)) +
  geom_point() +
  geom_abline(colour = "brown") +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")

trans_cts_long %>% 
  group_by(gene) %>% 
  summarise(mean_cts = mean(cts), var_cts = var(cts)) %>% 
  ggplot(aes(x = mean_cts, y = var_cts)) +
  geom_point()

trans_cts_long %>% 
  group_by(gene) %>% 
  summarise(mean_cts = mean(cts), var_cts = var(cts)) %>% 
  mutate(above_four = var_cts > 4) %>% 
  ggplot(aes(x = mean_cts, y = var_cts, colour = above_four)) +
  geom_point()

