library(tidyverse)

trans_cts <-  read_csv("data_rnaseq/counts_transformed.csv")
sample_info <- read_csv("data_rnaseq/sample_info.csv")

pca_matrix <- trans_cts %>% 
  column_to_rownames("gene") %>% 
  # converts our dataframe to a matrix
  as.matrix() %>% 
  # t is to transpose matrix
  t()

sample_pca <- prcomp(pca_matrix)
class(sample_pca)
str(sample_pca)
summary(sample_pca)

pca_matrix[1:10, 1:5]

as_tibble(pca_matrix)
as_tibble(pca_matrix, rownames = "sample")

pc_eigenvalues <- sample_pca$sdev^2

pc_eigenvalues <- tibble(PC = factor(1:length(pc_eigenvalues)),
                         variance = pc_eigenvalues) %>% 
  mutate(pct = variance/sum(variance)*100) %>% 
  mutate(pct_cum = cumsum(pct))
#if we didn't factor, the values would be considered continuous

# Parental plot
# First component (a gene in this case) is explaining about 37% of the variance

pc_eigenvalues %>% 
  ggplot(aes(x = PC)) +
  geom_col(aes(y = pct)) +
  geom_line(aes(y = pct_cum, group = 1)) +
  geom_point(aes(y = pct_cum)) +
  # geom_hline(yintercept = 90) +
  labs(x = "Principal Component", y = "Fraction variance explained")

pc_scores <- sample_pca$x %>% 
  as_tibble(rownames = "sample")

pc_scores %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point()

pc_scores %>% 
  full_join(sample_info, by = "sample") %>% 
  ggplot(aes(x = PC1, 
             y = PC2, 
             colour = minute, 
             shape = strain )) +
  geom_point()

pca_plot <- pc_scores %>% 
  full_join(sample_info, by = "sample") %>% 
  ggplot(aes(x = PC1, 
             y = PC2, 
             colour = factor(minute), 
             shape = strain )) +
  geom_point()
# Biggest variance is all about time but not strain + above 120 there's a stop in variance

pc_loadings <- sample_pca$rotation %>% 
  as_tibble(rownames = "gene")

top_genes <- pc_loadings %>% 
  select(gene, PC1, PC2) %>% 
  pivot_longer(matches("PC"), names_to = "PC", values_to = "loading") %>% 
  group_by(PC) %>% 
  arrange(desc(abs(loading))) %>% 
  slice(1:10) %>% 
  pull(gene) %>% 
  unique()

top_loadings <- pc_loadings %>% 
  filter(gene %in% top_genes)

ggplot(data = top_loadings) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.1, "in")),
               colour = "brown") +
  geom_text(aes(x = PC1, y = PC2, label = gene),
            nudge_y = 0.005, size = 3) +
  scale_x_continuous(expand = c(0.02, 0.02))

loadings_plot <- ggplot(data = top_loadings) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.1, "in")),
               colour = "brown") +
  geom_text(aes(x = PC1, y = PC2, label = gene),
            nudge_y = 0.005, size = 3) +
  scale_x_continuous(expand = c(0.02, 0.02))

# Displaying multiple plots at once
library(patchwork)

(pca_plot | loadings_plot)
(pca_plot | pca_plot | pca_plot)/ loadings_plot
(pca_plot | pca_plot | pca_plot)/ loadings_plot +
  plot_annotation(tag_levels = "A")

# Library has autoplot - the shortcut for what we have done before
library(ggfortify)

autoplot(sample_pca)

autoplot(sample_pca, data = sample_info, 
         colour = "minute", 
         shape = "strain")

library(broom)

tidy(sample_pca, matrix = "eigenvalues")
# Same as this tibble(PC = factor(1:length(pc_eigenvalues)),
#variance = pc_eigenvalues) %>% 
#  mutate(pct = variance/sum(variance)*100) %>% 
#  mutate(pct_cum = cumsum(pct))

tidy(sample_pca, matrix = "loadings")

autoplot(sample_pca, 
         data = sample_info %>% mutate(minute = as.factor(minute)), 
         colour = "minute", 
         shape = "strain")

#Differential expression results

test_results <- read_csv("data_rnaseq/test_result.csv")

test_results
# gene column -> gene name
# baseMean column -> normalized expression level of a gene
# log2FoldChange column -> amount of change between 2 condititons (time in this case)
# lfcE -> standard error associated to log2FoldChange value
# stat column -> the statistics value computed as log2FoldChange/lfcSE compared to standard normal distribution
# pvalue -> p-value associated with the change
# padj -> p-value corrected for multiple hypothesis testing
# comparison -> comparison group 

# MA plot
# 
# Challenge: generate a MA plot (baseMean against log2FoldChange), organise panels by comparison (time point). Hint: consider log-transform baseMean


test_results %>% 
  ggplot(aes(x = log10(baseMean), y = log2FoldChange)) +
  geom_point(alpha = 0.1) +
  facet_wrap(facets = vars(comparison))

ma_plot <- test_results %>% 
  mutate(sig = ifelse(padj < 0.01, log2FoldChange, NA)) %>% 
  ggplot(aes(x = log10(baseMean), y = log2FoldChange)) +
  geom_point(alpha = 0.1) +
  geom_point(aes(y = sig), colour = "tomato", size = 1) +
  geom_hline(yintercept = 0, colour = "dodgerblue") +
  facet_wrap(facets = vars(comparison))

(ma_plot | pca_plot)


# Visualising expression trends
# 1. Get candidate gene (aka padj < 0.01)

candidate_genes <- test_results %>% 
  filter(padj < 0.01) %>% 
  pull(gene) %>% # test_result[,"gene"] aka test_results$gene
  unique()


# 1a. Get trans_cts table in long format
trans_cts_long <- trans_cts %>% 
  pivot_longer(cols = wt_0_r1:mut_180_r3, 
               names_to = "sample",
               values_to = "cts") %>% 
  full_join(sample_info, by = "sample")

# 2. Filter trans_cts_long for candidate genes and compute mean expression value for each gene in each tp and each genotype
trans_cts_mean <- trans_cts_long %>% 
  filter(gene %in% candidate_genes) %>% 
  group_by(gene, strain, minute) %>% 
  summarise(mean_cts = mean(cts), nrep = n()) %>% 
  ungroup()

# 3. Plot trends
trans_cts_mean %>% 
  ggplot(aes(x = minute, y = mean_cts)) +
  geom_line(aes(group = gene), alpha = 0.3) +
  facet_grid(rows = vars(strain))

# 4.  Scaling data to improve visualisation
trans_cts_mean <- trans_cts_long %>% 
  filter(gene %in% candidate_genes) %>% 
  group_by(gene) %>% 
  mutate(cts_scaled = (cts - mean(cts)) / sd(cts)) %>% 
  group_by(gene, strain, minute) %>% 
  summarise(mean_cts_scaled = mean(cts_scaled),
            nrep = n()) %>% 
  ungroup()

trans_cts_mean %>% 
  ggplot(aes(x = minute, y = mean_cts_scaled)) +
  geom_line(aes(group = gene, alpha = 0.3)) +
  geom_hline(yintercept = 0, colour = "brown", linetype = "dashed") +
  facet_grid(rows = vars(strain)) +
  scale_x_continuous(breaks = unique(trans_cts_mean$minute))

#Clustering
# 1. Create a matrix of counts
hclust_matrix <- trans_cts %>% 
  select(-gene) %>% 
  as.matrix()

#assign rownames
rownames(hclust_matrix) <- trans_cts$gene

hclust_matrix <- hclust_matrix[candidate_genes,]

hclust_matrix <- hclust_matrix %>% 
  # transpose the matrix so genes are as columns
  t() %>% 
  # apply scalling to each column of the matrix (genes)
  scale() %>% 
  # transpose back so genes are as rows again
  t()

gene_dist <- dist(hclust_matrix)

#hierarchical clustering

gene_hclust <- hclust(gene_dist, method = "complete")

plot(gene_hclust, labels = FALSE)
abline(h = 10, col = "brown", lwd = 2)

# Make clusters based on the number that I want

cutree(gene_hclust, k = 5) # k in the number of clusters we want

gene_cluster <-  cutree(gene_hclust, k = 5) %>% 
  enframe() %>% 
  rename(gene = name, cluster = value)

trans_cts_cluster <- trans_cts_mean %>% 
  inner_join(gene_cluster, by = "gene")

trans_cts_cluster %>% 
  ggplot(aes(x = minute, y = mean_cts_scaled)) +
  geom_line(aes(group = gene)) +
  facet_grid(cols = vars(cluster), rows = vars(strain))

library(ComplexHeatmap)

Heatmap(hclust_matrix, show_row_names = FALSE)
