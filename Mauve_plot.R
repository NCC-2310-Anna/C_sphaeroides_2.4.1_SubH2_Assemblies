library(tidyverse)
library(gggenomes)
library(RColorBrewer)
library(ggplot2)
library(scales)

# convert Mauve main output file in a parsible table with some perl magic
# perl -ane 'BEGIN{$i=1; print "seq_id\tstart\tend\tstrand\tfeat_id\tcluster_id\n"} $i++ if /^=/; if(/^>\s?(\d+):(\d+)-(\d+)\s(.)/){print "s$1\t$2\t$3\t$4\tf$1-$i\tc$i\n"}' mauve-out > mauve-out.tsv

# read blocks as features
mauve_blocks <- read_tsv("C://Users/annan/Downloads/Whole.tsv") %>%
  add_count(cluster_id) %>%
  mutate(cluster_id = ifelse(n>1, cluster_id, NA))
# blocks are clustered - easiest way to add links
mauve_clusters <- select(mauve_blocks, cluster_id, feat_id) %>%
  drop_na()

mycol <- c(brewer.pal(n=8, name = "Pastel2"), brewer.pal(n=8, name = "Set2"), brewer.pal(n=8, name = "Pastel1"))


gggenomes(feats=mauve_blocks) %>%
  add_clusters(mauve_clusters, .track_id = feats) +
  geom_feat(aes(color=cluster_id), position="strand") +
  geom_link(aes(fill=cluster_id), color = NA, alpha = 0.3)+
  scale_color_manual(values = mycol)+
  scale_fill_manual(values = mycol)+
  scale_x_continuous(labels = comma_format(big.mark = ",", decimal.mark = "."))+
  theme(legend.position = "none", axis.text.x = element_text(size = 12))


# read blocks as features
mauve_blocks <- read_tsv("C://Users/annan/Downloads/PlADE(1).tsv") %>%
  add_count(cluster_id) %>%
  mutate(cluster_id = ifelse(n>1, cluster_id, NA))
# blocks are clustered - easiest way to add links
mauve_clusters <- select(mauve_blocks, cluster_id, feat_id) %>%
  drop_na()

mycol <- c(brewer.pal(n=8, name = "Pastel2"), brewer.pal(n=8, name = "Set2"), brewer.pal(n=8, name = "Pastel1"))


gggenomes(feats=mauve_blocks) %>%
  add_clusters(mauve_clusters, .track_id = feats) +
  geom_feat(aes(color=cluster_id), position="strand") +
  geom_link(aes(fill=cluster_id), color = NA, alpha = 0.3)+
  scale_color_manual(values = mycol)+
  scale_fill_manual(values = mycol)+
  scale_x_continuous(labels = comma_format(big.mark = ",", decimal.mark = "."))+
  theme(legend.position = "none", axis.text.x = element_text(size = 12))
