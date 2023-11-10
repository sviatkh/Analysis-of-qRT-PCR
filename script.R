
#download libraries
library(readxl)
library(DescTools)
library(ggplot2)

# data wrangling 

# group by Target
new_df <- data[order(data$Target), ]

# sample the dataframe and create new column Group
sample_group_ <- data.frame(
  sample_name = c("414", "415", "417", "418"),
  group_name = "Control"
)

# new column with NA
new_df$Group <- NA

# loop through Sample and write corresponding Group to Sample
for (i in 1:nrow(sample_group)) {
  new_df$Group[new_df$Sample %in% sample_group$sample_name[i]] <- sample_group$group_name[i]
}


# data reorder 
data$Group <- factor(data$Group, levels = c("Control", "CD", "AKG", "CD + AKG"))



ggplot(data, aes(Target, Ct)) +  
  geom_violin(position = position_dodge(width = .7), width = 2, aes(fill = Group), outlier.colour = NA, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  #geom_jitter(position = position_jitter(width = 1), alpha = 0.5) + 
  labs(y = "Relative expression", x = " ") + 
  ggtitle("qRT-PCR set 2. 2023-10-31") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  



# barplot 
ggplot(data, aes(Target, Ct, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  

# duncan test for all genes
genes <- unique(data$Target) 
for (gene in genes) {   
  gene_df <- subset(data, Target == gene)   
  cM.aov <- aov(Ct ~ Group, data = gene_df)
  print(gene)
  print(PostHocTest(cM.aov, method = "duncan")) 
}

# after a have to calculate the delta Ct