
#download libraries
library(readxl)
library(DescTools)
library(ggplot2)

library(dplyr)
source("functions.R")

# data wrangling 
data <- read_excel("../Example_data.xlsx")
# group by Target
new_df <- data[order(data$Target), ]

## sample the dataframe and create new column Group

sample_group_res_1 <- sample_group("Control", 414, 415, 417, 418) # how to manage the brackets? Does user 
# need to write them?

sample_group_res_2 <- sample_group("CD", 428, 430, 431, 432)
sample_group_res_3 <- sample_group("AK", 483, 484, 485, 486)
sample_group_res_4 <- sample_group("CDA", 423, 424, 426, 427)

sample_group_df <- rbind(sample_group_res_1, 
                      sample_group_res_2, 
                      sample_group_res_3, 
                      sample_group_res_4)


# new column with NA
new_df$Group <- NA

# loop through Sample and write corresponding Group to Sample in new column
for (i in 1:nrow(sample_group_df)) {
  new_df$Group[new_df$Sample %in% sample_group_df$sample_name[i]] <- sample_group_df$group_name[i]
}


# data reorder
new_df <- reorder_group_on_graph("Control", "CD", "AK", "CDA") # I could check this later. And rewrite to the function if i will use the reordering
unique(new_df$Target)

## calculating delta Ct
# I have to subtract the ct of every gene from RPL0

# function to receive the Control gene name and process further
control_data <- user_control("RPLP0") # change RPLP0 to the control gene in your dataset


# Merge the data frame with itself to get the Ct values for RPLP0 in the same row as other genes
merged_df <- merged_df_func("RPLP0") # receive from user


# Calculate delta Ct by subtracting the Ct values of RPLP0 from the corresponding Ct values of other genes
delta_ct_df <- subtract_function("RPLP0") # receive from user

# merge all this 3 function to one?

## calculate mean Ct for controls 
# create df with control genes
control_data <- delta_ct_df %>%
  filter (Group == "Control")

# calculate the mean for each gene in the control group
control_mean <- control_data %>%
  group_by(Target) %>% 
  summarise(MeanDeltaCtControl = mean(DeltaCt, na.rm = TRUE))

# Merge the mean values into original df
delta_ct_df <- delta_ct_df %>%
  left_join(control_mean, by = "Target")

## calculate delta-delta Ct
# subtract the delta Ct of Target from mean for Control group 
delta_ct_df <- delta_ct_df %>% 
  mutate(DeltaDeltaCt = DeltaCt - MeanDeltaCtControl)

## calculate the 2^-(ΔΔCt)
delta_ct_df <- delta_ct_df %>%
  mutate(RelativeFoldChange = 2^(-DeltaDeltaCt))


# Duncan test for all genes
# rewrite for function?
genes <- unique(delta_ct_df$Target) 
for (gene in genes) {   
  gene_df <- subset(delta_ct_df, Target == gene)   
  cM.aov <- aov(RelativeFoldChange ~ Group, data = gene_df)
  print(gene)
  print(PostHocTest(cM.aov, method = "duncan")) 
}





# plot violinplot 
plot_violin <- plot_violinplot(delta_ct_df)

# barplot 
# here I have to find out how to add error bars in ggplot 
plot_barplot <- plot_barplot(delta_ct_df) # check if works


