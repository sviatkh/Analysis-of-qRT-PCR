
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
new_df <- reorder_group_on_graph("Control", "CD", "AK", "CDA") # I could check this later
unique(new_df$Target)


# plot violin on data
plot_violin <- plot_violinplot(data)


# barplot 
# here I have to find out how to add error bars in ggplot 
plot_barplot <- function(data) { # check if works
  
  ggplot(data, aes(Target, Ct, fill = Group)) +
    geom_bar(stat = "identity", position = "dodge") + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  }

  

# Duncan test for all genes
# I have to rewrite the loop on new_df after calculating the delta Ct
genes <- unique(data$Target) 
for (gene in genes) {   
  gene_df <- subset(data, Target == gene)   
  cM.aov <- aov(Ct ~ Group, data = new_df)
  print(gene)
  print(PostHocTest(cM.aov, method = "duncan")) 
}

## calculating delta Ct
# I have to subtract the ct of every gene from RPL0
rplp0_data <- new_df %>% filter(Target == "RPLP0")

# Merge the data frame with itself to get the Ct values for RPLP0 in the same row as other genes
merged_df <- new_df %>%
  left_join(rplp0_data, by = c("Sample", "Group"), suffix = c("", "RPLP0"))

# Calculate delta Ct by subtracting the Ct values of RPLP0 from the corresponding Ct values of other genes
delta_ct_df <- merged_df %>%
  mutate(DeltaCt = Ct - CtRPLP0) %>%
  filter(Target != "RPLP0")


## calculate mean Ct for controls 
# create df with control genes
control_data <- delta_ct_df %>%
  filter (Group == "Control")

# calculate the mean for each gene in the control group
control_mean <- control_data %>%
  group_by(Target) %>% 
  summarise(Mean_DeltaCt_Control = mean(DeltaCt, na.rm = TRUE))

# Merge the mean values into original df
delta_ct_df <- delta_ct_df %>%
  left_join(control_mean, by = "Target")




