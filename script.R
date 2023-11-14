
#download libraries
library(readxl)
library(DescTools)
library(ggplot2)

# data wrangling 
data <- read_excel("../Example_data.xlsx")
# group by Target
new_df <- data[order(data$Target), ]

## sample the dataframe and create new column Group

# function to sample data
sample_group <- function(group, ...) {
  data.frame(
    sample_name = c(...),
    group_name = rep(group, length(list(...)))
  )
}

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
reorder_group_on_graph <- function(...) {
  levels <- c(...)
  new_df$Group <- factor(new_df$Group, levels = levels)
  return(new_df)
}

new_df <- reorder_group_on_graph("Control", "CD", "AK", "CDA") # I could check this later

unique(new_df$Target)



# function to plot violin on data
plot_violinplot <- function(data) { # check if works
  ggplot(data, aes(Target, Ct)) + 
    geom_violin(position = position_dodge(width = .7), width = 2, aes(fill = Group), 
                draw_quantiles = c(0.25, 0.5, 0.75)) + 
    labs(y = "Relative expression", x = " ") + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
}

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
  cM.aov <- aov(Ct ~ Group, data = gene_df)
  print(gene)
  print(PostHocTest(cM.aov, method = "duncan")) 
}

# after I have to calculate the delta Ct

# create file functions and import them here
