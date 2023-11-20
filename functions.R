# function to sample data
sample_group <- function(group, ...) {
  data.frame(
    sample_name = c(...),
    group_name = rep(group, length(list(...)))
  )
}

# function to reorder the data 
reorder_group_on_graph <- function(...) {
  levels <- c(...)
  new_df$Group <- factor(new_df$Group, levels = levels)
  return(new_df)
}

# function to plot violin on data
plot_violinplot <- function(data) { # check if works
  ggplot(data, aes(Target, RelativeFoldChange)) + 
    geom_violin(position = position_dodge(width = .7), width = 2, aes(fill = Group), 
                draw_quantiles = c(0.25, 0.5, 0.75)) + 
    labs(y = "Relative expression", x = " ") + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}

# function to plot barplot
plot_barplot <- function(data) { # check if works
  
  ggplot(data, aes(Target, RelativeFoldChange, fill = Group)) +
    geom_bar(stat = "identity", position = "dodge") + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}



# function to receive the name of control gene for further processing of delta values
user_control <- function(example_control) {
  new_df %>% filter(Target == example_control)
}

# function for merging the df
merged_df_func <- function(user_control) {
  new_df %>% left_join(control_data, by = c("Sample", "Group"), suffix = c("", user_control))
}


