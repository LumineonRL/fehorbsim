library(dplyr)

TRIALS <- 1000000
TARGET_CHAR <- 11

base_5_star_percent <- .08
pity_delta <- 0.005
shared_within_color <- 3
prob_of_color <- .25

result_orbs_spent <- rep(NULL, TRIALS)
result_stones_pulled <- rep(NULL, TRIALS)
result_tar_char_obtained <- rep(NULL, TRIALS)
result_non_5_star_obtained <- rep(NULL, TRIALS)
result_usd_spent <- rep(NULL, TRIALS)

get_orb_cost <- function(pull_num) {
  if (pull_num == 1L) {
    return(5L)
  }
  else if (pull_num %in% c(2L, 3L, 4L)) {
    return(4L)
  }
  else {
    return(3L)
  }
}

for (i in seq_along(1L:TRIALS)) {
  orbs_spent <- 0
  stones_pulled <- 0
  target_char_obtained <- 0
  non_target_5_star_obtained <- 0
  current_pity <- 0
  
  while (target_char_obtained < TARGET_CHAR) {
    color_rng <- runif(5L, 0, 1L)
    num_of_correct_color <- sum(color_rng < prob_of_color)
    
    if (current_pity < 120L) {
      prob_of_5 <- base_5_star_percent + (pity_delta * floor(current_pity / 5L))  
    }
    else {
      prob_of_5 <- 1L
    }
    
    prob_of_char <- prob_of_5 / shared_within_color
    prob_of_wrong_char <- prob_of_5 - prob_of_char
    
    if (num_of_correct_color == 0) {
      rng <- runif(1, 0, 1)
      
      orbs_spent <- orbs_spent + 5
      stones_pulled <- stones_pulled + 1
      if (rng <= prob_of_5) {
        non_target_5_star_obtained <- non_target_5_star_obtained + 1
        current_pity <- 0
      }
      else {
        current_pity <- current_pity + 1
      }
    }
    else {
      orbs_pulled_session <- 0
      pulled_5_star_this_session <- FALSE
      
      while (target_char_obtained < TARGET_CHAR & orbs_pulled_session < num_of_correct_color) {
        rng <- runif(1, 0, 1)
        
        if (rng <= prob_of_char) {
          target_char_obtained <- target_char_obtained + 1
          orbs_spent <- orbs_spent + get_orb_cost(orbs_pulled_session)
          stones_pulled <- stones_pulled + 1
          pulled_5_star_this_session <- TRUE
        }
        else if (rng > prob_of_char & rng < prob_of_5) {
          orbs_spent <- orbs_spent + get_orb_cost(orbs_pulled_session)
          stones_pulled <- stones_pulled + 1
          non_target_5_star_obtained <- non_target_5_star_obtained + 1
          pulled_5_star_this_session <- TRUE
        }
        else {
          orbs_spent <- orbs_spent + get_orb_cost(orbs_pulled_session)
          stones_pulled <- stones_pulled + 1
        }
        
        orbs_pulled_session <- orbs_pulled_session + 1
      }
      
      if (pulled_5_star_this_session) {
        current_pity <- 0
      }
      else {
        current_pity <- current_pity + num_of_correct_color
      }
    }
  }
  result_orbs_spent[i] <- orbs_spent
  result_stones_pulled[i] <- stones_pulled
  result_tar_char_obtained[i] <- target_char_obtained
  result_non_5_star_obtained[i] <- non_target_5_star_obtained
  result_usd_spent[i] <- (orbs_spent / 170) * 74.99
}

results_df <- tibble(orbs_spent = result_orbs_spent,
              stones_pulled = result_stones_pulled,
              tar_char_obtained = result_tar_char_obtained,
              non_target_5_stars = result_non_5_star_obtained,
              usd_spent = result_usd_spent
              )


summary(results_df)

boxplot(results_df$usd_spent)

library(ggplot2)
library(gridExtra)

my_palette <- c("#FF0000", "#0000FF", "#00FF00", "#888888")

my_theme <- theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"), # Add major grid for y-axis
        panel.grid.minor.y = element_line(color = "gray90", linetype = "dotted"), # Add minor grid for y-axis
        panel.grid.major.x = element_blank(), # Remove major grid for x-axis
        panel.grid.minor.x = element_blank(), # Remove minor grid for x-axis
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

plot1 <- ggplot(data = results_df) +
  geom_boxplot(aes(y = orbs_spent), fill = my_palette[1]) +
  labs(title = "Orbs Spent", y = "Orbs Spent") +
  my_theme

plot2 <- ggplot(data = results_df) +
  geom_boxplot(aes(y = stones_pulled), fill = my_palette[2]) +
  labs(title = "Stones Pulled", y = "Stones Pulled") +
  my_theme

plot3 <- ggplot(data = results_df) +
  geom_boxplot(aes(y = non_target_5_stars), fill = my_palette[3]) +
  labs(title = "Non-Target 5 Stars", y = "Non-Target 5 Stars") +
  my_theme

plot4 <- ggplot(data = results_df) +
  geom_boxplot(aes(y = usd_spent), fill = my_palette[4]) +
  labs(title = "USD Spent", y = "USD Spent") +
  my_theme

grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)