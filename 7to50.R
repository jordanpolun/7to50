library(plotly)
library(tidyverse)

calculateSum <- function(hand, goal=50) {
  sum <- 0
  num_aces <- 0
  faces <- c("J", "Q", "K")
  for (c in hand) {
    if (c %in% faces) {
      sum <- sum + 10
    } else if (c != "A") {
      sum <- sum + as.numeric(c)
    } else { # Special case for just aces
      num_aces <- num_aces + 1
    }
  }
  
  # Figure out what to do with aces (1 or 11)

  # The ones digit will always be the number of aces we have
  # The tens digit will always be between 0 and the number of aces we have
  # Try them all
  
  difference <- goal
  ace_sum <- goal
  for (t in 0:num_aces) {
    ace_value <- 10*t + num_aces
    if (abs(goal - sum - ace_value) < difference) {
      difference <- abs(goal - sum - ace_value)
      ace_sum <- ace_value
    }
  }
  
  return(sum + ace_sum)
}

generatePowerSet <- function(set) { 
  n <- length(set)
  masks <- 2^(1:n-1)
  lapply( 1:2^n-1, function(u) set[ bitwAnd(u, masks) != 0 ] )
}

# Set up what all cards are
cards <- rep(c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"), 4)

# Set up how large each hand is, variable for testing
hand_size <- 7

# Get power set for all indexes in hand, for trading in
power_set <- generatePowerSet(1:hand_size)





# Set up dataframe for counting data
stats_df <- data.frame("Sum" = (1*hand_size):(11*hand_size), "Count" = 0)
tens_df <- data.frame("Sum" = numeric(0), "Num.10" = numeric(0), "Win" = numeric(0))

# Deal out a ton of hands as trials and record stats
for (i in 0:10000) {
  
  # Deal random hand of hand_size
  hand <- sample(cards, hand_size, replace=F)
  
  # Calculate sum of hand and record it
  sum <- calculateSum(hand)
  stats_df[stats_df$Sum == sum,2] = stats_df[stats_df$Sum == sum,2] + 1
  
  # Add row to tens_df
  tens_df[nrow(tens_df) + 1,] <- c(sum, length(hand[hand %in% c("10", "J", "Q", "K")]), sum == 50)
  
}

# Calculate the probability of getting each sum
stats_df$Prob = stats_df$Count/sum(stats_df$Count)

# Plot it
p <- ggplot(stats_df, aes(x=Sum, y=Prob)) + 
  geom_line()
ggplotly(p)

# What are the odds of getting between 45 and 54? 68%
prob_45_54 <- stats_df %>%
  filter(Sum >= 45) %>%
  filter(Sum <= 54)
print(sum(prob_45_54$Prob))

# What is the average error from initial dealing?
sum(abs(50-stats_df$Sum) * stats_df$Prob)

View(stats_df)




# Not a significant difference
print(tens_df$Win)
p <- ggplot(tens_df, aes(x=Num.10, color=Win)) + 
  geom_density(adjust=hand_size)
p




# Now that we have some information, let's play some games

# Set up dataframe for keeping track of information
play_df <- data.frame(Initial.Sum = 50, Final.Sum = 50, Num.Trades = 0)[-1,]

# Play a ton of games
for (i in 1:1000) {

  # Shuffle deck
  deck <- sample(cards)
  
  # Choose hand
  hand <- deck[1:hand_size]
  
  # Remove hand from deck
  deck <- deck[hand_size + 1:length(deck)]
  
  # Stores best hand after trades
  best_hand <- hand
  best_hand_trades <- c()
  
  # Using power set, test trading every combination of cards
  for (trade_indexes in power_set) {
    
    # Do tests on a fresh copy of the current deck
    temp_deck <- deck
    
    # Test each card in hand against ones that need to be traded in
    # and discard those that match the current subset
    new_hand <- hand[!1:length(hand) %in% trade_indexes]
    
    # Draw however many new cards we are missing
    new_hand <- c(new_hand, head(temp_deck, length(trade_indexes)))
    
    # Test if this new hand is the best so far
    if (abs(50 - calculateSum(new_hand)) < abs(50 - calculateSum(best_hand))) {
      best_hand <- new_hand
      best_hand_trades <- hand[1:length(hand) %in% trade_indexes]
    }
    
  }
  
  # Add row to dataframe
  play_df[nrow(play_df) + 1,] <- c(calculateSum(hand), calculateSum(best_hand), length(best_hand_trades))
  
}

#View(play_df)

# Plot it
p <- ggplot(play_df, aes(x=Initial.Sum, y=Num.Trades)) + 
  geom_point()
#ggplotly(p)




