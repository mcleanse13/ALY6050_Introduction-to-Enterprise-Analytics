#Module 2, Lab 1
# Set seed for reproducibility
set.seed(123)

# Simulate 1000 random values for X
simulated_X <- replicate(1000, {
  initial_money <- 5
  cost_per_hand <- 1
  reward_probability <- 0.30
  num_hands <- 5
  
  money_left <- initial_money - cost_per_hand * num_hands
  
  for (i in 1:num_hands) {
    if (runif(1) < reward_probability) {
      money_left <- money_left + 2
    }
  }
  
  money_left
})

# Theoretical probability distribution of X
# X can take values from 0 to 13 (initial money - 5 * cost per hand + 2 * max hands)
theoretical_probs <- dbinom(0:13, size = 5, prob = 0.30)

# Calculate expected value (E(X)) and variance (VAR(X))
mean_theoretical <- sum((0:13) * theoretical_probs)
var_theoretical <- sum(((0:13) - mean_theoretical)^2 * theoretical_probs)

# Chi-square goodness-of-fit test
observed_freq <- table(simulated_X)
expected_freq <- theoretical_probs * sum(observed_freq)

# Corrected scaling of expected frequencies
expected_freq <- expected_freq * sum(observed_freq) / sum(expected_freq)

# Create a matrix with observed and expected frequencies
observed_expected_matrix <- cbind(observed_freq, expected_freq)

# Chi-square test
chi_square_test <- chisq.test(observed_expected_matrix)
chi_square_stat <- chi_square_test$statistic
p_value <- chi_square_test$p.value

# Print results
cat("Simulated Frequency Distribution:\n")
print(observed_freq)

cat("\nTheoretical Probability Distribution:\n")
print(theoretical_probs)

cat("\nExpected Value (E(X)):", mean_theoretical)
cat("\nVariance (VAR(X)):", var_theoretical)

cat("\n\nChi-square goodness-of-fit test:")
cat("\nChi-square statistic:", chi_square_stat)
cat("\nP-value:", p_value)
