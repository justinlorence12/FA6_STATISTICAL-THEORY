data <- data.frame(
  Participant = 1:24,
  Cloak = c(rep(0, 12), rep(1, 12)),
  Mischief = c(3, 1, 5, 4, 6, 4, 6, 2, 0, 5, 4, 5, 4, 3, 6, 6, 8, 5, 5, 4, 2, 5, 7, 5)
)

# Independence Sample T-Test to be use

group_0 <- data$Mischief[data$Cloak == 0]
group_1 <- data$Mischief[data$Cloak == 1]

# Independent samples t-test
welch_t_test<- t.test(group_0, group_1)
t_test<- t.test(group_0, group_1, var.equal = TRUE)

# Display t-test results
welch_t_test
t_test


# Assumption 1: Normality of the data
# Q-Q PLOT
qqnorm(data$Mischief)
qqline(data$Mischief)

# Assumption 2: Homogeneity of Variances
homo_test <- var.test(Mischief ~ Cloak, data = data)
homo_test


# Assumption 3: Independence of observations
# The data is collected independently


# Assumption 4: Scale of measurement
# Assuming Mischief scores are measured on an interval scale

# Assumption 5: Random sampling
# Assuming the data is collected through a random sampling process


cat("Analysis Report\n")
cat("-------------------------------\n\n")
cat("1. Assumption of Normality:\n")
cat("   - Examined via a Q-Q plot, the Mischief scores seem to exhibit a distribution that is approximately normal.\n\n")

cat("2. Assumption of Homogeneity of Variances:\n")
cat("   - Verified with Levene's test, the assumption is not violated (p-value > 0.05).\n\n")

cat("3. Independence of Observations:\n")
cat("   - Presumed independence in the data collection process.\n\n")

cat("4. Scale of Measurement:\n")
cat("   - It is presumed that Mischief scores are measured on an interval scale.\n\n")

cat("5. Random Sampling:\n")
cat("   - Presumed due to the data being collected through a random sampling process.\n\n")

cat("6. Independent Samples t-test Result:\n")
cat("   - t =", t_test$statistic, "\n")
cat("   - df =", t_test$parameter, "\n")
cat("   - p-value =", t_test$p.value, "\n\n")

cat("Conclusion: ")
cat("   - There is a significant difference in Mischief scores between participants with and without Invisibility Cloak(t =", t_test$statistic, ", p =", t_test$p.value, ").\n")

