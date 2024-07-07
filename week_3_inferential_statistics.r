install.packages("devtools")
library(devtools)

install.packages("dplyr")
install.packages("ggplot2")
install_github("StatsWithR/statsr")

#Setting a seed will cause R to sample the same sample each time you knit your document
set.seed(9102015)   

library(statsr)
library(dplyr)
library(ggplot2)

#load data
data(ames)

#choose a simple random sample of size 60 from the population
n <- 60
samp <- sample_n(ames, n)

#find the critical value for a 95% confidence interval
z_star_95 <- qnorm(0.975)
z_star_95

#calculate the confidence interval
samp %>%
  summarise(lower = mean(area) - z_star_95 * (sd(area) / sqrt(n)),
            upper = mean(area) + z_star_95 * (sd(area) / sqrt(n)))

#calculate true population mean, store in dataframe "params"
params <- ames %>%
  summarise(mu = mean(area))

#take 50 random samples of size n from population, compute the upper and lower bounds of the confidence intervals based on these samples for the first five intervals
ci <- ames %>%
        rep_sample_n(size = n, reps = 50, replace = TRUE) %>%
        summarise(lower = mean(area) - z_star_95 * (sd(area) / sqrt(n)),
                  upper = mean(area) + z_star_95 * (sd(area) / sqrt(n)))

ci %>%
  slice(1:5)


#create a new variable (using mutate) in the ci data frame that indicates whether the interval does or does not capture the true population mean
ci <- ci %>%
  mutate(capture_mu = ifelse(lower < params$mu & upper > params$mu, "yes", "no"))

#reorganize the data 
ci_data <- data.frame(ci_id = c(1:50, 1:50),
                      ci_bounds = c(ci$lower, ci$upper),
                      capture_mu = c(ci$capture_mu, ci$capture_mu))

#create a plot using ggplot
ggplot(data = ci_data, aes(x = ci_bounds, y = ci_id, 
                           group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  # add points at the ends, size = 2
  geom_line() +           # connect with lines
  geom_vline(xintercept = params$mu, color = "darkgray") # draw vertical line