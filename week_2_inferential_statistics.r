install.packages("devtools")
library(devtools)

install.packages("dplyr")
install.packages("ggplot2")
install.packages("shiny")
install_github("StatsWithR/statsr")

library(statsr)
library(dplyr)
library(shiny)
library(ggplot2)

#load data
data(ames)

#plot data
ggplot(data = ames, aes(x = area)) +
  geom_histogram(binwidth = 250)

#summarise data
ames %>%
  summarise(mu = mean(area), pop_med = median(area), 
            sigma = sd(area), pop_iqr = IQR(area),
            pop_min = min(area), pop_max = max(area),
            pop_q1 = quantile(area, 0.25),  # first quartile, 25th percentile
            pop_q3 = quantile(area, 0.75))  # third quartile, 75th percentile

#calculating mean of the random 50 samples
samp1 <- ames %>%
  sample_n(size = 50)

samp1 %>%
  summarise(x_bar = mean(area))

samp1 %>%
    summarise(samp1_mu = mean(area), samp1_med = median(area), 
              samp1_sigma = sd(area), samp1_iqr = IQR(area),
              samp1_min = min(area), samp1_max = max(area),
              samp1_q1 = quantile(area, 0.25),  # first quartile, 25th percentile
              samp1_q3 = quantile(area, 0.75))  # third quartile, 75th percentile

#calculating mean of the random 100 samples
samp2 <- ames %>%
  sample_n(size = 100)

samp2 %>%
  summarise(x_bar = mean(area))

#calculating mean of the random 150 samples
samp3 <- ames %>%
    sample_n(size = 1000)

samp3 %>%
    summarise(x_bar = mean(area))

#Not surprisingly, every time we take another random sample, we get a different sample mean. It’s useful to get a sense of just how much variability we should expect when estimating the population mean this way. The distribution of sample means, called the sampling distribution, can help us understand this variability. In this lab, because we have access to the population, we can build up the sampling distribution for the sample mean by repeating the above steps many times. Here we will generate 15,000 samples and compute the sample mean of each. Note that we are sampling with replacement, replace = TRUE since sampling distributions are constructed with sampling with replacement.#

sample_means50 <- ames %>%
                    rep_sample_n(size = 50, reps = 15000, replace = TRUE) %>%
                    summarise(x_bar = mean(area))

ggplot(data = sample_means50, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)


ames %>%
  sample_n(size = 50) %>%
  summarise(x_bar = mean(area))

#25 sample means from samples of size 10, check number of elements in Data Environment
sample_means_small <- ames %>%
    rep_sample_n(size = 10, reps = 25, replace = TRUE) %>%
    summarise(x_bar = mean(area))

ggplot(data = sample_means50, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)

#a random sample of size 50 from price
ames %>%
  sample_n(size = 50) %>%
  summarise(x_bar = mean(price))


# average price of 5000 samples from the population of size 50 and computing 5000 sample means
sample_means50 <- ames %>%
    rep_sample_n(size = 50, reps = 5000, replace = TRUE) %>%
    summarise(x_bar = mean(price))

sample_means50 %>%
    summarise(sample_means50_avg = mean(x_bar))

# average price of 5000 samples from the population of size 150 and computing 5000 sample means
sample_means150 <- ames %>%
    rep_sample_n(size = 150, reps = 5000, replace = TRUE) %>%
    summarise(x_bar = mean(price))

sample_means150 %>%
    summarise(sample_means150_avg = mean(x_bar))

#a sample of size 15 from the population and calculate the mean price
ames %>%
  sample_n(size = 15) %>%
  summarise(x_bar = mean(price))

# average price of 2000 samples from the population of size 15 and computing 2000 sample means
sample_means15 <- ames %>%
    rep_sample_n(size = 15, reps = 2000, replace = TRUE) %>%
    summarise(x_bar = mean(price))

sample_means15 %>%
    summarise(sample_means15_avg = mean(x_bar))

#population mean of price
ames %>%
  summarise(x_bar = mean(price))

# average price of 2000 samples from the population of size 150 and computing 2000 sample means
sample_means150 <- ames %>%
    rep_sample_n(size = 150, reps = 2000, replace = TRUE) %>%
    summarise(x_bar = mean(price))

sample_means150 %>%
    summarise(sample_means150_avg = mean(x_bar))

#graph the sample means
ggplot(data = sample_means50, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)