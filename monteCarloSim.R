# Monte Carlo Simulation

get_difference <- function(n, distribution) {
  if (distribution == "uniform") {
    x <- sample(x= 1:10, n, replace = TRUE)
  } else if (distribution == "normal") {
    x <- rnorm(n, mean = 0, sd = 1)
  } else if (distribution == "poisson") {
    x <- rpois(n, lambda = 0.75)
  } else if (distribution == "exponential") {
    x <- rexp(n, rate = 0.5)
  } else {stop}
  
  output <- median(x) - mean(x)
  return(output)
}

simulation <- function(sims, n) {
  distributions <- c("uniform", "normal", "poisson", "exponential")
  allsimulations <- lapply(distributions, function(distribution) {
    output <- replicate(sims, get_difference(n, distribution))
    data.frame(
      SampleSize = n,
      Distribution = distribution,
      MedianminusSAM = output
    )
  })
  data <- bind_rows(allsimulations)
  return(data)
}

size10 <- simulation(10000, 10)
size50 <- simulation(10000, 50)
size100 <-simulation(10000, 100)

monteCarlo_clean <- rbind(size10, size50, size100)
nrow(monteCarlo_clean)
ncol(monteCarlo_clean)
View(monteCarlo_clean)

# Making Plot
library(esquisse)
esquisser(monteCarlo_clean)

ggplot(monteCarlo_clean) +
  aes(x = MedianminusSAM, y = Distribution) +
  geom_violin(adjust = 1L, fill = "#112446") +
  theme_minimal() +
  facet_grid(vars(SampleSize))
