## Plot beta binomail
plot_beta_binomial <- function (alpha, beta, y = NULL, n = NULL, prior = TRUE, likelihood = TRUE, 
                                posterior = TRUE) 
{
  if (is.null(y) | is.null(n)) 
    warning("To visualize the posterior,\n            specify data y and n")
  g <- ggplot(data = data.frame(x = c(0, 1)), aes(x)) + labs(x = expression(pi), 
                                                             y = "density") + scale_fill_manual("", values = c(prior = "#f0e442", 
                                                                                                               `(scaled) likelihood` = "#0071b2", posterior = "#009e74"), 
                                                                                                breaks = c("prior", "(scaled) likelihood", "posterior"))
  if (prior == TRUE) {
    g <- g + stat_function(fun = dbeta, args = list(shape1 = alpha, 
                                                    shape2 = beta)) + stat_function(fun = dbeta, args = list(shape1 = alpha, 
                                                                                                             shape2 = beta), geom = "area", alpha = 0.5, aes(fill = "prior"))
  }
  if (!is.null(y) & !is.null(n)) {
    alpha_post <- alpha + y
    beta_post <- beta + n - y
    y_data <- y
    like_scaled <- function(x) {
      like_fun <- function(x) {
        dbinom(x = y_data, size = n, prob = x)
      }
      scale_c <- integrate(like_fun, lower = 0, upper = 1)[[1]]
      like_fun(x)/scale_c
    }
  }
  if (!is.null(y) & !is.null(n) & (likelihood != FALSE)) {
    g <- g + stat_function(fun = like_scaled) + stat_function(fun = like_scaled, 
                                                              geom = "area", alpha = 0.5, aes(fill = "(scaled) likelihood"))
  }
  if (!is.null(y) & !is.null(n) & posterior == TRUE) {
    g <- g + stat_function(fun = dbeta, args = list(shape1 = alpha_post, 
                                                    shape2 = beta_post)) + stat_function(fun = dbeta, 
                                                                                         args = list(shape1 = alpha_post, shape2 = beta_post), 
                                                                                         geom = "area", alpha = 0.5, aes(fill = "posterior"))
  }
  g
}

## Summarize Beta binomial
summarize_beta_binomial <- function (alpha, beta, y = NULL, n = NULL) 
{
  if (is.null(y) | is.null(n)) 
    warning("To summarize the posterior, \n            specify data y and n")
  beta_mean <- function(a, b) {
    a/(a + b)
  }
  beta_mode <- function(a, b) {
    if (a < 1 & b < 1) {
      mode <- "0 and 1"
    }
    else if (a <= 1 & b > 1) {
      mode <- 0
    }
    else if (a > 1 & b < 1) {
      mode <- 1
    }
    else {
      mode <- (a - 1)/(a + b - 2)
    }
  }
  beta_var <- function(a, b) {
    a * b/((a + b)^2 * (a + b + 1))
  }
  prior_mean <- beta_mean(alpha, beta)
  prior_mode <- beta_mode(alpha, beta)
  prior_var <- beta_var(alpha, beta)
  prior_sd <- sqrt(prior_var)
  if (is.null(y) & is.null(n)) {
    return(data.frame(model = c("prior"), alpha = alpha, 
                      beta = beta, mean = prior_mean, mode = prior_mode, 
                      var = prior_var, sd = prior_sd))
  }
  else {
    post_alpha <- y + alpha
    post_beta <- n - y + beta
    post_mean <- beta_mean(post_alpha, post_beta)
    post_mode <- beta_mode(post_alpha, post_beta)
    post_var <- beta_var(post_alpha, post_beta)
    post_sd <- sqrt(post_var)
    return(data.frame(model = c("prior", "posterior"), alpha = c(alpha, 
                                                                 post_alpha), beta = c(beta, post_beta), mean = c(prior_mean, 
                                                                                                                  post_mean), mode = c(prior_mode, post_mode), var = c(prior_var, 
                                                                                                                                                                       post_var), sd = c(prior_sd, post_sd)))
  }
}

summarise_beta_binomial <- summarize_beta_binomial

## Plot Beta Confidence Interval
plot_beta_ci <- function (alpha, beta, ci_level = 0.95) 
{
  p <- ggplot(data = data.frame(x = c(0, 1)), aes(x)) + stat_function(fun = dbeta, 
                                                                      n = 101, args = list(shape1 = alpha, shape2 = beta)) + 
    labs(x = expression(pi), y = expression(paste("f(", pi, 
                                                  ")")))
  q1 <- (1 - ci_level)/2
  q2 <- 1 - q1
  ci <- qbeta(c(q1, q2), alpha, beta)
  mode <- (alpha - 1)/(alpha + beta - 2)
  marks <- c(ci, mode)
  ggplot(data.frame(x = c(0, 1)), aes(x = x)) + stat_function(fun = dbeta, 
                                                              args = list(alpha, beta), xlim = ci, geom = "area", fill = "lightblue") + 
    stat_function(fun = dbeta, args = list(alpha, beta)) + 
    geom_segment(data = data.frame(x = marks, y1 = c(0, 0, 
                                                     0), y2 = dbeta(marks, alpha, beta)), aes(x = x, xend = x, 
                                                                                              y = y1, yend = y2)) + labs(x = expression(pi), y = "density")
}
