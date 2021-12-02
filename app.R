library(shiny)
library(TOSTER)
library(MASS)
library(tidyverse)

ui <- fluidPage(
  titlePanel("TOSTER Visualization Tool"),

    sidebarLayout(position = "left",
                sidebarPanel(
                  tabsetPanel(
                    tabPanel("Two-sample", value = 1),
                    tabPanel("Paired two-sample", value = 2),
                    tabPanel("Two-sample.prop", value = 3),
                    tabPanel("Correlations", value = 4),
                    id = "tabselected"
                  ),
                  
                  conditionalPanel(condition = "input.tabselected < 3 && input.sample == 'observed'",
                                   numericInput("mean1",
                                                label = h3("Mean of first group"), value = 22,
                                                min = 1),
                                   numericInput("mean2",
                                                label = h3("Mean of second group"), value = 22,
                                                min = 1),
                                   numericInput("sesoi",
                                                label = h3("Smallest Effect Size of Interest (SESOI)"),
                                                value = 4, step = 0.1),
                                   numericInput("sd1",
                                                label = h3("SD of first group"), value = 3,
                                                min = 0.01),
                                   numericInput("sd2",
                                                label = h3("SD of second group"), value = 3,
                                                min = 0.01)
                  ),
                  conditionalPanel(condition = "input.tabselected < 3 && input.sample == 'simulated'",
                                   numericInput("sim_mean1",
                                                label = h3("Mean of first simulated group"), value = 22),
                                   numericInput("sim_mean2",
                                                label = h3("Mean of second simulated group"), value = 22),
                                   numericInput("sim_sesoi",
                                                label = h3("Smallest Effect Size of Interest (SESOI)"),
                                                value = 4, step = 0.1, min = 0.01)
                  ),
                  conditionalPanel(condition = "input.tabselected == 1 && input.sample == 'simulated'",
                                   numericInput("sim_sd1",
                                                label = h3("SD of first simulated group"), value = 3,
                                                min = 0.01),
                                   numericInput("sim_sd2",
                                                label = h3("SD of second simulated group"), value = 3,
                                                min = 0.01)
                  ),
                  conditionalPanel(condition = "(input.tabselected == 1 || input.tabselected == 3) &&
                                   input.sample == 'observed'",
                                   numericInput("n1",
                                                label = h3("Sample size of first group"), value = 10,
                                                min = 1),
                                   numericInput("n2",
                                                label = h3("Sample size of second group"), value = 10,
                                                min = 1)
                  ),
                  conditionalPanel(condition = "(input.tabselected == 1 || input.tabselected == 3) &&
                                   input.sample == 'simulated'",
                                   numericInput("sim_n1",
                                                label = h3("Sample size of first simulated group"), value = 10,
                                                min = 1),
                                   numericInput("sim_n2",
                                                label = h3("Sample size of second simulated group"), value = 10,
                                                min = 1)
                  ),
                  conditionalPanel(condition = "input.tabselected == 2 && input.sample == 'observed'",
                                   numericInput("r_paired",
                                                label = h3("Correlation (Pearson's r)"), value = 0.75,
                                                step = 0.1, min = -0.99, max = 0.99),
                                   numericInput("n_paired",
                                                label = h3("Number of pairs of observations"),
                                                value = 10, min = 1)
                  ),
                  conditionalPanel(condition = "input.tabselected == 2 && input.sample == 'simulated'",
                                   numericInput("sim_sd_paired",
                                                label = h3("SD of a typical group"), value = 3,
                                                min = 0.01),
                                   numericInput("sim_r_paired",
                                                label = h3("Repeatability (r)"), value = 0.75,
                                                step = 0.1, min = 0.01, max = 0.99),
                                   numericInput("sim_n_paired",
                                                label = h3("Number of simulated pairs of observations"),
                                                value = 10, min = 1)
                  ),
                  conditionalPanel(condition = "input.tabselected > 2 && input.sample == 'observed'",
                                   numericInput("sesoi_r",
                                                label = h3("Smallest Effect Size of Interest (SESOI)"),
                                                value = 0.1, step = 0.05, min = 0.01)
                  ),
                  conditionalPanel(condition = "input.tabselected > 2 && input.sample == 'simulated'",
                                   numericInput("sim_sesoi_r",
                                                label = h3("Smallest Effect Size of Interest (SESOI)"),
                                                value = 0.1, step = 0.05, min = 0.01)
                  ),
                  conditionalPanel(condition = "input.tabselected == 3 && input.sample == 'observed'",
                                   numericInput("pr1",
                                                label = h3("Proportion in first group"),
                                                value = 0.5, step = 0.05, min = 0, max = 1),
                                   numericInput("pr2",
                                                label = h3("Proportion in second group"),
                                                value = 0.65, step = 0.05, min = 0, max = 1)
                  ),
                  conditionalPanel(condition = "input.tabselected == 3 && input.sample == 'simulated'",
                                   numericInput("sim_pr1",
                                                label = h3("Proportion in simulated first group"),
                                                value = 0.5, step = 0.05, min = 0, max = 1),
                                   numericInput("sim_pr2",
                                                label = h3("Proportion in simulated second group"),
                                                value = 0.65, step = 0.05, min = 0, max = 1)
                  ),
                  conditionalPanel(condition = "input.tabselected == 4 && input.sample == 'observed'",
                                   numericInput("n",
                                                label = h3("Number of pairs of observations"),
                                                value = 50, min = 1),
                                   numericInput("r",
                                                label = h3("Correlation (Pearson's r)"), value = 0.05,
                                                step = 0.05, min = -1, max = 1)
                  ),
                  conditionalPanel(condition = "input.tabselected == 4 && input.sample == 'simulated'",
                                   numericInput("sim_n",
                                                label = h3("Number of pairs of observations"),
                                                value = 50, min = 1),
                                   numericInput("sim_r",
                                                label = h3("Correlation (Pearson's r)"), value = 0.05,
                                                step = 0.05, min = -1, max = 1)
                  ),
                  conditionalPanel(condition = "input.sample == 'observed'",
                                   numericInput("alpha", label = h3("Alpha for statistical tests"), value = 0.05,
                                                step = 0.01, min = 0.000001, max = 1)
                  ),
                  conditionalPanel(condition = "input.sample == 'simulated'",
                                   numericInput("sim_alpha", label = h3("Alpha for statistical tests"), value = 0.05,
                                                step = 0.01, min = 0.000001, max = 1)
                  ),
                  radioButtons(
                    "sample", "Is input observed data or parameters to sample from:",
                    c("Observed" = "observed", "Simulated" = "simulated")),
                  conditionalPanel(condition = "input.sample == 'simulated'",
                                   numericInput(
                                     "n_iter", label = "Number of iterations", value = 1000, step = 500, min = 1)
                  ),
                  conditionalPanel(condition = "input.sample == 'simulated' && input.tabselected == 4",
                                   numericInput(
                                     "n_pop",
                                     label = "Total number of individuals in the virtual population",
                                     value = 100000, step = 100000, min = 1000, max = 10000000
                                   )
                  )
                ),
                
                mainPanel(
                  conditionalPanel(condition = "input.sample == 'simulated'",
                                   actionButton("sim_button", "Perform simulations")
                                   ),
                  h3("TOSTER Results"),
                  plotOutput("results")
                  )
                )
  )

server <- function(input, output, session) {
  
  observe({
    updateNumericInput(session, "n1", value = case_when(input$tabselected == 3 ~ 200,
                                                            TRUE ~ 10))
    updateNumericInput(session, "n2", value = case_when(input$tabselected == 3 ~ 200,
                                                        TRUE ~ 10))
    updateNumericInput(session, "sim_n1", value = case_when(input$tabselected == 3 ~ 200,
                                                        TRUE ~ 10))
    updateNumericInput(session, "sim_n2", value = case_when(input$tabselected == 3 ~ 200,
                                                        TRUE ~ 10))
  })

  output$results <- renderPlot({
    # store inputs into variables
    m1 <- input$mean1
    m2 <- input$mean2
    sd1 <- input$sd1
    sd2 <- input$sd2
    n1 <- input$n1
    n2 <- input$n2
    low_eqbound <- -input$sesoi
    high_eqbound <- input$sesoi
    alpha <- input$alpha
    low_eqbound_r <- -input$sesoi_r
    high_eqbound_r <- input$sesoi_r
    n <- input$n
    r <- input$r
    n_pop <- input$n_pop
    n_paired <- input$n_paired
    r_paired <- input$r_paired
    pr1 <- input$pr1
    pr2 <- input$pr2

    if (input$sample == "observed") #observed option calls the regular TOSTER functions with regular plots
    {
      switch(input$tabselected,
             `1` = TOSTtwo.raw(m1  = m1,
                               m2  = m2,
                               sd1 = sd1,
                               sd2 = sd2,
                               n1  = n1,
                               n2  = n2,
                               low_eqbound  = low_eqbound,
                               high_eqbound =  high_eqbound,
                               alpha = alpha),
             
             `2` = TOSTpaired.raw(n = n_paired,
                                  m1  = m1,
                                  m2  = m2,
                                  sd1 = sd1,
                                  sd2 = sd2,
                                  r12 = r_paired,
                                  low_eqbound = low_eqbound,
                                  high_eqbound = high_eqbound,
                                  alpha = alpha),
             `3` = TOSTtwo.prop(prop1 = pr1,
                                prop2 = pr2,
                                n1 = n1,
                                n2 = n2,
                                low_eqbound = low_eqbound_r,
                                high_eqbound = high_eqbound_r,
                                alpha = alpha),
             `4` = TOSTr(n = n,
                         r = r,
                         low_eqbound_r = low_eqbound_r,
                         high_eqbound_r = high_eqbound_r,
                         alpha = alpha)
      )
    } else { #simulated option calls custom simulation code with ggplot output
      
      get_TOSTT <- function(n1, n2, m1, m2, sd1, sd2, paired = FALSE, r_paired = 0.75, n_paired, sd_paired){
        
        if (paired == FALSE) {
          # generate random samples from given input
          samp1 <- rnorm(n = n1, mean = m1, sd = sd1)
          samp2 <- rnorm(n = n2, mean = m2, sd = sd2)
          # calculate summary statistics from the random samples
          m1_s <- mean(samp1)
          sd1_s <- sd(samp1)
          m2_s <- mean(samp2)
          sd2_s <- sd(samp2)
          se <- sqrt(sd1_s^2/n1 + sd2_s^2/n2)
          degree_f <- (sd1_s^2/n1 + sd2_s^2/n2)^2 /
            (((sd1_s^2/n1)^2/(n1 - 1)) + ((sd2_s^2/n2)^2/(n2 - 1)))
          
          # p value for NHST
          t <- (m1_s - m2_s)/se
          pttest <- 2 * pt(-abs(t), df = degree_f)
        } else {
          # first generate individual intercepts, then sample individual data points by adding the
          # individual variance around those intercepts and the effect size to the second sample
          
          # decomposition of standard variation to between- and within- components
          sd_between <- sqrt(sd_paired^2 * r_paired)
          sd_within <- sqrt(sd_between^2 * (1 - r_paired)/r_paired)
          
          d <- tibble(ind_intercept = rnorm(n_paired, m1, sd_between)) %>%
            mutate(ind = 1:n()) %>%
            rowwise() %>%
            mutate(samp1 = rnorm(1, ind_intercept, sd_within),
                   samp2 = rnorm(1, ind_intercept + m2 - m1, sd_within))
          m1_s <- mean(d$samp1)
          m2_s <- mean(d$samp2)
          se <- sd(d$samp1 - d$samp2)/sqrt(n_paired)
          
          degree_f <- n_paired - 1
          # p value for NHST
          t <- (m1_s - m2_s)/se
          pttest <- 2 * pt(abs(t), degree_f, lower.tail = FALSE)
        }
        
        t1 <- ((m1_s - m2_s) - low_eqbound)/se
        p1 <- pt(t1, degree_f, lower.tail = FALSE)
        t2 <- ((m1_s - m2_s) - high_eqbound)/se
        p2 <- pt(t2, degree_f, lower.tail = TRUE)
        # take higher p value as p value for the outcome of TOST
        ptost <- max(p1, p2)
        
        list(LL90 = (m1_s - m2_s) - qt(1 - alpha, degree_f) * se,
             UL90 = (m1_s - m2_s) + qt(1 - alpha, degree_f) * se,
             LL95 = (m1_s - m2_s) - qt(1 - (alpha/2), degree_f) * se,
             UL95 = (m1_s - m2_s) + qt(1 - (alpha/2), degree_f) * se,
             testoutcome = if_else(pttest < alpha, 1, 0),
             TOSToutcome = if_else(ptost < alpha, 1, 0))
      }
      
      get_TOSTcor <- function(data, n) {
        # sample n = "Number of pairs of observations" individuals from the total population and
        # save the correlation of the samples
        corr <- cor(dplyr::sample_n(data, n))[1, 2]
        
        z1 <- ((log((1 + abs(corr))/(1 - abs(corr)))/2) -
                 (log((1 + low_eqbound_r)/(1 - low_eqbound_r))/2))/(sqrt(1/(n - 3)))
        z2 <- ((log((1 + abs(corr))/(1 - abs(corr)))/2) -
                 (log((1 + high_eqbound_r)/(1 - high_eqbound_r))/2))/(sqrt(1/(n - 3)))
        # calculate p values for TOST
        p1 <- ifelse(low_eqbound_r < corr,
                     pnorm(-abs(z1)), 1 - pnorm(-abs(z1)))
        p2 <- ifelse(high_eqbound_r > corr,
                     pnorm(-abs(z2)), 1 - pnorm(-abs(z2)))
        # take higher p value as p value for the outcome of TOST
        ptost <- max(p1, p2)
        # p value for NHST
        pttest <- 2 * (1 - pt(
          abs(corr) * sqrt(n - 2)/
            sqrt(1 - abs(corr)^2), n - 2
        ))
        # confidence intervals
        zLL90 <- (log((1 + corr)/(1 - corr))/2) -
          qnorm(1 - alpha) * sqrt(1/(n - 3))
        zUL90 <- (log((1 + corr)/(1 - corr))/2) +
          qnorm(1 - alpha) * sqrt(1/(n - 3))
        zLL95 <- (log((1 + corr)/(1 - corr))/2) -
          qnorm(1 - (alpha/2)) * sqrt(1/(n - 3))
        zUL95 <- (log((1 + corr)/(1 - corr))/2) +
          qnorm(1 - (alpha/2)) * sqrt(1/(n - 3))
        
        list(LL90 = (exp(1)^(2 * zLL90) - 1)/(exp(1)^(2 * zLL90) + 1),
             UL90 = (exp(1)^(2 * zUL90) - 1)/(exp(1)^(2 * zUL90) + 1),
             LL95 = (exp(1)^(2 * zLL95) - 1)/(exp(1)^(2 * zLL95) + 1),
             UL95 = (exp(1)^(2 * zUL95) - 1)/(exp(1)^(2 * zUL95) + 1),
             testoutcome = ifelse(pttest < alpha, 1, 0),
             TOSToutcome = ifelse(ptost < alpha, 1, 0))
      }
      
      if (input$sim_button == 0) #wait until button is pressed to show first plot
        return()
      
      isolate({ #ensure that plot is only updated when button is pressed
        # store inputs into variables
        m1 <- input$sim_mean1
        m2 <- input$sim_mean2
        sd1 <- input$sim_sd1
        sd2 <- input$sim_sd2
        n1 <- input$sim_n1
        n2 <- input$sim_n2
        low_eqbound <- -input$sim_sesoi
        high_eqbound <- input$sim_sesoi
        alpha <- input$sim_alpha
        low_eqbound_r <- -input$sim_sesoi_r
        high_eqbound_r <- input$sim_sesoi_r
        n <- input$sim_n
        r <- input$sim_r
        n_pop <- input$n_pop
        n_paired <- input$sim_n_paired
        r_paired <- input$sim_r_paired
        sd_paired <- input$sim_sd_paired
        pr1 <- input$sim_pr1
        pr2 <- input$sim_pr2
        n_iter <- input$n_iter
        t_results <- data.frame(N = 1:n_iter)
        
        switch(input$tabselected,
               `1` =  {
                 # withProgress(message = "Making plot", value = 0, {
                 #   for (i in 1:n_iter){ # loop over iterations
                 #     # generate random samples from given input
                 #     samp1 <- rnorm(n = n1, mean = m1, sd = sd1)
                 #     samp2 <- rnorm(n = n2, mean = m2, sd = sd2)
                 #     # calculate summary statistics from the random samples
                 #     m1_s <- mean(samp1)
                 #     sd1_s <- sd(samp1)
                 #     m2_s <- mean(samp2)
                 #     sd2_s <- sd(samp2)
                 #     
                 #     # Increment the progress bar, and update the detail text.
                 #     incProgress(1/n_iter, detail = paste("Doing part", i))
                 #     
                 #     t1 <- ((m1_s - m2_s) - low_eqbound)/sqrt(sd1_s^2/n1 + sd2_s^2/n2)
                 #     degree_f <- (sd1_s^2/n1 + sd2_s^2/n2)^2/(((sd1_s^2/n1)^2/
                 #                                                 (n1 - 1)) + ((sd2_s^2/n2)^2/(n2 - 1)))
                 #     p1 <- pt(t1, degree_f, lower.tail = FALSE)
                 #     t2 <- ((m1_s - m2_s) - high_eqbound)/sqrt(sd1_s^2/n1 + sd2_s^2/n2)
                 #     p2 <- pt(t2, degree_f, lower.tail = TRUE)
                 #     # take higher p value as p value for the outcome of TOST
                 #     ptost <- max(p1, p2)
                 #     t <- (m1_s - m2_s)/sqrt(sd1_s^2/n1 + sd2_s^2/n2)
                 #     # p value for NHST
                 #     pttest <- 2 * pt(-abs(t), df = degree_f)
                 #     # confidence intervals
                 #     t_results$LL90[i] <- (m1_s - m2_s) - qt(1 - alpha, degree_f) *
                 #       sqrt(sd1_s^2/n1 + sd2_s^2/n2)
                 #     t_results$UL90[i] <- (m1_s - m2_s) + qt(1 - alpha, degree_f) *
                 #       sqrt(sd1_s^2/n1 + sd2_s^2/n2)
                 #     t_results$LL95[i] <- (m1_s - m2_s) - qt(1 - (alpha/2), degree_f) *
                 #       sqrt(sd1_s^2/n1 + sd2_s^2/n2)
                 #     t_results$UL95[i] <- (m1_s - m2_s) + qt(1 - (alpha/2), degree_f) *
                 #       sqrt(sd1_s^2/n1 + sd2_s^2/n2)
                 #     # test outcomes
                 #     t_results$testoutcome[i] <- ifelse(pttest < alpha, 1, 0)
                 #     t_results$TOSToutcome[i] <- ifelse(ptost < alpha, 1, 0)
                 #   }
                 #   #change plot title, so one plot call works on all types of input
                 #   x_Title <- "Mean Difference"
                 # })
                 x_Title <- "Mean Difference"
                 t_results <- t_results %>%
                   mutate(results = map(N, ~get_TOSTT(n1 = n1,
                                                      n2 = n2,
                                                      m1 = m1,
                                                      m2 = m2,
                                                      sd1 = sd1,
                                                      sd2 = sd2))) %>%
                   unnest_wider(results)

               },
               `2` = {
                 # withProgress(message = 'Preparing Simulation Plot', value = 0, {
                 #   # # effect size (mean difference)
                 #   # eff_size <- m2 - m1
                 #   
                 #   # decomposition of standard variation to between- and within- components
                 #     sd_between <- sqrt(sd_paired^2*r_paired)
                 #     sd_within <- sqrt(sd_between^2*(1-r_paired)/r_paired)
                 #   
                 #   for (i in 1:n_iter){ # loop over iterations
                 #     # first generate individual intercepts, then sample individual data points by adding the
                 #     # individual variance around those intercepts and the effect size to the second sample
                 #     d <- tibble(ind_intercept = rnorm(n_paired, m1, sd_between)) %>%
                 #       mutate(ind = 1:n()) %>%
                 #       rowwise() %>%
                 #       mutate(samp1 = rnorm(1, ind_intercept, sd_within),
                 #              samp2 = rnorm(1, ind_intercept + m2 - m1, sd_within))
                 #     
                 #     # Increment the progress bar, and update the detail text
                 #     incProgress(1/n_iter, detail = paste("Replication n", i))
                 #     
                 #     # calculate summary and test statistics for the random samples
                 #     m1_s <- mean(d$samp1)
                 #     m2_s <- mean(d$samp2)
                 #     sdif <- sd(d$samp1 - d$samp2)
                 #     se <- sdif/sqrt(n_paired)
                 #     t <- (m1_s - m2_s)/se
                 #     degree_f <- n_paired - 1
                 #     pttest <- 2 * pt(abs(t), degree_f, lower.tail = FALSE)
                 #     t1 <- ((m1_s - m2_s) - low_eqbound)/se
                 #     p1 <- pt(t1, degree_f, lower.tail = FALSE)
                 #     t2 <- ((m1_s - m2_s) - high_eqbound)/se
                 #     p2 <- pt(t2, degree_f, lower.tail = TRUE)
                 #     # take higher p value as p value for the outcome of TOST
                 #     ptost <- max(p1, p2)
                 #     # confidence intervals
                 #     t_results$LL90[i] <- ((m1_s - m2_s) - qt(1 - alpha, degree_f) * se)
                 #     t_results$UL90[i] <- ((m1_s - m2_s) + qt(1 - alpha, degree_f) * se)
                 #     t_results$LL95[i] <- ((m1_s - m2_s) - qt(1 - (alpha/2), degree_f) * se)
                 #     t_results$UL95[i] <- ((m1_s - m2_s) + qt(1 - (alpha/2), degree_f) * se)
                 #     # test outcomes
                 #     t_results$testoutcome[i] <- ifelse(pttest < alpha, 1, 0)
                 #     t_results$TOSToutcome[i] <- ifelse(ptost < alpha, 1, 0)
                 #   }
                 #   #change plot title, so one plot call works on all types of input
                 #   x_Title <- "Mean Difference"
                 # })
                 
                 x_Title <- "Mean Difference"
                 t_results <- t_results %>% 
                   mutate(results = map(N, ~get_TOSTT(paired = TRUE,
                                                      n_paired = n_paired,
                                                      m1 = m1,
                                                      m2 = m2,
                                                      sd_paired = sd_paired,
                                                      r_paired = r_paired))) %>%
                   unnest_wider(results)
               },
               `3` = {
                 # make two random binomial vectors
                 t_results$samp1 <- rbinom(n = n_iter, size = n1, prob = pr1)
                 t_results$samp2 <- rbinom(n = n_iter, size = n2, prob = pr2)
                 
                 # calculate test statistics, p values, confidence intervals all in one call (no loop)
                 t_results <- t_results %>%
                   mutate(
                     prop1 = samp1/n1, 
                     prop2 = samp2/n2,
                     prop_dif = prop1 - prop2,
                     prop_se = sqrt((prop1 * (1 - prop1))/n1 + (prop2 * (1 - prop2))/n2),
                     z1 = (prop_dif - low_eqbound_r)/prop_se,
                     z2 = (prop_dif - high_eqbound_r)/prop_se,
                     z = prop_dif/prop_se,
                     ztest = 1 - pnorm(abs(z)),
                     p1 = 1 - pnorm(z1),
                     p2 = pnorm(z2),
                     # take higher p value as p value for the outcome of TOST
                     ptost = pmax(p1, p2),
                     # p value for NHST
                     ztost = ifelse(abs(z1) < abs(z2), z1, z2),
                     # test outcomes
                     testoutcome = ifelse(ztest < (alpha/2), 1, 0),
                     TOSToutcome = ifelse(ptost < alpha, 1, 0),
                     # confidence intervals
                     LL90 = prop_dif - (qnorm(1 - alpha) * prop_se),
                     UL90 = prop_dif + (qnorm(1 - alpha) * prop_se),
                     LL95 = prop_dif - (qnorm(1 - (alpha/2)) * prop_se),
                     UL95 = prop_dif + (qnorm(1 - (alpha/2)) * prop_se)
                     )
                 #rename plot-related variables, so one plot call works on all types of input
                 low_eqbound <- low_eqbound_r
                 high_eqbound <- high_eqbound_r
                 x_Title <- "Proportion Difference"
               },
               `4` = {
                 # create two random vectors of size equal to the given input in 
                 # "Total number of individuals in the virtual population"
                 d <- tibble(samp1 = rnorm(n_pop, 0, 1),
                             samp2 = rnorm(n_pop, 0, 1))
                 corr_mat <- matrix(r, ncol = 2, nrow = 2) # target correlation
                 diag(corr_mat) <- 1
                 
                 mvdat <- mvrnorm(n = nrow(d), mu = c(0, 0), Sigma = corr_mat, empirical = TRUE)
                 
                 rx <- rank(mvdat[ , 1], ties.method = "first")
                 ry <- rank(mvdat[ , 2], ties.method = "first")
                 
                 # resort the two random vectors to obtain the desired correlation r
                 # in the total virtual population
                 dx_sorted <- sort(d$samp1)
                 dy_sorted <- sort(d$samp2)
                 
                 d$samp1 <- dx_sorted[rx]
                 d$samp2 <- dy_sorted[ry]
                
                 #rename plot-related variables, so one plot call works on all types of input
                 low_eqbound <- low_eqbound_r
                 high_eqbound <- high_eqbound_r
                 x_Title <- "Correlation"
                 
                 t_results <- t_results %>%
                   mutate(results = map(N, ~get_TOSTcor(d, n))) %>%
                   unnest_wider(results)
               }
        )
        # add test outcome and color information
        t_results <- t_results %>%
          mutate(outcome = paste0(testoutcome, TOSToutcome),
                 tcolor = ifelse(testoutcome == 1, "orchid1", "darkgray"),
                 TOSTcolor = ifelse(TOSToutcome == 1, "limegreen", "darkgray"))
        # calculate proportions of test outcomes types out of all iterations
        summ <- t_results %>% 
          group_by(outcome) %>%
          summarise(n = n()/n_iter)
        triv <- summ %>%
          filter(outcome == "11") %>%
          pull(n)
        triv <- ifelse(length(triv) == 0, 0, triv)
        str_eq <- summ %>%
          filter(outcome == "01") %>%
          pull(n)
        str_eq <- ifelse(length(str_eq) == 0, 0, str_eq)
        noneq <- summ %>% 
          filter(outcome == "10") %>% 
          pull(n)
        noneq <- ifelse(length(noneq) == 0, 0, noneq)
        inc <- summ %>%
          filter(outcome == "00") %>%
          pull(n)
        inc <- ifelse(length(inc) == 0, 0, inc)
        # table with information about the proportions of test outcomes
        labels <- tibble(x = c(low_eqbound, 0, 0, low_eqbound, high_eqbound, high_eqbound),
                         y = c(-20, -60, 1070, 1030, -20, 1030),
                         hjust = c(0, 0.5, 0.5, 0, 1, 1),
                         color = c("orchid1", "orchid1", "limegreen", "limegreen", "darkgray", "limegreen"),
                         text = c(paste("nonequiv =", noneq), paste("nonzero =", triv + noneq), 
                                  paste("equiv =", str_eq + triv), paste("str_equiv =", str_eq),
                                  paste("inc =", inc), paste("triv =", triv)))
        # plot results with labels
        t_results %>%
          ggplot() +
          geom_segment(aes(x = LL95, xend = UL95, y = N, yend = N, color = as.factor(tcolor))) +
          geom_segment(aes(x = LL90, xend = UL90, y = N, yend = N, color = as.factor(TOSTcolor))) +
          theme_bw() +
          scale_color_identity(guide = "none") +
          geom_text(data = labels, aes(x, y, color = color, label = text, hjust = hjust),
                    size = 5, fontface = "bold", check_overlap = TRUE) +
          xlab(x_Title) +
          geom_vline(xintercept = high_eqbound, linetype = 2, color = "black") +
          geom_vline(xintercept = low_eqbound, linetype = 2, color = "black") +
          theme(text = element_text(size=20))
        
      })
    }
  },
  height = 500, width = 1000) # size of plot
}

shinyApp(ui = ui, server = server)
