##Question 4: 

### Run a statistical test on the Palmer Penguins data set and produce a figure to explain it

```{r}
# Run a statistical test on the Palmer Penguins dataset

## Load packages
## Set Working Directory
## Remove NA cells from the penguins data set

cleaning <- function(penguins_now_clean){
  penguins_now_clean %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    drop_na()
}

penguinsMLS<- cleaning(penguins_now_clean)

## Create a linear model to analyse the relationship between Culmen Length and Sex

MLSmodel <- lm(culmen_length_mm~sex, penguinsMLS)

## Analyse the data using anova to investigate significance of the relationship between Culmen Length and Sex

anova(MLSmodel)

```

```{r}
# Produce a figure to explain the Palmer Penguins statistical test

mean_cl_boot95 <- function(x, conf = 0.95) {
    lconf <- (1 - conf)/2
    uconf <- 1 - lconf
    require(boot)
    bmean <- function(x, ind) mean(x[ind])
    bt <- boot(x, bmean, 1000)
    bb <- boot.ci(bt, type = "perc")
    data.frame(y = mean(x), ymin = quantile(bt$t, lconf), ymax = quantile(bt$t, 
                                                                           uconf))
}

MLS <- ggplot(penguinsMLS, aes(x = sex, y = culmen_length_mm)) +
  geom_violin(aes(color = sex), show.legend=TRUE, varwidth=TRUE) +
  geom_jitter(aes(color = sex), alpha = 0.3, show.legend = FALSE, position = position_jitter(width = 0.1, seed = 0)) + stat_unique(aes(sex, culmen_length_mm, color=sex)) +
  stat_summary(fun.data = mean_cl_boot95, geom = "errorbar", width=0.8) +
  stat_summary(fun = mean, geom = "point", show.legend=TRUE) +
  scale_color_manual(values = c("magenta2","royalblue")) +
  scale_x_discrete(labels=c("FEMALE","MALE")) +
  labs(title = 'Relationship Between Culmen Length and Sex in Pygoscelis sp.', subtitle = 'Error bars at 95% CI of the mean', x = 'Sex', y = 'Culmen Length (mm)') +
  theme_bw()

plot(MLS)
