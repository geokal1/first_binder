

# Reading our packages
library(tidyverse) # Load the tidyverse packages.
library(afex) # ANOVA functions
library(emmeans) # Needed for pairwise comparisons.

# Reading in our data
my_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/12_glm_anova_pt2/master/data/ancova_data.csv")
head(my_data)
View(my_data)

# Making Condition, which is coded as a character, a factor.
my_data <- my_data %>%
  mutate(Condition = factor(Condition))
head(my_data)

# Summarising our Data to see what factor has the greatest effect.
my_data %>%
  group_by(Condition) %>% #In the beggining, I added Gaming as well, but Gaming is not an IV. Why?
  summarise(mean_ability = mean(Ability), sd_ability = sd(Ability)) %>%
  print(n  = 45)

# I did that in the beginning to visualize our data. Why didn't we need that?
my_data %>%
  ggplot(aes(x = Condition, y = Ability, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .25) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") + 
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Condition", y = "Ability")

# Visualizing our data to see how Gaming affects Ability. This is the right way.
set.seed(1234)
ggplot(my_data,aes(x = Gaming, y = Ability, colour = Condition)) +
  geom_point(size = 3, alpha = .9) + # why did we use this function?
  labs(x = "Gaming Frequency (hours per week)",
       y = "Motor Ability") +
  theme_minimal() +
  theme(text = element_text(size = 11))

#---------------------- Building our ANOVA model----
anova_model <- aov_4(Ability ~ Condition + (1 | Participant), data = my_data)    # Why didn't I use (1 + Condition | Participant)?
anova(anova_model)                                                               # The reason you didn't use (1 + Condition | Participant) 
emmeans(anova_model, pairwise ~ Condition)                                       # in the model is that you chose to only include the fixed 
                                                                                 # effect of "Condition" (Condition) on "Ability" without 
                                                                                 # considering any potential random variability in the effect 
                                                                                 # across participants. By using (1 | Participant), you assume 
                                                                                 # that the effect of "Condition" is constant across all participants, 
                                                                                 # except for the overall intercept.

# ----------------------Building our ANCOVA model -----
model_ancova <- aov_4(Ability ~ Gaming + Condition + (1 | Participant), data = my_data, factorize = FALSE)
anova(model_ancova)
emmeans(model_ancova, pairwise ~ Condition) # Why is there such a decrease in the estimates compared to the ones in ANOVA model?

# ------------------- AN(C)OVA as a Special Case of Regression -----------------

#Visualizing our data (violin plot with jittered data)
my_data %>%
  ggplot(aes(x = Condition, y =  Ability, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .05, alpha = .8) +
  labs(x = "Condition",
       y = "Motor Ability") +
  stat_summary(fun.data = mean_cl_boot, colour = "black") +
  guides(colour = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 12))

#setting up our contrasts
contrasts(my_data$Condition)

#Fixing the contrasts ????????/
my_data <- my_data %>%
  mutate(Condition = fct_relevel(Condition,
                                 c("Water", "Double Espresso", "Single Espresso")))
         contrasts(my_data$Condition)
    
# -------------------- ANOVA as a Linear Model ---------
#modeling our linear model to take effect of each factor 
model_lm <- lm(Ability ~ Condition, data = my_data)        
model_lm     

#Ability for the Double Espresso group:
#Ability = Intercept + β1(Double Espresso) + β2(Single Espresso)
#Ability = 4.817 + 4.199(1) + 1.871(0)
#Ability = 4.817 + 4.199
#Ability = 9.016

#Ability for the Single Espresso group:
#Ability = 4.817 + 4.199(0) + 1.871(1)
#Ability = 4.817 + 1.871
#Ability = 6.688

# -------------------ANCOVA as a Linear Model ------------
model_ancova <- lm(Ability ~ Condition + Gaming, data = my_data)
model_ancova

#Gaming is not a factor. So we need to enter the mean of this variable.
mean(my_data$Gaming)

# Ability for the Water group
#Ability = Intercept + β1(Gaming) + β2(Double Espresso) + β3(Single Espresso)
#Ability = -3.4498 + 0.8538(12.62296) + (- 1.0085)(0) + (-0.4563)(0)
#Ability = -3.4498 + 10.777
#Ability = 7.33

# Ability for the Single Espresso group
# Ability = - 3.4498 + 0.8538(12.62296) + ( - 1.0085)(1) + (-0.4563)(0)

#Ability for the Double Espresso group
# Ability = - 3.4498 + 0.8538(12.62296) + ( - 1.0085)(0) + (-0.4563)(1)

#---------------Centering our covariate to make the interpretation of the coefficients easier---------
#than when using the linear model.
my_scaled_data <- my_data %>%
  mutate(centred_gaming = scale = (Gaming))

#Looking at both the uncentered and centered covariate to see that nothing has
#changed in the data, other than the variable mean is now centred on zero and
# the distribution has been scaled.
plot(density(my_scaled_data$Gaming))
plot(density(my_scaled_data$centred_gaming))

#Building our linear model with the scaled and centred covariate.
model_ancova_centred <- lm(Ability ~ centred_gaming + Condition, data = my_scaled_data)
model_ancova_centred
#Hopefully, you see that scaling and centering the covariate makes it a lot
#easier to then interpret the coefficients of our linear model.