library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(skimr)
library(broom)
library(RColorBrewer)

airline1 <- read_excel("airline_03.xlsx")

table(airline1$satisfaction)
table(airline1$kids)
table(airline1$load)
table(airline1$gender)

skim(airline1)

#removing outliers______________________________________________________________
airline2 <- airline1 %>% 
  filter(satisfaction != "-999", kids != ".")


table(airline2$satisfaction)
table(airline2$kids)
table(airline2$gender)

skim(airline2$satisfaction)

#_________transformation________________________________________________________
airline2$satisfaction <- as.integer(airline2$satisfaction)

#transforming kids variables and assigning "0" to "no kids"
airline2 <- airline2 %>% 
  mutate(kids = ifelse(kids == "0", "No kids", "kids"))


#transforming connection and assigning "0" to no connection and "1" connecting flight
airline2$connection <- ifelse(airline2$connection == "connecting flight", 1, 0)

#transforming trip and assigning "0" to leisure and "1" business

airline2$trip <- ifelse(airline2$business == "business", 1, 0)

airline2$delay <- ifelse (airline2$departure == "on time", 1, 0)

#transforming gender and assigning "others/unknown" to  "others" 

table(airline2$gender)

airline2$gender <- ifelse(airline2$gender == "male", "Male", ifelse(airline2$gender == "female", "Female", "Others"))

#_______________________________________________________________________________
#ordering variables_____________________________________________________________
#departure order
delay_order <- c("on time", "delay < 30min", "delay < 90min", "delay > 90min")
airline2$departure <- factor(airline2$departure, levels = delay_order)

#day order
day_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
airline2$day <- factor(airline2$day, levels = day_order)

#kids order
kids_order <- c("No kids", "kids")
airline2$kids <- factor(airline2$kids, levels = kids_order)

summary(airline2)

#Analysis_______________________________________________________________________
#____Visualizations_____________________________________________________________
# Distribution of satisfaction scores___________________________________________
fig1 <-  ggplot(airline2, aes(x = satisfaction)) + 
  geom_histogram(binwidth = 1, fill = "#191970") +
  labs(x = "Satisfaction Score", y = "Frequency", caption = "Distribution: Slightly left-skewed") + 
  theme_classic() + scale_x_continuous(breaks = seq(0, 10, by = 1)) + theme_classic() + theme(axis.text = element_text(size = 12, color = "black"),
                                                                                              axis.text.y = element_text(size = 12, color = "black", face = "bold"),
                                                                                              axis.title.y = element_text(size = 16, color = "black", face = "bold", margin = margin(r = 15)))

#print
fig1

#visualizing satisfaction by departure__________________________________________
fig2 <- airline2 %>% 
  filter(!is.na(departure)) %>% filter (!is.na(satisfaction)) %>% 
  ggplot(aes(x = departure, y = satisfaction, fill = departure)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", show.legend = FALSE) +
  labs(x = "", y = "Avg. Satisfaction") +
  scale_fill_brewer(palette = "Dark2") + coord_cartesian(ylim = c(5, 8)) + 
  theme_classic() + theme(axis.text = element_text(size = 12, color = "black"),
  axis.text.y = element_text(size = 12, color = "black", face = "bold"), 
  axis.title.y = element_text(size = 16, color = "black", face = "bold", 
  margin = margin(r = 15))) +  geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 2)), 
                                         position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5)

#print
fig2

#visualizing satisfaction by type_______________________________________________
fig3 <- airline2 %>% 
  filter(!is.na(type)) %>% 
  ggplot(aes(x = type, y = satisfaction, fill = type)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(y = "Satisfaction Score", fill = "Type",
       x = "", y = "Satisfaction Score") + scale_fill_brewer(palette = "Spectral") + 
  theme_classic() + theme(axis.text = element_text(size = 12, color = "black"),
                          axis.text.y = element_text(size = 12, color = "black", face = "bold"),
                          axis.title.y = element_text(size = 16, color = "black", face = "bold", margin = margin(r = 8)))

#print
fig3

#Visualizing business and connection____________________________________________
fig4 <- airline2 %>% filter(!is.na(business)) %>% filter(!is.na(connection)) %>% 
  filter(!is.na(satisfaction)) %>%
  ggplot(aes(x = business, y = satisfaction, fill = connection)) + 
  geom_boxplot() + labs(x = "", y = "Satisfaction Score", fill = "Connection") + 
  scale_fill_brewer(palette = "Set2") + theme_classic() + theme(axis.text = element_text(size = 12, color = "black"),
                                                                axis.text.y = element_text(size = 12, color = "black", face = "bold"),
                                                                axis.title.y = element_text(size = 16, color = "black", face = "bold", margin = margin(r = 6)))

#print
fig4

#regression_____________________________________________________________________
#regression analyzing satisfaction and departure
model1 <- lm(satisfaction ~ departure, data = airline2)
summary(model1)
tidy(model1)


#regression analyzing satisfaction, trip, and flight type
model2 <- lm(satisfaction ~ factor(trip) + type, data = airline2)
summary(model2)
tidy(model2)

#visualization

tidy_model2 <- tidy(model2, conf.int = TRUE)

r1 <- ggplot(data = subset(tidy_model2, term != c("(Intercept)")), 
             aes(y = term, x = estimate)) +
  geom_point() + geom_vline(xintercept = 0) + geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = 0.1)) +
  labs(y = element_blank()) +  theme_classic() + theme(axis.text = element_text(size = 12, color = "black"),
                                                       axis.text.y = element_text(size = 12, color = "black", face = "bold"),
                                                       axis.title.y = element_text(size = 16, color = "black", face = "bold", margin = margin(r = 6))) +
  scale_y_discrete(labels = c("Business ","Holiday flight ", "Intercontinental "))

#print
r1

#regression analysing satisfaction and departure, trip, connection, type
model3 <- lm(satisfaction ~ factor(departure) + factor(type) + factor(trip) + 
               factor(connect), data = airline2)
summary(model3)
tidy(model3)

tidy_model3 <- tidy(model3, conf.int = TRUE)

#visualization
r2 <- ggplot(data = subset(tidy_model3, term != c("(Intercept)")), 
             aes(y = term, x = estimate)) +
  geom_point() + geom_vline(xintercept = 0) + geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = 0.1)) +
  labs(y = element_blank())

#print
r2

#_______________________________________________________________________________
#regression analysing satisfaction and all variables
model4 <- lm(satisfaction ~ factor(departure) + route + factor(day) + 
               factor(type) + load + factor(gender) + factor(trip) + 
               factor(connect) + factor(kids), data = airline2)
summary(model4)
tidy(model4)

