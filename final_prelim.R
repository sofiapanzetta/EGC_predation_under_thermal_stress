#set working directory (home project folder)
setwd("/Users/panze/Desktop/MSc/code")

data_wd = "/Users/panze/Desktop/MSc/tabular_data/"

fig_wd = "/Users/panze/Desktop/MSc/figures/"


install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("conflicted")
install.packages("easypackages")


library(easypackages) # lets you load more than one package at once
libraries("conflicted", "tidyverse")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")


#loading data
prey23 = read.csv(paste0(data_wd, "prey_data.csv"))

prey23 <- subset(prey23, select = -X)
prey23 <- subset(prey23, select = -X.1)
prey23 <- subset(prey23, select = -X.2)

prey24 = read.csv(paste0(data_wd, "M24_prey_data.csv"))

prey24 <- subset(prey24, select = -X)
prey24 <- subset(prey24, select = -X.1)
prey24 <- subset(prey24, select = -X.2)
prey24 <- subset(prey24, select = -X.3)
prey24 <- subset(prey24, select = -X.4)

prey = rbind(prey23, prey24)

egc = read.csv(paste0(data_wd, "egc_data.csv"))

tank = read.csv(paste0(data_wd, "tank_parameter.csv"))


#creating histogram to view what gaps in data I have
#viewing gaps in temperature

#get average temp per trial
avg_temp <- tank %>% group_by(crab_id) %>% 
  summarise(mean_temp=mean(temperature),
            .groups = 'drop')

ggplot()+
  geom_histogram(data = avg_temp, aes(x=mean_temp), binwidth=1, fill = "lightblue", col = "black") +
  stat_count() +
  expand_limits(x= c(4, 26))+
  theme_classic() +
  theme(title = element_text(size=18))+
  theme(text = element_text(size=16))+
  labs(y = "Number of trials", x= "Temperature (°C)") +
  scale_x_continuous(breaks = seq(4, 26, by = 2), labels = seq(4, 26, by = 2)) +
  scale_y_continuous(breaks = seq(0, 30, by = 2), labels = seq(0, 30, by = 2))


#bigger bins
ggplot()+
  geom_histogram(data = avg_temp, aes(x=mean_temp), binwidth=4, fill = "lightblue", col = "black") +
  stat_count() +
  expand_limits(x= c(4, 26))+
  theme_classic() +
  theme(title = element_text(size=18))+
  theme(text = element_text(size=16))+
  labs(y = "Number of trials", x= "Temperature (°C)") +
  scale_x_continuous(breaks = seq(4, 26, by = 2), labels = seq(4, 26, by = 2)) +
  scale_y_continuous(breaks = seq(0, 30, by = 2), labels = seq(0, 30, by = 2))



#viewing gaps in carapace width
ggplot()+
  geom_histogram(data = egc, aes(x=carapace_width_before), binwidth=5, fill = "lightblue", col = "black") +
  stat_count() +
  expand_limits(x= c(40, 95))+
  theme_classic() +
  theme(title = element_text(size=18))+
  theme(text = element_text(size=16))+
  labs(y = "Number of trials", x= "Carapace width (mm)") +
  scale_x_continuous(breaks = seq(40, 110, by = 5), labels = seq(40, 110, by = 5))


#viewing gaps in body weight
ggplot()+
  geom_histogram(data = egc, aes(x=egc_weight_before), binwidth = 10, fill = "lightblue", col = "black") +
  stat_count() +
  expand_limits(x= c(10, 170))+
  theme_classic() +
  theme(title = element_text(size=18))+
  theme(text = element_text(size=16))+
  labs(y = "Number of trials", x= "Body weight (g)") +
  scale_x_continuous(breaks = seq(10, 170, by = 20), labels = seq(10, 170, by = 20))



#creating lm with polynomial terms for temp and prey consumed weight
prey <- prey %>% 
  mutate(eaten = as.factor(eaten))

consumed_prey = prey[prey$eaten == '1',]

consumed_prey <- consumed_prey[rowSums(is.na(consumed_prey)) != ncol(consumed_prey), ]


consumed_prey_weight <- consumed_prey %>% group_by(crab_id) %>% 
  summarise(consumed_weight=sum(prey_weight_lost))

prey_weight_temp <-  merge(consumed_prey_weight,avg_temp,by=c('crab_id'),all.x=T)

my.formula <- y ~ poly(x, 3, raw = TRUE)


ggplot(prey_weight_temp, aes(y=consumed_weight, x=mean_temp)) +
  geom_point(alpha=6/10, shape=21, fill="blue", colour="black", size=6) +
  geom_smooth(method = "lm", se = FALSE, 
              formula = my.formula, 
              colour = "red") +
  theme_classic() +
  theme(title = element_text(size=18)) +
  theme(text = element_text(size=18)) +
  labs(y = "Weight of prey consumed (g)", x= "Average temperature")


#scatterplot of carapace size vs temp

#new data frame
consumed_prey_weight <- consumed_prey %>% group_by(crab_id) %>% 
  summarise(consumed_weight=sum(prey_weight_lost))

crab_weight <- egc %>% 
  select(crab_id, egc_weight_before, carapace_width_before)

crab_prey_weights <-  merge(consumed_prey_weight,crab_weight,by=c('crab_id'),all.x=T)

temp_carapace_width <- merge(avg_temp,crab_weight,by="crab_id")

ggplot(temp_carapace_width, aes(mean_temp, carapace_width_before)) + 
  geom_point(alpha=3/5, shape=21, fill="blue", colour="black", size=5) +
  theme_classic() +
  expand_limits(x= c(4, 26), y= c(40, 110))+
  theme(title = element_text(size=18))+
  theme(text = element_text(size=18)) +
  labs(y = "Carapace width (mm)", x= "Average temperature") +
  scale_x_continuous(breaks = seq(4, 26, by = 2), labels = seq(4, 26, by = 2)) +
  scale_y_continuous(breaks = seq(40, 110, by = 10), labels = seq(40, 110, by = 10))


ggplot(temp_carapace_width, aes(mean_temp, carapace_width_before)) + 
  geom_point(alpha=3/5, shape=21, fill="blue", colour="black", size=8) +
  theme_classic() +
  expand_limits(x= c(4, 26), y= c(40, 110))+
  theme(title = element_text(size=18))+
  theme(text = element_text(size=16)) +
  labs(y = "Carapace width (mm)", x= "Average temperature") +
  scale_x_continuous(breaks = seq(4, 26, by = 2), labels = seq(4, 26, by = 2)) +
  scale_y_continuous(breaks = seq(40, 110, by = 10), labels = seq(40, 110, by = 10))


#glm grams of prey consumed by temp 
ggplot(prey_weight_temp, aes(y=consumed_weight, x=mean_temp)) + 
  theme_classic() +
  geom_point(alpha=3/5, shape=21, fill="blue", colour="black", size=8) +
  geom_smooth(method = "lm", se = FALSE, 
              formula = my.formula, 
              colour = "red") +
  theme(text = element_text(size=42)) +
  labs(y = "Weight of prey consumed (g)", x= "Average temperature (°C)")


#grams of prey to gram of crab by temp
prey_crab_prop<-crab_prey_weights %>% group_by(crab_id) %>% 
  summarise(gram_prop=consumed_weight/egc_weight_before)

prey_crab_prop_temp <- merge(prey_crab_prop,avg_temp,by=c('crab_id'),all.x=T)

ggplot(prey_crab_prop_temp, aes(y=gram_prop, x=mean_temp)) + 
  theme_classic() +
  geom_point(alpha=3/5, shape=21, fill="blue", colour="black", size=8) +
  geom_smooth(method = lm, color="red", se=FALSE) +
  theme(title = element_text(size=18)) +
  theme(text = element_text(size=16)) +
  labs(y = "Grams of prey consumed per gram of crab", x= "Average temperature")


#glm of grams prey to grams of crab by temo
ggplot(prey_crab_prop_temp, aes(y=gram_prop, x=mean_temp)) +
  geom_point(alpha=6/10, shape=21, fill="blue", colour="black", size=5) +
  geom_smooth(method = "lm", se = FALSE, 
              formula = my.formula, 
              colour = "red") +
  theme(title = element_text(size=20)) +
  theme(text = element_text(size=20)) +
  labs(y = "Grams of prey consumed per gram of crab", x= "Average temperature") +
  theme_classic()



#grams of prey by crab weight
prey_crab_prop_weight <- merge(prey_crab_prop,crab_prey_weights,by=c('crab_id'),all.x=T)


ggplot(prey_crab_prop_weight, aes(y=gram_prop, x=egc_weight_before)) + 
  theme_classic() +
  geom_point(alpha=3/5, shape=21, fill="blue", colour="black", size=8) +
  geom_smooth(method = "lm", color="red", se=FALSE) +
  theme(title = element_text(size=18)) +
  theme(text = element_text(size=16)) +
  labs(y = "Grams of prey consumed per gram of crab", x= "Crab weight (g)")


#glm of above
lm_grams_prey_per_crab_vs_crab_weight <- ggplot(prey_crab_prop_weight, aes(y=gram_prop, x=egc_weight_before)) +
  geom_point(alpha=6/10, shape=21, fill="blue", colour="black", size=5) +
  geom_smooth(method = "lm", se = FALSE, 
              formula = my.formula, 
              colour = "red") +
  theme(title = element_text(size=20)) +
  theme(text = element_text(size=20)) +
  labs(y = "Grams of prey consumed per gram of crab", x= "Crab weight (g)") +
  theme_classic()


#grams of prey by crab width

ggplot(prey_crab_prop_weight, aes(y=gram_prop, x=carapace_width_before)) + 
  theme_classic() +
  geom_point(alpha=3/5, shape=21, fill="blue", colour="black", size=8) +
  geom_smooth(method = "lm", color="red", se=FALSE) +
  theme(title = element_text(size=18)) +
  theme(text = element_text(size=16)) +
  labs(y = "Grams of prey consumed per gram of crab", x= "Crab carapace width (mm)")


#glm of above
ggplot(prey_crab_prop_weight, aes(y=gram_prop, x=carapace_width_before)) + 
  geom_point(alpha=6/10, shape=21, fill="blue", colour="black", size=5) +
  geom_smooth(method = "lm", se = FALSE, 
              formula = my.formula, 
              colour = "red") +
  theme_classic() +
  theme(title = element_text(size=18)) +
  theme(text = element_text(size=18)) +
  labs(y = "Grams of prey consumed per gram of crab", x= "Crab carapace width (mm)")


#binning by temps
temp_bins <- avg_temp %>% mutate(temp = case_when(
  mean_temp >= 18 ~ '>18°C',
  mean_temp >= 12  & mean_temp < 18 ~ '12-18°C',
  mean_temp < 12 ~ '<12°C')) 

bin_temp_crab_size <- merge(temp_bins,prey_crab_prop_weight,by="crab_id")

as.factor(bin_temp_crab_size$temp)

bin_temp_crab_size$temp <- factor(bin_temp_crab_size$temp, levels = c("<12°C", "12-18°C", ">18°C"))

leg_title = 'Temperature'

bin_temp_crab_size %>% 
  ggplot(aes(y=gram_prop, x=carapace_width_before, group=temp, colour=temp, fill=temp)) + 
  scale_fill_discrete(breaks=c('<12°C', '12-18°C', '>18°C')) +
  scale_color_manual(leg_title, values=c("steelblue", "orange", "firebrick3")) +
  scale_fill_manual(leg_title, values=c("steelblue", "orange", "firebrick3")) +
  theme_classic() +
  geom_point(alpha=8/10, shape=21, size=5) +
  geom_smooth(method = "lm", se = FALSE, 
              formula = my.formula) +
  theme(text = element_text(size=10)) +
  labs(y = "Mass specific prey consumption", x= "Crab carapace width (mm)")



#glm grams of prey consumed by temp binned by size
size_bins <- temp_carapace_width %>% mutate(size_class = case_when(
  carapace_width_before >= 81 ~ '>81mm',
  carapace_width_before >= 66  & mean_temp < 81 ~ '66-81mm',
  carapace_width_before < 66 ~ '<66mm')) 

bin_size_temp <- merge(size_bins,consumed_prey_weight,by="crab_id")

as.factor(bin_size_temp$size_class)

bin_size_temp$size_class <- factor(bin_size_temp$size_class, levels = c("<66mm", "66-81mm", ">81mm"))

Title = 'Carapace Width'

ggplot(bin_size_temp, aes(y=consumed_weight, x=mean_temp, colour = size_class, fill=size_class)) + 
  theme_classic() +
  scale_color_manual(Title, values=c("steelblue", "orange", "firebrick3")) +
  scale_fill_manual(Title, values=c("steelblue", "orange", "firebrick3")) +
  geom_point(alpha=3/5, shape=21, size=8) +
  geom_smooth(method = "lm", se = FALSE, 
              formula = my.formula) +
  theme(text = element_text(size=12)) +
  labs(y = "Weight of prey consumed (g)", x= "Average temperature (°C)")



