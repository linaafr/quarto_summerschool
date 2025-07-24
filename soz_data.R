library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)

soz <- read_excel("/Users/linafricke/Documents/Promotion/Studie 1/06_Data/Soziodemografische Daten/Soziodemografische_Daten_und_Einflussfaktoren.xlsx")
View(soz)

# Number participants
nrow(soz$vp)


# rename SM Group 
soz$sm_group <- factor(soz$sm_group,
                       levels = c(1, 2),
                       labels = c("high", "low"))

#Plot Age

boxplot(x = soz$age,
        main = "Boxplot of Response Time",
        ylab = "Response Time (ms)",
        col = "lightgreen",
        border = "darkgreen",
        notch = TRUE)


# Percentage
barplot(height = prop.table(table(soz$sex)) * 100,
        ylab = "Prozent",
        col = c("lightblue", "lightpink"),
        border = "black",
        names.arg = c("weiblich", "männlich"))

mean(soz$age)



ggplot() + 
  geom_histogram(data = soz, aes(x = age,  
                                 fill = sm_group))+
  geom_vline(data = soz, aes(xintercept = mean(age, na.rm = TRUE)),
             linetype="dashed", size=1)+
  geom_vline(data = soz %>% filter(sm_group =="high"), 
             aes(xintercept= mean(age, na.rm = TRUE), 
                 colour = sm_group),
             linetype="dashed", 
             size=1)+
  geom_vline(data = soz %>% filter(sm_group =="low"),
             aes(xintercept=mean(age, na.rm = TRUE), 
                 colour = sm_group),
             linetype="dashed", 
             size=1)+
  labs(x="Age", y = "Number")+
  theme_minimal()


#Plot sex

soz$sex

library(ggplot2)

ggplot(soz, aes(x = sex)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Persons by sex",
       x = "Sex",
       y = "Number") +
  theme_minimal()



#Plot BMI

## === 2 Boxplots in einem Diagramm ===
boxplot(formula = soz$bmi ~ soz$sm_group, 
        col = c("red", "blue"),
        xlab = "SM-Nutzung", ylab = "BMI")

library(ggplot2)

# Plot: Histogramm + Normalverteilung
ggplot(soz, aes(x = bmi)) +
  geom_histogram(aes(y = ..density..),  # y-Achse auf Dichte setzen
                 bins = 20,             # Anzahl der Balken (kannst du anpassen)
                 fill = "lightblue",
                 color = "black") +
  stat_function(fun = dnorm,            # Normalverteilung hinzufügen
                args = list(mean = mean(soz$bmi, na.rm = TRUE),
                            sd = sd(soz$bmi, na.rm = TRUE)),
                color = "red", size = 1) +
  labs(title = "BMI-Verteilung mit Normalverteilungskurve",
       x = "BMI",
       y = "Dichte") +
  theme_minimal()

lines(density(na.omit(soz$bmi)),        # Add a smooth density curve
      col = "darkblue", lwd = 2)        # Line color and width



# 2 Histogramms (goups)

ggplot() + 
  geom_histogram(data = soz, aes(x = bmi,  
                                fill = sm_group))+
  geom_vline(data = soz, aes(xintercept = mean(bmi, na.rm = TRUE)),
             linetype="dashed", size=1)+
  geom_vline(data = soz %>% filter(sm_group =="1"), 
             aes(xintercept= mean(bmi, na.rm = TRUE), 
                 colour = sm_group),
             linetype="dashed", 
             size=1)+
  geom_vline(data = soz %>% filter(sm_group =="2"),
             aes(xintercept=mean(bmi, na.rm = TRUE), 
                 colour = sm_group),
             linetype="dashed", 
             size=1)





table(soz$sm_group)
table(soz$sex)

soz_clean <- soz %>%
filter(!is.na(sm_group), !is.na(sex)) %>%
  filter(sm_group %in% c("low", "high")) %>%
  filter(sex %in% c("weiblich", "männlich"))

tab <- table(soz_clean$sm_group, soz_clean$sex)
prop_tab <- prop.table(tab, margin = 1) * 100
prop_tab_t <- t(prop_tab)

barplot(height = prop_tab_t,
        beside = TRUE,
        col = c("lightblue", "lightpink"),
        border = "black",
        ylab = "Prozent",
        legend.text = rownames(prop_tab_t),
        args.legend = list(title = "Geschlecht", x = "topright"),
        names.arg = rownames(tab))


