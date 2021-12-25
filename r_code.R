setwd("~/Desktop/SIM/ST2195/prac/st2195_Assignment_7")
install.packages("ggplot2")
library(ggplot2)

# Load data
titanic <- read.csv("titanic.csv")
str(titanic)

# Alternative is to do the changes for some variables prior to plotting.
# titanic$survived <- factor(titanic$survived, level s=c(0,1), labels=c("No", "Yes"))
# titanic$pclass <- factor(titanic$pclass, level s=c(1,2,3), labels=c("1st", "2nd", "3rd"))

# 1. Generate a series of bar charts to describe the : (a) gender, (b) ticket class, and (c) survival of the passengers onboard.

# 1a - Bar chart for gender
ggplot(titanic, aes(x=Sex, fill=Sex)) +
  geom_bar() +
  labs(title="Gender of the passangers onbroad")
  #ggtitle('Gender of the Passangers') #alternative way to set title
  
# 1b - Bar chart for ticket class (simple version)
ggplot(titanic, aes(x=Pclass, fill=factor(Pclass))) +
  geom_bar() +
  labs(title="Ticket class of the passangers onbroad") +
  xlab("ticket class") +
  theme(legend.position="none")
  
# 1b - Bar chart for ticket class (less simple version)
library(dplyr)
titanic %>%
  mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st","2nd","3rd"))) %>%
  ggplot(aes(x=Pclass, fill=Pclass)) +
  geom_bar() +
  ggtitle("ticket class of the passengers onbroad") +
    xlab("ticket class") +
    theme(legend.position="none")

# 1c - Bar chart for survival
titanic %>%
  mutate(Survived =factor(x=Survived, levels=c(0,1), labels=c("No","Yes"))) %>%
  ggplot(aes(x=Survived, fill=Survived)) +
  geom_bar() +
  ggtitle("Survival of the passengers onbroad") +
  theme(legend.position="none")

# 2. Generate a histogram for the passengers’ age. Furthermore, describe the passengers’ age using the following two boxplots: (a) age per ticket class and (b) age based on survival.

# 2a. Histogram(show frequency)
ggplot(titanic, aes(x=Age)) +
  geom_histogram(fill="steelblue", bins=10, na.rm=TRUE) +
  ggtitle("Distribution of age of the passengers onboard")

# 2a. Histogram(show density instead of frequency)
ggplot(titanic, aes(x=Age)) +
  geom_density(na.rm=TRUE) +
  ggtitle("Distribution of age of the passengers onboard")

# 2a. Histogram(show density and frequency)
ggplot(titanic, aes(x=Age, y=..density..)) +
  geom_histogram(fill="steelblue", bins=10, na.rm=TRUE) +
  geom_density(na.rm=TRUE) +
  ggtitle("Distribution of age of the passengers onboard")

# 2b. Boxplot (group by ticket class)
titanic %>%
  mutate(Pclass =factor(x=Pclass,  levels=c(1,2,3), labels=c("1st","2nd","3rd"))) %>%
  ggplot(aes(x=Pclass, y=Age, fill=Pclass)) +
   geom_boxplot(na.rm=TRUE) +
   ggtitle("Age of the passengers onbroad") +
   xlab("ticket class") +
   theme(legend.position="none")

# 2c. Boxplot (group by survival)
titanic %>%
  mutate(Survived =factor(x=Survived, levels=c(0,1), labels=c("No","Yes"))) %>%
  mutate(Pclass=factor(x=Pclass,  levels=c(1,2,3), labels=c("1st","2nd","3rd"))) %>%
  ggplot(aes(x=Survived, y=Age, fill=Survived)) +
      geom_boxplot(na.rm=TRUE) +
      ggtitle("Age of the passengers onbroad") +
      theme(legend.position="none")

# 3. Generate a histogram for the travel fare and a table showing the number of people who did not pay – you may want to check on Google why a handful of people was on board for free!

# some people could get the ticket for free because some of them maybe are the employees of the company, etc.

# 3a. Histogram for ticket fair
ggplot(titanic, aes(x=Fare, y=..density..)) +
  geom_histogram(fill="steelblue", bins=30, na.rm=TRUE) +
  ggtitle("Ticket fare of the passengers onboard")
  
# 3b. Table for paid vs unpaid
paid_table <- table(titanic$Fare != 0)
names(paid_table) <- c("did not pay", "paid")
paid_table

# 4. A chart of your choice to describe the family size per ticket class.

titanic$family_size <- titanic$SibSp + titanic$Parch + 1 #compute family size
table(titanic$family_size)

titanic %>%
  mutate(Pclass=factor(x=Pclass,  levels=c(1,2,3), labels=c("1st","2nd","3rd"))) %>%
  ggplot(aes(x=family_size, y=..density.., fill=Pclass)) +
  geom_histogram(bins=11) +
  facet_grid(Pclass ~ ., scales="free") +
  ggtitle("Family size per ticket class") 

titanic %>%
  mutate(Pclass=factor(x=Pclass,  levels=c(1,2,3), labels=c("1st","2nd","3rd"))) %>%
  ggplot(aes(x=family_size, y=..density.., fill=Pclass)) +
  geom_histogram(bins=11) +
  facet_grid(~ Pclass) +
  ggtitle("Family size per ticket class") 

# 5. A series of stacked bar charts to show the how survival differs for different (a) gender and  (b) ticket class

# 5a. Bar chart - survival by Gender
titanic %>%
  mutate(Survived =factor(x=Survived, levels=c(0,1), labels=c("No","Yes"))) %>%
  ggplot(aes(x=Sex, fill=Survived)) +
  geom_bar(position = "stack") +
  ggtitle("Survival by gender") +
  xlab("gender")

# 5b. Bar chart - survival by Ticket class

# Position='stack'
x <- titanic %>%
  mutate(Survived =factor(x=Survived, levels=c(0,1), labels=c("No","Yes"))) %>%
  mutate(Pclass=factor(x=Pclass,  levels=c(1,2,3), labels=c("1st","2nd","3rd"))) %>%
  ggplot(aes(x=Sex, fill=Survived)) +
  geom_bar(position = "stack") +
  ggtitle("Survival by ticket class") +
  xlab("ticket class")
x

# Position='stack' with data labels (1)
x + geom_text(aes(label=..count..),
              stat="count",
              size=3,
              position="stack",
              vjust=1.5)

# Position='stack' with data labels (2)
totals <- titanic %>% group_by(Pclass) %>% summarise(total=n())
x + geom_text(data = totals,
              aes(x=Pclass, y=total, label=total, fill=NULL),
              size = 3,
              nudge_y = 30)

# Position ="dodge"
titanic %>%
  mutate(Survived =factor(x=Survived, levels=c(0,1), labels=c("No","Yes"))) %>%
  mutate(Pclass=factor(x=Pclass,  levels=c(1,2,3), labels=c("1st","2nd","3rd"))) %>%
  ggplot(aes(x=Pclass, fill=Survived)) +
  geom_bar(position = "dodge") +
  ggtitle("Survival by ticket class") +
  xlab("ticket class")
  geom_text(
    aes(label=..count..),
    stat="count",
    size=3,
    position=position_dodge(width=0.9),
    vjust=1.5)

titanic %>%
  mutate(Survived =factor(x=Survived, levels=c(0,1), labels=c("No","Yes"))) %>%
  mutate(Pclass=factor(x=Pclass,  levels=c(1,2,3), labels=c("1st","2nd","3rd"))) %>%    
  ggplot(aes(x=Pclass, fill=Survived)) +
  geom_bar(position = "identity") +
  ggtitle("Survival by ticket class") +
  xlab("ticket class")

# 6. A violin chart describing how survival related to age and gender
titanic %>%
  mutate(Survived =factor(x=Survived, levels=c(0,1), labels=c("No","Yes"))) %>%
  ggplot(aes(x=Sex, y=Age, fill=Survived)) +
  geom_violin(na.rm=TRUE, adjust=1) #adjust bandwidth for density fit

# 7. A violin chart describing the survival rate related to age and ticket class
titanic %>%
  mutate(Survived =factor(x=Survived, levels=c(0,1), labels=c("No","Yes"))) %>%
  mutate(Pclass=factor(x=Pclass,  levels=c(1,2,3), labels=c("1st","2nd","3rd"))) %>%
  ggplot(aes(x=Pclass, y=Age, fill=Survived)) +
  geom_violin(na.rm=TRUE, adjust=1) +
  xlab("ticket class") #adjust bandwidth for density fit

# From the graphs above, what kind of associations between the variables like age, gender, ticket class and the survival do you observe?
# OBSERVATIONS : (i) children survival rates look higher, (ii) survival rates of older people, regardless of gender, look lower
titanic %>%
  mutate(Survived =factor(x=Survived, levels=c(0,1), labels=c("No","Yes"))) %>%
  mutate(Pclass=factor(x=Pclass,  levels=c(1,2,3), labels=c("1st","2nd","3rd"))) %>%
  ggplot(aes(x=Pclass, y=Age, fill=Survived)) +
  geom_violin(na.rm=TRUE, adjust=1) +
  facet_grid(Sex~.) +
  xlab("ticket class") #adjust bandwidth for density fit

remove(volume)
remove(volume_vector)
remove(r)

  