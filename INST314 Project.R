library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)

#Problem statement: Do factors like lunch type, taking test preparation courses and parental education have an effect on the test scores of students?

examsDataset <- read.csv('exams.csv', stringsAsFactors = T)
view(examsDataset)

#Delete the 'ethnicity' column, since this data was left anonymous, and no conclusions can be drawn based off of this column
examsDataset = subset(examsDataset, select = -c(race.ethnicity) )
view(examsDataset)


#Create a new column that sums the scores of all 3 categories and name it 'sum.scores'
examsDataset$sum.scores <- examsDataset$math.score + examsDataset$reading.score + examsDataset$writing.score

#Hypothesis: There is a higher test average with students who have better financial circumstances (standard priced lunches, higher parental education, affording test prep courses)
#Dependent variable: Student Test average
#Independent variable: Financial circumstances (Lunch type, parental education, test prep courses)


#Descriptive Statistics for the dataset
#View the SD for each score category based on test preparation (none or completed)
#You can tell by the mean that the test scores are higher for each category based on completed test preparation
examsDataset %>%
  group_by(test.preparation.course) %>%
  get_summary_stats(math.score, type="mean_sd")

examsDataset %>%
  group_by(test.preparation.course) %>%
  get_summary_stats(reading.score, type="mean_sd")

examsDataset %>%
  group_by(test.preparation.course) %>%
  get_summary_stats(writing.score, type="mean_sd")

examsDataset %>%
  group_by(test.preparation.course) %>%
  get_summary_stats(sum.scores, type="mean_sd")

#Creating box plots based off of the data provided, scores are higher based on completion of test preparation
ggboxplot(examsDataset, x = 'test.preparation.course', y = 'math.score')
ggboxplot(examsDataset, x = 'test.preparation.course', y = 'reading.score')
ggboxplot(examsDataset, x = 'test.preparation.course', y = 'writing.score')
ggboxplot(examsDataset, x = 'test.preparation.course', y = 'sum.scores')


#Histogram to see the spread of different scores
hist(examsDataset$sum.scores)


#View some descriptive statistics based on lunch type and test scores
examsDataset %>%
  group_by(lunch) %>%
  get_summary_stats(sum.scores, type="mean_sd")

#Some visuals for the data above
ggboxplot(examsDataset, x = 'lunch', y = 'sum.scores')


write.csv(examsDataset, file = 'updatedexams.csv')



#We will be conducting multiple significance tests for each category
#Starting with test prep courses using the independent T-test
#Check for any outliers 
examsDataset %>%
  group_by(test.preparation.course) %>%
  identify_outliers(sum.scores)
#2 outliers but they aren't extreme


#Normality test using Shapiro Wilk test
examsDataset %>%
  group_by(test.preparation.course) %>%
  shapiro_test(sum.scores)

#Creating a gg plot to visualize the distribution of data. Most of the data is inside of the trend line
ggqqplot(examsDataset, x = "sum.scores", facet.by = "test.preparation.course")

#Homogenity assumption test: Levene's test
#p is non-significant, assumption is met
examsDataset %>%
  levene_test(sum.scores ~ test.preparation.course)

#Computing t
IToutput <- examsDataset %>%
  t_test(sum.scores ~ test.preparation.course) %>%
  add_significance()
IToutput
#p-value is < 0.05, statistically significant, which indicates that there is a relationship between whether or not a student
#takes a test prep course and their score on the test

ggdensity(examsDataset, x = "sum.scores", rug = TRUE, fill = "lightgray") +
  scale_x_continuous(limits = c(15, 27)) +
  stat_central_tendency(type = "mean", color = "red", linetype = "dashed") +
  geom_vline(xintercept = 25, color = "blue", linetype = "dashed") + 
  labs(subtitle = get_test_label(stat.test,  detailed = TRUE))

#Create a boxplot to visualize significance test
bxp <- ggboxplot(
  examsDataset, x = "test.preparation.course", y = "sum.scores", 
  ylab = "Total Test Score", xlab = "Test Prep Completed", add = "jitter"
)
bxp

IToutput <- IToutput %>% add_xy_position(x = "test.preparation.course")
bxp + 
  stat_pvalue_manual(IToutput, tip.length = 0) +
  labs(subtitle = get_test_label(IToutput, detailed = TRUE))



#Independent sample T-test for gender and test scores

## check for outliers
examsDataset %>%
  group_by(gender) %>%
  identify_outliers(sum.scores)

# 2 outliers were identified in the female gender group. But they were not
# extreme so we won't remove them

# check normality using the Shapiro Wilk test
examsDataset %>%
  group_by(gender) %>%
  shapiro_test(sum.scores)

# p is substantially smaller than 0.05. Visualize normality using qq plot

ggqqplot(examsDataset, x = "sum.scores", facet.by = "gender")

# p is significant

# compute t
t.test(sum.scores ~ gender, data = examsDataset,var.equal=TRUE,paired=FALSE)

IToutput <- examsDataset %>%
  t_test(sum.scores ~ gender) %>%
  add_significance()
IToutput

# p value < 0.05. significant. There is significant different
# between the scores between female and male genders


#One way ANOVA test for the parental level of education category
examsDataset %>%
  group_by(parental.level.of.education) %>%
  get_summary_stats(sum.scores, type="mean_sd")

# box plot to understand data
bxp <- ggboxplot(examsDataset, x = "parental.level.of.education", y = "sum.scores", add = "point") 
bxp

## outliers
examsDataset %>%
  group_by(parental.level.of.education) %>%
  identify_outliers(sum.scores)
# 4 data points were identified as outliers, but not extreme outliers. 


# build linear model
model <- lm(sum.scores ~parental.level.of.education, data = examsDataset)

# use the Shapiro Wilk test of normality 
shapiro_test(residuals(model))

# the p value of the Shapiro test is less than 0.05. It is significant

# check the homogeneity of variance assumption
# Levenes' test of homogeneity
examsDataset %>%
  levene_test(sum.scores ~ parental.level.of.education)

## Shapiro test
examsDataset %>%
  group_by(parental.level.of.education) %>%
  shapiro_test(sum.scores)

#qqplot 
ggqqplot(examsDataset, "sum.scores", facet.by = "parental.level.of.education")

#compute the ANOVA
pg.aov <-  anova_test(data = examsDataset, sum.scores ~ parental.level.of.education )
pg.aov
# The p-value is less than 0.05. So there is significant difference

# Tukey's test shows the specific groups between which the difference exists
pg.pwc <- examsDataset %>% tukey_hsd(sum.scores ~ parental.level.of.education)
pg.pwc



#Independent sample T-test for lunch and their effect on test scores

## check for outliers
examsDataset %>%
  group_by(lunch) %>%
  identify_outliers(sum.scores)

# 1 outlier was identified in the standard lunch group. But it was not
# too far removed from the rest of the data so we won't remove it

# checking the normality using Shapiro Wilk test
examsDataset %>%
  group_by(lunch) %>%
  shapiro_test(sum.scores)

# p is substantially smaller than 0.05 for standard priced meals, but this is not the case 
# for those with free/reduced meals. 

# Now we visualize the normality using a qq plot

ggqqplot(examsDataset, x = "sum.scores", facet.by = "lunch")
ggboxplot(examsDataset, x = 'lunch', y = 'sum.scores')

# p is significant, and the data falls in the expected/calculated range for both free/reduced 
# and standard lunch

# computing t
IToutput <- examsDataset %>%
  t_test(sum.scores ~ lunch) %>%
  add_significance()
IToutput

# p value is much less 0.05, meaning it is statistically significant. There is significant difference
# between the scores between people who get free/reduced lunch and those who get the standard lunch.


