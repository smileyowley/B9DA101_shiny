
library(ggplot2)
library(GGally)
library(gridExtra)
library(magrittr)
library(ggpubr)
library(ggthemes)
library(dplyr)
library(plot3D)

#Data source
SourceLink = "https://www.kaggle.com/spscientist/students-performance-in-exams/download/StudentsPerformance.csv"

#Importing data
filename = "StudentsPerformance.csv"
datafr <- read.csv(filename)


View(datafr)

#creating histograms

#math scores by gender
p1=ggplot(datafr, aes(math.score)) + geom_histogram(binwidth=5, color="white", aes(fill=gender))
p1 <- p1 + xlab("Math Scores") + ylab("Gender") + ggtitle("Math Scores by Gender")
p1
                     

#reading score by gender
p2=ggplot(datafr, aes(reading.score)) + geom_histogram(binwidth=5, color="white", aes(fill=gender))
p2 <- p2 + xlab("Reading Scores") + ylab("Gender") + ggtitle("Reading Scores by Gender")
p2

#writing score by gender
p3=ggplot(datafr, aes(writing.score)) + geom_histogram(binwidth=5, color="white", aes(fill=gender))
p3 <- p3 + xlab("Writing Scores") + ylab("Gender") + ggtitle("Writing Scores by Gender")
p3

#average score by preparatio  course completed
datafr$Average.score<-NA #creating a new column
sum_score<-datafr$math.score+datafr$reading.score+datafr$writing.score
datafr$Average.score<-sum_score/3
names(datafr)[5]<-'Prep.Course' #renaming as the column name is too long for the legend


p4=ggplot(datafr, aes(Average.score)) + geom_histogram(binwidth=5, color="white", aes(fill=Prep.Course))
p4 <- p4 + xlab("Average Scores") + ylab("Prep.Course") + ggtitle("Average Scores by Prep course")
p4



View(datafr)#check

#view multiple plots

grid.arrange(p1,p2,p3,p4,ncol=2,nrow=2)




# Boxplot of scores and Test Prep by Gender
b <- ggplot(datafr, aes(gender, writing.score, color = Prep.Course))
b <- b + geom_boxplot()
b <- b + ggtitle("Writing scores by Gender")
b <- b + xlab("Gender") + ylab("Writing Scores")
b

b1 <- ggplot(datafr, aes(gender, math.score, color = Prep.Course))
b1 <- b1 + geom_boxplot()
b1 <- b1 + ggtitle("Math scores by Gender")
b1 <- b1 + xlab("Gender") + ylab("Math Scores")
b1

b2 <- ggplot(datafr, aes(gender, reading.score, color = Prep.Course))
b2 <- b2 + geom_boxplot()
b2 <- b2 + ggtitle("Reading scores by Gender")
b2 <- b2 + xlab("Gender") + ylab("Reading Scores")
b2

#multiple boxplots

boxplot_rep<-ggarrange(b, b1, b2,
          common.legend = TRUE, legend = "bottom", nrow = 1, ncol = 3)
multi_boxplot<-annotate_figure(boxplot_rep, top = text_grob("Boxplot visualization", color = "red", face = "bold", size = 14))
multi_boxplot

#visualisation median total score by parental education and lunch

datafr %>% 
  mutate(total_score = math.score+reading.score+writing.score) %>% 
  group_by(education = parental.level.of.education, lunch) %>% 
  summarise(median_score = median(total_score)) %>% 
  ggplot(aes(x = education, y = median_score, fill = lunch))+
  geom_col(position='dodge',col='black')+
  theme_hc()+
  scale_fill_manual(values=ggthemes_data$hc$darkunica[c(4,2)])+
  labs(x='',y='Median Total Score', fill = 'Lunch')+
  geom_text(aes(label=median_score),position=position_dodge(width=1), vjust =-.5)

#frequency of ethnicities by gender

datafr %>% 
  count(gender,race.ethnicity) %>% 
  ggplot(aes(x=gender,y=n,fill=race.ethnicity))+
  geom_bar(stat='identity',position='dodge', col='black')+
  theme_hc()+
  scale_fill_hc('darkunica')+
  labs(x='',y='Number of students', fill='Race group')+
  geom_text(aes(label=n),position = position_dodge(width=1), vjust=-.5)


#correlation between each exam score

l0<-scatter3D(x = datafr$math.score, y = datafr$reading.score, z =datafr$writing.score,  xlab = "Maths Marks", ylab = "Reading Marks", zlab = "Writing Marks", phi = 0, bty ="g", main = "Exam Marks of Students", ticktype = "detailed")        
l0

l1 <- ggplot(datafr, aes( reading.score,math.score)) +
  geom_point() +
  stat_smooth(method = lm)
l1


l2 <- ggplot(datafr, aes( writing.score,math.score)) +
  geom_point() +
  stat_smooth(method = lm)
l2


l3 <- ggplot(datafr, aes( writing.score,reading.score)) +
  geom_point() +
  stat_smooth(method = lm)
l3

grid.arrange(l1,l2,l3,ncol=2,nrow=2)


#for fun
l4 <- ggplot(datafr, aes( writing.score,reading.score)) +
  geom_point(aes(colour=datafr$race.ethnicity)) +
  stat_smooth(method = lm)
l4<-l4+labs(colour = "Ethnicity")
l4

#normal distribution example


p7 <- ggplot(datafr, aes(math.score))  + 
  geom_histogram(aes(y = ..density..), binwidth = 5, colour = "black",fill='green') + 
  stat_function(geom='point',fun = dnorm, args = list(mean = mean(datafr$math.score), sd = sd(datafr$math.score)),colour='red')
p7<-p7 + xlab("Math Scores") + ylab("number of students") + ggtitle("Math Scores by Prep course")
p7


### below to be reviewed

#normal distribution


p_distribution<-hist(datafr$math.score)
mu=mean(datafr$math.score)
sigma=sd(datafr$math.score)
distribution<-dnorm(datafr$math.score,mean=mu,sd=sigma)

#test
ggplot(datafr, aes(Average.score)) + geom_density(aes(y=5 * ..count..))+ geom_histogram(binwidth=5, color="white", aes(fill=Prep.Course))+ xlab("Average Scores") + ylab("Prep.Course") + ggtitle("Average Scores by Prep course")





