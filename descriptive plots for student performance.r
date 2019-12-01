
library(ggplot2)
library(GGally)
library(gridExtra)
library(magrittr)
library(ggpubr)


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
View(sum_score)#check

#view multiple plots

grid.arrange(p1,p2,ncol=2)
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

grid.arrange(b,b1,b2,legend,ncol=3,nrow=2)






### below to be reviewed

#normal distribution


p_distribution<-hist(datafr$math.score)
mu=mean(datafr$math.score)
sigma=sd(datafr$math.score)
distribution<-dnorm(datafr$math.score,mean=mu,sd=sigma)

#test
ggplot(datafr, aes(Average.score)) + geom_density(aes(y=5 * ..count..))+ geom_histogram(binwidth=5, color="white", aes(fill=Prep.Course))+ xlab("Average Scores") + ylab("Prep.Course") + ggtitle("Average Scores by Prep course")

?geom_density

p_distribution


p7 <- ggplot(datafr, aes(math.score))  + 
  geom_histogram(aes(y = ..density..), binwidth = 5, colour = "black",fill='blue') + 
  stat_function(fun = dnorm, args = list(mean = mean(datafr$math.score), sd = sd(datafr$math.score)),colour='red')
p7<-p7 + xlab("Math Scores") + ylab("number of students") + ggtitle("Math Scores by Prep course")
p7


?stat_function
