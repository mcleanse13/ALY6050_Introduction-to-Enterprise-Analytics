#ALY 6050 Module 2 Assignment

#Part 1
#(i)Perform Simulation of 10,000 benefit-cost ratios for Dam #1

#Specify the specification of the triangular distribution:
a <-0 
b <-19.1 
c <-14.2 

#Generate ten-million random values according to the standard uniform distribution:  
set.seed(123)
N <-10^4
r1 <-runif(N)
head(r1)

#Implement formula to generate ten million triangular random values (labeled as a1):
A <-a+sqrt((b-a)*(c-a)*r1) 
B <-b-sqrt((b-a)*(b-c)*(1-r1)) 
C <-(c-a)/(b-a) 
a1 <-ifelse(r1<C,A,B) 

#Perform Simulation of 10,000 benefit-cost ratios for Dam #2

#Specify the specification of the triangular distribution:
d <-0 
e <-20.1 
f <-15.8 

#Generate ten-million random values according to the standard uniform distribution:  
set.seed(456)
N <-10^4
r2 <-runif(N)
head(r2)

#Implement formula to generate ten million triangular random values (labeled as a2):
D <-d+sqrt((e-d)*(f-d)*r2) 
E <-e-sqrt((e-d)*(e-f)*(1-r2)) 
F <-(f-d)/(e-d) 
a2 <-ifelse(r2<F,D,E) 

#(ii)Construct both a tabular and a graphical frequency distribution for a1 and a2 separately (a 
#tabular and a graphical distribution for a1 and a tabular and a graphical distribution for a2- a 
#total of 4 distributions).
# Create a tabular distribution for a1
frequency_table <- table(a1)

# Display the frequency table
print(a1)

#Create a graphical frequency table for a1
hist(a1,freq=F,main="Distribution of the Simulation") 
lines(density(a1),lwd=2,col="blue") 

#Create a tabular distribution for a2
frequency_table <- table(a2)

#Display the frequency table
print(a2)

#Create a graphical frequency table for a2
hist(a2,freq=F,main="Distribution of the Simulation") 
lines(density(a2),lwd=2,col="red") 

#(iii)For each of the two dam projects, perform the necessary calculations in order to complete the 
#following table. 

#The observed and the theoretical numerical characteristics of data for Dam 1:

#Specify the specification of the triangular distribution for Dam 1 Benefits:
a3 <-0 
b3 <-14.6 
c3 <-9.8 

#Generate ten-million random values according to the standard uniform distribution:  
set.seed(689)
N <-10^4
r3 <-runif(N)
head(r3)

#Implement formula to generate ten million triangular random values:
A3 <-a3+sqrt((b3-a3)*(c3-a3)*r3) 
B3 <-b3-sqrt((b3-a3)*(b3-c3)*(1-r3)) 
C3 <-(c3-a3)/(b3-a3) 
benefit <-ifelse(r3<C3,A3,B3) 

#Specify the specification of the triangular distribution for Dam 1 Costs:
d4 <-3.5
e4 <-19.1
f4 <-14.2 

#Generate ten-million random values according to the standard uniform distribution:  
set.seed(456)
N <-10^4
r4 <-runif(N)
head(r4)

#Implement formula to generate ten million triangular random values:
D4 <-d4+sqrt((e4-d4)*(f4-d4)*r4) 
E4 <-e4-sqrt((e4-d4)*(e4-f4)*(1-r4)) 
F4 <-(f4-d4)/(e4-d4) 
cost <-ifelse(r4<F4,D4,E4)

#Specify the specification of the triangular distribution for Dam 2 Benefits:
a5 <-0 
b5 <-13.6 
c5 <-12.2

#Generate ten-million random values according to the standard uniform distribution:  
set.seed(485)
N <-10^4
r5 <-runif(N)
head(r5)

#Implement formula to generate ten million triangular random values:
A5 <-a5+sqrt((b5-a5)*(c5-a5)*r5) 
B5 <-b5-sqrt((b5-a5)*(b5-c5)*(1-r5)) 
C5 <-(c5-a5)/(b5-a5) 
benefits <-ifelse(r5<C5,A5,B5) 

#Specify the specification of the triangular distribution for Dam 2 Costs:
d6 <-3.8
e6 <-20.1
f6 <-15.8

#Generate ten-million random values according to the standard uniform distribution:  
set.seed(249)
N <-10^4
r6 <-runif(N)
head(r6)

#Implement formula to generate ten million triangular random values:
D6 <-d6+sqrt((e6-d6)*(f6-d6)*r6) 
E6 <-e6-sqrt((e6-d6)*(e6-f6)*(1-r6)) 
F6 <-(f6-d6)/(e6-d6) 
costs <-ifelse(r6<F6,D6,E6)

#Tabular Results - Dam 1

mean(benefit)  #Mean of the Total Benefits for Dam 1
(a3+b3+c3)/3 #Theoretical Mean of the Benefit-cost Ratio for Dam 1

sd(benefit)  #SD of the Total Benefits for Dam 1
sqrt((a3^2+b3^2+c3^2-a3*b3-a3*c3-b3*c3)/18) #Theoretical SD of the Benefit-Cost Ratio for Dam 1

mean(cost)  #Mean of the Total Cost for Dam 1
(d4+e4+f4)/3 #Theoretical Mean of the Benefit-cost Ratio for Dam 1

sd(cost)  #SD of the Total Cost for Dam 1
sqrt((d4^2+e4^2+f4^2-d4*e4-d4*f4-e4*f4)/18) #Theoretical SD of the Benefit-Cost Ratio for Dam 1

mean(a1)  #Observed Mean of the Benefit-cost Ratio for Dam 1
(a+b+c)/3 #Theoretical Mean of the Benefit-cost Ratio

sd(a1)  #Observed SD of the Benefit-cost Ratio for Dam 1
sqrt((a^2+b^2+c^2-a*b-a*c-b*c)/18) #Theoretical SD of the Benefit-Cost Ratio

#Tabular Results - Dam 2

mean(benefits)  #Mean of the Total Benefits for Dam 2
(a5+b5+c5)/3 #Theoretical Mean of the Benefit-cost Ratio for Dam 2

sd(benefits)  #SD of the Total Benefits for Dam 2
sqrt((a5^2+b5^2+c5^2-a5*b5-a5*c5-b5*c5)/18) #Theoretical SD of the Benefit-Cost Ratio for Dam 2

mean(costs)  #Mean of the Total Cost for Dam 2
(d6+e6+f6)/3 #Theoretical Mean of the Benefit-cost Ratio for Dam 2

sd(costs)  #SD of the Total Cost for Dam 2
sqrt((d6^2+e6^2+f6^2-d6*e6-d6*f6-e6*f6)/18) #Theoretical SD of the Benefit-Cost Ratio for Dam 2

mean(a2)  #Observed Mean of the Benefit-cost Ratio for Dam 2
(d+e+f)/3 #Theoretical Mean of the Benefit-cost Ratio for Dam 2

sd(a2)  #Observed SD of the Benefit-cost Ratio for Dam 2
sqrt((d^2+e^2+f^2-d*e-d*f-e*f)/18) #Theoretical SD of the Benefit-cost Ratio for Dam 2


#Creating a data frame for the tabular results - Dam 1
dam_titles <- c("Mean of the Total Benefits", "SD of the Total Benefits", "Mean of the Total Cost", "SD of the Total Cost", "Mean of the Benefit-cost Ratio", "SD of the Benefit-cost Ratio")
observed_values <- c(8.12, 3.04, 12.30, 3.22, 11.07, 4.03)
theoretical_values <- c(8.13, 3.04, 12.27, 3.26, 8.6, 3.05)

# Create a data frame
my_table <- data.frame(
  Dam_1 = dam_titles,
  Observed = observed_values,
  Theoretical = theoretical_values
)

# Print the data frame
print(my_table)

#Creating a data frame for the tabular results - Dam 2
dam_titles <- c("Mean of the Total Benefits", "SD of the Total Benefits", "Mean of the Total Cost", "SD of the Total Cost", "Mean of the Benefit-cost Ratio", "SD of the Benefit-cost Ratio")
observed_values <- c(8.53, 3.11, 13.31, 3.41, 12.01, 4.28)
theoretical_values <- c(8.6, 3.05, 13.23, 3.45, 12.27, 3.26)

# Create a data frame
my_table1 <- data.frame(
  Dam_1 = dam_titles,
  Observed = observed_values,
  Theoretical = theoretical_values
)

# Print the data frame
print(my_table1)


#Part 2: Create Goodness of Fit Test

#State the hypotheses
#H0: There is no difference in the estimates between the benefits and costs.
#H1: There is a difference in the estimates between the benefits and costs.

#Find the Critical Value
#df = 5, 18, a = 0.05, Critical Value = 4.96

#Set the significant level
alpha <- 0.05

dam3_titles <- c("b1", "b2", "b3", "b4", "b5", "b6")
dam4_titles <- c("c1", "c2")

#Create a data frame for benefit minimums
benefits_min <- c(1.1, 8, 1.4, 6.5, 1.7, 0)

#Create a data frame for benefit modes
benefits_mode <- c(2, 12, 1.4, 9.8, 2.4, 1.6)

#Create a data frame for benefit maximums
benefits_max <- c(2.8, 14.9, 2.2, 14.6, 3.6, 2.4)

#Create a data frame for costs minimums
costs_min <- c(13.2, 3.5)

#Create a data frame for costs modes
costs_mode <- c(14.2, 4.9)

#Create a data frame for costs maximums
costs_max <- c(19.1, 7.4)

# Create a benefits data frame
dam3 <- data.frame(
  BenefitCost = dam3_titles,
  Minimum = benefits_min,
  Mode = benefits_mode,
  Maximum = benefits_max
)

# Print the data frame
print(dam3)

# Create a costs data frame
dam4 <- data.frame(
  BenefitCost = dam4_titles,
  Minimum = costs_min,
  Mode = costs_mode,
  Maximum = costs_max
)

# Print the data frame
print(dam4)

#Combine data frames
dams <- rbind(dam3, dam4)
dams
View(dams)
str(dams)

# Revise the data frame for ANOVA testing
data <- data.frame(Category = dam3_titles, Benefits = benefits_min, benefits_max, benefits_mode, 
                   Costs = costs_min, costs_max, costs_mode)
data

#Run the ANOVA test
data <- aov(Benefits ~ Costs, data = data)

#View the model summary
summary(data)

#Save summary to an object
a.summary <- summary(data)

#Degrees of freedom
# k-1: between group variance ~ numerator
df.numerator <- a.summary[[1]][1, "Df"]
df.numerator

#N ~ k: within group variance ~ denominator
df.denominator <- a.summary[[1]][2, "Df"]
df.denominator

#Extract the F test value from the summary
F.value <- a.summary[[1]][[1, "F value"]]
F.value

#Extract the p-value from the summary
p.value <- a.summary[[1]][[1, "Pr(>F)"]]
p.value

#Make the decision
#Because the test value of 1.946 is less than the critical value of 4.96 we 
#should not reject the null hypothesis.

#Summarize the results, and explain where the differences in the means are.
#There is not enough evidence to conclude that a difference in estimate 
#amounts exists between the two categories, with the values being pretty similar
#in all three columns of benefits and costs in the triangular distribution.

#Part 3
#(i)Use the results of your simulations and perform the necessary calculations in order to complete the 
#table below.

install.packages("fBasics")
library(fBasics)

#Dam 1
summary(a1)
sd(a1)
var(a1)
skewness(a1)
ptriangular<-function(x,a,b,c) { 
  A <- (1/((b-a)*(c-a)))*(x-a)^2 
  B <- 1-(1/((b-a)*(b-c)))*(b-x)^2 
  Prob <- ifelse(x<c, A, B) 
  return(Prob) 
} 

1-ptriangular(2,0,19.1,14.2)
1-ptriangular(1.8,0,19.1,14.2) 
1-ptriangular(1.5,0,19.1,14.2)
1-ptriangular(1.2,0,19.1,14.2)
1-ptriangular(1,0,19.1,14.2)

#Dam 2
summary(a2)
sd(a2)
var(a2)
skewness(a2)

ptriangular<-function(x,a,b,c) { 
  A <- (1/((b-a)*(c-a)))*(x-a)^2 
  B <- 1-(1/((b-a)*(b-c)))*(b-x)^2 
  Prob <- ifelse(x<c, A, B) 
  return(Prob) 
} 

1-ptriangular(2,0,20.1,15.8)
1-ptriangular(1.8,0,20.1,15.8) 
1-ptriangular(1.5,0,20.1,15.8)
1-ptriangular(1.2,0,20.1,15.8)
1-ptriangular(1,0,20.1,15.8)

#Creating a data frame for the tabular results
dam_titles1 <- c("Minimum", "Maximum", "Mean", "Median", "Variance", "Standard Deviation",
           "SKEWNESS", "P(a1>2)", "P(a1>1.8)", "P(a1>1.5)", "P(a1>1.2)", "P(a1>1)")
observed_values1 <- c(0.13, 19.03, 11.07, 11.58, 16.22, 4.03,
                     -0.42, 0.985, 0.988, 0.992, 0.995, 0.996)
theoretical_values1 <- c(0.30, 20.04, 12.01, 12.65, 18.28, 4.28, -0.45,
                         0.987, 0.989, 0.993, 0.996, 0.997)

# Create a data frame
my_table2 <- data.frame(
  Dams = dam_titles1,
  a1 = observed_values1,
  a2 = theoretical_values1
)

# Print the data frame
print(my_table2)

#(ii)final conclusion of your report an estimate for the probability that a1
#will be greater than a2 
# H0: (null hypothesis) - μ1 ≤ μ2, 
# H1: (alternative hypothesis) - μ1 > μ2

# Perform a one-sided t-test
result <- t.test(a1, a2, alternative = "greater")

# Extract the p-value
p_value <- result$p.value

# Calculate the estimated probability that mean(a1) > mean(a2)
estimated_probability <- 1 - p_value

# Print the result
print(result)
cat("Estimated probability that mean(a1) > mean(a2):", estimated_probability, "\n")
