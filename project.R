#Part 1
#Load file 
bf=read.csv('/Users/LENOVO/Desktop/SM/Lab/Project/bodyPerformance.csv')
#explore the data
#to Know the structure of the file
str(bf)
# to find the summary 
summary(bf)
#See the first 6 rows
head(bf)
# See the last 6 rows
tail(bf)
# View the data set
View(bf)
#Lists name of variables in a dataset
names(bf)


#Descriptive statistics
summary(bf)
str(bf)
#for the age
mean(bf$age)
median(bf$age)
quantile(bf$age)
quantile(bf$age, c(.32, .57, .98))
max(bf$age)
min(bf$age)
max(bf$age)-min(bf$age) # range
IQR(bf$age)
var(bf$age)
sd(bf$age)

#for the broad.jump_cm 
mean(bf$broad.jump_cm)
median(bf$broad.jump_cm)
quantile(bf$broad.jump_cm)
quantile(bf$broad.jump_cm, c(.20, .67, .88))
max(bf$broad.jump_cm)
min(bf$broad.jump_cm)
max(bf$broad.jump_cm)-min(bf$broad.jump_cm)
IQR(bf$broad.jump_cm)
var(bf$broad.jump_cm)
sd(bf$broad.jump_cm)

#for the body.fat_.
mean(bf$body.fat_.)
median(bf$body.fat_.)
quantile(bf$body.fat_.)
quantile(bf$body.fat_., c(.27, .47, .98))
max(bf$body.fat_.)
min(bf$body.fat_.)
max(bf$body.fat_.)-min(bf$body.fat_.)
IQR(bf$body.fat_.)
var(bf$body.fat_.)
sd(bf$body.fat_.)

#Data Visualization
#pie
class=bf$class
class.freq=table(class)
class.freq
class.relfreq =class.freq/nrow(bf)

class.relfreq
pie(class.relfreq)
class.freq

gender=bf$gender
gender.freq=table(gender)
gender.freq
gender.relfreq =gender.freq/nrow(bf)

gender.relfreq

pie(gender.relfreq)

#barplot
age=bf$age
age.freq=table(age)
age.freq
age.relfreq =age.freq/nrow(bf)

age.relfreq
barplot(age.relfreq)

barplot(gender.relfreq)

#plot
cor(bf$height_cm , bf$weight_kg)
plot(bf$height_cm , bf$weight_kg)

cor(bf$body.fat_. , bf$weight_kg)
plot(bf$body.fat_., bf$weight_kg)



hTest = read.csv("Test.csv")
str(hTest)
summary(hTest)


# Linear Regression
model1 = lm(broad.jump_cm ~ sit.ups.counts, data=bf)
summary(model1)


# Sum of Squared Errors
model1$residuals
SSE = sum(model1$residuals^2)
SSE

predictTrain = predict(model1, newdata=bf)
predictTrain

# Compute R-squared
SSE = sum((bf$broad.jump_cm - predictTrain)^2)
SST = sum((bf$broad.jump_cm - mean(bf$broad.jump_cm))^2)
1 - SSE/SST

predictTest = predict(model1, newdata=hTest)
predictTest

# Compute R-squared
SSE = sum((hTest$broad.jump_cm  - predictTest)^2)
SST = sum((hTest$broad.jump_cm  - mean(hTest$broad.jump_cm ))^2)
1 - SSE/SST

# Linear regression with many variables
model = lm(body.fat_. ~ weight_kg + height_cm + sit.ups.counts + age+systolic, data=bf)
summary(model)

# Sum of Squared Errors
model$residuals
SSE = sum(model$residuals^2)
SSE

predictTrain = predict(model, newdata=bf)
predictTrain

# Compute R-squared
SSE = sum((bf$body.fat_. - predictTrain)^2)
SST = sum((bf$body.fat_. - mean(bf$body.fat_.))^2)
1 - SSE/SST

predictTest = predict(model, newdata=hTest)
predictTest

# Compute R-squared
SSE = sum((hTest$body.fat_.  - predictTest)^2)
SST = sum((hTest$body.fat_. - mean(hTest$body.fat_.))^2)
1 - SSE/SST

#part2
di = read.csv('/Users/LENOVO/Desktop/SM/Lab/Project/diabetes.csv')
str(di)
summary(di)

#the average of BMI
mean(di$BMI)

#maximum and minimum value for BloodPressure
max(di$BloodPressure)
min(di$BloodPressure)

# Which age are in the bottom 25% of Glucose and the top 25% of the Glucose?

perc25 <- quantile(di$Glucose, c(.25))
perc75 <- quantile(di$Glucose, c(.75))

# Top 25% of Glucose
di[di$Glucose > perc75, 'Age']

# Bottom 25% of Glucose
di[di$Glucose < perc25, 'Age']

# distributions of variable
#Histogram
par("mfcol"=c(3, 3))
hist(di$Pregnancies)
hist (di$Glucose)
hist(di$BloodPressure)
hist(di$SkinThickness)
hist(di$Insulin)
hist(di$BMI)
hist(di$DiabetesPedigreeFunction)
hist(di$Age)
hist(di$Outcome)
par("mfcol"=c(1, 1))

#correlation
cor(di)
