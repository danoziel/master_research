set.seed(1234)
library(wakefield)
df.patients <- r_data_frame(n = 250, 
                            age(x = 30:78, 
                                name = 'Age'), 
                            sex(x = c("Male", "Female"), 
                                prob = c(0.70, 0.30), 
                                name = "Sex"))
df.patients$Sample <- as.factor('Patients')

summary(df.patients)

set.seed(1234)
df.population <- r_data_frame(n = 1000, 
                              age(x = 18:80, 
                                  name = 'Age'), 
                              sex(x = c("Male", "Female"), 
                                  prob = c(0.50, 0.50), 
                                  name = "Sex"))
df.population$Sample <- as.factor('Population')

summary(df.population)


mydata <- rbind(df.patients, df.population)
mydata$Group <- as.logical(mydata$Sample == 'Patients')
mydata$Distress <- ifelse(mydata$Sex == 'Male', age(nrow(mydata), x = 0:42, name = 'Distress'),
                          age(nrow(mydata), x = 15:42, name = 'Distress'))

pacman::p_load(tableone)
table1 <- CreateTableOne(vars = c('Age', 'Sex', 'Distress'), 
                         data = mydata, 
                         factorVars = 'Sex', 
                         strata = 'Sample')

set.seed(1234)
match.it <- matchit(Group ~ Distress, data = mydata, method="nearest", ratio=1)
a <- summary(match.it)

plot(match.it, type = 'jitter', interactive = FALSE)

df.match <- match.data(match.it)[1:ncol(mydata)]
rm(df.patients, df.population)


pacman::p_load(tableone)
table4 <- CreateTableOne(vars = c( 'Distress'), 
                         data = df.match, 
                         factorVars = 'Distress', 
                         strata = 'Sample')


pacman::p_load(knitr, wakefield, MatchIt, tableone, captioner)