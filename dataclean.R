data<-read.csv(file.choose(),header=TRUE)
head(data)
str(data)
dim(data)
library(ggplot2)
library(stringr)
library(dplyr)
library(Amelia)
missmap(data)



#for colomn male
length(unique(data$male))
sum(is.na(data$male))
ggplot(data, aes(x = male)) +
  geom_histogram()
#for colomn age
length(unique(data$age))
sum(is.na(data$age))
ggplot(data, aes(x = age)) +
  geom_histogram()


age0= data %>%
  mutate(age = as.factor(age)) %>%
  group_by(age) %>%
  summarise(count = n(),
            percentage = count / nrow(data)*100) %>%
  arrange(-count)

ggplot(age0[1:10,],aes(x = reorder(age, -count), y = count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18)) +
  xlab("age")

#for colomn education
length(unique(data$education))
sum(is.na(data$education))
ggplot(data, aes(x =education))+
  geom_histogram()
#for colomn currentSmoker
length(unique(data$currentSmoker))
sum(is.na(data$currentSmoker))
ggplot(data, aes(x = currentSmoker))+
  geom_histogram()


#for colomn cigsPerDay
length(unique(data$cigsPerDay))
sum(is.na(data$cigsPerDay))
ggplot(data, aes(x =cigsPerDay))+
  geom_histogram()
#for colomn BPMeds
length(unique(data$BPMeds))
sum(is.na(data$BPMeds))
ggplot(data, aes(x = BPMeds))+
  geom_histogram()
#for colomn prevalentStroke
length(unique(data$prevalentStroke))
sum(is.na(data$prevalentStroke))
ggplot(data, aes(x = prevalentStroke))+
  geom_histogram()
#for colomn prevalentHyp
length(unique(data$prevalentHyp))
sum(is.na(data$prevalentHyp))
ggplot(data, aes(x =prevalentHyp))+
  geom_histogram()
#for colomn diabetes
length(unique(data$diabetes))
sum(is.na(data$diabetes))
ggplot(data, aes(x =diabetes))+
  geom_histogram()

diabeteo= data %>%
mutate(diabetes=as.factor(diabetes))%>%
  group_by(diabetes)%>%
  summarise(count = n(),
            percentage = count / nrow(data)*100) %>%
  arrange(-count)

ggplot(diabeteo[1:10,], aes(x = reorder(diabetes,-count),y=count))+
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18)) +
  xlab("diabetes")
#for colomn totchol
length(unique(data$totChol))
sum(is.na(data$totChol))
ggplot(data, aes(x =totChol))+
  geom_histogram()

totCholo= data %>%
  mutate(totChol=as.factor(totChol))%>%
  group_by(totChol)%>%
  summarise(count = n(),
            percentage = count / nrow(data)*100) %>%
  arrange(-count)

ggplot(totCholo[1:10,], aes(x = reorder(totChol,-count),y=count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18)) +
  xlab("totChol")

#for colomn sysBP
length(unique(data$sysBP))
sum(is.na(data$sysBP))
ggplot(data, aes(x =sysBP)) +
  geom_histogram()

lot_order = data %>%
  mutate(sysBP=as.factor(sysBP))%>%
  group_by(sysBP)%>%
  summarise(count = n(),
            percentage = count / nrow(data)*100) %>%
  arrange(-count)

ggplot(lot_order[1:10,], aes(x = reorder(sysBP,-count),y= count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18)) +
  xlab("sysBP")

#for colomn diABP
length(unique(data$diaBP))
sum(is.na(data$diaBP))
ggplot(data, aes(x =diaBP)) +
  geom_histogram()

diabpo= data %>%
  mutate(totChol=as.factor(diaBP))%>%
  group_by(diaBP)%>%
  summarise(count = n(),
            percentage = count / nrow(data)*100) %>%
  arrange(-count)

ggplot(diabpo[1:10,], aes(x = reorder(diaBP,-count),y=count))+
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18)) +
  xlab("diaBP")
#for colomn BMI
length(unique(data$BMI))
sum(is.na(data$BMI))
ggplot(data, aes(x = BMI)) +
  geom_histogram()
#for colomn heartRate
length(unique(data$heartRate))
sum(is.na(data$heartRate))
ggplot(data, aes(x =heartRate))+
  geom_histogram()
#for colomn gluccose
length(unique(data$glucose))
sum(is.na(data$glucose))
ggplot(data, aes(x =glucose))+
  geom_histogram()

length(unique(data$TenYearCHD))
sum(is.na(data$TenYearCHD))