#Experiment 1A
#Load dependencies
library(lme4)
library(ggplot2)

#Read data (four speakers)
dP <- read.csv("C:\\Users\\Sammy\\Box\\Abkhaz fieldwork\\Elicitation materials and scripts\\Python\\Code\\Pilot run\\Experiment1\\Experiment1A (pilot).csv", encoding = "UTF-8")
dNPOne <- read.csv("C:\\Users\\Sammy\\Box\\Abkhaz fieldwork\\Elicitation materials and scripts\\Python\\Code\\Non-pilot 1\\Experiment1\\Experiment1A (NP1).csv", encoding = "UTF-8")
dNPTwo <- read.csv("C:\\Users\\Sammy\\Box\\Abkhaz fieldwork\\Elicitation materials and scripts\\Python\\Code\\Non-pilot 2\\Experiment1\\Experiment1A (NP2).csv", encoding = "UTF-8")
dNPThree <- read.csv("C:\\Users\\Sammy\\Box\\Abkhaz fieldwork\\Elicitation materials and scripts\\Python\\Code\\Non-pilot 3\\Experiment1\\Experiment1A (NP3).csv", encoding = "UTF-8")

#Z-score the pitch data by speaker
dP$FZeroZSCORE <- (dP$PitchMean - mean(dP$PitchMean, na.rm = TRUE)) / sd(dP$PitchMean, na.rm = TRUE)
dNPOne$FZeroZSCORE <- (dNPOne$PitchMean - mean(dNPOne$PitchMean, na.rm = TRUE)) / sd(dNPOne$PitchMean, na.rm = TRUE)
dNPTwo$FZeroZSCORE <- (dNPTwo$PitchMean - mean(dNPTwo$PitchMean, na.rm = TRUE)) / sd(dNPTwo$PitchMean, na.rm = TRUE)
dNPThree$FZeroZSCORE <- (dNPThree$PitchMean - mean(dNPThree$PitchMean, na.rm = TRUE)) / sd(dNPThree$PitchMean, na.rm = TRUE)

#Z-score intensity data by speaker
dP$IntensityZSCORE <- (dP$IntensityPeak - mean(dP$IntensityPeak, na.rm = TRUE)) / sd(dP$IntensityPeak, na.rm = TRUE)
dNPOne$IntensityZSCORE <- (dNPOne$IntensityPeak - mean(dNPOne$IntensityPeak, na.rm = TRUE)) / sd(dNPOne$IntensityPeak, na.rm = TRUE)
dNPTwo$IntensityZSCORE <- (dNPTwo$IntensityPeak - mean(dNPTwo$IntensityPeak, na.rm = TRUE)) / sd(dNPTwo$IntensityPeak, na.rm = TRUE)
dNPThree$IntensityZSCORE <- (dNPThree$IntensityPeak - mean(dNPThree$IntensityPeak, na.rm = TRUE)) / sd(dNPThree$IntensityPeak, na.rm = TRUE)

#Z-score duration data by speaker
dP$DurationZSCORE <- (dP$Duration - mean(dP$Duration, na.rm = TRUE)) / sd(dP$Duration, na.rm = TRUE)
dNPOne$DurationZSCORE <- (dNPOne$Duration - mean(dNPOne$Duration, na.rm = TRUE)) / sd(dNPOne$Duration, na.rm = TRUE)
dNPTwo$DurationZSCORE <- (dNPTwo$Duration - mean(dNPTwo$Duration, na.rm = TRUE)) / sd(dNPTwo$Duration, na.rm = TRUE)
dNPThree$DurationZSCORE <- (dNPThree$Duration - mean(dNPThree$Duration, na.rm = TRUE)) / sd(dNPThree$Duration, na.rm = TRUE)

#Add information about speaker identity before merging dataframes
dP$Speaker <- rep("A", nrow(dP))
dNPOne$Speaker <- rep("B", nrow(dNPOne))
dNPTwo$Speaker <- rep("C", nrow(dNPTwo))
dNPThree$Speaker <- rep("D", nrow(dNPThree))

#Merge the dataframes into one
dAll <- rbind(dP, dNPOne, dNPTwo, dNPThree)

#Turn condition into a factor and relevel with
#Unstressed as reference level.
dAll$Stress <- factor(dAll$Stress, ordered = FALSE)
dAll$Stress <- relevel(dAll$Stress, ref = "Unstressed")

#Turn position into a factor and relevel in
#the order Initial, Medial, Final
dAll$Position <- factor(dAll$Position, ordered = FALSE)
dAll$Position <- relevel(dAll$Position, ref = "Medial")
dAll$Position <- relevel(dAll$Position, ref = "Initial")

#Add an index column which will help with removing outliers
dAll$Index <- 1:nrow(dAll)

#Identification of possible outliers (F0)
outliers <- dAll[which(dAll$FZeroZSCORE > 3 | dAll$FZeroZSCORE < -3),]

#Exclude items from outliers if they look correct after manual inspection
outliers <- subset(outliers, Index != 907)

#Exclusion of outliers
dAll$FZeroZSCORE[dAll$Index %in% outliers$Index] <- NA



#Identification of possible outliers (intensity)
outliers <- dAll[which(dAll$IntensityZSCORE > 3 | dAll$IntensityZSCORE < -3),]

#Exclude items from outliers if they look correct after manual inspection
outliers <- subset(outliers, Index != 227)
outliers <- subset(outliers, Index != 699)

#Exclusion of outliers
dAll$IntensityZSCORE[dAll$Index %in% outliers$Index] <- NA

#Last-minute exclusion of tokens of "агәЫԥқәа" which
#should not be in here due to a secondary stress confound
#on the final vowel
dAll <- subset(dAll, EnglishWord != "агәЫԥқәа")

#Visualization (F0)
ggplot(dAll, aes(x = Stress, y = FZeroZSCORE)) + xlab("Stress") +  ylab("F0 (Z-score)") + ggtitle("F0 (Z-score) by stress") + geom_boxplot()
ggplot(dAll, aes(x = Vowel, y = FZeroZSCORE, fill = Stress)) + xlab("Vowel") +  ylab("F0 (Z-score)") + ggtitle("F0 (Z-score) by stress and vowel") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dAll, aes(x = Stress, y = FZeroZSCORE, fill = Position)) + xlab("Stress") +  ylab("F0 (Z-score)") + ggtitle("F0 (Z-score) by stress and position") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#B0B0B0", "#808080"))
ggplot(dAll, aes(x = Speaker, y = FZeroZSCORE, fill = Stress)) + xlab("Speaker") +  ylab("F0 (Z-score)") + ggtitle("F0 (Z-score) by stress and speaker") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dAll, aes(x = Filename, y = FZeroZSCORE)) + xlab("Trial number") +  ylab("F0 (Z-score)") + ggtitle("F0 (Z-score) by trial number") + geom_point() + geom_smooth(method = "lm")

#Visualization (intensity)
ggplot(dAll, aes(x = Stress, y = IntensityZSCORE)) + xlab("Stress") +  ylab("Intensity (Z-score)") + ggtitle("Intensity (Z-score) by stress") + geom_boxplot()
ggplot(dAll, aes(x = Vowel, y = IntensityZSCORE, fill = Stress)) + xlab("Vowel") +  ylab("Intensity (Z-score)") + ggtitle("Intensity (Z-score) by stress and vowel") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dAll, aes(x = Stress, y = IntensityZSCORE, fill = Position)) + xlab("Stress") +  ylab("Intensity (Z-score)") + ggtitle("Intensity (Z-score) by stress and position") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#B0B0B0", "#808080"))
ggplot(dAll, aes(x = Speaker, y = IntensityZSCORE, fill = Stress)) + xlab("Speaker") +  ylab("Intensity (Z-score)") + ggtitle("Intensity (Z-score) by stress and speaker") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dAll, aes(x = Filename, y = IntensityZSCORE)) + xlab("Trial number") +  ylab("Intensity (Z-score)") + ggtitle("Intensity (Z-score) by trial number") + geom_point() + geom_smooth(method = "lm")

#Visualization (duration)
ggplot(dAll, aes(x = Stress, y = DurationZSCORE)) + xlab("Stress") +  ylab("Duration (Z-score)") + ggtitle("Duration (Z-score) by stress") + geom_boxplot()
ggplot(dAll, aes(x = Vowel, y = DurationZSCORE, fill = Stress)) + xlab("Vowel") +  ylab("Duration (Z-score)") + ggtitle("Duration (Z-score) by stress and vowel") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dAll, aes(x = Stress, y = DurationZSCORE, fill = Position)) + xlab("Stress") +  ylab("Duration (Z-score)") + ggtitle("Duration (Z-score) by stress and position") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#B0B0B0", "#808080"))
ggplot(dAll, aes(x = Speaker, y = DurationZSCORE, fill = Stress)) + xlab("Speaker") +  ylab("Duration (Z-score)") + ggtitle("Duration (Z-score) by stress and speaker") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dAll, aes(x = Filename, y = DurationZSCORE)) + xlab("Trial number") +  ylab("Duration (Z-score)") + ggtitle("Duration (Z-score) by trial number") + geom_point() + geom_smooth(method = "lm")

#Modeling (F0)
FZero.base <- lmer(FZeroZSCORE ~ Vowel + Position + (1|EnglishWord) + (1|Speaker), data = dAll, REML = FALSE)
summary(FZero.base)
coef(FZero.base)
FZero.mid <- lmer(FZeroZSCORE ~ Vowel + Position + (1|EnglishWord) + (1+Stress|Speaker), data = dAll, REML = FALSE)
summary(FZero.mid)
coef(FZero.mid)
FZero.full <- lmer(FZeroZSCORE ~ Stress + Vowel + Position + (1|EnglishWord) + (1+Stress|Speaker), data = dAll, REML = FALSE)
summary(FZero.full)
coef(FZero.full)
FZero.interaction <- lmer(FZeroZSCORE ~ Stress * Vowel + Position + (1|EnglishWord) + (1+Stress|Speaker), data = dAll, REML = FALSE)
summary(FZero.interaction)
coef(FZero.interaction)

anova(FZero.base, FZero.mid)
anova(FZero.mid, FZero.full)
anova(FZero.full, FZero.interaction)

#Modeling (intensity)
Intensity.base <- lmer(IntensityZSCORE ~ Vowel + Position + (1|EnglishWord) + (1|Speaker), data = dAll, REML = FALSE)
summary(Intensity.base)
coef(Intensity.base)
Intensity.mid <- lmer(IntensityZSCORE ~ Vowel + Position + (1|EnglishWord) + (1+Stress|Speaker), data = dAll, REML = FALSE)
summary(Intensity.mid)
coef(Intensity.mid)
Intensity.full <- lmer(IntensityZSCORE ~ Stress + Vowel + Position + (1|EnglishWord) + (1+Stress|Speaker), data = dAll, REML = FALSE)
summary(Intensity.full)
coef(Intensity.full)
Intensity.interaction <- lmer(IntensityZSCORE ~ Stress * Vowel + Position + (1|EnglishWord) + (1+Stress|Speaker), data = dAll, REML = FALSE)
summary(Intensity.interaction)
coef(Intensity.interaction)

anova(Intensity.base, Intensity.mid)
anova(Intensity.mid, Intensity.full)
anova(Intensity.full, Intensity.interaction)

#Modeling (duration)
Duration.base <- lmer(DurationZSCORE ~ Vowel + Position + (1|EnglishWord) + (1|Speaker), data = dAll, REML = FALSE)
summary(Duration.base)
coef(Duration.base)
Duration.mid <- lmer(DurationZSCORE ~ Vowel + Position + (1|EnglishWord) + (1+Stress|Speaker), data = dAll, REML = FALSE)
summary(Duration.mid)
coef(Duration.mid)
Duration.full <- lmer(DurationZSCORE ~ Stress + Vowel + Position + (1|EnglishWord) + (1+Stress|Speaker), data = dAll, REML = FALSE)
summary(Duration.full)
coef(Duration.full)
Duration.interaction <- lmer(DurationZSCORE ~ Stress * Vowel + Position + (1|EnglishWord) + (1+Stress|Speaker), data = dAll, REML = FALSE)
summary(Duration.interaction)
coef(Duration.interaction)

anova(Duration.base, Duration.mid)
anova(Duration.mid, Duration.full)
anova(Duration.full, Duration.interaction)

#Checking assumptions (F0)
plot(fitted(FZero.full), residuals(FZero.full))
hist(residuals(FZero.full))

#Checking assumptions (intensity)
plot(fitted(Intensity.full), residuals(Intensity.full))
hist(residuals(Intensity.full))

#Checking assumptions (duration)
plot(fitted(Duration.full), residuals(Duration.full))
hist(residuals(Duration.full))



#CODE FOR APPENDIX
#Here I fit separate models for each participant
#This code should be run after the code above,
#specifically the code Z-scoring data, combining
#it into one dataframe (dAll) and excluding outliers

#Create datasets for each speaker
dP <- dAll[which(dAll$Speaker == "A"),]
dNPOne <- dAll[which(dAll$Speaker == "B"),]
dNPTwo <- dAll[which(dAll$Speaker == "C"),]
dNPThree <- dAll[which(dAll$Speaker == "D"),]

#Visualization of F0
ggplot(dP, aes(x = Vowel, y = FZeroZSCORE, fill = Stress)) + xlab("Vowel") +  ylab("F0 (Z-score)") + ggtitle("F0 (Z-score) by stress and vowel for speaker A") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPOne, aes(x = Vowel, y = FZeroZSCORE, fill = Stress)) + xlab("Vowel") +  ylab("F0 (Z-score)") + ggtitle("F0 (Z-score) by stress and vowel for speaker B") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPTwo, aes(x = Vowel, y = FZeroZSCORE, fill = Stress)) + xlab("Vowel") +  ylab("F0 (Z-score)") + ggtitle("F0 (Z-score) by stress and vowel for speaker C") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPThree, aes(x = Vowel, y = FZeroZSCORE, fill = Stress)) + xlab("Vowel") +  ylab("F0 (Z-score)") + ggtitle("F0 (Z-score) by stress and vowel for speaker D") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))

ggplot(dP, aes(x = Position, y = FZeroZSCORE, fill = Stress)) + xlab("Position") +  ylab("F0 (Z-score)") + ggtitle("F0 (Z-score) by stress and position for speaker A") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPOne, aes(x = Position, y = FZeroZSCORE, fill = Stress)) + xlab("Position") +  ylab("F0 (Z-score)") + ggtitle("F0 (Z-score) by stress and position for speaker B") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPTwo, aes(x = Position, y = FZeroZSCORE, fill = Stress)) + xlab("Position") +  ylab("F0 (Z-score)") + ggtitle("F0 (Z-score) by stress and position for speaker C") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPThree, aes(x = Position, y = FZeroZSCORE, fill = Stress)) + xlab("Position") +  ylab("F0 (Z-score)") + ggtitle("F0 (Z-score) by stress and position for speaker D") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))

#Visualization of intensity
ggplot(dP, aes(x = Vowel, y = IntensityZSCORE, fill = Stress)) + xlab("Vowel") +  ylab("Intensity (Z-score)") + ggtitle("Intensity (Z-score) by stress and vowel for speaker A") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPOne, aes(x = Vowel, y = IntensityZSCORE, fill = Stress)) + xlab("Vowel") +  ylab("Intensity (Z-score)") + ggtitle("Intensity (Z-score) by stress and vowel for speaker B") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPTwo, aes(x = Vowel, y = IntensityZSCORE, fill = Stress)) + xlab("Vowel") +  ylab("Intensity (Z-score)") + ggtitle("Intensity (Z-score) by stress and vowel for speaker C") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPThree, aes(x = Vowel, y = IntensityZSCORE, fill = Stress)) + xlab("Vowel") +  ylab("Intensity (Z-score)") + ggtitle("Intensity (Z-score) by stress and vowel for speaker D") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))

ggplot(dP, aes(x = Position, y = IntensityZSCORE, fill = Stress)) + xlab("Position") +  ylab("Intensity (Z-score)") + ggtitle("Intensity (Z-score) by stress and position for speaker A") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPOne, aes(x = Position, y = IntensityZSCORE, fill = Stress)) + xlab("Position") +  ylab("Intensity (Z-score)") + ggtitle("Intensity (Z-score) by stress and position for speaker B") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPTwo, aes(x = Position, y = IntensityZSCORE, fill = Stress)) + xlab("Position") +  ylab("Intensity (Z-score)") + ggtitle("Intensity (Z-score) by stress and position for speaker C") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPThree, aes(x = Position, y = IntensityZSCORE, fill = Stress)) + xlab("Position") +  ylab("Intensity (Z-score)") + ggtitle("Intensity (Z-score) by stress and position for speaker D") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))

#Visualization of duration
ggplot(dP, aes(x = Vowel, y = DurationZSCORE, fill = Stress)) + xlab("Vowel") +  ylab("Duration (Z-score)") + ggtitle("Duration (Z-score) by stress and vowel for speaker A") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPOne, aes(x = Vowel, y = DurationZSCORE, fill = Stress)) + xlab("Vowel") +  ylab("Duration (Z-score)") + ggtitle("Duration (Z-score) by stress and vowel for speaker B") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPTwo, aes(x = Vowel, y = DurationZSCORE, fill = Stress)) + xlab("Vowel") +  ylab("Duration (Z-score)") + ggtitle("Duration (Z-score) by stress and vowel for speaker C") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPThree, aes(x = Vowel, y = DurationZSCORE, fill = Stress)) + xlab("Vowel") +  ylab("Duration (Z-score)") + ggtitle("Duration (Z-score) by stress and vowel for speaker D") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))

ggplot(dP, aes(x = Position, y = DurationZSCORE, fill = Stress)) + xlab("Position") +  ylab("Duration (Z-score)") + ggtitle("Duration (Z-score) by stress and position for speaker A") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPOne, aes(x = Position, y = DurationZSCORE, fill = Stress)) + xlab("Position") +  ylab("Duration (Z-score)") + ggtitle("Duration (Z-score) by stress and position for speaker B") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPTwo, aes(x = Position, y = DurationZSCORE, fill = Stress)) + xlab("Position") +  ylab("Duration (Z-score)") + ggtitle("Duration (Z-score) by stress and position for speaker C") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))
ggplot(dNPThree, aes(x = Position, y = DurationZSCORE, fill = Stress)) + xlab("Position") +  ylab("Duration (Z-score)") + ggtitle("Duration (Z-score) by stress and position for speaker D") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#808080"))

#Modeling (speaker A)
#F0
FZero.dPbase <- lmer(FZeroZSCORE ~ Vowel + Position + (1|EnglishWord), data = dP, REML = FALSE)
summary(FZero.dPbase)
coef(FZero.dPbase)
FZero.dPfull <- lmer(FZeroZSCORE ~ Stress + Vowel + Position + (1|EnglishWord), data = dP, REML = FALSE)
summary(FZero.dPfull)
coef(FZero.dPfull)
FZero.dPinteraction <- lmer(FZeroZSCORE ~ Stress * Vowel + Position + (1|EnglishWord), data = dP, REML = FALSE)
summary(FZero.dPinteraction)
coef(FZero.dPinteraction)

anova(FZero.dPbase, FZero.dPfull)
anova(FZero.dPfull, FZero.dPinteraction)

#Intensity
Intensity.dPbase <- lmer(IntensityZSCORE ~ Vowel + Position + (1|EnglishWord), data = dP, REML = FALSE)
summary(Intensity.dPbase)
coef(Intensity.dPbase)
Intensity.dPfull <- lmer(IntensityZSCORE ~ Stress + Vowel + Position + (1|EnglishWord), data = dP, REML = FALSE)
summary(Intensity.dPfull)
coef(Intensity.dPfull)
Intensity.dPinteraction <- lmer(IntensityZSCORE ~ Stress * Vowel + Position + (1|EnglishWord), data = dP, REML = FALSE)
summary(Intensity.dPinteraction)
coef(Intensity.dPinteraction)

anova(Intensity.dPbase, Intensity.dPfull)
anova(Intensity.dPfull, Intensity.dPinteraction)

#Duration
Duration.dPbase <- lmer(DurationZSCORE ~ Vowel + Position + (1|EnglishWord), data = dP, REML = FALSE)
summary(Duration.dPbase)
coef(Duration.dPbase)
Duration.dPfull <- lmer(DurationZSCORE ~ Stress + Vowel + Position + (1|EnglishWord), data = dP, REML = FALSE)
summary(Duration.dPfull)
coef(Duration.dPfull)
Duration.dPinteraction <- lmer(DurationZSCORE ~ Stress * Vowel + Position + (1|EnglishWord), data = dP, REML = FALSE)
summary(Duration.dPinteraction)
coef(Duration.dPinteraction)

anova(Duration.dPbase, Duration.dPfull)
anova(Duration.dPfull, Duration.dPinteraction)

#Modeling (speaker B)
#F0
FZero.dNPOnebase <- lmer(FZeroZSCORE ~ Vowel + Position + (1|EnglishWord), data = dNPOne, REML = FALSE)
summary(FZero.dNPOnebase)
coef(FZero.dNPOnebase)
FZero.dNPOnefull <- lmer(FZeroZSCORE ~ Stress + Vowel + Position + (1|EnglishWord), data = dNPOne, REML = FALSE)
summary(FZero.dNPOnefull)
coef(FZero.dNPOnefull)
FZero.dNPOneinteraction <- lmer(FZeroZSCORE ~ Stress * Vowel + Position + (1|EnglishWord), data = dNPOne, REML = FALSE)
summary(FZero.dNPOneinteraction)
coef(FZero.dNPOneinteraction)

anova(FZero.dNPOnebase, FZero.dNPOnefull)
anova(FZero.dNPOnefull, FZero.dNPOneinteraction)

#Intensity
Intensity.dNPOnebase <- lmer(IntensityZSCORE ~ Vowel + Position + (1|EnglishWord), data = dNPOne, REML = FALSE)
summary(Intensity.dNPOnebase)
coef(Intensity.dNPOnebase)
Intensity.dNPOnefull <- lmer(IntensityZSCORE ~ Stress + Vowel + Position + (1|EnglishWord), data = dNPOne, REML = FALSE)
summary(Intensity.dNPOnefull)
coef(Intensity.dNPOnefull)
Intensity.dNPOneinteraction <- lmer(IntensityZSCORE ~ Stress * Vowel + Position + (1|EnglishWord), data = dNPOne, REML = FALSE)
summary(Intensity.dNPOneinteraction)
coef(Intensity.dNPOneinteraction)

anova(Intensity.dNPOnebase, Intensity.dNPOnefull)
anova(Intensity.dNPOnefull, Intensity.dNPOneinteraction)

#Duration
Duration.dNPOnebase <- lmer(DurationZSCORE ~ Vowel + Position + (1|EnglishWord), data = dNPOne, REML = FALSE)
summary(Duration.dNPOnebase)
coef(Duration.dNPOnebase)
Duration.dNPOnefull <- lmer(DurationZSCORE ~ Stress + Vowel + Position + (1|EnglishWord), data = dNPOne, REML = FALSE)
summary(Duration.dNPOnefull)
coef(Duration.dNPOnefull)
Duration.dNPOneinteraction <- lmer(DurationZSCORE ~ Stress * Vowel + Position + (1|EnglishWord), data = dNPOne, REML = FALSE)
summary(Duration.dNPOneinteraction)
coef(Duration.dNPOneinteraction)

anova(Duration.dNPOnebase, Duration.dNPOnefull)
anova(Duration.dNPOnefull, Duration.dNPOneinteraction)

#Modeling (speaker C)
#F0
FZero.dNPTwobase <- lmer(FZeroZSCORE ~ Vowel + Position + (1|EnglishWord), data = dNPTwo, REML = FALSE)
summary(FZero.dNPTwobase)
coef(FZero.dNPTwobase)
FZero.dNPTwofull <- lmer(FZeroZSCORE ~ Stress + Vowel + Position + (1|EnglishWord), data = dNPTwo, REML = FALSE)
summary(FZero.dNPTwofull)
coef(FZero.dNPTwofull)
FZero.dNPTwointeraction <- lmer(FZeroZSCORE ~ Stress * Vowel + Position + (1|EnglishWord), data = dNPTwo, REML = FALSE)
summary(FZero.dNPTwointeraction)
coef(FZero.dNPTwointeraction)

anova(FZero.dNPTwobase, FZero.dNPTwofull)
anova(FZero.dNPTwofull, FZero.dNPTwointeraction)

#Intensity
Intensity.dNPTwobase <- lmer(IntensityZSCORE ~ Vowel + Position + (1|EnglishWord), data = dNPTwo, REML = FALSE)
summary(Intensity.dNPTwobase)
coef(Intensity.dNPTwobase)
Intensity.dNPTwofull <- lmer(IntensityZSCORE ~ Stress + Vowel + Position + (1|EnglishWord), data = dNPTwo, REML = FALSE)
summary(Intensity.dNPTwofull)
coef(Intensity.dNPTwofull)
Intensity.dNPTwointeraction <- lmer(IntensityZSCORE ~ Stress * Vowel + Position + (1|EnglishWord), data = dNPTwo, REML = FALSE)
summary(Intensity.dNPTwointeraction)
coef(Intensity.dNPTwointeraction)

anova(Intensity.dNPTwobase, Intensity.dNPTwofull)
anova(Intensity.dNPTwofull, Intensity.dNPTwointeraction)

#Duration
Duration.dNPTwobase <- lmer(DurationZSCORE ~ Vowel + Position + (1|EnglishWord), data = dNPTwo, REML = FALSE)
summary(Duration.dNPTwobase)
coef(Duration.dNPTwobase)
Duration.dNPTwofull <- lmer(DurationZSCORE ~ Stress + Vowel + Position + (1|EnglishWord), data = dNPTwo, REML = FALSE)
summary(Duration.dNPTwofull)
coef(Duration.dNPTwofull)
Duration.dNPTwointeraction <- lmer(DurationZSCORE ~ Stress * Vowel + Position + (1|EnglishWord), data = dNPTwo, REML = FALSE)
summary(Duration.dNPTwointeraction)
coef(Duration.dNPTwointeraction)

anova(Duration.dNPTwobase, Duration.dNPTwofull)
anova(Duration.dNPTwofull, Duration.dNPTwointeraction)

#Modeling (speaker D)
#F0
FZero.dNPThreebase <- lmer(FZeroZSCORE ~ Vowel + Position + (1|EnglishWord), data = dNPThree, REML = FALSE)
summary(FZero.dNPThreebase)
coef(FZero.dNPThreebase)
FZero.dNPThreefull <- lmer(FZeroZSCORE ~ Stress + Vowel + Position + (1|EnglishWord), data = dNPThree, REML = FALSE)
summary(FZero.dNPThreefull)
coef(FZero.dNPThreefull)
FZero.dNPThreeinteraction <- lmer(FZeroZSCORE ~ Stress * Vowel + Position + (1|EnglishWord), data = dNPThree, REML = FALSE)
summary(FZero.dNPThreeinteraction)
coef(FZero.dNPThreeinteraction)

anova(FZero.dNPThreebase, FZero.dNPThreefull)
anova(FZero.dNPThreefull, FZero.dNPThreeinteraction)

#Intensity
Intensity.dNPThreebase <- lmer(IntensityZSCORE ~ Vowel + Position + (1|EnglishWord), data = dNPThree, REML = FALSE)
summary(Intensity.dNPThreebase)
coef(Intensity.dNPThreebase)
Intensity.dNPThreefull <- lmer(IntensityZSCORE ~ Stress + Vowel + Position + (1|EnglishWord), data = dNPThree, REML = FALSE)
summary(Intensity.dNPThreefull)
coef(Intensity.dNPThreefull)
Intensity.dNPThreeinteraction <- lmer(IntensityZSCORE ~ Stress * Vowel + Position + (1|EnglishWord), data = dNPThree, REML = FALSE)
summary(Intensity.dNPThreeinteraction)
coef(Intensity.dNPThreeinteraction)

anova(Intensity.dNPThreebase, Intensity.dNPThreefull)
anova(Intensity.dNPThreefull, Intensity.dNPThreeinteraction)

#Duration
Duration.dNPThreebase <- lmer(DurationZSCORE ~ Vowel + Position + (1|EnglishWord), data = dNPThree, REML = FALSE)
summary(Duration.dNPThreebase)
coef(Duration.dNPThreebase)
Duration.dNPThreefull <- lmer(DurationZSCORE ~ Stress + Vowel + Position + (1|EnglishWord), data = dNPThree, REML = FALSE)
summary(Duration.dNPThreefull)
coef(Duration.dNPThreefull)
Duration.dNPThreeinteraction <- lmer(DurationZSCORE ~ Stress * Vowel + Position + (1|EnglishWord), data = dNPThree, REML = FALSE)
summary(Duration.dNPThreeinteraction)
coef(Duration.dNPThreeinteraction)

anova(Duration.dNPThreebase, Duration.dNPThreefull)
anova(Duration.dNPThreefull, Duration.dNPThreeinteraction)
