#Experiment 3
#Load dependencies
library(lme4)
library(ggplot2)

#Read data (four speakers)
dP <- read.csv("C:\\Users\\Sammy\\Box\\Abkhaz fieldwork\\Elicitation materials and scripts\\Python\\Code\\Pilot run\\Experiment3\\Experiment3 (pilot).csv", encoding = "UTF-8")
dNPOne <- read.csv("C:\\Users\\Sammy\\Box\\Abkhaz fieldwork\\Elicitation materials and scripts\\Python\\Code\\Non-pilot 1\\Experiment3\\Experiment3 (NP1).csv", encoding = "UTF-8")
dNPTwo <- read.csv("C:\\Users\\Sammy\\Box\\Abkhaz fieldwork\\Elicitation materials and scripts\\Python\\Code\\Non-pilot 2\\Experiment3\\Experiment3 (NP2).csv", encoding = "UTF-8")
dNPThree <- read.csv("C:\\Users\\Sammy\\Box\\Abkhaz fieldwork\\Elicitation materials and scripts\\Python\\Code\\Non-pilot 3\\Experiment3\\Experiment3 (NP3).csv", encoding = "UTF-8")

#Add information about speaker identity before merging dataframes
dP$Speaker <- rep("A", nrow(dP))
dNPOne$Speaker <- rep("B", nrow(dNPOne))
dNPTwo$Speaker <- rep("C", nrow(dNPTwo))
dNPThree$Speaker <- rep("D", nrow(dNPThree))

#Merge the dataframes into one
dAll <- rbind(dP, dNPOne, dNPTwo, dNPThree)

#Add an index column which will help with removing outliers
dAll$Index <- 1:nrow(dAll)

#Complicated operations to be able to Z-score pitch measurements
original_dataframe <- dAll

included_columns <- c("Cpitch20", "Cpitch40", "Cpitch60", "Cpitch80", "Cpitch100",
                      "Vpitch20", "Vpitch40", "Vpitch60", "Vpitch80", "Vpitch100")

new_dataframe <- data.frame(SourceColumn = character(),
                            Data = numeric(),
                            stringsAsFactors = FALSE)

for (column_name in included_columns) {
  column_values <- original_dataframe[, column_name]
  
  new_rows <- data.frame(SourceColumn = column_name,
                         Data = column_values,
                         stringsAsFactors = FALSE)
  
  new_dataframe <- rbind(new_dataframe, new_rows)
}

other_columns <- setdiff(names(original_dataframe), included_columns)
for (column_name in other_columns) {
  new_dataframe[, column_name] <- original_dataframe[, column_name]
}

# Create a new column for Z-scores
new_dataframe$ZScore <- NA

# Group by Speaker and calculate Z-score within each group
speakers <- unique(new_dataframe$Speaker)
for (speaker in speakers) {
  # Subset the dataframe for the current speaker
  subset_indices <- new_dataframe$Speaker == speaker
  
  # Get the subset of data for the current speaker, ignoring NA values
  subset_data <- new_dataframe$Data[subset_indices]
  subset_data <- subset_data[!is.na(subset_data)]
  
  # Calculate Z-scores for the subsetted data
  subset_data_zscore <- (subset_data - mean(subset_data)) / sd(subset_data)
  
  # Assign the calculated Z-scores back to the original dataframe
  new_dataframe$ZScore[subset_indices][!is.na(new_dataframe$Data[subset_indices])] <- subset_data_zscore
}



#Identification of possible outliers (F0)
outliers <- new_dataframe[which(new_dataframe$ZScore > 3 | new_dataframe$ZScore < -3),]

# Define a custom order for SourceColumn
custom_order <- c("CPitch20", "CPitch40", "CPitch60", "CPitch80", "CPitch100",
                  "VPitch20", "VPitch40", "VPitch60", "VPitch80", "VPitch100")

# Sort outliers dataframe by Index and SourceColumn with custom order
outliers <- outliers[order(outliers$Index, match(outliers$SourceColumn, custom_order)), ]

#Keep only outliers which after manual inspection turned out to be real outliers
#in the outliers dataframe
outliers <- outliers[
  (outliers$Speaker == "A" | outliers$Speaker == "B") |
    (outliers$Speaker == "C" & outliers$Filename == 44 & outliers$SourceColumn == "Cpitch100") |
    (outliers$Speaker == "D" & outliers$Filename == 374 & outliers$SourceColumn == "Cpitch20"),
]


#Exclusion of outliers from dAll
# Loop through each row in outliers dataframe
for (i in 1:nrow(outliers)) {
  # Get the Index value from outliers
  index_value <- outliers$Index[i]
  
  # Get the corresponding row index in dAll
  row_index <- which(dAll$Index == index_value)
  
  # Get the SourceColumn value from outliers
  source_column <- outliers$SourceColumn[i]
  
  # Set the corresponding value to NA in dAll
  dAll[row_index, source_column] <- NA
}

#Now that we've removed some data, we need to check again
#that we're only using rows which have at least 3 measurements
#for consonant pitch, and 3 measurements for vowel pitch.
cpitch_columns <- c("Cpitch20", "Cpitch40", "Cpitch60", "Cpitch80", "Cpitch100")
vpitch_columns <- c("Vpitch20", "Vpitch40", "Vpitch60", "Vpitch80", "Vpitch100")

dAll <- dAll[rowSums(!is.na(dAll[cpitch_columns])) >= 3 &
               rowSums(!is.na(dAll[vpitch_columns])) >= 3, ]

#Next we need to re-set the values in dAll$PitchExtremumLocation
#to match the new (outliers excluded) data. We're careful to use
#max for speakers A-C, but min for speaker D.
#Maximizing speakers
subset_rows <- dAll$Speaker %in% c("A", "B", "C")

min.col <- function(m, ...) max.col(-m, ...)

# Replace NA values with -Inf for speakers A-C
dAll[included_columns][subset_rows & is.na(dAll[included_columns])] <- -Inf

# Replace NA values with +Inf for speaker D
dAll[included_columns][!subset_rows & is.na(dAll[included_columns])] <- Inf

dAll$PitchExtremumLocation <- NA
dAll$PitchExtremumLocation[subset_rows] <- max.col(dAll[included_columns][subset_rows, ])
dAll$PitchExtremumLocation[!subset_rows] <- min.col(dAll[included_columns][!subset_rows, ])

#Replace Inf and -Inf with NA again
dAll[dAll == Inf | dAll == -Inf] <- NA

#Add condition information
condition_C <- c("баб", "бабџьар", "бажәа", "бан", "башәа")
condition_V <- c("багәоит", "баԥхьоит", "басуеит", "бахоит", "башьҭоуп")

dAll$Condition <- ifelse(dAll$Word %in% condition_C, "Consonant",
                         ifelse(dAll$Word %in% condition_V, "Vowel", "Indeterminate"))

#Turn condition into a factor and relevel with
#the order Consonant, Vowel, Indeterminate
dAll$Condition <- factor(dAll$Condition, ordered = FALSE)
dAll$Condition <- relevel(dAll$Condition, ref = "Vowel")
dAll$Condition <- relevel(dAll$Condition, ref = "Consonant")

#Visualization (V:CV duration ratio)
ggplot(dAll, aes(x = Condition, y = VtoCVPercent)) + xlab("Condition") +  ylab("V:CV duration %") + ggtitle("V:CV duration % by condition") + geom_boxplot()
ggplot(dAll, aes(x = Speaker, y = VtoCVPercent, fill = Condition)) + xlab("Speaker") +  ylab("V:CV duration %") + ggtitle("V:CV duration % by condition and speaker") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#B0B0B0", "#808080"))

#Visualization (F0 extremum location)
ggplot(dAll, aes(x = Condition, y = jitter(PitchExtremumLocation))) + xlab("Condition") +  ylab("F0 extremum location") + ggtitle("F0 extremum location by condition") + geom_boxplot()
ggplot(dAll, aes(x = Speaker, y = jitter(PitchExtremumLocation), fill = Condition)) + xlab("Speaker") +  ylab("F0 extremum location") + ggtitle("F0 extremum location by condition and speaker") + geom_boxplot() + scale_fill_manual(values = c("#FFFFFF", "#B0B0B0", "#808080"))

#Modeling (V:CV duration ratio)
Duration.base <- lmer(VtoCVPercent ~ Filename + (1|Word) + (1|Speaker), data = dAll, REML = FALSE)
summary(Duration.base)
coef(Duration.base)
Duration.intermediate <- lmer(VtoCVPercent ~ Filename + (1|Word) + (1+Condition|Speaker), data = dAll, REML = FALSE)
summary(Duration.intermediate)
coef(Duration.intermediate)
Duration.full <- lmer(VtoCVPercent ~ Filename + Condition + (1|Word) + (1+Condition|Speaker), data = dAll, REML = FALSE)
summary(Duration.full)
coef(Duration.full)

anova(Duration.base, Duration.intermediate)
anova(Duration.intermediate, Duration.full)

#Modeling (F0 extremum location)
Pitch.base <- lmer(PitchExtremumLocation ~ Filename + (1|Word) + (1|Speaker), data = dAll, REML = FALSE)
summary(Pitch.base)
coef(Pitch.base)
Pitch.intermediate <- lmer(PitchExtremumLocation ~ Filename + (1|Word) + (1+Condition|Speaker), data = dAll, REML = FALSE)
summary(Pitch.intermediate)
coef(Pitch.intermediate)
Pitch.full <- lmer(PitchExtremumLocation ~ Filename + Condition + (1|Word) + (1+Condition|Speaker), data = dAll, REML = FALSE)
summary(Pitch.full)
coef(Pitch.full)

anova(Pitch.base, Pitch.intermediate)
anova(Pitch.intermediate, Pitch.full)
