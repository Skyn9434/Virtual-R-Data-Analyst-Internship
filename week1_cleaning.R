# Virtual R Data Analyst Internship - Week 1
library(dplyr)

url <- "https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv"
df <- read.csv(url, stringsAsFactors = FALSE)

# Initial inspection
dim(df)
str(df)
colSums(is.na(df))
sum(duplicated(df$PassengerId))

# Remove very sparse Cabin field
df_clean <- df %>% select(-Cabin)

# Impute Age using Pclass x Sex group medians
df_clean <- df_clean %>%
  group_by(Pclass, Sex) %>%
  mutate(Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age)) %>%
  ungroup()

# Impute missing Embarked with mode
mode_embarked <- names(sort(table(df_clean$Embarked), decreasing = TRUE))[1]
df_clean$Embarked[is.na(df_clean$Embarked)] <- mode_embarked

# Feature engineering
df_clean <- df_clean %>%
  mutate(FamilySize = SibSp + Parch + 1,
         IsAlone = ifelse(FamilySize == 1, 1, 0),
         SexBinary = ifelse(Sex == "male", 1, 0))

# IQR-based Fare outlier treatment
Q1 <- quantile(df_clean$Fare, 0.25, na.rm = TRUE)
Q3 <- quantile(df_clean$Fare, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
upper <- Q3 + 1.5 * IQR_value
df_clean$Fare_Capped <- pmin(df_clean$Fare, upper)

# Standardization
df_clean$Age_Z <- as.numeric(scale(df_clean$Age))
df_clean$Fare_Z <- as.numeric(scale(df_clean$Fare_Capped))

summary(df_clean)
colSums(is.na(df_clean))
write.csv(df_clean, "titanic_cleaned.csv", row.names = FALSE)
