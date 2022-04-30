library(tidyverse)
nba_train <- read.csv("Desktop/Stats Courses/Spring '21/STATS 101A/Final Project/NBATrain.csv")
nba_train <- nba_train[, -1]
nba_test <- read.csv("Desktop/Stats Courses/Spring '21/STATS 101A/Final Project/NBATestNoY.csv")
nba_test <- nba_test[, -1]
baseline <- lm(Salary ~., data = nba_train)
summary(baseline)
cols <- colnames(nba_train[, c(-1,-ncol(nba_train))])
cors <- numeric(length(cols))
idx <- 1
# Find correlation of every numerical variable with salary
for (i in cols) {
  if (is.numeric(nba_train[, i])) {
    cors[idx] <- cor(nba_train[, i], nba_train$Salary)
  } else {
    cors[idx] <- 0
  }
  idx <- idx + 1
}
# Order largest to smallest correlation
cor_mat <- data.frame(Variable = cols, Correlation = abs(cors))
cor_mat <- cor_mat %>%
  arrange(desc(Correlation))
selected_cols <- cor_mat[cor_mat$Correlation > .3,][, "Variable"]
new_data <- nba_train[, c(as.character(selected_cols), "Salary")]
summary(lm(Salary~., data = new_data))

is.numeric(nba_train[, "Age"][[1]])

colnames(nba_train[, -ncol(nba_train)])
is.numeric(nba_train$Age)
c(selected_cols, "Salary")

library(car)
vif(baseline)
sig_cols <- c("FT.", "FTA", "FT", "FG", "GS", "Pos", "DBPM", "G", "Age", "Salary")
summary(lm(Salary~., data = nba_train[, sig_cols]))
test <- nba_train[, c("NBA_Country","TM","Pos","Team","T.Conf", "T.Div")]

mdl1 <- lm(Salary ~ WS + Pos + Pos:WS, data = nba_train)
summary(mdl1)

sig_cols <- c("FT.", "FTA", "FG", "GS", "Pos", "DBPM", "G", "Age", "Salary", "NBA_Country")
mdl2 <- lm(Salary~., data = nba_train[-c(73, 81), sig_cols])
vif(mdl2)
summary(mdl2)
plot(mdl2)

levs <- hatvalues(mdl2)
ri <- rstandard(mdl2)
cutoff <- 28 / 420
which(((levs >= cutoff) & (abs(ri) > 2)))

levs <- hatvalues(baseline)
ri <- rstandard(baseline)
cutoff <- 128 / 420
outs <- which(((levs >= cutoff) & (abs(ri) > 2)))
outs

nba_train$NBA_Country <- ifelse(nba_train$NBA_Country == "USA", "Yes", "No")
nba_test$NBA_Country <- ifelse(nba_test$NBA_Country == "USA", "Yes", "No")
cols <- c(3, 24, 30, 43, 49, 53, 55, 57, 58, 59, 62, 66)
baseline <- lm(Salary ~., data = nba_train[-outs, -cols])
summary(baseline)
cleaned_nba <- nba_train[-outs, -cols]
set.seed(005544529)
val_train_idx <- sample(1:418, size = 288, replace = F)
val_train <- cleaned_nba[val_train_idx, ]
val_test_idx <- (1:418)[-val_train_idx]
val_test <- cleaned_nba[val_test_idx,]

val_train_mdl <- lm(Salary ~., data = val_train)
summary(val_train_mdl)

y_hat <- predict(val_train_mdl, data = val_train, newdata = val_test)
rss <- sum((val_test$Salary - y_hat)^2)
sst <- 129 * var(val_test$Salary)
r2 <- 1 - (rss / sst)
r2

summary(baseline)

y_hat <- data.frame(Ob = 1:180, Salary = predict(baseline, data = nba_train[-outs,], newdata = nba_test))

table(ifelse(nba_train$NBA_Country == "USA", "Yes", "No"))
table(nba_train$NBA_Country)

write_csv(y_hat, "Desktop/Stats Courses/Spring '21/STATS 101A/HW4/y_hat.csv")
View(y_hat)

which(colnames(nba_train) == "PTS")
vifs <- vif(baseline)
which(vifs[, "GVIF"] == max(vifs[, "GVIF"]))
vifs

any(vifs[, "GVIF"] > 5)

# Starting point of obtaining valid model
while (any(vifs > 5)) {
  # Find VIFs
  vifs <- vif(baseline)
  # Find column name with max VIF
  temp <- names(which(vifs == max(vifs)))
  # Find index of column name
  idx <- which(colnames(nba_train) == temp)
  cols <- c(cols, idx)
  baseline <- lm(Salary ~., data = nba_train[, -cols])
}

independents <- colnames(nba_train[-outs, -cols])

levs <- hatvalues(baseline)
ri <- rstandard(baseline)
cutoff <- (2 * (420 - 394)) / 420
outs <- which(((levs >= cutoff) & (abs(ri) > 2)))
outs

baseline <- lm(Salary ~., data = nba_train[-outs, -cols])
summary(baseline)
par(mfrow = c(2,2))
plot(baseline)

cors <- numeric(length(independents))
idx <- 1
# Find correlation of every numerical variable with salary
for (i in independents) {
  if (is.numeric(nba_train[, i])) {
    cors[idx] <- cor(nba_train[, i], nba_train$Salary)
  } else {
    cors[idx] <- 0
  }
  idx <- idx + 1
}

# Order largest to smallest correlation
cor_mat <- data.frame(Variable = independents, Correlation = abs(cors))
cor_mat <- cor_mat %>%
  arrange(desc(Correlation))
fin_cols <- c(as.character(cor_mat$Variable[1:11]), "NBA_Country", "T.Conf")

baseline <- lm(Salary ~., nba_train[, fin_cols])

levs <- hatvalues(baseline)
ri <- rstandard(baseline)
cutoff <- (2 * (420 - 407)) / 420
outs <- which(((levs >= cutoff) & (abs(ri) > 2)))
outs

cleaned_nba <- nba_train[, fin_cols]
set.seed(005544529)
val_train_idx <- sample(1:420, size = 294, replace = F)
val_train <- cleaned_nba[val_train_idx, ]
val_test_idx <- (1:420)[-val_train_idx]
val_test <- cleaned_nba[val_test_idx,]

val_train_mdl <- lm(Salary ~., data = val_train)
summary(val_train_mdl)

y_hat <- predict(val_train_mdl, data = val_train, newdata = val_test)
rss <- sum((val_test$Salary - y_hat)^2)
sst <- 129 * var(val_test$Salary)
r2 <- 1 - (rss / sst)
r2

y_hat <- data.frame(Ob = 1:180, Salary = predict(baseline, data = nba_train[, fin_cols], newdata = nba_test))
write.csv(y_hat, "Desktop/Stats Courses/Spring '21/STATS 101A/Final Project/y_hat2.csv", row.names = F)


summary(powerTransform(cbind(
  Salary,
  OWS + 2.2,
  GS + 0.01,
  FG + 0.01,
  G,
  Age,
  AST + 0.01,
  TS. + 0.01,
  FT. + 0.01,
  FTr + 0.01,
  X2P. + 0.01) ~ 1,
  data = nba_train
  ))

for (i in fin_cols) {
  if (is.numeric(nba_train[, i]) && any(nba_train[, i] <= 0)) {
    print(i)
    print(min(nba_train[, i]))
  }
}

nba_train$tSalary <- (nba_train$Salary)^(0.16)
nba_train$tOWS <- (nba_train$OWS + 2.2)^(1/3)
nba_train$tGS <- (nba_train$GS + 0.01)^(0.16)
nba_train$tFG <- (nba_train$FG + 0.01)^(0.9)
nba_train$tAge <- (nba_train$Age)^(-1/2)
nba_train$tAST <- sqrt(nba_train$AST + 0.01)
nba_train$tTS. <- (nba_train$TS. + 0.01)^(3/2)
nba_train$tFT. <- (nba_train$FT. + 0.01)^(5/2)
nba_train$tFTr <- sqrt(nba_train$FTr + 0.01)
nba_train$tX2P. <- (nba_train$X2P. + 0.01)^(3/2)

t_cols <- c("tSalary",
            "tOWS",
            "tGS",
            "tFG",
            "G",
            "tAge",
            "tAST",
            "tTS.",
            "tFT.",
            "tFTr",
            "tX2P.",
            "NBA_Country",
            "T.Conf"
            )
tbaseline <- lm(tSalary ~., data = nba_train[-c(176, 227, 308), t_cols])
summary(tbaseline)
par(mfrow = c(2, 2))
plot(tbaseline)

levs <- hatvalues(tbaseline)
ri <- rstandard(tbaseline)
cutoff <- (2 * (418 - 405)) / 418
cutoff
outs <- which(((levs >= cutoff) & (abs(ri) > 2)))
outs


cleaned_nba <- nba_train[-c(176, 227, 308), t_cols]
set.seed(005544529)
val_train_idx <- sample(1:417, size = 292, replace = F)
val_train <- cleaned_nba[val_train_idx, ]
val_test_idx <- (1:417)[-val_train_idx]
val_test <- cleaned_nba[val_test_idx,]

val_train_mdl <- lm(tSalary ~., data = val_train)
summary(val_train_mdl)


y_hat <- (predict(val_train_mdl, data = val_train, newdata = val_test))^(1/0.16)
rss <- sum(((val_test$tSalary)^(1/0.16) - y_hat)^2)
sst <- 124 * var((val_test$tSalary)^(1/0.16))
r2 <- 1 - (rss / sst)
r2
y_hat

nba_test$tSalary <- (nba_test$Salary)^(0.16)
nba_test$tOWS <- (nba_test$OWS + 2.2)^(1/3)
nba_test$tGS <- (nba_test$GS + 0.01)^(0.16)
nba_test$tFG <- (nba_test$FG + 0.01)^(0.9)
nba_test$tAge <- (nba_test$Age)^(-1/2)
nba_test$tAST <- sqrt(nba_test$AST + 0.01)
nba_test$tTS. <- (nba_test$TS. + 0.01)^(3/2)
nba_test$tFT. <- (nba_test$FT. + 0.01)^(5/2)
nba_test$tFTr <- sqrt(nba_test$FTr + 0.01)
nba_test$tX2P. <- (nba_test$X2P. + 0.01)^(3/2)

y_hat <- data.frame(Ob = 1:180, Salary = (predict(tbaseline, data = nba_train, newdata = nba_test))^(1/0.16))
y_hat

write.csv(y_hat, "Desktop/Stats Courses/Spring '21/STATS 101A/Final Project/y_hat3.csv", row.names = F)

tSalary <- (nba_train$Salary)^(0.15)
tOWS <- (nba_train$OWS + 2.2)^(2/5)
tGS <- (nba_train$GS + 0.01)^(0.16)
tAge <- (nba_train$Age)^(-1/2)
G <- nba_train$G
FG <- nba_train$FG

summary(tbaseline)

par(mfrow = c(2, 2))
plot(tbaseline)

inverseResponsePlot(baseline)

summary(lm(Salary ~ WS, data = nba_train))
plot(lm(Salary ~ WS, data = nba_train))
summary(powerTransform(cbind(Salary, WS + 1.200001) ~ 1, data = nba_train))

# Create transformed position variable
group_positions <- function(x) {
  if (x == "PG" | x == "PG-SG" | x == "SG") {
    "Guard"
  } else {
    if (x == "C") {
      "Center"
    } else {
      "Forward"
    }
  }
}
nba_train$tPos <- apply(data.frame(nba_train$Pos), 1, group_positions)

aggregate(DRB~tPos, data = nba_train, FUN = sd)

z_score <- function(x) {
  (x - mean(x)) / sd(x)
}

std_DRB <- nba_train %>%
  group_by(tPos) %>%
    transmute(std_DRB = z_score(DRB))

nba_train$std_DRB <- as.matrix(std_DRB[, 2])
summary(lm(Salary ~ std_DRB, data = nba_train))

nbaTrain <- read.csv("Desktop/Stats Courses/Spring '21/STATS 101A/Final Project/NBATrain.csv")
nbaTrain <- nbaTrain[, -1]

# Find columns that are numeric and categoric
num_cols <- character(0)
cat_cols <- character(0)

for (i in colnames(nbaTrain)) {
  if (is.numeric(nbaTrain[, i])) {
    num_cols <- c(num_cols, i)
  } else {
    cat_cols <- c(cat_cols, i)
  }
}
num_cols

cors <- numeric(length(num_cols))
idx <- 1
# Find correlation of every numerical variable with salary
for (i in num_cols) {
  cors[idx] <- cor(nbaTrain$Salary, nbaTrain[, i])
  idx <- idx + 1
}
# Order largest to smallest correlation
cor_mat <- data.frame(Variable = num_cols, Correlation = abs(cors))
cor_mat <- cor_mat %>%
  arrange(desc(Correlation))

cor_mat

# Current model starts here (5/17)

# Eliminate variables with high VIF until none are greater than 5
vif_cols <- num_cols[-c(61, 51)]
baseline <- lm(Salary ~., data = nbaTrain[, num_cols[-51]])
vifs <- vif(baseline)
while (any(vifs > 5)) {
  # Find VIFs
  vifs <- vif(baseline)
  # Find column name with max VIF
  temp <- names(which(vifs == max(vifs))[1])
  # Find index of column name
  idx <- which(vif_cols == temp)
  # Remove column name
  vif_cols <- vif_cols[-idx]
  baseline <- lm(Salary ~., data = nbaTrain[, c("Salary", vif_cols)])
}
# Transform NBA_Country column
nbaTrain$NBA_Country <- ifelse(nbaTrain$NBA_Country == "USA", "Yes", "No")
cat_cols <- c("NBA_Country", "Pos", "T.Div")
summary(lm(Salary~., data = nbaTrain[, c("Salary", vif_cols, cat_cols)]))

# Transform Pos column
t_positions <- function(x) {
  if (x == "PG-SG") {
    "SG"
  } else {
    if (x == "SF-SG") {
      "SF"
    } else {
      x
    }
  }
}
nbaTrain$Pos <- apply(data.frame(nbaTrain$Pos), 1, t_positions)

idx <- 1
p_vals <- numeric(length(cur_cols))
# Run partial F-test for every variable
for (i in cur_cols) {
  mdl_full <- lm(Salary~., data = nbaTrain[, c("Salary", cur_cols)])
  mdl_reduced <- lm(Salary~., data = nbaTrain[, c("Salary", cur_cols[-idx])])
  partial_f <- anova(mdl_reduced, mdl_full)
  p_vals[idx] <- partial_f$`Pr(>F)`[2]
  idx <- idx + 1
}

pvals_df <- data.frame(Variable = cur_cols, Pvals = p_vals)
pvals_df %>%
  arrange(desc(Pvals))

# Save remaining columns
sig_cols <- c("T.DRtg", "G", "T.Div", "Pos", "FG", "OWS", "GS", "Age")
sig_mdl <- lm(Salary ~0 + ., data = nbaTrain[-outliers, c("Salary", sig_cols)])
summary(sig_mdl)
par(mfrow = c(2,2))
plot(sig_mdl)

# Remove outliers
levs <- hatvalues(sig_mdl)
ri <- rstandard(sig_mdl)
cutoff <- (2 * (420 - 404)) / 420
cutoff
outs <- which(((levs >= cutoff) & (abs(ri) > 2)))
outs

outliers <- c(278, 163, 313)

# Power transform variables
summary(powerTransform(cbind(T.DRtg, G, FG+0.0001, OWS+2.10001, GS+0.0001, Age)~1, data = nbaTrain))
summary(powerTransform(cbind(Salary)~1, data = nbaTrain))
nbaTrain_clean <- nbaTrain[-outliers, ]
dim(nbaTrain_clean)
tSalary <- (nbaTrain$Salary)^(0.1)
tT.DRtg <- (nbaTrain$T.DRtg)^(8.5)
tFG <- (nbaTrain$FG + 0.0001)^(.83)
tOWS <- sqrt(nbaTrain$FG + 2.10001)
tGS <- (nbaTrain$GS + 0.0001)^(.16)
tAge <- (nbaTrain$Age)^(-2/3)
# Saved transformed data in a new dataframe
tData <- data.frame(
  tSalary = tSalary,
  tT.DRtg = tT.DRtg,
  tFG = tFG, 
  tOWS = tOWS,
  tGS = tGS, 
  tAge = tAge, 
  G = nbaTrain$G, 
  T.Div = nbaTrain$T.Div, 
  Pos = nbaTrain$Pos
  )
View(tData)
dim(tData)
# Create model with transformed data
tSig_mdl <- lm(tSalary~., data = tData)
summary(tSig_mdl)
par(mfrow = c(2,2))
plot(tSig_mdl)

# Cross-validation
set.seed(005544529)
val_train_idx <- sample(1:420, size = 292, replace = F)
val_train <- tData[val_train_idx, ]
val_test_idx <- (1:420)[-val_train_idx]
val_test <- tData[val_test_idx,]

val_train_mdl <- lm(tSalary ~., data = val_train)
summary(val_train_mdl)

# Check validation R^2
y_hat_test_val <- (predict(val_train_mdl, data = val_train, newdata = val_test))^(10)
rss <- sum(((val_test$tSalary)^(10) - y_hat_test_val)^2)
sst <- 124 * var((val_test$tSalary)^(10))
r2 <- 1 - (rss / sst)
r2
y_hat

nbaTest <- read.csv("Desktop/Stats Courses/Spring '21/STATS 101A/Final Project/NBATestNoY.csv")
nbaTest <- nbaTest[, -1]

nbaTest$Pos <- apply(data.frame(nbaTest$Pos), 1, t_positions)

testT.DRtg <- (nbaTest$T.DRtg)^(8.5)
testFG <- (nbaTest$FG + 0.0001)^(.83)
testOWS <- sqrt(nbaTest$FG + 2.10001)
testGS <- (nbaTest$GS + 0.0001)^(.16)
testAge <- (nbaTest$Age)^(-2/3)

testData <- data.frame(
  tT.DRtg = testT.DRtg,
  tFG = testFG, 
  tOWS = testOWS,
  tGS = testGS, 
  tAge = testAge, 
  G = nbaTest$G, 
  T.Div = nbaTest$T.Div, 
  Pos = nbaTest$Pos
)

y_hat <- predict(tSig_mdl, data = tData, newdata = testData)^10

submission2 <- data.frame(Ob = 1:180, Salary = y_hat)
submission2

write.csv(submission2, "Desktop/Stats Courses/Spring '21/STATS 101A/HW5/submission2.csv", row.names = F)

summary(powerTransform(cbind(Salary)~1, data = nbaTrain_clean))

inverseResponsePlot(tSig_mdl)

y_hat <- predict(sig_mdl, data = nba_train[, c("Salary", sig_cols)], newdata = nba_test[, sig_cols])

submission <- data.frame(Ob = 1:180, Salary = y_hat)
submission

write.csv(submission, "Desktop/Stats Courses/Spring '21/STATS 101A/HW5/submission.csv", row.names = F)

par(mfrow = c(2, 2))
plot(sig_mdl)

cur_cols <- c(vif_cols, "Pos", "T.Div")
summary(lm(Salary~., data = nbaTrain[, c("Salary", cur_cols)]))

partial_f <- anova(mdl_full, mdl_reduced)

summary(mdl_full)
summary(mdl_reduced)

cors <- numeric(length(vif_cols))
idx <- 1
# Find correlation of every numerical variable with salary
for (i in vif_cols) {
  cors[idx] <- cor(nbaTrain$Salary, nbaTrain[, i])
  idx <- idx + 1
}
# Order largest to smallest correlation
cor_mat <- data.frame(Variable = vif_cols, Correlation = abs(cors))
cor_mat <- cor_mat %>%
  arrange(desc(Correlation))

cor_mat

# Group by richest
poorest <- c("New Orleans Pelicans", "Milwaukee Bucks", "Philadelphia 76ers", "Minnesota Timberwolves", "Memphis Grizzlies")
eights <- c("Atlanta Hawks", "Indiana Pacers", "Detroit Pistons", "Denver Nuggets")
nines <- c("Orlando Magic", "Sacramento Kings", "Oklahoma City Thunder", "Portland Trail Blazers", "Toronto Raptors")
billions <- c("Cleveland Cavaliers", "San Antonio Spurs", "Miami Heat", "Dallas Mavericks", "Houston Rockets", "Brooklyn Nets", "Golden State Warriors")
multi_bills <- c("Los Angeles Clippers", "Boston Celtics", "Chicago Bulls", "Los Angeles Lakers", "New York Knicks")

group_teams <- function(x) {
  if (x %in% poorest) {
    "Fifth"
  } else {
    if (x %in% eights) {
      "Fourth"
    } else {
      if (x %in% nines) {
        "Third"
      } else {
        if (x %in% billions) {
          "Second"
        } else {
          "First"
        }
      }
    }
  }
}

nba_train$Money_Rank <- sapply(nba_train$Team, group_teams)

which(colnames(nbaTrain) == "T.W")

ggplot(nba_train, aes(x = OWS, y = Salary, group = Money_Rank, color = Money_Rank)) + geom_point() + 
  geom_smooth(method = "lm")
# Team, TM, T.Conf, T.Div, NBA_Country, T.L, STL.
select_cols <- c(53, 3, 54, 55, 1, 57, 66, 56)
colnames(nbaTrain)[select_cols]

anova(lm(Salary~., data = cbind(nbaTrain[, -53], nba_train$Money_Rank)), lm(Salary~., data = nbaTrain[, -53]))
anova(lm(Salary~., data = nbaTrain[-c(176, 371), -select_cols]), lm(Salary~., data = nbaTrain[-c(176, 371), -select_cols[-length(select_cols)]]))

baseline <- lm(Salary~., data = nbaTrain[-c(176, 371), -select_cols])
summary(baseline)

# Remove outliers
levs <- hatvalues(baseline)
ri <- rstandard(baseline)
cutoff <- (2 * (418 - 356)) / 418
cutoff
outs <- which(((levs >= cutoff) & (abs(ri) > 2)))
outs

par(mfrow = c(2, 2))
plot(baseline)

y_hat <- predict(baseline, data = nbaTrain[-c(176, 371), -select_cols], newdata = nba_test[, -select_cols])

submission3 <- data.frame(Ob = 1:180, Salary = y_hat)
submission3

write.csv(submission3, "Desktop/Stats Courses/Spring '21/STATS 101A/HW5/submission3.csv", row.names = F)

candidates <- colnames(nbaTrain)[-c(select_cols, 67)]

temp_cols <- select_cols
remaining <- 58
# While there are still insignificant variables
repeat {
  p_vals <- numeric(remaining)
  idx <- 1
  # Run partial F-test for every variable
  for (i in (1:67)[-c(temp_cols, 67)]) {
    mdl_full <- lm(Salary~., data = nbaTrain[-c(176, 371), -temp_cols])
    mdl_reduced <- lm(Salary~., data = nbaTrain[-c(176, 371), -c(temp_cols, i)])
    partial_f <- anova(mdl_reduced, mdl_full)
    p_vals[idx] <- partial_f$`Pr(>F)`[2]
    idx <- idx + 1
  }
  # Create df of variables and their p-values
  pvals_df <- data.frame(Variable = colnames(nbaTrain)[(1:67)[-c(temp_cols, 67)]], Pvals = p_vals)
  # Sort by descending p-value
  pvals_df <- pvals_df %>%
    arrange(desc(Pvals))
  # If variable to remove is significant, break
  if (pvals_df[1, 2] < 0.05) {
    break
  }
  # Find index of removed column and append to temp_cols
  remove_idx <- which(colnames(nbaTrain) == pvals_df[1, 1])
  temp_cols <- c(temp_cols, remove_idx)
  # Print R^2 of current model
  temp_mdl <- summary(lm(Salary~., data = nbaTrain[-c(176, 371), -temp_cols]))
  print(paste("Current R^2: ", as.character(temp_mdl$r.squared)))
  print(paste("Predictors Remaining: ", as.character(remaining - 1)))
  remaining <- remaining - 1
}

pvals_df
cur_train <- nbaTrain[-c(176, 371, 163, 313, 383, 278, 386, 101, 260, 59), -temp_cols]
nbaTrain[c(176, 371, 163, 313, 383, 278, 386, 101, 260, 59), -temp_cols]
cur_train_2 <- nbaTrain[-c(176, 371, 163, 313, 383, 278, 386, 101, 260, 59), ]
tSalary <- (cur_train$Salary)^0.1
MPG <- cur_train$MP / cur_train$G 
cur_mdl <- lm(Salary~.+Age:MP + Age:G, data = cur_train)
anova(lm(Salary~. + Age:MP, data = cur_train), lm(Salary~. + Age:MP + Age:G, data = cur_train))
anova(lm(tSalary~.+Age:MP + Age:G - Salary - DWS - BPM - G - PTS, data = cur_train), 
      lm(tSalary~.+Age:MP + Age:G - Salary - DWS - BPM - G - PTS - MP, data = cur_train)
      )
summary(cur_mdl)
par(mfrow = c(2, 2))
plot(lm(tSalary~.+Age:MP + Age:G - Salary - DWS - BPM - G - PTS - MP, data = cur_train))

y_hat <- predict(cur_mdl, data = cur_train, newdata = nbaTest[, -temp_cols])


submission5 <- data.frame(Ob = 1:180, Salary = y_hat)
submission5

write.csv(submission5, "Desktop/Stats Courses/Spring '21/STATS 101A/HW5/submission5.csv", row.names = F)

mdl_sig <- lm(tSalary[-c(380, 223, 241, 302)]~.+ 0 + Age:MP + Age:G - DWS - BPM - G - PTS - MP, data = cur_train[-c(380, 223, 241, 302), -27])
summary(mdl_sig)
anova(mdl_sig) %>%
  arrange(desc(`Sum Sq`))

data.frame(vif(mdl_sig)) %>%
  arrange(desc(GVIF))


# Cross-validation
set.seed(005544529)
val_train_idx <- sample(1:410, size = 287, replace = F)
val_train <- cur_train[val_train_idx, ]
val_test_idx <- (1:410)[-val_train_idx]
val_test <- cur_train[val_test_idx,]

val_train_mdl <- lm(cur_train$Salary[val_train_idx]~.+Age:MP + Age:G, data = val_train)
summary(val_train_mdl)

# Check validation R^2
y_hat_test_val <- predict(val_train_mdl, data = val_train, newdata = val_test)
rss <- sum((cur_train$Salary[val_test_idx] - y_hat_test_val)^2)
sst <- 124 * var(cur_train$Salary[val_test_idx])
r2 <- 1 - (rss / sst)
r2

# diagPlot code
diagPlot<-function(model, cutoff, cook_cutoff){
  p1<-ggplot(model, aes(model$fitted,
                        model$residuals),label=rownames(bonds))+geom_point()
  p1<-p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red",
                                                linetype="dashed")
  p1<-p1+xlab("Fitted values")+ylab("Residuals")
  p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_bw()
  
  p2<-ggplot(model,aes(sample=rstandard(model))) + stat_qq() + stat_qq_line()
  p2<-p2+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
  p2<-p2+ggtitle("Normal Q-Q")
  
  p3<-ggplot(model, aes(model$fitted,
                        sqrt(abs(rstandard(model)))))+geom_point(na.rm=TRUE)
  p3<-p3+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value")
  p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
  p3<-p3+ggtitle("Scale-Location")+theme_bw()+geom_hline(yintercept=sqrt(2),
                                                         col="red", linetype="dashed")
  p4<-ggplot(model, aes(seq_along(cooks.distance(model)),
                        cooks.distance(model)))+geom_bar(stat="identity", position="identity")
  p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
  p4<-p4+ggtitle("Cook's distance")+theme_bw()+
    geom_hline(yintercept=cook_cutoff,
                                 col="red", linetype="dashed")
  p5<-ggplot(model, aes(hatvalues(model),
                        rstandard(model)))+geom_point(aes(size=cooks.distance(model)), na.rm=TRUE)
  p5<-p5+stat_smooth(method="loess", na.rm=TRUE)
  p5<-p5+xlab("Leverage")+ylab("Standardized Residuals")
  p5<-p5+ggtitle("Residual vs Leverage Plot")
  p5<-p5+scale_size_continuous("Cook's Distance", range=c(1,5))
  p5<-p5+theme_bw()+theme(legend.position="bottom")+geom_hline(yintercept=c(-2,2), col="red",
                                                               linetype="dashed")+geom_vline(xintercept=cutoff, col="blue",
                                                                                             linetype="dashed")+ylim(-4,4)
  
  p6<-ggplot(model, aes(hatvalues(model),
                        cooks.distance(model)))+geom_point(na.rm=TRUE)+stat_smooth(method="loess",
                                                                                   na.rm=TRUE)
  p6<-p6+xlab("Leverage hii")+ylab("Cook's Distance")
  p6<-p6+ggtitle("Cook's dist vs Leverage")
  p6<-p6+geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")
  p6<-p6+theme_bw()
  return(list(rvfPlot=p1, qqPlot=p2, sclLocPlot=p3, cdPlot=p4, rvlevPlot=p5,
              cvlPlot=p6))
}

lev_cutoff <- (2*(409 - 382))/409
lev_cutoff
levs <- hatvalues(mdl_sig)
ri <- rstandard(mdl_sig)
which(levs >= lev_cutoff & abs(ri) >= 2)

y_hat <- predict(mdl_sig, newdata = nbaTest[, -temp_cols])^10


submission6 <- data.frame(Ob = 1:180, Salary = y_hat)
submission6

write.csv(submission6, "Desktop/Stats Courses/Spring '21/STATS 101A/HW5/submission6.csv", row.names = F)
summary(y_hat)
summary(nbaTrain$Salary)

library(leaps)
install.packages("leaps")

backAIC <- step(lm(Salary~., data = nbaTrain), direction = "backward", data = nbaTrain, k = log(420))

tSalary <- nbaTrain$Salary^(0.1)
backBIC_mdl <- lm(tSalary ~ - Salary + Age + G + MP + PER + TS. + TOV. + DWS + OBPM + Pos + 
                 FG + X2P + X2PA + FT + PTS, data = nbaTrain)
summary(backBIC_mdl)
vif(backBIC_mdl)
backAIC <- step(lm(Salary~., data = nbaTrain), direction = "backward", data = nbaTrain)
backAIC_mdl <- lm(Salary ~ NBA_Country + Age + TM + G + MP + PER + DRB. + BLK. + 
                    TOV. + USG. + OWS + DWS + WS + WS.48 + OBPM + DBPM + BPM + 
                    Pos + GS + FG + FGA + X3PA + X2P + X2PA + FT + FTA + FT. + 
                    TRB + PTS, data = nbaTrain)
summary(backAIC_mdl)
anova(backBIC_mdl, backAIC_mdl)

y_hat <- predict(mdl_sig, newdata = nbaTest[, -temp_cols])^10

par(mfrow = c(2, 2))
plot(backBIC_mdl)

submission7 <- data.frame(Ob = 1:180, Salary = y_hat)
submission7

write.csv(submission7, "Desktop/Stats Courses/Spring '21/STATS 101A/HW5/submission7.csv", row.names = F)

y_hat <- predict()
