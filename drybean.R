# Outline -----------------------------------------------------------------
# Subject:
# Author: Samuel Bates
#
require(tidyverse)
require(caret)
require(readxl)
require(log4r)
require(RColorBrewer)

options(digits = 5)

## Logging functions ------------------------------------------------------
z__logger <- NULL

#-------------------------------------------------------------
# Set up a simple log with time stamps. The log will write to
# both the console and a log file.
#
log_start <- function() {
  if (!dir.exists("logs"))
    dir.create("logs")
  filename <- format(Sys.time(), "logs/%Y%m%d_%H%M%S.txt")
  console_appender <- console_appender(layout = default_log_layout())
  file_appender <- file_appender(filename, append = TRUE,
                                 layout = default_log_layout())
  z__logger <<- log4r::logger(threshold = "INFO",
                              appenders = list(console_appender,
                                               file_appender))
}

#-------------------------------------------------------------
# Write a message to the console and a log file.
log_info <- function(msg) {
  if (is.null(z__logger))
    log_start()
  log4r::info(z__logger, msg)
}


# Main Program ------------------------------------------------------------


## 1. Load data --------------------------------------------------------------
log_info("Dry Bean Dataset analysis")
# log_info("Downloading data files...")
# beanUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00602/DryBeanbeans.zip"
# download.file(beanUrl, destfile = "bean.zip")
# unzip("bean.zip", files = c("DryBeanDataset/Dry_Bean_Dataset.txt",
#                             "DryBeanDataset/Dry_Bean_Dataset.xlsx"))
# file.remove("bean.zip")
# remove(beanUrl)

log_info("Loading data...")
beans <- read_excel("DryBeanDataset/Dry_Bean_Dataset.xlsx")
beans <- beans %>% mutate(Class = factor(str_to_title(Class)))
colnames(beans) <- c("A", "P", "X", "x", "K", "Ec", "C", "Ed", "Ex", "S", "R",
                     "CO", "SF1", "SF2", "SF3", "SF4", "Class")
bean_types <- unique(beans$Class)
bean_proportions <- beans %>%
  group_by(Class) %>%
  summarize(n = n(), p = n() / nrow(beans))

features <- setdiff(colnames(beans), "Class")

## 2. Verify mathematical relationships among features------------------------
#  Do this by showing the absolute value of the maximum difference is very
#  close to zero.
#
log_info("Verifying mathematical relationships...")

near_zero <- 10^-10
stopifnot("Aspect ratio: K is not equal to X/x" =
            max(abs(beans$K - beans$X / beans$x)) < near_zero)

stopifnot("Equivalent diameter: A is not equal to pi*Ed^2/4" =
            max(abs(beans$A - pi * beans$Ed^2 / 4)) < near_zero)

stopifnot("Solidity: S is not equal to A/C" =
            max(abs(beans$S - beans$A / beans$C)) < near_zero)

stopifnot("Roundness: R is not equal to 4*pi*A/P^2" =
            max(abs(beans$R - 4 * pi * beans$A / beans$P^2)) < near_zero)

stopifnot("Compactness: CO is not equal to Ed/X" =
            max(abs(beans$CO - beans$Ed / beans$X)) < near_zero)

## 3. Compare accuracy of several models--------------------------------------
#
log_info("Creating training and testing sets...")
suppressWarnings(set.seed(16, sample.kind = "Rounding"))
test_index <- createDataPartition(beans$Class, times = 1, p = 0.1,
                                  list = FALSE)
training <- beans[-test_index, ]
testing <- beans[test_index, ]
# Verify all classes are in both sets
stopifnot(unique(testing$Class) == bean_types,
          unique(training$Class) == bean_types)

remove(test_index, beans)

# Trains a model on a set of features and reports the accuracy of the result.
# Parameters:
#   trainMethod  - the model to train.
#   features     - a data frame containing the features.
#   results      - a vector containing the classification results.
#   trControl    - an optional training control method.
#   tuneGrid     - optional tuning parameters.
#
compute_model_accuracy <- function(trainMethod, features, results,
                                   trControl = trainControl(),
                                   tuneGrid = NULL) {
  log_info(sprintf("Checking model '%s' with features {%s}", trainMethod,
                   paste(colnames(features), collapse = ", ")))

  suppressWarnings(set.seed(16, sample.kind = "Rounding"))
  suppressWarnings(fit <- train(features, results, method = trainMethod,
                                trControl = trControl,
                                tuneGrid = tuneGrid))
  p <- predict(fit, newdata = features)
  acc <- confusionMatrix(p, results)$overall[["Accuracy"]]
  log_info(sprintf("  Accuracy: %f", acc))
  acc
}

log_info("Computing the accuracy of several models...")

features.min <- setdiff(features, c("K", "Ed", "R", "C", "CO", "S"))

models <- c("naive_bayes", "lda", "qda", "knn", "kknn", "treebag")

accuracies <- map_dfr(models, function(m) {
  tibble(model = m,
         acc.all = compute_model_accuracy(
           m,
           training %>% select(all_of(features)),
           training$Class),
         acc,min = compute_model_accuracy(
           m,
           training %>% select(all_of(features.min)),
           training$Class))
})
saveRDS(accuracies, file == "accuracies.rds")
