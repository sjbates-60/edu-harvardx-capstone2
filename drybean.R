# Outline -----------------------------------------------------------------
# Subject: Selecting Features in a Dry Bean Dataset
# Author: Samuel Bates
#
require(tidyverse)
require(caret)
require(readxl)
require(log4r)
require(RColorBrewer)

options(digits = 5)

## Utility functions ------------------------------------------------------
# Converts a vector of strings into a string containing a comma-separated list.
v2s <- function(v) paste(v, collapse = ",")

# Converts a string containing a comma-separated list into a vector of strings.
s2v <- function(s) strsplit(s, ",") %>% unlist()

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


## 3. Compare the accuracy of several models----------------------------------
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

# Trains a model on a set of features and reports its accuracy on a different
# set of features.
# Parameters:
#   trainMethod  - the model to train.
#   features.tr  - a data frame containing the features for training.
#   features.pr  - a data frame containing the features for predicting.
#   results      - a vector containing the actual values on features.pr.
# Returns:
#   The accuracy of the model's predictions.
#
compute_model_accuracy <- function(trainMethod, features.tr, features.pr,
                                   results) {
  log_info(sprintf("Checking model '%s' with features {%s}", trainMethod,
                   v2s(colnames(features.tr))))

  suppressWarnings(set.seed(16, sample.kind = "Rounding"))
  fit <- suppressWarnings(train(features, results, method = trainMethod))
  p <- predict(fit, newdata = features.pr)
  acc <- confusionMatrix(p, results)$overall[["Accuracy"]]
  log_info(sprintf("  Accuracy: %f", acc))
  acc
}

log_info("Performing principal components analysis...")

pca <- prcomp(training %>% select(-Class))

log_info("Computing the accuracy of several models on multiple feature sets...")

features.min <- setdiff(features, c("K", "Ed", "R", "C", "CO", "S"))

models <- c("lda", "qda", "knn", "treebag")

accuracies <- map_dfr(models, function(m) {
  tibble(model = m,
         acc.all = compute_model_accuracy(
           m,
           training %>% select(all_of(features)),
           training %>% select(all_of(features)),
           training$Class),
         acc.min = compute_model_accuracy(
           m,
           training %>% select(all_of(features.min)),
           training %>% select(all_of(features.min)),
           training$Class),
         acc.pca2 = compute_model_accuracy(
           m,
           pca$x[, 1:2],
           pca$x[, 1:2],
           training$Class),
         acc.pca6 = compute_model_accuracy(
           m,
           pca$x[, 1:6],
           pca$x[, 1:6],
           training$Class))
})
saveRDS(accuracies, file = "training_accuracies.rds")


## 4. Discover a minimal set of features that maximizes accuracy--------------
#

# Finds features that maximize the accuracy of a model when added to an
# existing set of features. This is a recursive function that halts either
# when there are no more features to add, or when no feature increases the
# accuracy of the model.
#
# Parameters:
#   current     - The current state, containing:
#                   model - The model being trained.
#                   fset  - A comma-separated list of features.
#                   acc   - The accuracy of the model on fset.
#   feature_set - The complete set of features that can be used.
# Returns:
#   A final state, containing model, fset, and acc, such that no feature
#   in feature_set will increase the accuracy of the model.
#
fewest_features <- function(current, feature_set) {
  # Get the lists of used and unused features.
  used <- s2v(current$fset)
  unused <- setdiff(feature_set, used)

  if (length(unused) == 0)
    # In this case, there are no more features to try,
    # so we return the current result.
    current
  else {
    # Loop through the unused features, calculating the accuracy
    # achieved when adding each to the used features.
    accs <- sapply(unused, function(f) {
      compute_model_accuracy(current$model,
                             training %>% select(all_of(c(used, f))),
                             training %>% select(all_of(c(used, f))),
                             training$Class)
    })
    # Get the highest accuracy value.
    ind <- which.max(accs)
    if (accs[ind] <= current$acc)
      # In this case, none of the new features yields a higher accuracy
      # than what we already have, so we return the current result.
      current
    else {
      # In this case, at least one feature yields a higher accuracy value.
      # Take the one that maximizes the accuracy, add it to the set of
      # used features, and call the function again with the new accuracy
      # and used feature set.
      fset2 <- v2s(c(used, unused[ind]))
      fewest_features(tibble(model = current$model,
                             acc = accs[ind],
                             fset = fset2),
                      feature_set)
    }
  }
}

options(pillar.sigfig = 6)

# For each model, start with the empty set of features and find a minimal
# set of features that maximizes accuracy when all features are available.
model_results <- map_dfr(models, function(m) {
  bestfit <- fewest_features(tibble(model = m, acc = 0.0, fset = ""), features)
  log_info(sprintf("All features: Fewest for '%s' is {%s} with accuracy %f.",
                   bestfit$model, bestfit$fset, bestfit$acc))
  log_info("")
  bestfit
})
saveRDS(model_results, file = "fewest_all.rds")

# For each model, start with the empty set of features and find a minimal
# set of features that maximizes accuracy when only the mathemtically
# independent features are available.
model_results <- map_dfr(models, function(m) {
  bestfit <- fewest_features(tibble(model = m, acc = 0.0, fset = ""),
                             features.min)
  log_info(sprintf("Minimum features: Fewest for '%s' is {%s} with accuracy %f.",
                   bestfit$model, bestfit$fset, bestfit$acc))
  log_info("")
  bestfit
})
saveRDS(model_results, file = "fewest_min.rds")


## 5. Compare the minimal sets to treebag's own reporting of importance-------
#

log_info(sprintf("Training model 'treebag' on {%s}...", v2s(features)))
suppressWarnings(set.seed(16, sample.kind = "Rounding"))
fit_all <- suppressWarnings(train(training %>% select(all_of(features)),
                                  training$Class,
                                  method = "treebag"))
varImp_all <- varImp(fit_all)$importance
mostImp_all <- varImp_all %>% filter(Overall > 50) %>% rowNames()
log_info(sprintf("Most important features are {%s}.", v2s(mostImp_all)))
acc.mostImp_all <- compute_model_accuracy(
  "treebag",
  training %>% select(all_of(mostImp_all)),
  training %>% select(all_of(mostImp_all)),
  training$Class)

log_info(sprintf("Training model 'treebag' on {%s}...", v2s(features.min)))
suppressWarnings(set.seed(16, sample.kind = "Rounding"))
fit_min <- suppressWarnings(train(training %>% select(all_of(features.min)),
                                  training$Class,
                                  method = "treebag"))
varImp_min <- varImp(fit_min)$importance
mostImp_min <- varImp_min %>% filter(Overall > 50) %>% rowNames()
log_info(sprintf("Most important features are {%s}.", v2s(mostImp_min)))
acc.mostImp_min <- compute_model_accuracy(
  "treebag",
  training %>% select(all_of(mostImp_min)),
  training %>% select(all_of(mostImp_min)),
  training$Class)


## 6. Calculate accuracies of the various models on the test set--------------
#

