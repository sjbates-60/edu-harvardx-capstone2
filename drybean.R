# Outline -----------------------------------------------------------------
# Subject: Selecting Features in a Dry Bean Dataset
# Author: Samuel Bates
#
# This file contains the code used to achieve the results reported in
# drybean.Rmd and its output, drybean.pdf. It has the following sections:
#
# Utility functions:
#     Functions for converting between vectors of strings and strings
#     containing a comma-separated list. Used to compress and expand
#     sets of features.
#
# Logging functions:
#     Functions for writing results with timestamps to the console and
#     a log file. Used to gauge how long various operations take.
#
# Main program:
#   1. Loads the data.
#   2. Verifies mathematical relationships among the features.
#   3. Compares the accuracy of four models on three sets of features.
#   4. Discovers a minimal set of features that maximizes accuracy
#      for each model.
#   5. Finds the most important features as reported by the treebag model,
#   6. Calculates the accuracy of each model and set of features on the
#      test set.
#
# The main program creates several .rds files that are used in the .Rmd
# file, so it should be executed first.
#
require(tidyverse)
require(caret)
require(readxl)
require(log4r)
require(RColorBrewer)

options(digits = 5, pillar.sigfig = 5)

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
# beanUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00602/DryBeanDataset.zip"
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

features_all <- setdiff(colnames(beans), "Class")


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
stopifnot("The testing set does not have all bean types" =
            unique(testing$Class) == bean_types)
stopifnot("The training set does not have all bean types" =
          unique(training$Class) == bean_types)

remove(test_index, beans, bean_types)

# Trains a model on a dataset and reports its accuracy on a different dataset.
#
# Parameters:
#   train_model   - the model to train.
#   train_data    - a data frame containing the observations for training.
#   train_results - a vector containing the actual values on train_data.
#   pred_data     - a data frame containing the observations for predicting.
#   pred_results  - a vector containing the actual values on pred_data.
# Returns:
#   The accuracy of the model's predictions.
#
compute_model_accuracy <- function(train_model, train_data, train_results,
                                   pred_data, pred_results) {
  log_info(sprintf("Checking model '%s' with features {%s}", train_model,
                   v2s(colnames(train_data))))

  suppressWarnings(set.seed(16, sample.kind = "Rounding"))
  fit <- suppressWarnings(train(train_data, train_results,
                                method = train_model))
  p <- predict(fit, newdata = pred_data)
  acc <- confusionMatrix(p, pred_results)$overall[["Accuracy"]]
  log_info(sprintf("  Accuracy: %f", acc))
  acc
}

log_info("Performing principal components analysis...")

pca <- prcomp(training %>% select(-Class))

log_info("Computing the accuracy of several models on multiple feature sets...")

features_min <- setdiff(features_all, c("K", "Ed", "R", "C", "CO", "S"))

models <- c("lda", "qda", "knn", "rpart2", "treebag")

training_accuracies <- map_dfr(models, function(m) {
  acc_all <- compute_model_accuracy(m,
                                    training %>% select(all_of(features_all)),
                                    training$Class,
                                    training %>% select(all_of(features_all)),
                                    training$Class)
  acc_min <- compute_model_accuracy(m,
                                    training %>% select(all_of(features_min)),
                                    training$Class,
                                    training %>% select(all_of(features_min)),
                                    training$Class)
    tibble(model = c(m, m),
           name = c("whole", "min"),
           accuracy = c(acc_all, acc_min),
           label = c(v2s(features_all), v2s(features_min)))
})
saveRDS(training_accuracies, file = "training_accuracies.rds")


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
#
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
                             training$Class,
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
      # Take one that maximizes the accuracy, add it to the set of used
      # features, and call the function again with the new accuracy and
      # used feature set.
      fset2 <- v2s(c(used, unused[ind]))
      fewest_features(tibble(model = current$model,
                             fset = fset2,
                             acc = accs[ind]),
                      feature_set)
    }
  }
}

# For each model, start with the empty set of features and find a minimal
# set of features that maximizes accuracy when all features are available.
fewest_all <- map_dfr(models, function(m) {
  bestfit <- fewest_features(tibble(model = m, acc = 0.0, fset = ""),
                             features_all)
  log_info(sprintf("All features: Fewest for '%s' is {%s} with accuracy %f.",
                   bestfit$model, bestfit$fset, bestfit$acc))
  log_info("")
  tibble(model = m,
         name = "few.whole",
         accuracy = bestfit$acc,
         label = bestfit$fset)
})
saveRDS(fewest_all, file = "fewest_all.rds")

# For each model, start with the empty set of features and find a minimal
# set of features that maximizes accuracy when only the mathemtically
# independent features are available.
fewest_min <- map_dfr(models, function(m) {
  bestfit <- fewest_features(tibble(model = m, acc = 0.0, fset = ""),
                             features_min)
  log_info(sprintf(
    "Minimum features: Fewest for '%s' is {%s} with accuracy %f.",
    bestfit$model, bestfit$fset, bestfit$acc))
  log_info("")
  tibble(model = m,
         name = "few.min",
         accuracy = bestfit$acc,
         label = bestfit$fset)
})
saveRDS(fewest_min, file = "fewest_min.rds")


## 5. Compare the minimal sets to a model's own reporting of importance-------
#
models_with_importance <- c("rpart2", "treebag")

importance_accuracies <- map_dfr(models_with_importance, function(m) {
  log_info(sprintf("Training model '%s' on {%s}...", m, v2s(features_all)))
  suppressWarnings(set.seed(16, sample.kind = "Rounding"))
  fit_all <- suppressWarnings(train(training %>% select(all_of(features_all)),
                                    training$Class,
                                    method = m))
  var_imp_all <- varImp(fit_all)$importance
  saveRDS(var_imp_all, file = sprintf("%s_var_imp_all.rds", m))
  most_imp_all <- var_imp_all %>% filter(Overall > 50) %>% rownames()
  log_info(sprintf("Most important features are {%s}.", v2s(most_imp_all)))
  acc_most_imp_all <- compute_model_accuracy(
    m,
    training %>% select(all_of(most_imp_all)),
    training$Class,
    training %>% select(all_of(most_imp_all)),
    training$Class)

  log_info(sprintf("Training model '%s' on {%s}...", m, v2s(features_min)))
  suppressWarnings(set.seed(16, sample.kind = "Rounding"))
  fit_min <- suppressWarnings(train(training %>% select(all_of(features_min)),
                                    training$Class,
                                    method = "treebag"))
  var_imp_min <- varImp(fit_min)$importance
  saveRDS(var_imp_min, file = sprintf("%s_var_imp_min.rds", m))
  most_imp_min <- var_imp_min %>% filter(Overall > 50) %>% rownames()
  log_info(sprintf("Most important features are {%s}.", v2s(most_imp_min)))
  acc_most_imp_min <- compute_model_accuracy(
    m,
    training %>% select(all_of(most_imp_min)),
    training$Class,
    training %>% select(all_of(most_imp_min)),
    training$Class)

  tibble(model = c(m, m),
         name = c("imp.whole", "imp.min"),
         accuracy = c(acc_most_imp_all, acc_most_imp_min),
         label = c(v2s(most_imp_all), v2s(most_imp_min)))
})
saveRDS(importance_accuracies, file = "importance_accuracies.rds")


## 6. Calculate accuracies of the various models on the test set--------------
#

test_accuracies <- map_dfr(models, function(m) {
  fewest_fset_all <- fewest_all %>% filter(model == m) %>% pull(label) %>% s2v()
  fewest_fset_min <- fewest_min %>% filter(model == m) %>% pull(label) %>% s2v()
  acc_all <- compute_model_accuracy(
    m,
    training %>% select(all_of(features_all)),
    training$Class,
    testing %>% select(all_of(features_all)),
    testing$Class)
  acc_min <- compute_model_accuracy(
    m,
    training %>% select(all_of(features_min)),
    training$Class,
    testing %>% select(all_of(features_min)),
    testing$Class)
  acc_few_all <- compute_model_accuracy(
    m,
    training %>% select(all_of(fewest_fset_all)),
    training$Class,
    testing %>% select(all_of(fewest_fset_all)),
    testing$Class)
  acc_few_min <- compute_model_accuracy(
    m,
    training %>% select(all_of(fewest_fset_min)),
    training$Class,
    testing %>% select(all_of(fewest_fset_min)),
    testing$Class)
  tibble(model = rep(m, 4),
         name = c("whole", "min", "few.whole", "few.min"),
         accuracy = c(acc_all, acc_min, acc_few_all, acc_few_min),
         label = c("", "", v2s(fewest_fset_all), v2s(fewest_fset_min)))
})
saveRDS(test_accuracies, file = "test_accuracies.rds")
