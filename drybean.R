# Outline -----------------------------------------------------------------
# Subject:
# Author: Samuel Bates
#
require(tidyverse)
require(caret)
require(readxl)
require(log4r)
require(RColorBrewer)

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
# beanUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00602/DryBeanbeans.zip"
# download.file(beanUrl, destfile = "bean.zip")
# unzip("bean.zip", files = c("DryBeanbeans/Dry_Bean_beans.txt",
#                             "DryBeanbeans/Dry_Bean_beans.xlsx"))
# file.remove("bean.zip")

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

# suppressWarnings(set.seed(16, sample.kind = "Rounding"))
# test_index <- createDataPartition(beans$Class, times = 1, p = 0.1,
#                                   list = FALSE)
# training <- beans[-test_index, ]
# test <- beans[test_index, ]
# # Verify all classes are in both sets
# stopifnot(unique(test$Class) == bean_types,
#           unique(training$Class) == bean_types)
#
# remove(beanUrl, test_index, beans)
