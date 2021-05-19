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
beans <- beans %>% mutate(Class = factor(Class))
colnames(beans) <- c("A", "P", "L", "l", "K", "Ec", "C", "Ed", "Ex", "S", "R",
                     "CO", "SF1", "SF2", "SF3", "SF4", "Class")
bean_types <- unique(beans$Class)
bean_proportions <- beans %>% 
  group_by(Class) %>%
  summarize(p = n() / nrow(beans))

features <- setdiff(colnames(beans), "Class")

suppressWarnings(set.seed(16, sample.kind = "Rounding"))
test_index <- createDataPartition(beans$Class, times = 1, p = 0.1, 
                                  list = FALSE)
training <- beans[-test_index, ]
test <- beans[test_index, ] 
# Verify all classes are in both sets
stopifnot(unique(test$Class) == bean_types, 
          unique(training$Class) == bean_types)

remove(beanUrl, test_index, beans)

