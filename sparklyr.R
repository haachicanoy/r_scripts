options(warn = -1); options(scipen = 999)

suppressMessages(library(sparklyr))
# Open the Spark connection
sc <- spark_connect(master = "local")

suppressMessages(library(rsparkling))

options(rsparkling.sparklingwater.version = "1.6.8")

# Load libraries
suppressMessages(library(h2o))
suppressMessages(library(dplyr))

# Load mtcars to Spark memory
mtcars_tbl <- copy_to(sc, mtcars, "mtcars")

# Transform our data set, and then partition into 'training', 'test'
partitions <- mtcars_tbl %>%
  filter(hp >= 100) %>%
  mutate(cyl8 = cyl == 8) %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 1099)

# Partitionate datasets
training <- as_h2o_frame(sc, partitions$training, strict_version_check = FALSE)
test <- as_h2o_frame(sc, partitions$test, strict_version_check = FALSE)

spark_disconnect(sc)
