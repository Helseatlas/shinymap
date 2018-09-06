# Just testing


myfile <- "tests/testthat/data/eldre.js"


tmp <- readIAjson(json_file = myfile)

tmp2 <- readRDS("tests/testthat/data/json1")

length(tmp2$bo)


length(tmp$bo)

akershus <- dplyr::filter(tmp, bo == "Førde")
akershus2 <- dplyr::filter(tmp2, bo == "Førde")

test3 <- df.changes(akershus2, akershus, KEYS = c("bo"))


plotVariasjon(fullDataFrame = tmp, which_id = "i3")

test <- compare::compare(tmp, tmp2)

test2 <- dplyr::all_equal(tmp, tmp2)


test3 <- df.changes(tmp2, tmp, KEYS = c("level1"))

test2

test$tMpartial

httr::set_config(httr::use_proxy(url="http://www-proxy.helsenord.no", port=8080))
devtools::install_github("helseatlas/shinymap", ref = "readIAjson")



# Read the json file
json_data <- jsonlite::fromJSON(json_file)

# make it a tibble data fram
tbl <- tibble::as_data_frame(json_data$geographies)

# Names of areas are located in json_data$geographies$features
bo <- data.frame(tbl$features)

# Name of reference is located in json_data$geographies$comparisonFeatures
ref <- data.frame(tbl$comparisonFeatures)

# All data
themes <- data.frame(tbl$themes) %>% tibble::as_data_frame()

# Names, highest level
level1 <- themes$name

# Test that number of highest level is equal the length of themes$indicators
if (length(themes$name) != length(themes$indicators)){
  stop("Something fishy in your json file. ")
}

all_data <- data.frame()

test <- themes$indicators[2]

test2 <- data.frame(test)

test2$date

print()

next_level <- data.frame(themes$indicators[1])

next_level$id
next_level$values

themes$name
next_level$values[1]

next_level$associates


all_data <- data.frame()
for (i in 1:length(themes$indicators)){
  level1 <- themes$name[i]
  next_level <- data.frame(themes$indicators[i])
  rates <- data.frame(next_level$values)  %>% tibble::as_data_frame()
  for (j in 1:length(next_level)){
    if (!is.na(next_level$id[j])){
      level2 <- next_level$name[j]
      level3 <- next_level$date[j]
      selection_id <- next_level$id[j]
      combined <- data.frame(bo$name, level1, level2, level3, selection_id, rates[j])

      colnames(combined)[1] <- "bo"
      colnames(combined)[2] <- "level1"
      colnames(combined)[3] <- "level2"
      colnames(combined)[4] <- "level3"
      colnames(combined)[5] <- "id"
      colnames(combined)[6] <- "rate"

      all_data <- rbind(all_data, combined)
    }
  }
}

print(all_data)


# From https://codereview.stackexchange.com/questions/94253/identify-changes-between-two-data-frames-explain-deltas-for-x-columns

df.changes <- function(df.old, df.new,
                       KEYS = c("id"),
                       VAL = NULL,
                       retain.columns = NULL) {
  # input checks
  stopifnot(KEYS %in% names(df.old),
            KEYS %in% names(df.new),
            VAL %in% names(df.old),
            VAL %in% names(df.new),
            retain.columns %in% names(df.new),
            retain.columns %in% names(df.old))

  # add columns to help us track new/old provenance
  N <- transform(df.new, is = TRUE)
  O <- transform(df.old, is = TRUE)

  # merge
  M <- merge(N, O, by = KEYS, all = TRUE, suffixes = c(".new",".old"))
  M$is.new <- !is.na(M$is.new) # replace NA with FALSE
  M$is.old <- !is.na(M$is.old) # replace NA with FALSE

  # this will be our output
  O <- M[KEYS]

  # add rows.changed
  O$row.changed <- with(M, ifelse(is.old & is.new, "10.Retained",
                                  ifelse(is.old,          "05. Lost",
                                         "00. New")))
  # add data from new
  original.vars <- setdiff(names(df.new), KEYS)
  for (var in original.vars)
    O[[var]] <- M[[paste0(var, ".new")]]

  # modify data for retain.columns
  for (var in retain.columns)
    O[[var]] <- ifelse(M$is.new, M[[paste0(var, ".new")]],
                       M[[paste0(var, ".old")]])


  # add comparisons
  for (var in VAL) {
    old.var <- paste0(var, ".old")
    new.var <- paste0(var, ".new")
    del.var <- paste0(var, ".delta")
    O[[del.var]] <- M[[new.var]] - M[[old.var]]
    O[[old.var]] <- M[[old.var]]
    O[[new.var]] <- M[[new.var]]
  }

  # reorder rows
  O[order(O$row.changed), ]
}
