# Install necessary packages
library(data.table) # data.table_1.14.8
library(sqldf)      # sqldf_0.4-11
library(stringr)    # stringr_1.5.0

start_time <- Sys.time()

# Define the chunk size (e.g., 1,000,000 rows)
chunk_size <- 1000000

# Open a connection to source file (must be in the same folder as this file)
con <- file(paste0(getwd(),"/tweets2009-07.txt"), "r")

# Initialize empty data.tables to store the results
July_1 <- data.table(from = character(), to = character(), weight = integer())
July_2 <- data.table(from = character(), to = character(), weight = integer())
July_3 <- data.table(from = character(), to = character(), weight = integer())
July_4 <- data.table(from = character(), to = character(), weight = integer())
July_5 <- data.table(from = character(), to = character(), weight = integer())
topic_1 <- data.table(user = character(), topic_of_interest = character())
topic_2 <- data.table(user = character(), topic_of_interest = character())
topic_3 <- data.table(user = character(), topic_of_interest = character())
topic_4 <- data.table(user = character(), topic_of_interest = character())
topic_5 <- data.table(user = character(), topic_of_interest = character())

# Skip the first line
lines <- readLines(con, n = 1)

# Read the data in chunks of size multiple of 3 (no loss of data)
while (length(lines <- readLines(con, n = 3*chunk_size)) > 0) {
  
  # Remove empty lines
  non_empty_lines <- lines[nzchar(lines)]
  
  # Check if there are any lines remaining
  if (length(non_empty_lines) > 0) {
    
    # Split T, U, W lines into columns and create a data.table from the chunks
    chunk_dt <- transpose(as.data.table(split(non_empty_lines, 
                                      (seq_along(non_empty_lines) - 1) %/% 3)))
    query <- paste("SELECT SUBSTRING(V1,3,10) AS DAY, LOWER(SUBSTRING(V2,22))",
                " AS ORIGIN, LOWER(SUBSTRING(V3,3)) AS TEXT FROM chunk_dt",
                "WHERE V3 LIKE '%@%' OR V3 LIKE '%#%'")
    view <- as.data.table(sqldf(query))
    
    # Find mentions and topics
    # https://help.twitter.com/en/
    # using-twitter/replies-not-showing-up-and-hashtag-problems
    # https://www.quora.com/Can-Twitter-hashtags-have-symbols
    hashtag_regex <- "(\\s|^)#([A-Za-z0-9_]*[A-Za-z])(?!\\w)"
    view$TOPIC <- as.character(str_extract_all(view$TEXT, hashtag_regex))
    username_regex <- "(\\s|^)@([A-Za-z0-9_]{1,15})(?!\\w)"
    view$TEXT <- as.character(str_extract_all(view$TEXT, username_regex))
    
    # Filter rows based on condition and select desired columns
    topic <- view[TOPIC != "character(0)", .(DAY, user = str_trim(ORIGIN), 
                                         topic_of_interest = str_trim(TOPIC))]
    view[, TOPIC := NULL]
    
    # Split rows with multiple mentions into separate rows
    view <- view[, .(TEXT = unlist(strsplit(TEXT, ","))), by = .(DAY, ORIGIN)]
    
    # Split rows with multiple topics into separate rows
    topic <- topic[, .(topic_of_interest = unlist(strsplit(topic_of_interest, 
                                                    ","))), by = .(DAY, user)]
    
    # Remove not wanted symbols
    view[, TEXT := gsub("@", "", TEXT)]
    view[, TEXT := gsub('\\bc\\(', "", TEXT)]
    view[, TEXT := gsub(")", "", TEXT)]
    view[, TEXT := gsub('"', "", TEXT)]
    view[, TEXT := gsub('character\\(0', NA, TEXT)]
    view[, TEXT := gsub('\\x{3000}', "", TEXT)]
    topic[, topic_of_interest := gsub('\\bc\\(', "", topic_of_interest)]
    topic[, topic_of_interest := gsub(")", "", topic_of_interest)]
    topic[, topic_of_interest := gsub('"', "", topic_of_interest)]
    topic[, topic_of_interest := gsub('\\x{3000}', "", topic_of_interest)]
    
    # Bind the chunk data.table to the main data.table for each day
    # July 1st
    temp_view <- view[DAY == "2009-07-01" & !is.na(TEXT) & TEXT != "",
                  .(from = str_trim(ORIGIN), to = str_trim(TEXT), weight = 1)]
    July_1 <- rbind(July_1, temp_view)
    temp_view <- topic[DAY == "2009-07-01", .(user = trimws(user), 
                                topic_of_interest = trimws(topic_of_interest))]
    topic_1 <- rbind(topic_1, temp_view)
    # July 2nd
    temp_view <- view[DAY == "2009-07-02" & !is.na(TEXT) & TEXT != "",
                  .(from = str_trim(ORIGIN), to = str_trim(TEXT), weight = 1)]
    July_2 <- rbind(July_2, temp_view)
    temp_view <- topic[DAY == "2009-07-02", .(user = trimws(user), 
                                topic_of_interest = trimws(topic_of_interest))]
    topic_2 <- rbind(topic_2, temp_view)
    # July 3rd
    temp_view <- view[DAY == "2009-07-03" & !is.na(TEXT) & TEXT != "",
                  .(from = str_trim(ORIGIN), to = str_trim(TEXT), weight = 1)]
    July_3 <- rbind(July_3, temp_view)
    temp_view <- topic[DAY == "2009-07-03", .(user = trimws(user), 
                                topic_of_interest = trimws(topic_of_interest))]
    topic_3 <- rbind(topic_3, temp_view)
    # July 4th
    temp_view <- view[DAY == "2009-07-04" & !is.na(TEXT) & TEXT != "",
                  .(from = str_trim(ORIGIN), to = str_trim(TEXT), weight = 1)]
    July_4 <- rbind(July_4, temp_view)
    temp_view <- topic[DAY == "2009-07-04", .(user = trimws(user), 
                                topic_of_interest = trimws(topic_of_interest))]
    topic_4 <- rbind(topic_4, temp_view)
    # July 5th
    temp_view <- view[DAY == "2009-07-05" & !is.na(TEXT) & TEXT != "",
                  .(from = str_trim(ORIGIN), to = str_trim(TEXT), weight = 1)]
    July_5 <- rbind(July_5, temp_view)
    temp_view <- topic[DAY == "2009-07-05", .(user = trimws(user), 
                                topic_of_interest = trimws(topic_of_interest))]
    topic_5 <- rbind(topic_5, temp_view)
  }
}

# Close the connection
close(con)

# Show time spent to load the data
end_time <- Sys.time()
execution_time <- end_time - start_time
print(execution_time)

# Clean up unused variables
rm(chunk_dt, topic, view, non_empty_lines, temp_view)
invisible(gc())

# Aggregates
# July 1st
query <- paste("SELECT a.'from', a.'to', SUM(a.'weight') as 'weight'",
               "FROM July_1 a WHERE a.'from' != a.'to' GROUP BY 1, 2")
July_1 <- as.data.table(sqldf(query))
fwrite(July_1, "data/edgelist_July_1.csv")
query <- paste("SELECT user, topic_of_interest FROM (SELECT user,", 
  "topic_of_interest, COUNT(*) AS frequency, ROW_NUMBER() OVER (PARTITION BY",
  "user ORDER BY COUNT(topic_of_interest) DESC) AS rn FROM topic_1 GROUP BY", 
  "user, topic_of_interest) sub WHERE rn = 1 UNION SELECT DISTINCT a.'from' as",
  "user, NULL as topic_of_interest FROM July_1 a WHERE a.'from' NOT IN (SELECT", 
  "user FROM topic_1) UNION SELECT DISTINCT a.'to' as user, NULL as", 
  "topic_of_interest FROM July_1 a WHERE a.'to' NOT IN (SELECT user FROM", 
  "topic_1)")
topic_1 <- as.data.table(sqldf(query))
fwrite(topic_1, "data/nodelist_July_1.csv")
# July 2nd
query <- paste("SELECT a.'from', a.'to', SUM(a.'weight') as 'weight'",
               "FROM July_2 a WHERE a.'from' != a.'to' GROUP BY 1, 2")
July_2 <- as.data.table(sqldf(query))
fwrite(July_2, "data/edgelist_July_2.csv")
query <- paste("SELECT user, topic_of_interest FROM (SELECT user,", 
 "topic_of_interest, COUNT(*) AS frequency, ROW_NUMBER() OVER (PARTITION BY", 
 "user ORDER BY COUNT(topic_of_interest) DESC) AS rn FROM topic_2 GROUP BY", 
 "user, topic_of_interest) sub WHERE rn = 1 UNION SELECT DISTINCT a.'from' as",
 "user, NULL as topic_of_interest FROM July_2 a WHERE a.'from' NOT IN (SELECT", 
 "user FROM topic_2) UNION SELECT DISTINCT a.'to' as user, NULL as", 
 "topic_of_interest FROM July_2 a WHERE a.'to' NOT IN (SELECT user FROM", 
 "topic_2)")
topic_2 <- as.data.table(sqldf(query))
fwrite(topic_2, "data/nodelist_July_2.csv")
# July 3rd
query <- paste("SELECT a.'from', a.'to', SUM(a.'weight') as 'weight'",
               "FROM July_3 a WHERE a.'from' != a.'to' GROUP BY 1, 2")
July_3 <- as.data.table(sqldf(query))
fwrite(July_3, "data/edgelist_July_3.csv")
query <- paste("SELECT user, topic_of_interest FROM (SELECT user,", 
 "topic_of_interest, COUNT(*) AS frequency, ROW_NUMBER() OVER (PARTITION BY", 
 "user ORDER BY COUNT(topic_of_interest) DESC) AS rn FROM topic_3 GROUP BY", 
 "user, topic_of_interest) sub WHERE rn = 1 UNION SELECT DISTINCT a.'from' as",
 "user, NULL as topic_of_interest FROM July_3 a WHERE a.'from' NOT IN (SELECT", 
 "user FROM topic_3) UNION SELECT DISTINCT a.'to' as user, NULL as", 
 "topic_of_interest FROM July_3 a WHERE a.'to' NOT IN (SELECT user FROM", 
 "topic_3)")
topic_3 <- as.data.table(sqldf(query))
fwrite(topic_3, "data/nodelist_July_3.csv")
# July 4th
query <- paste("SELECT a.'from', a.'to', SUM(a.'weight') as 'weight'",
               "FROM July_4 a WHERE a.'from' != a.'to' GROUP BY 1, 2")
July_4 <- as.data.table(sqldf(query))
fwrite(July_4, "data/edgelist_July_4.csv")
query <- paste("SELECT user, topic_of_interest FROM (SELECT user,", 
 "topic_of_interest, COUNT(*) AS frequency, ROW_NUMBER() OVER (PARTITION BY", 
 "user ORDER BY COUNT(topic_of_interest) DESC) AS rn FROM topic_4 GROUP BY", 
 "user, topic_of_interest) sub WHERE rn = 1 UNION SELECT DISTINCT a.'from' as",
 "user, NULL as topic_of_interest FROM July_4 a WHERE a.'from' NOT IN (SELECT", 
 "user FROM topic_4) UNION SELECT DISTINCT a.'to' as user, NULL as", 
 "topic_of_interest FROM July_4 a WHERE a.'to' NOT IN (SELECT user FROM", 
 "topic_4)")
topic_4 <- as.data.table(sqldf(query))
fwrite(topic_4, "data/nodelist_July_4.csv")
# July 5th
query <- paste("SELECT a.'from', a.'to', SUM(a.'weight') as 'weight'",
               "FROM July_5 a WHERE a.'from' != a.'to' GROUP BY 1, 2")
July_5 <- as.data.table(sqldf(query))
fwrite(July_5, "data/edgelist_July_5.csv")
query <- paste("SELECT user, topic_of_interest FROM (SELECT user,", 
 "topic_of_interest, COUNT(*) AS frequency, ROW_NUMBER() OVER (PARTITION BY", 
 "user ORDER BY COUNT(topic_of_interest) DESC) AS rn FROM topic_5 GROUP BY", 
 "user, topic_of_interest) sub WHERE rn = 1 UNION SELECT DISTINCT a.'from' as",
 "user, NULL as topic_of_interest FROM July_5 a WHERE a.'from' NOT IN (SELECT", 
 "user FROM topic_5) UNION SELECT DISTINCT a.'to' as user, NULL as", 
 "topic_of_interest FROM July_5 a WHERE a.'to' NOT IN (SELECT user FROM", 
 "topic_5)")
topic_5 <- as.data.table(sqldf(query))
fwrite(topic_5, "data/nodelist_July_5.csv")
