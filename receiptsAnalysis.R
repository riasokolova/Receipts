library(pdftools)
library(tidyverse)
library(lubridate)
library(ggplot2)

# Initial setup: 
    ## loads file names into file_list
    ## creates a tibble to store items from receipts
folder_path <- "./Receipts"
file_list <- list.files(path = folder_path, full.names = TRUE)
processedReceipts = tibble(ItemName = NA, Cost = NA, Date = NA) %>% 
  mutate(Cost = as.numeric(Cost), Date = dmy(Date))

# processReceipts function - processes one PDF file at a time
processReceipts <- function(file) {
  ## Entire PDF to text
  receiptFile <- pdftools::pdf_text(file) %>% 
    read_delim(delim = "\n", col_names = FALSE, trim_ws = TRUE)
  processedTable <- receiptFile %>% 
    ## Filters in all the lines that show purchased items
    ## Any line that starts with "Savings" just shows the discount
    filter(str_detect(X1, "\\@"), !str_starts(X1, "Savings")) %>%
    ## All the lines are the same length, so this is trimming the excess
    mutate(X1 = substr(X1, start = 0, stop = 53)) %>% 
    ## Extracts the date the item was purchased from the top of the receipt
    mutate(Date = dmy(str_extract(
      receiptFile[which(str_starts(receiptFile$X1, "Rec#")),1],
                                                "\\d{1,2}/\\d{2}/\\d{4}"))) %>% 
    ## Truncates the lines to get item name and cost
    mutate(ItemName = substr(X1, start = 1, stop = 21)) %>% 
    mutate(Cost = substr(X1, start = 45, stop = 55)) %>% 
    mutate(Cost = as.numeric(gsub("\\$", "", str_trim(Cost)))) %>% 
    select(ItemName, Cost, Date)
  
  return(processedTable)
}
# Runs the function above for every file in the file list
for (file in file_list) {
  processedReceipts <- rbind(processedReceipts, processReceipts(file))
}

# A little example of how this data could be processed.
processedReceipts %>% 
  filter_all(all_vars(!is.na(.))) %>% 
  group_by(year = year(Date), week = week(Date)) %>% 
  summarise(weeklySpending = sum(Cost)) %>% 
  arrange(desc(weeklySpending)) %>% 
  ggplot(aes(week, weeklySpending)) +
  geom_col(aes(fill = as.factor(year)), position = "dodge", colour = "gray40") +
  labs(title = "Weekly Spending",
       ylab = "Amount spent per week",
       xlab = "Week",
       fill = "Year") +
  scale_fill_manual(values=c("deeppink2", "darkturquoise", "darkorchid"))
