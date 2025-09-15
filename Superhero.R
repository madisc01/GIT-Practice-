library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load the dataset
df <- read_excel("Dream_Supershop_Dataset.xlsx")

# View structure
str(df)

# Remove duplicate rows
df <- df %>% distinct()

# Replace missing numeric values with median
df <- df %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))))

# Remove rows with any remaining missing values
df <- df %>% drop_na()

# Summarize sales by category
sales_summary <- df %>%
  group_by(Category) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))

# Plot
ggplot(sales_summary, aes(x = reorder(Category, -Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Sales by Product Category",
       x = "Category",
       y = "Total Sales") +
  theme_minimal()
#test



