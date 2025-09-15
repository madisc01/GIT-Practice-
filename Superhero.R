library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load the dataset
df <- read_excel("Dream_Supershop_Dataset.xlsx", sheet = "Sales Data")

# View structure
str(df)

# Remove duplicate rows
df <- df %>% distinct()

# Replace missing numeric values with median
df <- df %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))))

# Remove rows with any remaining missing values
df <- df %>% drop_na()


# Summarise total revenue by category
revenue_by_category <- df %>%
  group_by(`Category Name`) %>%
  summarise(TotalRevenue = sum(Revenue, na.rm = TRUE)) %>%
  arrange(desc(TotalRevenue))

library(scales)  # for label formatting

ggplot(revenue_by_category, aes(x = reorder(`Category Name`, -TotalRevenue), y = TotalRevenue)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  labs(title = "Total Revenue by Category",
       x = "Category",
       y = "Total Revenue") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +  # shows in millions
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  coord_flip() #flip chart







