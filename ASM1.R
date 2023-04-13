#import library
library(tidyverse) 
library(dplyr)
library(janitor)
library(viridis)
library(DataExplorer)
library(ggplot2)
library(highcharter)
library(patchwork)

#import the products to dataframe
products <- read_csv("products_info.csv",na = c("NaN", "Na"))
products_clean <- na.omit(products)


str(products_clean) #show summary data of tibble

head(products_clean)  #show the first 6 entries

unique(products_clean$`Sector(s)`) #Sectors column

#we can see that there lots of error in the data that need to cleaning
#clean the data : "PreK-122|PreK-112|PPreK-12" to "PreK-12"
any(is.na(products_clean$`Sector(s)`)) 
products_clean <- products %>% 
  mutate(`Sector(s)` = ifelse(grepl("(PreK-122|PreK-112|PPreK-12)", `Sector(s)`, ignore.case = TRUE), "PreK-12",  `Sector(s)`),
         `Sector(s)` = ifelse(grepl("PreK-12; Higher; Corporate", `Sector(s)`, ignore.case = TRUE), "PreK-12; Higher Ed; Corporate",  `Sector(s)`))
unique(products_clean$`Sector(s)`)
essential<-products_clean%>%group_by(`Sector(s)`)%>%count(`Sector(s)`,name="Most served sectors")%>%arrange(-`Most served sectors`)
essential1<- essential[-4,]
ggplot(data=essential1)+
  geom_col(mapping=aes(x=reorder(`Sector(s)`,`Most served sectors`),y=`Most served sectors`,fill=`Sector(s)`))+
  coord_flip()+
  scale_fill_brewer(palette="Pastel1")+
  theme(legend.position="none")+
  labs(title="Most served sectors",x="Sectors",subtitle="Total number of products served to each sector")+
  geom_label(aes(`Sector(s)`,`Most served sectors`,label=`Most served sectors`))

sectors<-products_clean%>%group_by(`Primary Essential Function`)%>%count(`Primary Essential Function`,name="Most popular product functions")%>%arrange(-`Most popular product functions`)
comp<-na.omit(sectors)
pie_chart<-comp%>% hchart("pie", hcaes(x = `Primary Essential Function`,y=`Most popular product functions`,fill=`Primary Essential Function` )
)%>%hc_title(text = "Most popular primary essential functions.")%>%hc_size(height=450,width=1100)
pie_chart



#import districts data
districts <- read_csv("districts_info.csv",na = c("NaN", "Na", ""))
districts_clean <- na.omit(districts)
sum(!complete.cases(districts))

str(districts_clean) #show summary data of tibble

head(districts_clean)  #show the first 6 entries
# Here we have serveral variables that we will turn into a factor. State, district_id, and locale are fairly self-explanitory. 
# The pct_black.hispanic and pct_free.reduced variables appear to be ordered categorical variables that represent a percentage-group. 
# We will convert the label so it is easier to interpret in later analysis.
# Clean data, remove unnecessary characters, change it to percentage.
unique(districts_clean$locale)# check the unique data

districts_clean <- districts_clean %>% mutate(locale = ifelse(locale == "Cit", "City", 
                                          ifelse(locale == "Sub", "Suburb", locale)))

districts_clean$`pct_black/hispanic` <- str_trim(districts_clean$`pct_black/hispanic`) # remove white space
districts_clean$`pct_black/hispanic` <- ifelse(districts_clean$`pct_black/hispanic` == "[0, 0.2[" , "0-20%", 
                                      (ifelse(districts_clean$`pct_black/hispanic` == "[0.2, 0.4[" , "20-40%",
                                              (ifelse(districts_clean$`pct_black/hispanic` == "[0.4, 0.6[" , "40-60%",
                                                      (ifelse(districts_clean$`pct_black/hispanic` == "[0.6, 0.8[" , "60-80%",
                                                              (ifelse(districts_clean$`pct_black/hispanic` == "[0.8, 1[" , "80-100%", NaN) )))))))) # change the data to percentage

districts_clean$`pct_free/reduced` <- str_trim(districts_clean$`pct_free/reduced`) # remove white space
districts_clean$`pct_free/reduced` <- ifelse(districts_clean$`pct_free/reduced` == "[0, 0.2[" , "0-20%", 
                                    (ifelse(districts_clean$`pct_free/reduced`== "[0.2, 0.4[" , "20-40%",
                                            (ifelse(districts_clean$`pct_free/reduced` == "[0.4, 0.6[" , "40-60%",
                                                    (ifelse(districts_clean$`pct_free/reduced` == "[0.6, 0.8[" , "60-80%",
                                                            (ifelse(districts_clean$`pct_free/reduced` == "[0.8, 1[" , "80-100%", NaN) )))))))) # change the data to percentage



## PP_TOTAL_RAW
districts_clean$pp_total_raw <- gsub( "[", "", districts_clean$pp_total_raw, fixed = TRUE ) # replace "[" to " "
districts_clean$pp_total_raw <- gsub( ", ", "-", districts_clean$pp_total_raw, fixed = TRUE ) # replace "," to "-"
districts_clean$pp_total_raw <- ifelse(districts_clean$pp_total_raw == '' , NaN, districts_clean$pp_total_raw ) # change no value to NaN
## county connection ratio 
districts_clean$county_connections_ratio <- gsub( "[", "", districts_clean$county_connections_ratio, fixed = TRUE ) # replace "[" to " "
districts_clean$county_connections_ratio <- gsub( ", ", "-", districts_clean$county_connections_ratio, fixed = TRUE )# replace "," to "-"
districts_clean$county_connections_ratio  <- ifelse(districts_clean$county_connections_ratio == '' , NaN, districts_clean$county_connections_ratio )# change no value to NaN

glimpse(districts_clean)

#visualize the districts
options(repr.plot.height = 14, repr.plot.width = 20)

#locale ,pct_black.hispanic fill 
   
#city has equal proportion of each, suburbs have the highest total count of districts. town and rural have low percentage black/hispanic

#locale , `pct_free/reduced` fill 
ggplot(districts_clean, aes(x=locale, fill= `pct_free/reduced` )) + 
  geom_bar() +
  scale_fill_brewer(palette = "Set3") +  
  ggtitle("Locale Counts Color-Coded by `pct_free/reduced`")


ggplot(districts_clean, aes(x=pp_total_raw, fill= locale )) + 
  geom_bar() +
  scale_fill_brewer(palette = "Dark2") + coord_flip() + 
  ggtitle("pp_total_raw Counts Color-Coded by Locale") 

ggplot(districts_clean, aes(x=`pct_black/hispanic` , fill= `pct_free/reduced` )) + 
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +  
  ggtitle("`pct_black/hispanic` Counts Color-Coded by  `pct_free/reduced`")


#plot the charts

# create pie chart for locale
districts_clean %>% 
  count(locale) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(x = "", y = pct, fill = locale)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Locale Distribution", fill = "Locale") +
  scale_fill_discrete(labels = c("City", "Rural", "Suburb", "Town")) +
  geom_text(aes(label = paste0(round(pct), "%")), position = position_stack(vjust = 0.5)) +
  theme_void()



# create pie chart for pct_black/hispanic
districts_clean %>%
  count(`pct_black/hispanic`) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(x = "", y = pct, fill = `pct_black/hispanic`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Percentage of Black or Hispanic Students", fill = "Percentage") +
  scale_fill_discrete(labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")) +
  geom_text(aes(label = paste0(round(pct), "%")), position = position_stack(vjust = 0.5)) +
  theme_void()

# create pie chart for pct_free/reduced
districts_clean %>%
  count(`pct_free/reduced`) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(x = "", y = pct, fill = `pct_free/reduced`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Percentage of Students Eligible for Free or Reduced-price Lunch", fill = "Percentage") +
  scale_fill_discrete(labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")) +
  geom_text(aes(label = paste0(round(pct), "%")), position = position_stack(vjust = 0.5)) +
  theme_void()



#import the engage to the dataset
# change the name of the products to "lp_id" to merge with the engagement table
names(products_clean)[1] <- "lp_id"
Engagement_Data <- list.files("Engagement Data/")
Engagement_Data_2 <- paste0("Engagement Data//", Engagement_Data)

engage <- lapply(Engagement_Data_2, read_csv) %>%
  bind_rows(.id = "source")

disctrict_joiner <- data.frame(district_id = districts_clean$district_id, source = as.character(1:nrow(districts_clean)))

engage <- engage %>%
  left_join(disctrict_joiner, by = "source")

engage <- engage %>%
  left_join(districts_clean, by = "district_id")

engage <- engage %>%
  left_join(products_clean, by = "lp_id")

engage_clean <- na.omit(engage)
unique(engage_clean$time)



## Visualization




# Use the aggregate() function to calculate the total engagement index for each product
total_engagement <- aggregate(engagement_index ~ engage_clean$`Product Name`, data = engage_clean, FUN = sum)

# Sort the total_engagement data frame in descending order by total_engagement_index
total_engagement <- total_engagement[order(total_engagement$engagement_index, decreasing = TRUE),]

# Select the top 20 products based on their total engagement index using the head() function
top_products <- head(total_engagement, 20)

# Rename the column "engage$Product Name" to "product_name"
colnames(top_products)[colnames(top_products) == "engage_clean$`Product Name`"] <- "product_name"
top_products$product_name <- reorder(top_products$product_name, top_products$engagement_index)





# Create a bar plot of the top 20 products using ggplot2
ggplot(top_products, aes(x = engagement_index, y = product_name, fill = product_name)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("Product Name") +
  ylab("Total Engagement Index") +
  ggtitle("Top 20 Products by Engagement Index") +
  scale_x_continuous(labels = scales::comma) +
  guides(fill = FALSE)




# Create a logical index of rows to keep
keep_rows <- !(engage_clean$time %in% c("31/12/1020", "1/01/2044", "1/01/2050", "1/01/2033","1/01/2022"))

# Subset the data frame to keep only the rows that meet the condition
engage_clean <- subset(engage_clean, keep_rows)

unique(engage_clean$time)




# Convert the 'time' column to a date format
engage_clean$time <- as.Date(engage_clean$time, format = "%d/%m/%Y")

# Create a new column for the month
engage_clean$month <- format(engage_clean$time, "%m")

# Group by month and calculate the mean engagement_index
engage_monthly <- engage_clean %>%
  group_by(month) %>%
  summarize(mean_engagement = mean(engagement_index))

pct_access_monthly <- engage_clean %>%
  group_by(month) %>%
  summarize(mean_pct_access = mean(pct_access))

library(ggplot2)



# Create the plot for the Engagement_index
# Create data frame to define the summer break period
summer_break <- data.frame(xmin = 5.5, xmax = 8.5, ymin = -Inf, ymax = Inf)

# Create data frame to define the COVID-19 start annotation
annotation <- data.frame(x = 2.5, y = 1.6, label = "COVID-19 Start")

# Create the plot using ggplot2
ggplot(data = engage_monthly, aes(x = month, y = mean_engagement)) + # Define data and aesthetic mappings
  geom_line(aes(group = 1), color = "#117DBB", size = 1.5) + # Add a line for the mean engagement
  geom_rect(data = summer_break, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), # Add a rectangle for the summer break period
            fill = "blue", alpha = 0.3, inherit.aes = FALSE) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "red", size = 1) + # Add a vertical line for the COVID-19 start
  labs(x = "Month", y = "Mean Engagement Index", title = "Monthly Mean Engagement Index") + # Add axis labels and title
  annotate("text", x = 7, y = 250, label = "Summer Break", color = "blue", size = 4.5) + # Add a label for the summer break period
  annotate("text", x = annotation$x, y = annotation$y, label = annotation$label, # Add an annotation for the COVID-19 start
           color = "red", size = 4) +
  scale_x_discrete(limits = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), # Set x-axis limits and labels
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_minimal() + # Use the "minimal" theme
  theme(plot.title = element_text(face = "bold", size = 16), # Customize the plot title
        axis.title = element_text(face = "bold", size = 14), # Customize the axis titles
        axis.text = element_text(size = 12), # Customize the axis text
        axis.line = element_line(color = "black"), # Customize the axis lines
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines
        panel.border = element_blank()) # Remove plot border



# Create the plot for the Pct_Access
ggplot(data = pct_access_monthly, aes(x = month, y = mean_pct_access)) + # Define data and aesthetic mappings
  geom_line(aes(group = 1), color = "#117DBB", size = 1.5) + # Add a line for the mean percentage access
  geom_rect(data = summer_break, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), # Add a rectangle for the summer break period
            fill = "blue", alpha = 0.3, inherit.aes = FALSE) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "red", size = 1) + # Add a vertical line for the COVID-19 start
  labs(x = "Month", y = "Mean Percentage Access", title = "Monthly Mean Percentage Access") + # Add axis labels and title
  annotate("text", x = 7, y = 1.75, label = "Summer Break", color = "blue", size = 4.5) + # Add a label for the summer break period
  annotate("text", x = annotation$x, y = annotation$y, label = annotation$label, # Add an annotation for the COVID-19 start
           color = "red", size = 4) +
  scale_x_discrete(limits = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), # Set x-axis limits and labels
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_minimal() + # Use the "minimal" theme
  theme(plot.title = element_text(face = "bold", size = 16), # Customize the plot title
        axis.title = element_text(face = "bold", size = 14), # Customize the axis titles
        axis.text = element_text(size = 12), # Customize the axis text
        axis.line = element_line(color = "black"), # Customize the axis lines
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines
        panel.border = element_blank()) # Remove plot border




library(ggplot2)

library(ggplot2)










