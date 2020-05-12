# Check Reich lab data
# ML Antonio
# 2020.05.12

library(fuzzyjoin)
library(stringr)
library(ggrepel)
library(ggplot2)
require("cowplot")
library(viridis)
library(tidyr)
require('gtools')
library(grid)
library(gridExtra)
library(knitr)
library(tibble)
library(plyr)
library(plotly)

file = "https://reichdata.hms.harvard.edu/pub/datasets/amh_repo/curated_releases/V42/V42.4/SHARE/public.dir/v42.4.1240K_HO.anno"
dir = "~/Documents/projects/reich_data/"

# Read data and clean column names
dat <- read.csv(file,
                header = T,
                sep = "\t",
                stringsAsFactors = F)

cols = colnames(dat)
colnames(dat) <- gsub("[.].*", "",cols)


# #1) Find cases where reported date range does not match the reported average date
# Sometimes because BCE and CE get flipped

dat_date_check <- dat %>%
  filter(Date != "..") %>%
  mutate(Date = gsub(",", ";", Date)) %>%
  mutate(Average = as.numeric(as.character(Average))) %>%
  rename("AverageBP" = "Average") %>%
  mutate(Average_0Scale = ifelse(AverageBP >=1950, -1*(AverageBP-1950), 1950-AverageBP)) %>%
  mutate(Date_isCalibrated = ifelse(str_detect(Date, "cal"), TRUE, FALSE)) %>%
  #select(Date) %>%
  mutate(Date2 = str_replace(Date," \\(.*\\)",""),
         Date2 = gsub("cal", "", Date2),
         Date2 = gsub(" ", "", Date2)) %>%
  separate(Date2, into = c("Date2_Lower", "Date2_Upper"), "-") %>%
  # Get date distinction for upper and lower dates
  mutate(Date2_Upper_Time = ifelse(str_detect(Date2_Upper,"BCE"), "BCE", ifelse(str_detect(Date2_Upper,"CE"), "CE", "UNKNOWN")),
         Date2_Lower_Time = ifelse(str_detect(Date2_Lower,"BCE"), "BCE", ifelse(str_detect(Date2_Lower,"CE"), "CE", Date2_Upper_Time))) %>%
  # Strip dates of BCE/CE
  mutate(Date2_Lower = as.numeric(gsub("[B|C].*", "", Date2_Lower)),
         Date2_Upper = as.numeric(gsub("[B|C].*", "", Date2_Upper))) %>%
  # Put Dates on 0 scale
  mutate(Date2_Lower_0Scale = ifelse(Date2_Lower_Time == "BCE", -1 * Date2_Lower, ifelse(Date2_Lower_Time == "CE", Date2_Lower, NA)),
         Date2_Upper_0Scale = ifelse(Date2_Upper_Time == "BCE", -Date2_Upper, ifelse(Date2_Upper_Time == "CE", Date2_Upper, NA))) %>%
  # If both BCE, make sure the older date is Date_Lower. Make temp variable for swap
  mutate(Date_TEMP = NA) %>%
  mutate(Date_TEMP = ifelse(Date2_Lower_Time == "BCE" & Date2_Upper_Time == "BCE" & Date2_Upper_0Scale < Date2_Lower_0Scale, Date2_Upper_0Scale, NA),
         Date2_Upper_0Scale = ifelse(!is.na(Date_TEMP), Date2_Lower_0Scale, Date2_Upper_0Scale),
         Date2_Lower_0Scale = ifelse(!is.na(Date_TEMP), Date_TEMP, Date2_Lower_0Scale),
         Average_0Scale_Corrected = (Date2_Upper_0Scale + Date2_Lower_0Scale) / 2
  ) %>%
  select(-Date_TEMP)

# Save inconsitent results to csv
dat_date_check %>%
  mutate(DATE_INCONSISTENT_REASON = ifelse(
    abs(Average_0Scale_Corrected-Average_0Scale) > 1, "IN_DATE_RANGE_NOT_AVERAGE",
    ifelse(Average_0Scale > Date2_Upper_0Scale | Average_0Scale < Date2_Lower_0Scale, "OUT_OF_DATE_RANGE", "NA")),
    DATE_INCONSISTENT_REASON = ifelse(DATE_INCONSISTENT_REASON)) %>%
  filter(!is.na(DATE_INCONSISTENT_REASON)) %>%
  rename("AverageBP_Reported" = "AverageBP",
         "Date_Reported" = "Date") %>%
  select(Instance, Publication, Group, AverageBP_Reported, Average_0Scale, 
         Date_Reported, Date2_Lower_0Scale, Date2_Upper_0Scale, Average_0Scale_Corrected,
         DATE_INCONSISTENT_REASON) %>%
  write.csv(paste0(dir,"date_inconsistencies.csv"), 
              quote = F, 
              row.names = F)


dat_date_check %>%
  mutate(Average_0Scale_Corrected = round(Average_0Scale_Corrected, digits = 0)) %>%
  select(Instance, Publication,Group,Date,Average_0Scale, Average_0Scale_Corrected) %>%
  write.csv(paste0(dir,"date_inconsistence_average.csv"),
            quote = F,
            row.names = F)

#### 2) Geographical coordinate check
# Check cases where latitude and longitude of reported country
# do not fall near sampling location

# idea: could plot coordinates of each sample from a given country,
# then visually inspect :(

# Function to find polygon hulls
find_hull <- function(df) df[chull(df$Long, df$Lat), ]


dat_coords <- dat_date_check %>%
  filter(Long != "..",
         Lat != "..") %>%
  mutate(Long = round(as.numeric(Long), digits = 1),
         Lat = round(as.numeric(Lat), digits = 1)) #%>%
  #filter(Long > -20 & Long < 60 &
   #      Lat > 30 & Lat < 70)


# Create hulls
hulls <- ddply(dat_coords %>%
                 filter(Country != "Russia"),
               "Country", find_hull) 

# Label hulls - don't need this for plotly
hull_labels <- hulls %>%
  group_by(Country) %>%
  dplyr::summarize(Long = mean(Long), 
            Lat = mean(Lat))

# Plot polygons and points on map
p <- ggplot(data = dat_coords) +
  geom_polygon(data = world, #%>%
                 #filter(long > -20 & long < 60 &
                 #         lat > 30 & lat < 70), #%>%,
               aes(x=long,y=lat,group=group),
               color = "white", fill = "lightgray") +
  geom_point(aes(x = Long, y = Lat,
                 text = paste0("Sample: ", Instance, 
                               "\nGroup: ",Group,
                               #"\nDate: ", Date,
                               "\nLocality: ", Locality)),
             alpha = 0.5) +
  
  geom_polygon(data = hulls,
               aes(x = Long, y = Lat, group = Country), 
                    color = "black", alpha = 0.3,
               fill = "goldenrod")+
  guides(color = F) +
  theme_bw() +
  ggtitle("Reich Ancient DNA Data: Reported coordinates of samples grouped by reported country")

# Plotly to mouse over points!
ggplotly(p)

# Save as HTML
saveWidget(ggplotly(p), paste0(dir,"coord_check_plotly.html"), 
           selfcontained = T)

#### 3) Locality check
# Check cases where latitude and longitude of reported country
# Example: Crypta Balbi outlier has Sardinia in Locality, but coordinates are correct 
# How to do this....

