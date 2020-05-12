# Check Reich lab data

file = "https://reichdata.hms.harvard.edu/pub/datasets/amh_repo/curated_releases/V42/V42.4/SHARE/public.dir/v42.4.1240K_HO.anno"

dat <- read.csv(file,
                header = T,
                sep = "\t",
                stringsAsFactors = F)

cols = colnames(dat)
colnames(dat) <- gsub("[.].*", "",cols)


head(dat)


# Cases where results are inaccurate

dat_date_check <- dat %>%
  filter(Date != "..") %>%
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
  select(-Date_TEMP) %>%
  mutate(Date_Average_isInconsistent = ifelse(Average_0Scale > Date2_Upper_0Scale | Average_0Scale < Date2_Lower_0Scale, TRUE, FALSE))


# Save inconsitent results to csv
dat_date_check %>%
  filter(Date_Average_isInconsistent == TRUE) %>%
  rename("Average_Reported" = "AverageBP",
         "Date_Reported" = "Date") %>%
  select(Instance, Publication, Group, Average_Reported, Average_0Scale, Date_Reported, Date2_Lower_0Scale, Date2_Upper_0Scale, Average_0Scale_Corrected) %>%
  write.csv("~/Documents/projects/reich_data/inconsistent_dates.csv", quote = F, row.names = F)
