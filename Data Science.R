library(tidyverse)
library(Hmisc)

ds_salary = read_csv(file.choose())

head(ds_salary)

colSums(is.na(ds_salary))
sum(duplicated(ds_salary))

ds_salary = ds_salary %>%
  mutate_if(is.character, as_factor) %>%
  select(-salary, -salary_currency)

str(ds_salary)


# Top 10 Data Science Jobs in 2023
filter(ds_salary, work_year == 2023) %>% 
  count(job_title) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(job_title, -n),  y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = 1.5, color = "white") +
  xlab("Job Title") +
  ylab("Number of Professionals") + 
  ggtitle("Top 10 Data Science Jobs Across the World in 2023") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
  theme(axis.title = element_text(face = "bold"))

# Experience Levels
ds_salary %>%
  mutate(experience_level = recode(experience_level, "EN" = "Entry Level",
                                   "MI" = "Mid Level", "SE" = "Senior Level",
                                   "EX" = "Executive Level")) %>%
  count(experience_level) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(experience_level, -n),  y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = 1.5, color = "white") +
  xlab("Experience Level") +
  ylab("Number of Professionals") + 
  ggtitle("Experience Level of Data Science Professionals") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
  theme(axis.title = element_text(face = "bold"))

  


# Top 10 Paying Countries for Data Science Professionals
ds_salary %>%
  group_by(company_location) %>%
  summarise(mean_salary = mean(salary_in_usd)) %>%
  arrange(desc(mean_salary))

ds_salary %>% 
  mutate(company_location = recode(company_location, "IL" = "Israel", "PR" = "Puerto Rico", 
                                   "US" = "United States", "RU" = "Russia", "CA" = "Canada", 
                                   "NZ" = "New Zealand", "BA" = "Bosnia", "IE" = "Ireland",
                                   "JP" = "Japan", "SE" = "Sweden")) %>%
  group_by(company_location) %>%
  summarise(mean_salary = mean(salary_in_usd)) %>%
  arrange(desc(mean_salary)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(company_location, -mean_salary),  y = mean_salary)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::dollar_format()) +
  geom_text(aes(label = scales::dollar(round(mean_salary, digits = 1))), vjust = 1.5, color = "white") +
  xlab("Country Name") +
  ylab("Average Pay") + 
  ggtitle("Top 10 Highest Paying Countries for Data Science Position") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
  theme(axis.title = element_text(face = "bold"))
 
# Average Pay Based on Experience Level
ds_salary %>%
  mutate(experience_level = recode(experience_level, "EN" = "Entry Level",
                                   "MI" = "Mid Level", "SE" = "Senior Level",
                                   "EX" = "Executive Level")) %>%
  group_by(experience_level) %>%
  summarise(mean_salary = mean(salary_in_usd)) %>%
  arrange(desc(mean_salary)) %>%
  ggplot(aes(x = reorder(experience_level, -mean_salary),  y = mean_salary)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::dollar_format()) +
  geom_text(aes(label = scales::dollar(round(mean_salary, digits = 1))), vjust = 1.5, color = "white") +
  xlab("Experience Level") +
  ylab("Average Pay") + 
  ggtitle("Average Pay by Experience Level") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
  theme(axis.title = element_text(face = "bold"))

# Employee Residence (Top 10)
ds_salary %>% count(employee_residence) %>%
  arrange(desc(n))

ds_salary %>%
  mutate(employee_residence = recode(employee_residence, "US" = "United States",
                                   "GB" = "Great Britain", "CA" = "Canada",
                                   "ES" = "Spain", "IN" = "India", 
                                   "DE" = "Germany", "FR" = "France", "PT" = "Portugal", 
                                   "BR" = "Brazil", "GR" = "Greece")) %>%
  count(employee_residence) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(employee_residence, -n),  y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -1.5, color = "black") +
  xlab("Employee Residence") +
  ylab("Count") + 
  ggtitle("Residence of Hired Data Science Professionals") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
  theme(axis.title = element_text(face = "bold"))
  

# Average Pay Based on the Company Size and Experience Level
ds_salary %>%
  mutate(experience_level = recode(experience_level, "EN" = "Entry Level",
                                   "MI" = "Mid Level", "SE" = "Senior Level",
                                   "EX" = "Executive Level")) %>%
  mutate(company_size = recode(company_size, "S" = "Small", "M" = "Medium", 
                               "L" = "Large")) %>%
  group_by(experience_level, company_size) %>%
  summarise(mean_salary = mean(salary_in_usd)) %>%
  arrange(desc(mean_salary)) %>%
  ggplot(aes(x = reorder(experience_level, -mean_salary),  y = mean_salary , fill = company_size)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = scales::dollar_format()) +
  geom_text(aes(label = scales::dollar(round(mean_salary, digits = 1))), vjust = 1.5, color = "white", 
            position = position_dodge(width = 0.9)) +
  xlab("Experience Level") +
  ylab("Average Pay") + 
  ggtitle("Average Pay by Experience Level and Company Size") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
  theme(axis.title = element_text(face = "bold")) + 
  guides(fill = guide_legend(title = "Company Size"))
