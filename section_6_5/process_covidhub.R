# download the entire data-processed directory onto SSD using command
# svn checkout https://github.com/bgs25/covid19-forecast-hub/trunk/data-processed
# on terminal.
library(stringr)
library(readr)
library(dplyr)
# then set the working directory to data-processed
folder_names = list.files()
target_id = "inc case" # if want incidental cases. otherwise set as "inc death" for incidental deaths

max_loc = 56

target_nchar = nchar(target_id)

df_names = c()
for (folder_id in folder_names) {
  print(folder_id)
  file_names = list.files(path = folder_id)
  file_names = file_names[str_sub(list.files(path = folder_id),1,4) == "2020"]
  file_names = file_names[str_sub(list.files(path = folder_id),-3,-1) == "csv" ]
  for (file_id in file_names) {
    print(file_id)
    temp_table = read_csv(paste0(folder_id, "/", file_id), col_types = cols(location = col_number()))
    temp_table = data.frame(temp_table)
    temp_table = temp_table[(temp_table$location <= max_loc) %in% TRUE ,] #with max_loc = 56 this means we only are using state-level information (otherwise the size of dataset may become overly large)
    if (dim(temp_table)[1] == 0) {
      print("Skipping")
      next
    }
    temp_table$type = as.factor(temp_table$type)
    temp_table = temp_table[temp_table$type == "point",]
    if (dim(temp_table)[1] == 0) {
      print("Skipping")
      next
    }
    temp_table$target = as.factor(temp_table$target)
    target_names = levels(temp_table$target)
    target_names = target_names[str_sub(target_names, start = -target_nchar) == target_id]
    temp_table = temp_table[temp_table$target %in% target_names,]
    if (dim(temp_table)[1] == 0) {
      print("Skipping")
      next
    }
    df_id = str_replace_all(str_sub(file_id,1,-5),"-","_")
    assign(df_id,temp_table)
    df_names = c(df_names, df_id)
  }
}

is_first_df = TRUE
for (df_id in df_names) {
  print(df_id)
  temp_df = get(df_id)
  temp_df['model'] = str_sub(df_id, 12, -1)
  if (is_first_df) {
    combined_case_df = temp_df
    is_first_df = FALSE
  } else {
    combined_case_df = rbind(combined_case_df, temp_df)
  }
}

combined_case_df = combined_case_df[c("forecast_date","target","target_end_date","location","model","value")]
combined_case_df$forecast_date = as.Date(combined_case_df$forecast_date)
combined_case_df$target = as.factor(as.character(combined_case_df$target))
combined_case_df$target_end_date = as.Date(combined_case_df$target_end_date)
combined_case_df$location = as.factor(combined_case_df$location)
combined_case_df$model = as.factor(combined_case_df$model)
combined_case_df$target = as.factor(combined_case_df$target)

# now combine with the true value for training

setwd("..")
# assuming it's case incidence
true_df = read_csv("truth-incident-Cases.csv", col_types = cols(location = col_number()))
true_df = data.frame(true_df)
true_df$date = as.Date(true_df$date)
#true_df = true_df[ order(true_df$date), ]
true_df = true_df[ (true_df$location <= max_loc) %in% TRUE, ]
true_df = true_df[ order(true_df$location),]
# now need to aggregate these and match them to the rows in the main data frame
last_from_state = c(0, which(diff(true_df$date)<0))
true_df$value_7rollsum = c(rep(0, 6), rollsum(true_df$value, 7))
keep_row = rep(TRUE, dim(true_df)[ 1 ])
for (i in 1:6) keep_row[ i + last_from_state ] = FALSE
true_df = true_df[ keep_row, ]

case_df = merge(combined_case_df, dplyr::select(true_df, -c("value")), by.x = c("location","target_end_date"), by.y = c("location","date"), all.x = TRUE)
case_df$case_error = case_df$value_7rollsum - case_df$value

# 1. Compute 7 day rolling sum column for true_df
# 2. Match these entries with the combined data frame
# 3. 