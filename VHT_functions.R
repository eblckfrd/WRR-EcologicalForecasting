#VHT code in functions to use in large data analysis
#1/11/15

library(ggplot2)
library(dplyr)
library(viridis)

###preprocessing function ------------------------------------------------

preprocessing <- function(data, date_col, depth_col) {
  data <- data[complete.cases(data), ]  #remove missing data rows
  
  data <- data %>%
    mutate(!!sym(date_col) := as.Date(!!sym(date_col), format = "%Y-%m-%d")) %>% #edit date format for data you are using!!
    arrange(!!sym(date_col))  #arrange by date
  
  #check if date conversion worked
  if (all(is.na(data[[date_col]]))) {
    stop("Date conversion failed. Check the format in the input data.")
  }
  
  data <- data %>%
    group_by(!!sym(date_col)) %>%                               
    arrange(!!sym(depth_col), .by_group = TRUE) %>%  #make sure depth starts at 0 and goes down
    ungroup()
  
  return(data)
}

#example use
#Pallette_data <- preprocessing(Pallette_data, "Date", "depth_m")


#calculate VHT function -------------------------------------------------
#either use 3mg/L DO (3.5) and 17C (17.5), or Wisconsin DNR standard of 6mg/L DO (6.5) and 22 C (22.5)

calculate_VHT <- function(data, date_col, temp_col, o2_col, depth_col) {
  
  data <- data %>%
    group_by(.data[[date_col]]) %>%
    mutate(
      temp_depth = ifelse(any(.data[[temp_col]] >= 22.5), #find depth where temp reaches 17.5 or more
                          .data[[depth_col]][which.min(.data[[temp_col]] >= 22.5)], 
                          0),
      
      #used for BM, had fixed depth
      #o2_depth = ifelse(any(.data[[o2_col]] <= 3.5), #find depth where DO reaches 3.5 or less
                       # .data[[depth_col]][which.max(.data[[o2_col]] <= 3.5)], 
                        #18)
      
      o2_depth = ifelse(
        any(.data[[o2_col]] <= 6.5),  #check if any DO value is ≤ 3.5
        min(.data[[depth_col]][.data[[o2_col]] <= 6.5]),  #get the shallowest depth where DO ≤ 3.5
        max(.data[[depth_col]])  #use the deepest available depth in the dataset
      )
      
    ) %>%
    mutate(
      temp_depth = round(temp_depth), #round
      o2_depth = round(o2_depth),
      VHT = pmax(0, o2_depth - temp_depth) #subtract to find VHT
    ) %>%
    ungroup()
  
  full_data <- data %>%
    select(all_of(c(date_col, depth_col, temp_col, o2_col, "temp_depth", "o2_depth", "VHT"))) #table with all calculated steps
  
  VHT_data <- data %>%
    distinct(.data[[date_col]], .keep_all = TRUE) %>% #new table with only date and VHT
    select(all_of(c(date_col, "VHT")))
  
  assign("full_data", full_data, envir = .GlobalEnv) #for double checking the calculations worked correctly
  
  return(VHT_data)
}

#example use
#VHT_Pallette <- calculate_VHT(Pallette_data, "Date", "Temp", "DO", "Depth")


#plot for all years function -------------------------------------------------

plot_allyears <- function(data, date_col, vht_col) {
  plotting <<- data %>%
    mutate(
      Year = format(!!sym(date_col), "%Y"),  #extract Year
      Month = format(!!sym(date_col), "%m"),  #extract Month
      Day = as.integer(format(!!sym(date_col), "%d")),  #extract Day
      month_section = ifelse(Day <= 15, "Early", "Late"),  #classify as Early/Late
      month_label = paste(Month, month_section, sep = " ")
    ) %>%
    filter(Month %in% sprintf("%02d", 5:10))

  ggplot(plotting, aes(x = month_label, y = !!sym(vht_col), color = factor(Year), group = Year)) +
    geom_line() + 
    scale_color_viridis_d() +
    labs(y = "Vertical Habitat Thickness (m)", color = "Year") +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 11),  
      axis.text.x = element_text(size = 11),   
      axis.text.y = element_text(size = 11)
    )
}

#example use
#plot_allyears(VHT_Pallette, "Date", "VHT")


#plot for average years function -------------------------------------------------


plot_averageyears <- function(data, date_col, vht_col) {
  data <- data %>%
    mutate(Month = format(!!sym(date_col), "%m"))
  
  average <- data %>% #calculate average each month
    group_by(Month) %>%
    summarize(avg_vht = mean(!!sym(vht_col), na.rm = TRUE))

  percentile <- data %>% #calculate 95th and 5th percentiles for each month
    group_by(Month) %>%
    summarize(
      dvht_95 = quantile(!!sym(vht_col), 0.95, na.rm = TRUE),
      dvht_05 = quantile(!!sym(vht_col), 0.05, na.rm = TRUE)
    )
  
  plot_data <- merge(average, percentile, by = "Month") #merge to one data frame
  
  ggplot(plot_data, aes(x = Month, y = avg_vht, group = 1)) +
    geom_line() +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +  #starts y-axis at 0
    geom_ribbon(aes(ymin = dvht_05, ymax = dvht_95), fill = "#6BAED6", alpha = 0.3) + 
    labs(x = "Month", y = "Vertical Habitat Thickness (m)") +
    theme_minimal() +
    ggtitle("Oxythermal Habitat Climatology")
}

#example use
#plot_averageyears(VHT_Pallette, "Date", "VHT")


##test ----------------------------------------------------------------------------

#Pallette_data <- read.csv("~/Desktop/Pallette_data.csv")
#Processed_pallette <- preprocessing(Pallette_data, "Date", "Depth")
#VHT_Pallette <- calculate_VHT(Processed_pallette, "Date", "Temp", "DO", "Depth")
#plot_allyears(VHT_Pallette, "Date", "VHT")
#plot_averageyears(VHT_Pallette, "Date", "VHT")


#use rLakeAnalyzer to calculate location of thermocline from temp profile; use first sample date in August --------------

#isolate first sample date depths and w temps into vector, run through thermocline function

filter_summer_months <- function(data) {
  data %>%
    filter(format(sampledate, "%m") %in% c("07", "08", "09"))
}

# Function to find the first sample date in August for each year
get_first_aug_sample <- function(data) {
  data %>%
    filter(format(sampledate, "%m") == "08") %>%  # Keep only August dates
    group_by(Year = format(sampledate, "%Y")) %>%
    filter(sampledate == min(sampledate, na.rm = TRUE)) %>%  # Select all rows with the earliest August date
    ungroup()  # Remove grouping to maintain full depth range
}

# Function to compute thermocline depth
compute_thermocline <- function(data) {
  data %>%
    group_by(Year) %>%
    summarise(Thermocline_Depth = thermo.depth(wtemp, depth, seasonal = FALSE))
}

# Function to find water temperature and oxygen at thermocline depth
get_temp_o2_at_thermocline <- function(data, thermocline_data) {
  data %>%
    inner_join(thermocline_data, by = "Year") %>%
    group_by(Year) %>%
    summarise(
      Closest_Depth = depth[which.min(abs(depth - Thermocline_Depth))],
      Water_Temp = wtemp[which.min(abs(depth - Thermocline_Depth))],
      O2_Conc = o2[which.min(abs(depth - Thermocline_Depth))]
    )
}

# Function to apply the full process to each dataset

process_lake_data <- function(data) {
  data_summer <- filter_summer_months(data)
  aug_sample <- get_first_aug_sample(data_summer)
  thermocline_results <- compute_thermocline(aug_sample)
  final_results <- get_temp_o2_at_thermocline(aug_sample, thermocline_results)
  return(final_results)
}