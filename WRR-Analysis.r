library(ggplot2)
library(dplyr)
library(viridis)
library(factoextra)
library(corrplot)
library(rLakeAnalyzer)
library(tidyr)
library(zoo)
library(Kendall)
library(lubridate)

source("~/Documents/R Repositories/LTER_Analysis/VHT_functions.R")

#load in data from LTER
PhysicalLimnologyofLTERLakes <- read.csv("~/Desktop/Research/Data/LTER/PhysicalLimnologyofLTERLakes.csv")

#list of lakes: AL, BM, CR, FI, ME, MO, SP, TR, WI
#north: AL, BM, CR, SP, TR
#south: FI, ME, MO, WI


#first pull data from the northern lakes ----------------------------------------

AL <- PhysicalLimnologyofLTERLakes %>%
  dplyr::filter(!is.na(wtemp), !is.na(o2)) %>%  
  dplyr::filter(lakeid == "AL") %>%
  dplyr::select(sampledate, depth, wtemp, o2)

BM = PhysicalLimnologyofLTERLakes %>%
  dplyr::filter(!is.na(wtemp), !is.na(o2)) %>%  
  dplyr::filter(lakeid == "BM") %>%
  dplyr::select(sampledate, depth, wtemp, o2)

CR = PhysicalLimnologyofLTERLakes %>%
  dplyr::filter(!is.na(wtemp), !is.na(o2)) %>%  
  dplyr::filter(lakeid == "CR") %>%
  dplyr::select(sampledate, depth, wtemp, o2)

SP = PhysicalLimnologyofLTERLakes %>%
  dplyr::filter(!is.na(wtemp), !is.na(o2)) %>%  
  dplyr::filter(lakeid == "SP") %>%
  dplyr::select(sampledate, depth, wtemp, o2)

TR = PhysicalLimnologyofLTERLakes %>%
  dplyr::filter(!is.na(wtemp), !is.na(o2)) %>%  
  dplyr::filter(lakeid == "TR") %>%
  dplyr::select(sampledate, depth, wtemp, o2)

#now for the southern lakes ---------

FI <- PhysicalLimnologyofLTERLakes %>%
  dplyr::filter(!is.na(wtemp), !is.na(o2)) %>%  
  dplyr::filter(lakeid == "FI") %>%
  dplyr::select(sampledate, depth, wtemp, o2)

ME <- PhysicalLimnologyofLTERLakes %>%
  dplyr::filter(!is.na(wtemp), !is.na(o2)) %>%  
  dplyr::filter(lakeid == "ME") %>%
  dplyr::select(sampledate, depth, wtemp, o2)

MO <- PhysicalLimnologyofLTERLakes %>%
  dplyr::filter(!is.na(wtemp), !is.na(o2)) %>%  
  dplyr::filter(lakeid == "MO") %>%
  dplyr::select(sampledate, depth, wtemp, o2)


#now run them through VHT calculating functions -----------------------------------------------------

AL_data <- preprocessing(AL, "sampledate", "depth")
VHT_AL <- calculate_VHT(AL_data, "sampledate", "wtemp", "o2", "depth")

BM_data <- preprocessing(BM, "sampledate", "depth")
#not full depth data, remove line
BM_data <- BM_data %>%
  filter(sampledate != as.Date("1984-07-17"))
VHT_BM <- calculate_VHT(BM_data, "sampledate", "wtemp", "o2", "depth")

CR_data <- preprocessing(CR, "sampledate", "depth")
VHT_CR <- calculate_VHT(CR_data, "sampledate", "wtemp", "o2", "depth")

SP_data <- preprocessing(SP, "sampledate", "depth")
VHT_SP <- calculate_VHT(SP_data, "sampledate", "wtemp", "o2", "depth")

TR_data <- preprocessing(TR, "sampledate", "depth")
VHT_TR <- calculate_VHT(TR_data, "sampledate", "wtemp", "o2", "depth")

#now for the southern lakes! ---------

FI_data <- preprocessing(FI, "sampledate", "depth")
VHT_FI <- calculate_VHT(FI_data, "sampledate", "wtemp", "o2", "depth")

ME_data <- preprocessing(ME, "sampledate", "depth")
VHT_ME <- calculate_VHT(ME_data, "sampledate", "wtemp", "o2", "depth")

MO_data <- preprocessing(MO, "sampledate", "depth")
VHT_MO <- calculate_VHT(MO_data, "sampledate", "wtemp", "o2", "depth")


#now use plotting functions ---------------
#AND take plotting data for each lake and turn it into new dataset, then average across JA ---------


plot_allyears(VHT_AL, "sampledate", "VHT")
plot_averageyears(plotting, "sampledate", "VHT")

plotting$Month <- as.numeric(plotting$Month)

JA_AL <- plotting %>%
  filter(Month %in% c(7, 8)) %>% 
  group_by(Year) %>%  
  summarise(Mean_VHT = mean(VHT, na.rm = TRUE)) 


plot_allyears(VHT_BM, "sampledate", "VHT")
plot_averageyears(plotting, "sampledate", "VHT")

plotting$Month <- as.numeric(plotting$Month)

JA_BM <- plotting %>%
  filter(Month %in% c(7, 8)) %>% 
  group_by(Year) %>%  
  summarise(Mean_VHT = mean(VHT, na.rm = TRUE)) 


plot_allyears(VHT_CR, "sampledate", "VHT")
plot_averageyears(plotting, "sampledate", "VHT")

plotting$Month <- as.numeric(plotting$Month)

JA_CR <- plotting %>%
  filter(Month %in% c(7, 8)) %>% 
  group_by(Year) %>%  
  summarise(Mean_VHT = mean(VHT, na.rm = TRUE)) 


plot_allyears(VHT_SP, "sampledate", "VHT")
plot_averageyears(plotting, "sampledate", "VHT")

plotting$Month <- as.numeric(plotting$Month)

JA_SP <- plotting %>%
  filter(Month %in% c(7, 8)) %>% 
  group_by(Year) %>%  
  summarise(Mean_VHT = mean(VHT, na.rm = TRUE)) 


plot_allyears(VHT_TR, "sampledate", "VHT")
plot_averageyears(plotting, "sampledate", "VHT")

plotting$Month <- as.numeric(plotting$Month)

JA_TR <- plotting %>%
  filter(Month %in% c(7, 8)) %>% 
  group_by(Year) %>%  
  summarise(Mean_VHT = mean(VHT, na.rm = TRUE))


#now for the southern lakes! ---------

plot_allyears(VHT_FI, "sampledate", "VHT")
plot_averageyears(plotting, "sampledate", "VHT")

plotting$Month <- as.numeric(plotting$Month)

JA_FI <- plotting %>%
  filter(Month %in% c(7, 8)) %>% 
  group_by(Year) %>%  
  summarise(Mean_VHT = mean(VHT, na.rm = TRUE)) 


plot_allyears(VHT_ME, "sampledate", "VHT")
plot_averageyears(plotting, "sampledate", "VHT")

plotting$Month <- as.numeric(plotting$Month)

JA_ME <- plotting %>%
  filter(Month %in% c(7, 8)) %>% 
  group_by(Year) %>%  
  summarise(Mean_VHT = mean(VHT, na.rm = TRUE)) 


plot_allyears(VHT_MO, "sampledate", "VHT")
plot_averageyears(plotting, "sampledate", "VHT")

plotting$Month <- as.numeric(plotting$Month)

JA_MO <- plotting %>%
  filter(Month %in% c(7, 8)) %>% 
  group_by(Year) %>%  
  summarise(Mean_VHT = mean(VHT, na.rm = TRUE)) 


#trim all lakes to 1996-2022, bc those are the only years that match all lakes

JA_AL <- JA_AL %>%
  filter(Year >= 1996 & Year <= 2022)

JA_BM <- JA_BM %>%
  filter(Year >= 1996 & Year <= 2022)

JA_CR <- JA_CR %>%
  filter(Year >= 1996 & Year <= 2022)

JA_FI <- JA_FI %>%
  filter(Year >= 1996 & Year <= 2022)

#fill 2020 with 0; missing year
new_row <- tibble(Year = "2020", Mean_VHT = 0)
#new_row$Year <- as.numeric(new_row$Year)
JA_FI <- bind_rows(JA_FI, new_row) %>%
  arrange(Year)

JA_ME <- JA_ME %>%
  filter(Year >= 1996 & Year <= 2022)

JA_MO <- JA_MO %>%
  filter(Year >= 1996 & Year <= 2022)

JA_SP <- JA_SP %>%
  filter(Year >= 1996 & Year <= 2022)

JA_TR <- JA_TR %>%
  filter(Year >= 1996 & Year <= 2022)


#bring in global climate indices ---------

climate <- read.csv("~/Desktop/WRR Special Issue/climate_indices.csv") %>%
  mutate(date = as.Date(date),
         Year = year(date),
         Month = month(date)) %>%
  filter(Year >= 1996, Year <= 2022,
         Month %in% c(3, 4, 5, 6)) %>%          # Mar, Apr, May, Jun only
  select(Year, Month, Nino34, PDO, PNA) %>%
  pivot_longer(cols = c(Nino34, PDO, PNA),
               names_to = "Index", values_to = "Value") %>%
  mutate(Month = factor(Month, levels = 3:6,
                        labels = c("Mar","Apr","May","Jun")))

#Bind JAS data  ------------------------------------

VHT_all <- bind_rows(
JA_AL %>% mutate(lakeid = "AL"),
JA_BM %>% mutate(lakeid = "BM"),
JA_CR %>% mutate(lakeid = "CR"),
JA_SP %>% mutate(lakeid = "SP"),
JA_TR %>% mutate(lakeid = "TR"),
JA_FI %>% mutate(lakeid = "FI"),
JA_ME %>% mutate(lakeid = "ME"),
JA_MO %>% mutate(lakeid = "MO")
) %>%
  ungroup()

#test correlations --------------------------------

VHT_all <- VHT_all %>%
  mutate(Year = as.numeric(Year))

kendall_by_lake <- climate %>%
  inner_join(VHT_all, by = "Year") %>%     
  group_by(lakeid, Index, Month) %>%
  summarise(
    n = n(),
    tau = tryCatch(Kendall(Value, Mean_VHT)$tau[1], error = function(e) NA),
    p_value = tryCatch(Kendall(Value, Mean_VHT)$sl[1], error = function(e) NA),
    .groups = "drop"
  )

kendall_by_lake <- kendall_by_lake %>%
  mutate(sig_90 = p_value < 0.10,
         sig_95 = p_value < 0.05)

# see how many combos pass each threshold
kendall_by_lake %>% summarise(n_sig_90 = sum(sig_90, na.rm = TRUE),
                              n_sig_95 = sum(sig_95, na.rm = TRUE))

sig_combos <- kendall_by_lake %>%
  filter(sig_90) %>%                      
  select(lakeid, Index, Month)

# VHT terciles, computed per-lake ---------------------------------------------------

binary_cat <- function(x) {
  ntile(x, 2) %>% factor(levels = 1:2, labels = c("Low","High"))
}

VHT_all <- VHT_all %>%
  group_by(lakeid) %>%
  mutate(VHT_cat = binary_cat(Mean_VHT)) %>%
  ungroup()

climate <- climate %>%
  group_by(Index, Month) %>%
  mutate(Phase = binary_cat(Value)) %>%
  ungroup()

joined_filtered <- climate %>%
  inner_join(VHT_all, by = "Year") %>%
  semi_join(sig_combos, by = c("lakeid", "Index", "Month"))

contingency_filtered <- joined_filtered %>%
  count(lakeid, Index, Month, Phase, VHT_cat) %>%
  group_by(lakeid, Index, Month, Phase) %>%
  mutate(pct = 100 * n / sum(n),
         n_total = sum(n)) %>%
  ungroup()


# screening results table -----------------------------------------

kendall_screen_table <- kendall_by_lake %>%
  filter(sig_90) %>%
  arrange(lakeid, Index, Month) %>%
  mutate(`Kendall's tau` = sprintf("%.2f", tau),
         `p-value` = sprintf("%.3f", p_value)) %>%
  select(Lake = lakeid, `Climate Index` = Index, `Lag Month` = Month,
         `Kendall's tau`, `p-value`, n)

write.csv(kendall_screen_table, "kendall_screening_table.csv", row.names = FALSE)
kendall_screen_table


#trying another figure --------------------

# Raw scatter, restricted to screened lake x index x month combos
support_data <- climate %>%
  inner_join(VHT_all, by = "Year") %>%
  semi_join(sig_combos, by = c("lakeid", "Index", "Month"))

support_data <- support_data %>%
  mutate(facet_label = paste0(lakeid, " – ", Index, " (", Month, ")"))

slope_table <- support_data %>%
  group_by(lakeid, Index, Month) %>%
  summarise(
    slope = coef(lm(Mean_VHT ~ Value))[2],
    .groups = "drop"
  ) %>%
  left_join(kendall_by_lake %>% select(lakeid, Index, Month, tau, p_value, n),
            by = c("lakeid", "Index", "Month")) %>%
  arrange(desc(abs(tau)))

print(slope_table)


# plot
facet_order <- support_data %>%
  distinct(lakeid, Index, Month, facet_label) %>%
  arrange(factor(Index, levels = c("PDO", "PNA")), lakeid, Month) %>%
  pull(facet_label)

support_data <- support_data %>%
  mutate(facet_label = factor(facet_label, levels = facet_order))

ggplot(support_data, aes(x = Value, y = Mean_VHT)) +
  geom_point(size = 2.2, alpha = 0.75, color = "#0B3D91") +
  geom_smooth(method = "lm", se = TRUE, color = "black",
              fill = "#AED6F1", linewidth = 0.7, alpha = 0.4) +
  facet_wrap(~ facet_label, scales = "free_y", nrow = 2, ncol = 5, dir = "h") +
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 1)) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = "Spring Climate Index Values", y = "July-August Oxythermal Habitat (VHT)") +
  theme_minimal(base_size = 13) +
  theme(
    text = element_text(color = "black"),
    axis.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    strip.text = element_text(face = "bold", color = "black", size = 10),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "gray90", color = "black")
  )

ggsave("VHT_climate_scatter_support.png", width = 14, height = 6, units = "in", dpi = 300)

#table of final results ----------------------------------------------------------------

final_kendall_table <- kendall_by_lake %>%
  semi_join(sig_combos, by = c("lakeid", "Index", "Month")) %>%
  arrange(factor(Index, levels = c("PDO", "PNA")), lakeid, Month) %>%
  mutate(
    `Kendall's tau` = sprintf("%.2f", tau),
    `p-value` = sprintf("%.3f", p_value)
  ) %>%
  select(
    Lake = lakeid,
    `Climate Index` = Index,
    `Lag Month` = Month,
    `Kendall's tau`,
    `p-value`,
    n
  )

#save
write.csv(final_kendall_table, "kendall_final_table.csv", row.names = FALSE)
