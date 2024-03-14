# DOI: 10.1111/gcb.15393
# functions from the paper to calculate stress factors on grain-setting due to extreme high temperatures during reproductive stage
# Import necessary libraries
library(readxl)
library(data.table)
library(ggplot2)
# Import the normal and artificial weather data 
# normal weather data
weather_data <- read_excel('weather data Ludhiana India PULU0001.xlsx', skip = 4) |> as.data.table()
weather_highTmax <- read_excel('weather data Ludhiana India PULU0001-increase T.xlsx', skip = 4) |> as.data.table()
weather_data <- read_excel('weather.xlsx', .name_repair = "universal") |> as.data.table()

setnames(weather_data, c("DAS", "GDD", "Tmax", "Tmin", "WeatherDate"))
heading_st <- 74
heading_ed <- heading_st + 7
weather_data[(DAS >= heading_st & (DAS <= heading_ed)), ':=' (Tmax = Tmax + 5, 
                                                Tmin = Tmin + 5)]
# get fake hourly temp data from daily data
calculate_Tj <- function(Tmax, Tmin, j) {
    Tj <- (Tmax + Tmin) / 2 + (Tmax - Tmin) / 2 * cos((j - 14) / 12 * pi)
    return(Tj)
}

weather_data[1]


# Function 9 - produce the output for function 8
# tau is now a constant equal to 35

HDI <- function(Temp, tau = 35 ) { 
    hdi <- sum(pmax(Temp - tau, 0)) / 24.0
    return(hdi)
}
# Function to calculate the DVS using GDD and a parameter P3 - It is off - 
# The output of this one 
calculate_DVS <- function(GDD, GDD_1_3, GDD_START_HEADING) {
    # Define the DVS values
    DVS_START <- 1
    DVS_END <- 1.3
    DVS <- DVS_START + (GDD - GDD_START_HEADING)*(DVS_END - DVS_START)/(GDD_1_3 - GDD_START_HEADING) #REMEMBER THAT GDD IS FROM START OF PERIOD 3
    return(DVS)
}

# Function 10 - this function is needed before calling function 8
# Function to calculate parameter b using the specific Gaussian distribution-type function
calculate_specific_b <- function(DVS) {
    b0 <- 0.08
    A <- 0.04
    xc <- 1.03
    w <- 0.16
    pi <- pi
    b <- b0 + (A / (w * sqrt(pi / 2))) * exp(-2 * ((DVS - xc) / w)^2)
    return(b)
}

# Function 8
f_hdi <- function(HDI, b) {
    c <- 3.0 / b  # c is calculated as 3/b
    logistic <- 1.0 / (1.0 + exp(b * (HDI - c)))
    return(logistic)
}


TRF_GS <- function(Temp, GDD, GDD_START_HEADING, GDD_1_3, number_of_headings = 1) {
    trf_gs <- 1.0
    for (i in 1:number_of_headings) {
        DVS <- calculate_DVS(GDD, GDD_START_HEADING, GDD_1_3)
        if (DVS >= 1) {
            hdi_value <- HDI(Temp)
            specific_b <- calculate_specific_b(DVS)
            trf_gs <- trf_gs - (1 - f_hdi(hdi_value, specific_b))
        }
    }
    return(trf_gs)
}

### set up the constant and heading period
GDD_START_HEADING <- 1269
GDD_1_3 <- 1386
DOY <- 74
tau_cultivar <- 30.9

# Function to populate the temperature array for every hour in a day
l <- list()
days <- DOY : (DOY+7)
for (day in days){
  day_temp <- weather_data[DAS == day]
  hourly_T <- rep(0, 24)
  
  for (hour in 1:24) {
    hourly_T[hour] <- calculate_Tj(day_temp$Tmax, day_temp$Tmin, hour)
  }
  l[[day]] <- data.table(Temp = hourly_T, Hour = 1:24)
  
}
hourly_temp <- rbindlist(l, idcol = "DAS") 

reprduct_temp_daily <- weather_data[(DAS >= DOY)&(DAS <= DOY + 7),]
# Create an unique id for easy plotting 
hourly_temp[, ID := 1:.N] 

hourly_combined <- hourly_temp[reprduct_temp_daily, on = "DAS"]
hourly_combined |> 
  ggplot(aes(ID, Temp)) +
  geom_line(linewidth = 1.5)+
  geom_point(aes(y = Tmax, color = "Tmin")) +
  geom_point(aes(y = Tmin, color = "Tmax"))+
  scale_color_manual(name = "Real Temperature", values = c(Tmax = "red", Tmin = "blue"),
                     guide = guide_legend(override.aes = list(size = 6)))+
  labs(x = "Day after planting", 
       y = "Temperature degree C")+
  theme_bw()+
  theme(text = element_text(size = 16))+
  scale_x_continuous(breaks = seq(min(hourly_combined$ID), max(hourly_combined$ID), by = 24), 
                     labels = seq(min(hourly_combined$DAS), max(hourly_combined$DAS), by = 1))

ggsave("checktemp.png", dpi = 300 )

# testing TRF_GS
TRF_GS_test <- TRF_GS(hourly_combined[day == DOY,]$Temp , 1000, GDD_START_HEADING, GDD_1_3)

# CALCULATING gdd
survival <- as.numeric("")
for( i in heading_st : heading_ed) {
    # print(i)
    Temperature_24 <- hourly_combined[DAS == i,]$Temp  # Replace with actual temperature data
    # print(Temperature_24)
    GDD <- weather_data[DAS == i,]$GDD
    # print(GDD)
    survival[i] <- TRF_GS(Temperature_24, GDD, GDD_START_HEADING = GDD_START_HEADING , GDD_1_3 = GDD_1_3  )
    # print(survival)
}
new_grain_setting_rate <- data.table(DAS = heading_st: heading_ed, grain_setting_rate = survival[!is.na(survival)])
prod(new_grain_setting_rate$grain_setting_rate)


temp_fake <- seq(25, 40, 1)
temp_fake_tmax <- temp_fake + 5
temp_fake_tmin <- temp_fake - 5

temp_fake_DT <- data.table(dailymean = temp_fake, temp_fake_tmax, temp_fake_tmin)
l_fake <- list()

for (mean in temp_fake){
   hourly_T <- rep(0, 24)
   max_min <- temp_fake_DT[dailymean == mean]

    for (hour in 1:24) {
    hourly_T[hour] <- calculate_Tj(max_min$temp_fake_tmax, max_min$temp_fake_tmin, hour)
    }
    l_fake[[mean]] <- data.table(Temp = hourly_T, Hour = 1:24)

}
hourly_temp_fake <- rbindlist(l_fake, idcol = "dailymean") 
hourly_combined$GDD |> unique()
for( i in temp_fake) {
    print(i)
    Temperature_24 <- hourly_temp[day == i,]$Temp  # Replace with actual temperature data
    GDD <- 1300
    survival <- TRF_GS(Temperature_24, GDD, GDD_START_HEADING = GDD_START_HEADING , GDD_1_3 = GDD_1_3  )
    print(survival)
}

GDD_range <- unique(hourly_combined$GDD)
b_list <- vector("list", length = length(GDD_range))
# names(b_list) <- GDD_range
i = 1
for(GDD in GDD_range){

  dvs <- calculate_DVS(GDD, GDD_1_3 = GDD_1_3, GDD_START_HEADING = GDD_START_HEADING)
  b <- calculate_specific_b(dvs)
  b_list[[i]] <- data.table(DVS = dvs, b = b)
  i = i+ 1
}

b_over_dvs <- rbindlist(b_list)
b_over_dvs |> 
  ggplot(aes(DVS, b))+
  geom_line()
b_over_dvs[, id := 1:.N]
hourly_combined
hourly_temp_fake

fertility_list <- list()

for (j in 1:nrow(b_over_dvs)) {
    for (i in temp_fake) {
        temp_int <- HDI(hourly_temp_fake[dailymean == i,]$Temp, tau_cultivar)
        fertility_list[[paste0("fertility_", j, "_", i)]] <- f_hdi(temp_int, b_over_dvs[j]$b)
    }
}

b_dvs <- rbindlist(lapply(1:nrow(b_over_dvs), function(j) {
    data.table(fertility = unlist(fertility_list[paste0("fertility_", j, "_", temp_fake)]), 
                         temp_range = temp_fake, 
                         b_DVS = j)
}), idcol = "id")

b_dvs[b_over_dvs, on = "id"] |> 
    ggplot(aes(temp_range, fertility, color = factor(DVS))) +
    geom_line() +
    labs(color = "DVS")



b_over_dvs[, hdi := HDI(Temp = )]

# indian expeirment simulation suggests that we have the reproductive stage at DOY = 231

HDI_test <- HDI(reprduct_temp[day == DOY,]$Temp)

# Function to calculate DVS using GDD and a parameter P3
# GDD is the growing degree days from the start of period 3
tbase <- 10


# testing 
DVS_test <- calculate_DVS(1000, GDD_1_3, GDD_START_HEADING)

b_test <- calculate_specific_b(DVS_test)
f_test <- f(HDI_test, b_test)
TRF_GS_test <- TRF_GS(reprduct_temp[day == DOY,]$Temp , 1000, GDD_START_HEADING, GDD_1_3)

TRF_GS_test
f_test
DOY_planting <- 149
weather_data[, Tt := 0]

st<-reprduct_temp$DATE[1]
ed <- reprduct_temp$DATE[nrow(reprduct_temp)]

trf_gs <- as.double(rep(NA, ed-st+1))
for (i in st:ed){

    T <- hourly_temp[day == i,]$Temp  # Replace with actual temperature data
    GDD <- reprduct_temp[DATE == i,]$GDD

    hdi <- HDI(T, tau_cultivar)
    DVS <- calculate_DVS(GDD, GDD_1_3, GDD_START_HEADING)
    b <- calculate_specific_b(DVS)
    f_hdi <- f(hdi, b)
    trf_gs[i] <- f_hdi
    
    # daily_survival <- TRF_GS(T, GDD, GDD_START_HEADING, GDD_1_3)

}
1 - sum(1 - trf_gs[!is.na(trf_gs)])

reprduct_temp


# grain setting - function 11 - maybe unnecessary

# Function to multiply all the penalties over the period of interest and subtract the value from GPP
# calculate_penalty_and_adjust_GPP <- function(GPP = 1, start_day, end_day) {
    total_penalty <- 1.0
    for (i in start_day:end_day) {
        T <- reprduct_temp[day == DOY,]$Temp  # Replace with actual temperature data
        daily_penalty <- TRF_GS(T, GDD, GDD_START_HEADING, GDD_1_3)
        total_penalty <- total_penalty * daily_penalty
    }
    adjusted_GPP <- GPP - total_penalty
    # return(adjusted_GPP)
# }
calculate_penalty_and_adjust_GPP(1, 231, 240)



