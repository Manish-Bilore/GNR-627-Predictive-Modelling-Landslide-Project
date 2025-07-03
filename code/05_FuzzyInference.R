# install.packages("sets")
library(sets)
library(terra)
library(RColorBrewer)
library(viridis)
library(ggplot2)


data <- read.csv(file = "D:/GNR 627 Predictive Modelling Landslide Project/output/data_straight.csv")

summary(data)


# Define Input Variables

# Create fuzzy variables for Elevation
sets_options("universe", seq(0, 2100, by = 1))  # Define the range


elevation <- fuzzy_partition(varnames = c(Very_Low = 26, Low = 500, Medium = 1000, High = 1500, Very_High = 2000), 
                             FUN = fuzzy_cone)  # Triangular membership function

# Create fuzzy variables for Slope
slope <- fuzzy_partition(varnames = c(Very_Low = 00, Low = 5, Medium = 40, High = 50, Very_High = 65), 
                         FUN = fuzzy_cone)

# Create fuzzy variables for Topographic Wetness Index
twi <- fuzzy_partition(varnames = c(Very_Low = 00, Low = 7, Medium = 15, High = 20, Very_High = 25), 
                          FUN = fuzzy_cone)

# Create fuzzy variables for NDVI
ndvi <- fuzzy_partition(varnames = c(Very_Low = 0, Low = 0.2, Medium = 0.4, High = 0.6, Very_High = 0.8), 
                       FUN = fuzzy_cone)


# Triangular membership function
triangular_membership <- function(x, a, b, c) {
  if (x <= a || x >= c) {
    return(0)
  } else if (x > a && x <= b) {
    return((x - a) / (b - a))
  } else if (x > b && x < c) {
    return((c - x) / (c - b))
  }
}


# Membership function
fuzz_membership <- function(x, vlw_start, vlw_peak, vlw_end, 
                               low_start, low_peak, low_end, 
                               mid_start, mid_peak, mid_end,
                               hig_start, hig_peak, hig_end,
                               vhi_start, vhi_peak, vhi_end
                            ) {
  list(
    Very_Low  = triangular_membership(x, vlw_start, vlw_peak, vlw_end),
    Low       = triangular_membership(x, low_start, low_peak, low_end),
    Medium    = triangular_membership(x, mid_start, mid_peak, mid_end),
    High      = triangular_membership(x, hig_start, hig_peak, hig_end),
    Very_High = triangular_membership(x, vhi_start, vhi_peak, vhi_end)
  )
}

################################################################################
#Visualize Membership Functions

# Elevation
elevation_values <- seq(26, 2100, by = 1)

# Membership parameter values for Elevation
vlw_start <- 26; vlw_peak <- 500; vlw_end <- 700
low_start <- 700; low_peak <- 900; low_end <- 1100
mid_start <- 1100; mid_peak <- 1300; mid_end <- 1500
hig_start <- 1500; hig_peak <- 1700; hig_end <- 1900
vhi_start <- 1700; vhi_peak <- 2000; vhi_end <- 2100

elevation_membership <- sapply(elevation_values, 
                               function(x) fuzz_membership(x, vlw_start, vlw_peak, vlw_end, 
                                                              low_start, low_peak, low_end, 
                                                              mid_start, mid_peak, mid_end,
                                                              hig_start, hig_peak, hig_end,
                                                              vhi_start, vhi_peak, vhi_end))

# Set the file path and name for the PNG file
png(here("plots/Issue_out/membership_functions_elevation.png"), width = 5000, height = 2500, res = 300)

# Adjust margins to make space for the plot and legend
par(mar = c(5, 4, 4, 2))

# Plot all membership functions
plot(elevation_values, elevation_membership["Very_High", ], type = "l", col = "red",
     xlab = "Elevation Values", ylab = "Membership Degree",
     main = "Elevation Membership Functions",
     ylim = c(0, 1.15))

lines(elevation_values, elevation_membership["High", ], col = "green")
lines(elevation_values, elevation_membership["Medium", ], col = "orange")
lines(elevation_values, elevation_membership["Low", ], col = "purple")
lines(elevation_values, elevation_membership["Very_Low", ], col = "blue")

# Add legend at the top (adjust positioning)
legend("top", legend = c("Very Low", "Low", "Medium", "High", "Very High"),
       col = c("blue", "purple", "orange", "green", "red"), lty = 1, cex = 1.2, 
       box.lwd = 0.5, ncol = 5, xpd = TRUE, inset = c(0, 0.0))

# Close the PNG device
dev.off()

################################################################################

# Slope
slope_values <- seq(0, 20, by = 1)

# Membership parameter values for Slope
vlw_start <- 0;   vlw_peak <- 2;   vlw_end <- 4    # Very Low
low_start <- 3;    low_peak <- 6;    low_end <- 8    # Low
mid_start <- 7;    mid_peak <- 10;   mid_end <- 13  # Medium
hig_start <- 12;   hig_peak <- 16;   hig_end <- 18  # High
vhi_start <- 17;   vhi_peak <- 19;   vhi_end <- 20  # Very High


slope_membership <- sapply(slope_values, 
                           function(x) fuzz_membership(x, vlw_start, vlw_peak, vlw_end, 
                                                          low_start, low_peak, low_end, 
                                                          mid_start, mid_peak, mid_end,
                                                          hig_start, hig_peak, hig_end,
                                                          vhi_start, vhi_peak, vhi_end))
# Set the file path and name for the PNG file
png(here("plots/Issue_out/membership_functions_slope.png"), width = 5000, height = 2500, res = 300)

# Adjust margins to make space for the plot and legend
par(mar = c(5, 4, 4, 2))

# Plot all membership functions
plot(slope_values, slope_membership["Very_High", ], type = "l", col = "red",
     xlab = "slope Values", ylab = "Membership Degree",
     main = "Slope Membership Functions",
     ylim = c(0, 1.15))

lines(slope_values, slope_membership["High", ], col = "green")
lines(slope_values, slope_membership["Medium", ], col = "orange")
lines(slope_values, slope_membership["Low", ], col = "purple")
lines(slope_values, slope_membership["Very_Low", ], col = "blue")

# Add legend at the top (adjust positioning)
legend("top", legend = c("Very Low", "Low", "Medium", "High", "Very High"),
       col = c("blue", "purple", "orange", "green", "red"), lty = 1, cex = 1.2, 
       box.lwd = 0.5, ncol = 5, xpd = TRUE, inset = c(0, 0.0))

# Close the PNG device
dev.off()

################################################################################

# NDVI
ndvi_values <- seq(-0.09741, 0.68559, by = 0.00001)

# Membership parameter values for NDVI
vlw_start <- -0.09741; vlw_peak <- 0.05; vlw_end <- 0.15    # Very Low
low_start <- 0.12;     low_peak <- 0.20; low_end <- 0.30    # Low
mid_start <- 0.28;     mid_peak <- 0.40; mid_end <- 0.50    # Medium
hig_start <- 0.48;     hig_peak <- 0.55; hig_end <- 0.60    # High
vhi_start <- 0.58;     vhi_peak <- 0.65; vhi_end <- 0.68559 # Very High

ndvi_membership <- sapply(ndvi_values, 
                           function(x) fuzz_membership(x, vlw_start, vlw_peak, vlw_end, 
                                                          low_start, low_peak, low_end, 
                                                          mid_start, mid_peak, mid_end,
                                                          hig_start, hig_peak, hig_end,
                                                          vhi_start, vhi_peak, vhi_end))

# Set the file path and name for the PNG file
png(here("plots/Issue_out/membership_functions_ndvi.png"), width = 5000, height = 2500, res = 300)

# Adjust margins to make space for the plot and legend
par(mar = c(5, 4, 4, 2))

# Plot all membership functions
plot(ndvi_values, ndvi_membership["Very_High", ], type = "l", col = "red",
     xlab = "NDVI Values", ylab = "Membership Degree",
     main = "NDVI Membership Functions",
     ylim = c(0, 1.15))

lines(ndvi_values, ndvi_membership["High", ], col = "green")
lines(ndvi_values, ndvi_membership["Medium", ], col = "orange")
lines(ndvi_values, ndvi_membership["Low", ], col = "purple")
lines(ndvi_values, ndvi_membership["Very_Low", ], col = "blue")

# Add legend at the top (adjust positioning)
legend("top", legend = c("Very Low", "Low", "Medium", "High", "Very High"),
       col = c("blue", "purple", "orange", "green", "red"), lty = 1, cex = 1.2, 
       box.lwd = 0.5, ncol = 5, xpd = TRUE, inset = c(0, 0.0))

# Close the PNG device
dev.off()

################################################################################

# twi
twi_values <- seq(-3.000, 28.096, by = 0.001)

# Membership parameter values for TWI
vlw_start <- -3.000; vlw_peak <- 1.000; vlw_end <- 5.000    # Very Low
low_start <- 4.500;  low_peak <- 7.500; low_end <- 10.000   # Low
mid_start <- 9.500;  mid_peak <- 15.000; mid_end <- 20.000  # Medium
hig_start <- 19.500; hig_peak <- 23.000; hig_end <- 26.000  # High
vhi_start <- 25.500; vhi_peak <- 27.000; vhi_end <- 28.096  # Very High

twi_membership <- sapply(twi_values, 
                          function(x) fuzz_membership(x, vlw_start, vlw_peak, vlw_end, 
                                                      low_start, low_peak, low_end, 
                                                      mid_start, mid_peak, mid_end,
                                                      hig_start, hig_peak, hig_end,
                                                      vhi_start, vhi_peak, vhi_end))

# Set the file path and name for the PNG file
png(here("plots/Issue_out/membership_functions_twi.png"), width = 5000, height = 2500, res = 300)

# Adjust margins to make space for the plot and legend
par(mar = c(5, 4, 4, 2))

# Plot all membership functions with extended ylim
plot(twi_values, twi_membership["Very_High", ], type = "l", col = "red",
     xlab = "TWI Values", ylab = "Membership Degree",
     main = "TWI Membership Functions",
     ylim = c(0, 1.15))  # Extend ylim to 1.2 for visual spacing

lines(twi_values, twi_membership["High", ], col = "green")
lines(twi_values, twi_membership["Medium", ], col = "orange")
lines(twi_values, twi_membership["Low", ], col = "purple")
lines(twi_values, twi_membership["Very_Low", ], col = "blue")

# Add legend at the top (adjust positioning)
legend("top", legend = c("Very Low", "Low", "Medium", "High", "Very High"),
       col = c("blue", "purple", "orange", "green", "red"), lty = 1, cex = 1.2, 
       box.lwd = 0.5, ncol = 5, xpd = TRUE, inset = c(0, 0.0))

# Close the PNG device
dev.off()

################################################################################


# Apply fuzzy rules
evaluate_rules <- function(elev, slope, twi, ndvi) {
  # Compute membership values for each input
  # elev_m <- fuzz_membership(elev,     0, 10, 25, 10, 25, 40, 25, 40, 50)
  elev_m <- fuzz_membership(elev, # Membership parameter values for Elevation
                            vlw_start <- 26, vlw_peak <- 500, vlw_end <- 700,
                            low_start <- 700, low_peak <- 900, low_end <- 1100,
                            mid_start <- 1100, mid_peak <- 1300, mid_end <- 1500,
                            hig_start <- 1500, hig_peak <- 1700, hig_end <- 1900,
                            vhi_start <- 1700, vhi_peak <- 2000, vhi_end <- 2100)
  
  # slope_m <- fuzz_membership(slope,   0,  6, 10,  8, 12, 16, 15, 16, 20) 
  slope_m <- fuzz_membership(slope, vlw_start <- 0,   vlw_peak <- 2,   vlw_end <- 4,
                             low_start <- 3,    low_peak <- 6,    low_end <- 8,
                             mid_start <- 7,    mid_peak <- 10,   mid_end <- 13,  # Medium
                             hig_start <- 12,   hig_peak <- 16,   hig_end <- 18,  # High
                             vhi_start <- 17,   vhi_peak <- 19,   vhi_end <- 20)  # Very High)
  

  twi_m <- fuzz_membership(twi, vlw_start <- -3.000, vlw_peak <- 1.000, vlw_end <- 5.000,    # Very Low
                           low_start <- 4.500,  low_peak <- 7.500, low_end <- 10.000,   # Low
                           mid_start <- 9.500,  mid_peak <- 15.000, mid_end <- 20.000,  # Medium
                           hig_start <- 19.500, hig_peak <- 23.000, hig_end <- 26.000,  # High
                           vhi_start <- 25.500, vhi_peak <- 27.000, vhi_end <- 28.096)  # Very High)
  
  ndvi_m <- fuzz_membership(ndvi, # Membership parameter values for NDVI
                            vlw_start <- -0.09741, vlw_peak <- 0.05, vlw_end <- 0.15,    # Very Low
                            low_start <- 0.12,     low_peak <- 0.20, low_end <- 0.30,    # Low
                            mid_start <- 0.28,     mid_peak <- 0.40, mid_end <- 0.50,    # Medium
                            hig_start <- 0.48,     hig_peak <- 0.55, hig_end <- 0.60,    # High
                            vhi_start <- 0.58,     vhi_peak <- 0.65, vhi_end <- 0.68559) # Very High)
  
  # Rule 1: Very High Susceptibility
  # If Elevation is Very High OR Slope is Very High AND 
  # TWI is Very High OR TWI is High OR TWI is Medium AND 
  # NDVI is Very Low OR NDVI is Low OR NDVI is Medium
  # very_high_susceptibility <- max(
  #   elev_m$Very_High,  # Elevation is Very High
  #   slope_m$Very_High * max(twi_m$Very_High, twi_m$High, twi_m$Medium) * max(ndvi_m$Very_Low, ndvi_m$Low, ndvi_m$Medium)
  # )
  very_high_susceptibility <- max(
    elev_m$Very_High,  # Elevation is Very High
    slope_m$Very_High * max(ndvi_m$Very_Low, ndvi_m$Low)
  )
  # very_high_susceptibility <- max(
  #   elev_m$Very_High,  # Elevation is Very High
  #   slope_m$Very_High 
  # )
  
  # # # Rule 1: If Elevation is High OR Slope is High, then Landslide Susceptibility is High
  # very_high_susceptibility <- max(elev_m$Very_High, slope_m$Very_High)
  
  # Rule 2: High Susceptibility
  # If Elevation is Very OR Slope is High AND TWI is High AND NDVI is Low
  high_susceptibility <- max(
    elev_m$High,  # Elevation is High
    slope_m$High * max(twi_m$Very_High, twi_m$High, twi_m$Medium) * max(ndvi_m$Very_Low, ndvi_m$Low, ndvi_m$Medium)
  )
  
  # # Rule 2: If Elevation is High OR Slope is High, then Landslide Susceptibility is High
  # high_susceptibility <- max(elev_m$High, slope_m$High)
  
  # Rule 3: Medium Susceptibility
  # If Elevation is Very OR Slope is Medium AND TWI is Medium AND NDVI is Medium
  medium_susceptibility <- max(elev_m$Medium, 
                             slope_m$Medium * twi_m$Medium * ndvi_m$Medium)
  
  # Rule 4: Low Susceptibility
  # If Elevation is Low AND Slope is Low AND TWI is Low AND NDVI is High
  low_susceptibility <- min(elev_m$Low, slope_m$Low, twi_m$Low, ndvi_m$High)
  
  # Rule 5: Very Low Susceptibility
  # If Elevation is Very Low AND Slope is Very Low AND TWI is Very Low AND NDVI is Very High
  very_low_susceptibility <- min(elev_m$Very_Low, slope_m$Very_Low, twi_m$Very_Low, ndvi_m$Very_High)
  
  
  # ##############################################################################
  # # Defuzzify using a weighted average
  # # Handle Zero Denominator: 
  # # If all membership values (very_high_susceptibility, high_susceptibility, etc.) are 0, 
  # # the denominator will result in division by zero. To prevent this, add a condition:
  # denominator <- very_high_susceptibility , 
  #                high_susceptibility , 
  #                medium_susceptibility , 
  #                low_susceptibility , 
  #                very_low_susceptibility
  # 
  # if (denominator == 0) {
  #   crisp_output <- 0  # Default to the lowest susceptibility
  # } else {
  # 
  #   crisp_output <- (
  #     very_high_susceptibility    * 4 ,
  #       high_susceptibility       * 3 ,
  #       medium_susceptibility     * 2 ,
  #       low_susceptibility        * 1 ,
  #       very_low_susceptibility   * 0
  #   ) / denominator
  # }
  # 
  # return(crisp_output)
  # ##############################################################################
  
  # Mean of Maximum (MOM) Defuzzification

  # Define the midpoints for each fuzzy set (Very Low, Low, Medium, High, Very High)
  midpoints <- c(0, 1, 2, 3, 4)  # These correspond to the fuzzy sets Very Low, Low, Medium, High, Very High
  
  # Calculate the maximum membership values for each fuzzy set
  max_values <- c(very_low_susceptibility, low_susceptibility, medium_susceptibility, high_susceptibility, very_high_susceptibility)
  
  # Identify the fuzzy sets that have the maximum membership value
  max_membership <- which.max(max_values)
  
  # Calculate the crisp output using the Mean of Maximum method
  if (max_values[max_membership] == 0) {
    crisp_output <- 0  # If all values are 0, assign 0
  } else {
    # Find the mean of the maximum values (i.e., take the average of the midpoints for the fuzzy sets with the highest membership)
    selected_midpoints <- midpoints[max_membership]
    crisp_output <- mean(selected_midpoints)
  }
  
  return(crisp_output)
  
  
}


# Apply the fuzzy rules to the dataset

data$Landslide_Susceptibility <- mapply(
  evaluate_rules,
  data$Elevation_Class,
  data$Slope_Class,
  data$TWI_Class,
  data$NDVI_Class
)

################################################################################


# Round the 'Score' column to the nearest integer
# Round values and constrain between 1 and 5
# data$Landslide_Susceptibility <- pmin(pmax(round(data$Landslide_Susceptibility), 1), 5)

summary(data)

# Check

df_landslide_fuzzy <- data[, c(1,2,11)]
summary(df_landslide_fuzzy)

# Convert the dataframe to a spatial vector
spatial_df <- vect(df_landslide_fuzzy, geom = c("x", "y"), crs = "EPSG:4326")

# Convert the dataframe to a SpatRaster
raster_landslide <- terra::rast(
  x = df_landslide_fuzzy,
  type = "xyz",
  crs = crs(spatial_df)  
)

raster_landslide

# Apply a smoothing filter (3x3 moving average)
smoothed_raster <- focal(raster_landslide, w=matrix(1, 3, 3), fun=mean, na.rm=TRUE)

help(focal)

smoothed_raster


# Save as a TIFF file
dir_path <- here("output/process")
output_file_raster_landslide <- file.path(dir_path, "raster_landslide.tif")

terra::writeRaster(smoothed_raster, filename = output_file_raster_landslide, overwrite = TRUE)

# Areal extent
extent <- terra::ext(spatial_df)
xmin <- extent[1]
xmax <- extent[2]
ymin <- extent[3]
ymax <- extent[4]


#################################################################################

# Define a custom color palette (example: blue to red)
custom_palette <- colorRampPalette(c("lightgrey", "lightblue", "yellow", "orange", "red"))


# Get the extent of the raster
e <- ext(smoothed_raster)

# Calculate the aspect ratio (width/height of the extent)
aspect_ratio <- (e[2] - e[1]) / (e[4] - e[3])

# Set a fixed height, and calculate width based on the aspect ratio
height <- 2500  # Set the desired height
width <- height * aspect_ratio

# Open PNG device with the desired resolution
png(here("plots/Issue_out/Landslide_sus.png"), width = width, height = height, res = 300)

# Plot the SpatRaster with a specified resolution for visualization
plot(smoothed_raster, 
     main = "Landslide Susceptibility (Jan 2024)",           # Title
     axes = TRUE,                  # Show axes
     legend = TRUE,                # Show legend
     cex.main = 1.5,               # Title size (reasonable size)
     cex.axis = 1,                 # Axis label size (slightly bigger)
     cex.lab = 1.2,                # Axis text size (x and y labels)
     cex.legend = 1,               # Legend text size
     tck = 0.02,                   # Length of axis ticks
     mgp = c(3, 1, 0),             # Margins for axis labels and ticks
     xlim = c(xmin, xmax),         # Set xmin and xmax
     ylim = c(ymin, ymax),         # Set ymin and ymax
     col = custom_palette(100))         

# Overlay contour lines
# lines(contours, col = "black", lwd = 0.5)

# Close the PNG device
dev.off()
################################################################################



# Convert raster to data frame
df_raster <- as.data.frame(smoothed_raster, xy = TRUE, na.rm = TRUE)

summary(df_raster)

# Rename columns for clarity
colnames(df_raster) <- c("Longitude", "Latitude", "Susceptibility")

# Create the plot
ggplot(df_raster, aes(x = Longitude, y = Latitude, fill = Susceptibility)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = c("lightgrey", "lightblue", "yellow", "orange", "red"), 
    breaks = c(1, 2, 3, 4, 5),                       # Legend breaks for all levels
    labels = c("Very Low", "Low", "Medium", "High", "Very High"),  # Custom labels
    name = "Susceptibility"                          # Legend title
  ) +
  labs(
    title = "Landslide Susceptibility Plot", 
    x = "Longitude", 
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",                # Legend positioned to the right
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10),   # Size of legend labels
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Centered bold title
    axis.title = element_text(size = 12, face = "bold")  # Bold axis titles
  )


################################################################################
# # Check NDVI Barren areas
# df_NDVI_barren <- data[, c(1,2,9)]
# head(df_NDVI_barren)
# ggplot(df_NDVI_barren, aes(x = x, y = y, color = NDVI_Class)) +
#   geom_point(size = 4) +
#   scale_color_gradient(low = "blue", high = "gray90") +
#   labs(
#     title = "NDVI Class Distribution",
#     x = "Longitude (x)",
#     y = "Latitude (y)",
#     color = "NDVI Class"
#   ) +
#   theme_minimal()
# 
# # Check Elevation and Slope for Very High and High areas
# df_chk_elevation <- data[, c(1,2,3)]
# df_chk_elevation$Elevation_Class[df_chk_elevation$Elevation_Class <= 20] <- 0
# head(df_chk_elevation)
# 
# df_chk_slope <- data[, c(1,2,4)]
# df_chk_slope$Slope_Class[df_chk_slope$Slope_Class <= 20] <- 0
# head(df_chk_slope)
# 
# ggplot(df_chk_elevation, aes(x = x, y = y, color = Elevation_Class)) +
#   geom_point(size = 4) +
#   scale_color_gradient(low = "blue", high = "gray90") +
#   labs(
#     title = "Check Elevation High and Very High Distribution",
#     x = "Longitude (x)",
#     y = "Latitude (y)",
#     color = "Elevation Class"
#   ) +
#   theme_minimal()
