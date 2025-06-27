# paper_figures.R
# This script is designed to generate figures for the paper
# Last modified: January 2025
# Author: Candelaria Bergero

#Here we analyze AR6 outputs as well as MAGCIC temperature trajectories
#We have 407 total AR6 scenario (we use energy data from AR6 and reanalysis data from Gidden et al 2023)
#C1 70;  C2  106;  C3 231
#reanalysis: https://www.nature.com/articles/s41586-023-06724-y#Abs1
#Ar6 Data: https://zenodo.org/records/10158920

#-------------------------------------------------------------------------------------------------------------------------------------------------------
#### Load packages ####
library(plyr)
library(readr)
library(ggplot2)
library(devtools)
library(tidyr)
library(dplyr)
library(directlabels)
library(viridisLite)
library(usmap)

library(forcats)
library(processx)
library(magrittr)
library(reticulate)
library(plotly)
library(zoo)
library(purrr)
library(viridis)

#Set working directory
setwd("/Users/mariacandelariabergero/Library/CloudStorage/GoogleDrive-cande.bergero@gmail.com/My Drive/Education/2021 (UCI)/Research/3_IIASA")

#### Data files ####
    #AR6 complete
    AR6_data_complete<- read_csv("data/AR6_Scenarios_Database_World_v1.0.csv") 

    #AR6 selected files
    c1_ar6_output <- read_csv("data/C1_ar6_snapshot_1718200280.csv/ar6_snapshot_1718200280.csv") %>% mutate(Path = "C1")#Global data on carbon sequestration
    c2_ar6_output <- read_csv("data/C2_ar6_snapshot_1718200280.csv/ar6_snapshot_1718200331.csv") %>% mutate(Path = "C2")#Global data on carbon sequestration
    c3_ar6_output <- read_csv("data/C3_ar6_snapshot_1718200280.csv/ar6_snapshot_1718200368.csv") %>% mutate(Path = "C3")#Global data on carbon sequestration
    
    AR6_data <- bind_rows(c1_ar6_output, c2_ar6_output, c3_ar6_output)
    
    #Now load reanalysis data from Gidden for Land Sequestration
    gidden_land_reanalysis<- read_csv("data/10.5281_zenodo.10158920_gidden_et_al_2023_ar6_reanalysis_data.csv")

    #Load historical emissions to fix 2015
    #Note: we interpolate future years when in between decades are missing, but for 2015 we use the change in historical emissions and apply it to 2010
    history_ar6 <- read_csv("data/history_ar6.csv")
    history_ar6_mapping <- read_csv("data/history_ar6_mapping.csv")
    
    history_ar6 %>%
      select(Variable, Unit, `2010`, `2015`) %>%
      left_join(history_ar6_mapping, by = c("Variable" = "Variable_original")) %>%
      select(-Variable) %>%
      rename(Variable = Variable.y,
             `2010_history` = `2010`,
             `2015_history` = `2015`)-> history_ar6_fixed
    
##### Other #####
    #Filter
    years_to_keep <-  seq(from = 2010, to = 2100, by = 5)
    years_to_keep_extended <-  seq(from = 2005, to = 2100, by = 5)
    paths <- c("C1", "C2", "C3")
    
    #Colors
    colors<- c("C1" = "#D14592", "C2" = "#f59d66", "C3" = "#6a60a9")
    
    colors_variable <- c("Novel CDR" = "#120D32FF", "Land CDR (reanalysis)" = "#A3307EFF", "fossil CCS" = "#FEA873FF")
    
    colors_function <- c("offsetting CDR" = "#C83E73FF", "restoring CDR" = "#5A167EFF", 
                         "Fossil CCS (Electricity)" = "#F97C5DFF", "Fossil CCS (Industry)" ="#FED395FF",
                         "Fossil CCS (Others)" = "#FEA873FF", "Industrial Processes CCS" = "#FCFDBFFF")
    
    scales::show_col(viridis_pal(option = "magma")(12))  
    (viridis_pal(option = "magma")(12))  
    
    # colors<- c("C1" = "#fee08b", 
    #            "C2" = "#9e0142", 
    #            "C3" = "#5e4fa2")
    # 
    # colors_variable <- c("Novel CDR" = "#d53e4f", 
    #                      "Land CDR (reanalysis)" = "#f46d43", 
    #                      "fossil CCS" = "#fdae61")
    # 
    # colors_function <- c("offsetting CDR" = "#3288bd", 
    #                      "restoring CDR" = "#66c2a5", 
    #                      "Fossil CCS (Electricity)" = "#abdda4", 
    #                      "Fossil CCS (Others)" = "#e6f598", 
    #                      "Fossil CCS (Industry)" ="#ffffbf",
    #                      "Industrial Processes CCS" = "#F1F9C7")
    #Order
    category_order <- c("fossil CCS in Net-zero", "fossil CCS in 2100",
                        "Land CDR (reanalysis) in Net-zero", "Land CDR (reanalysis) in 2100",
                        "Novel CDR in Net-zero", "Novel CDR in 2100")
    
    variable_order <- c("fossil CCS", "Land CDR (reanalysis)", "Novel CDR") 
    
    function_order <- c("Industrial Processes CCS", "Fossil CCS (Industry)", "Fossil CCS (Others)", 
                        "Fossil CCS (Electricity)", "offsetting CDR", "restoring CDR")
    
    underdelivered_order <- c("0%", "10%", "20%","30%", "40%", "50%",
                              "60%", "70%", "80%", "90%", "100%")
    
    #Figure format
    figure_theme <- theme(panel.background = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.grid.major.x = element_line( size=.4, color="gray"),
                          panel.grid.major.y = element_line( size=.4, color="gray"),
                          panel.spacing = unit(2, "lines"),
                          plot.title = element_text(face="bold", size=25, hjust = 0.5),
                          axis.title.x = element_text(size=10),
                          axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
                          axis.title.y = element_text(size=15),
                          axis.text.y  = element_text(size=15),
                          strip.text = element_text(size=15),
                          legend.title = element_text(size = 15),
                          legend.text = element_text(size = 10))
    
    #Create tables with scenario and path
    c1_ar6_output %>%
      dplyr::select(Model, Scenario, Path) %>%
      unique() -> C1
    
    c2_ar6_output %>%
      dplyr::select(Model, Scenario, Path) %>%
      unique() -> C2
    
    c3_ar6_output %>%
      dplyr::select(Model, Scenario, Path) %>%
      unique() -> C3
    
    C_paths <- bind_rows(C1, C2, C3)
    
#-------------------------------------------------- FIGURE 1 -----------------------------------------------------------------------
#Figure 1 shows total CDR and CCS in AR6 by type (novel CDR, land CDR, fossil CCS) and function (offsetting, restoring)
#### 1. Prepare data ####
  #1. Bring in reanalysis sequestration
    #Reanalysis data
    gidden_land_reanalysis %>%
      filter(Variable == "AR6 Reanalysis|OSCARv3.2|Carbon Removal|Land|Direct", #All 407 scenarios have this variable
             Region == "World") %>%
      gather(year, value, -Model, -Scenario, -Region, -Variable, -Unit) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year >= 2010) %>%
      filter(year %in% years_to_keep) %>%
      left_join(C_paths, by = c("Scenario", "Model")) %>% #This has the 540 AR6 scenarios
      filter(!is.na(Path)) %>% #Here we filter out the scenarios that are not in the reanalysis data
      unite(Model_Scenario, c(Model, Scenario), sep = "_", remove = FALSE) %>%
      mutate(value_Gt_reanalysis = value / 10^3,
             Unit = "GtCO2/yr") -> reanalysis_CO2AFOLU_seq
    
    #Create filter
    scenarios_to_keep <- reanalysis_CO2AFOLU_seq %>%
      pull(Model_Scenario) %>%
      unique()
    
#2. Load AR6 carbon sequestration data    
    AR6_data %>%
      filter(Variable %in% c("Carbon Sequestration|Direct Air Capture", #novel
                             "Carbon Sequestration|Enhanced Weathering", #novel
                             "Carbon Sequestration|Other", #novel
                             "Carbon Sequestration|CCS|Biomass", #novel
                             "Carbon Sequestration|CCS|Industrial Processes", #fossil CCS
                             "Carbon Sequestration|CCS|Fossil")) %>% #fossil CCS
      gather(year, value, -Path, -Model, -Scenario, -Region, -Variable, -Unit) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year %in% years_to_keep) %>%
      group_by(Model, Scenario, Path, Region, Variable, Unit) %>%
      arrange(year) %>%
      mutate(value = zoo::na.approx(value, na.rm = FALSE)) %>% # interpolate between decades
      ungroup() %>%
      group_by(Model, Scenario, Region, Variable, Unit, Path) %>%
      mutate(value = ifelse(year == 2010 & is.na(value),
                            value[year == 2015],
                            value)) %>% # Set 2 NAS in 2010 to 2015 values
      ungroup() %>%
      unite(Model_Scenario, c(Model, Scenario), sep = "_", remove = FALSE) %>%
      mutate(value = value / 10^3,
             Unit = "GtCO2/yr") -> ar6_output_sequestration_breakdown
    
    #Rename and regroup variables into: novel CDR, land CDR, fossil CCS
    ar6_output_sequestration_breakdown %>%
      mutate(Variable = if_else(Variable  %in% c("Carbon Sequestration|Direct Air Capture",
                                                 "Carbon Sequestration|Enhanced Weathering",
                                                 "Carbon Sequestration|Other",
                                                 "Carbon Sequestration|CCS|Biomass"), "Novel CDR", "fossil CCS")) %>%
      group_by(Model_Scenario, Model, Scenario, Region, Variable, Unit, Path, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      #filter out scenarios that are not in reanalysis
      filter(Model_Scenario %in% scenarios_to_keep)-> ar6_output_sequestration_categories
    
#3. Merge both
    reanalysis_CO2AFOLU_seq %>% #This is variable "AR6 Reanalysis|OSCARv3.2|Carbon Removal|Land|Direct"
      select(-value) %>%
      rename(value = value_Gt_reanalysis) %>%
      mutate(Unit = "GtCO2/yr",
             Variable = "Land CDR (reanalysis)") %>%
      #Merge with AR6 data
      bind_rows(ar6_output_sequestration_categories)-> ar6_reanalysis_output_sequestration_categories
  
#### 2. Fossil CCS ####
    ar6_reanalysis_output_sequestration_categories %>%
      filter(Variable == "fossil CCS") -> ar6_reanalysis_output_sequestration_fossil_CCS #405 scenarios
    
    #Calculate median in GtCO2/yr
    total_CO2_emissions_median <- ar6_reanalysis_output_sequestration_fossil_CCS %>%
      group_by(Path, Variable, year) %>%
      summarise(median = median(value, na.rm = TRUE),
                min = min(value, na.rm = TRUE),
                max = max(value, na.rm = TRUE),
                P5 = quantile(value, 0.05, na.rm = TRUE),
                P15 = quantile(value, 0.15, na.rm = TRUE),
                P25 = quantile(value, 0.25, na.rm = TRUE),
                P75 = quantile(value, 0.75, na.rm = TRUE),
                P85 = quantile(value, 0.85, na.rm = TRUE),
                P95 = quantile(value, 0.95, na.rm = TRUE))

    p <- ggplot() +
      geom_ribbon(data = total_CO2_emissions_median %>% filter(Path == "C3"), aes(x = year, ymin = P5, ymax = P95, fill = Path), alpha = 0.2) +
      geom_ribbon(data = total_CO2_emissions_median %>% filter(Path == "C3"), aes(x = year, ymin = P15, ymax = P85, fill = Path), alpha = 0.3) +
      geom_ribbon(data = total_CO2_emissions_median %>% filter(Path == "C3"), aes(x = year, ymin = P25, ymax = P75, fill = Path), alpha = 0.4) +
      geom_line(data = total_CO2_emissions_median, 
                aes(x = year, y = median, color = Path), size = 1.2, linetype = "solid") +
      geom_hline(yintercept = 0, color = "black") +
      labs(title = "Fossil CCS",
           subtitle = "Variables: Carbon Sequestration|CCS|Industrial Processes, Carbon Sequestration|CCS|Fossil 
           (P5, P15, P25, median, P75, P85, P95)",
           x = "Year",
           y = "CO2 sequestration (GtCO2/yr)",
           fill = "Scenario and Model") +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      #facet_wrap (~ Path) +
      scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
      scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) +
      theme_minimal() +
      figure_theme+
      theme(panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            legend.position = "bottom") 
    #ggsave("figures/paper_figures/V3/1a.fossilCCS_timeline.png", p, width = 12, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/V3/1a.fossilCCS_timeline.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    p
    
    # Calculate cumulative sequestration using trapezoidal rule
    ar6_reanalysis_output_sequestration_categories %>%
      filter(Variable == "fossil CCS", year >= 2020, year <= 2100) %>%
      group_by(Model_Scenario, Model, Scenario, Region, Variable, Path, Unit) %>%
      arrange(year) %>%
      summarise(cumulative_capture = sum(diff(year) * (head(value, -1) + tail(value, -1)) / 2)) %>%
      ungroup() -> ar6_reanalysis_output_sequestration_fossil_CCS_cumulative
    
    medians <- ar6_reanalysis_output_sequestration_fossil_CCS_cumulative %>%
      group_by(Path) %>%
      summarise(median_value = median(cumulative_capture, na.rm = TRUE),
                P5 = quantile(cumulative_capture, 0.05, na.rm = TRUE),
                P95 = quantile(cumulative_capture, 0.95, na.rm = TRUE))
    
    # Create boxplot
    p_cumulative_boxplot <- ggplot(ar6_reanalysis_output_sequestration_fossil_CCS_cumulative, 
                                   aes(x = Path, y = cumulative_capture, fill = Path)) +
      geom_boxplot(outlier.shape = NA) +
      # Add median text labels
      geom_text(data = medians, 
                aes(x = Path, y = median_value, label = sprintf("%.1f", median_value)), 
                vjust = -0.5, size = 4, color = "white") +
      labs(
        title = "Cumulative 2020–2100 Fossil CCS Sequestration",
        subtitle = "Fossil Carbon Capture and Storage (GtCO2)",
        x = "Pathway",
        y = "Cumulative Sequestration (GtCO2)",
        fill = "Pathway"
      ) +
      scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 300)) +
      scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
    #ggsave("figures/paper_figures/V3/1a.fossilCCS_cumulative.png", p_cumulative_boxplot, width = 12, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/V3/1a.fossilCCS_cumulative.svg", p_cumulative_boxplot, width = 12, height = 12, units = "in", dpi = 600)
    p_cumulative_boxplot
    
    #Now create plot showing distirbution per path in 2100
    total_CO2_emissions_median_2100 <- total_CO2_emissions_median %>%
      filter(year == 2100)
    
    # Create the plot
    p<- ggplot(total_CO2_emissions_median_2100, aes(x = Path, color = Path, fill = Path)) +
      geom_errorbar(aes(ymin = P5, ymax = P95), width = 0.2, size = 1, alpha = 0.6) +  # P5 to P95 range
      geom_errorbar(aes(ymin = P15, ymax = P85), width = 0.3, size = 1.2, alpha = 0.7) +  # P15 to P85 range
      geom_errorbar(aes(ymin = P25, ymax = P75), width = 0.4, size = 1.5, alpha = 0.8) + # P25 to P75 range
      geom_point(aes(y = median), size = 3, shape = 21, color = "black", fill = "white", stroke = 1.2) +  # Median
      labs(
        title = "Fossil CCS Distribution in 2100",
        x = "Path",
        y = "GtCO2/yr"
      ) +
      scale_color_manual(values = colors) +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
    #ggsave("figures/paper_figures/V3/1a.fossilCCS_distribution.png", p, width = 12, height = 12, units = "in", dpi = 600)    
    #ggsave("figures/paper_figures/V3/1a.fossilCCS_distirbution.svg", p, width = 12, height = 12, units = "in", dpi = 600) 
    
#### 3. CDR (land and novel) ####
    # Filter out entire Model_Scenarios where any value is negative
    ar6_reanalysis_output_sequestration_categories %>%
      filter(Variable != "fossil CCS") %>%
      group_by(Model_Scenario) %>%
      # Exclude Model_Scenarios with any negative values, which means we go from 407 scenarios to 385
      #This is mostly POLES ENGAGE scenarios that have negative carbon sequestration from land
      #This means there is a reversal in the direct fluxes
      filter(!any(value < 0)) %>% 
      ungroup() %>%
      group_by(Model_Scenario, Model, Scenario, Region, Unit, year, Path) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      mutate(Variable = "total CDR") -> ar6_reanalysis_output_sequestration_total_CDR
    
    #Calculate median in GtCO2/yr
    total_CO2_seq_median <- ar6_reanalysis_output_sequestration_total_CDR %>%
      group_by(Path, Variable, year) %>%
      summarise(median = median(value, na.rm = TRUE),
                min = min(value, na.rm = TRUE),
                max = max(value, na.rm = TRUE),
                P5 = quantile(value, 0.05, na.rm = TRUE),
                P15 = quantile(value, 0.15, na.rm = TRUE),
                P25 = quantile(value, 0.25, na.rm = TRUE),
                P75 = quantile(value, 0.75, na.rm = TRUE),
                P85 = quantile(value, 0.85, na.rm = TRUE),
                P95 = quantile(value, 0.95, na.rm = TRUE))
    
    p <- ggplot() +
      geom_ribbon(data = total_CO2_seq_median %>% filter(Path == "C3"), 
                  aes(x = year, ymin = P5, ymax = P95, fill = Path), alpha = 0.2) +
      geom_ribbon(data = total_CO2_seq_median %>% filter(Path == "C3"), 
                  aes(x = year, ymin = P15, ymax = P85, fill = Path), alpha = 0.3) +
      geom_ribbon(data = total_CO2_seq_median %>% filter(Path == "C3"), 
                  aes(x = year, ymin = P25, ymax = P75, fill = Path), alpha = 0.4) +
      geom_line(data = total_CO2_seq_median, 
                aes(x = year, y = median, color = Path), size = 1.2, linetype = "solid") +
      geom_hline(yintercept = 0, color = "black") +
      labs(title = "Total CDR",
           subtitle = "Variables: Carbon Sequestration|Direct Air Capture, Enhanced Weathering, Other, CCS|Biomass and
           AR6 Reanalysis|OSCARv3.2|Carbon Removal|Land|Direct (P5, P15, P25, median, P75, P85, P95)",
           x = "Year",
           y = "CO2 sequestration (GtCO2/yr)",
           color = "Scenario and Model",
           fill = "Scenario and Model") +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
      scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) +
      theme_minimal() +
      figure_theme+
      theme(panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            legend.position = "bottom")  # Ensure the legend is removed
    #ggsave("figures/paper_figures/V3/1b.totalCDR_timeline.png", p, width = 12, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/V3/1b.totalCDR_timeline.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    p
    
    # Calculate cumulative sequestration using the trapezoidal rule
    ar6_reanalysis_output_sequestration_total_CDR %>%
      filter(year >= 2020, year <= 2100) %>%
      group_by(Model_Scenario, Model, Scenario, Region, Variable, Path, Unit) %>%
      arrange(year) %>%
      summarise(cumulative_capture = sum(diff(year) * (head(value, -1) + tail(value, -1)) / 2)) %>%
      ungroup() -> ar6_reanalysis_output_total_CDR_cumulative
    
    medians <- ar6_reanalysis_output_total_CDR_cumulative %>%
      group_by(Path) %>%
      summarise(median_value = median(cumulative_capture, na.rm = TRUE),
                P5 = quantile(cumulative_capture, 0.05, na.rm = TRUE),
                P95 = quantile(cumulative_capture, 0.95, na.rm = TRUE))
    
    # Create boxplot for total CDR cumulative sequestration
    p_total_CDR_boxplot <- ggplot(ar6_reanalysis_output_total_CDR_cumulative, 
                                  aes(x = Path, y = cumulative_capture, fill = Path)) +
      geom_boxplot(outlier.shape = NA) +
      # Add median text labels
      geom_text(data = medians, 
                aes(x = Path, y = median_value, label = sprintf("%.1f", median_value)), 
                vjust = -0.5, size = 4, color = "white") +
      labs(
        title = "Cumulative 2020–2100 Total CDR Sequestration",
        subtitle = "Total Carbon Dioxide Removal (GtCO2)",
        x = "Pathway",
        y = "Cumulative Sequestration (GtCO2)",
        fill = "Pathway"
      ) +
      scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 300)) +
      scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
    #ggsave("figures/paper_figures/V3/1b.totalCDR_cumulative.png", p_total_CDR_boxplot, width = 12, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/V3/1b.totalCDR_cumulative.svg", p_total_CDR_boxplot, width = 12, height = 12, units = "in", dpi = 600)
    p_total_CDR_boxplot
    
    #All carbon managmenet (fossil + novel + land) 
    ar6_reanalysis_output_sequestration_categories %>%
      group_by(Model_Scenario, Region, Unit, year, Path) %>%
      summarise(value = sum(value)) %>%
      ungroup() -> ar6_reanalysis_output_sequestration_categories_total
    
    #Get cumulative
    ar6_reanalysis_output_sequestration_categories_total %>%
      filter(year >= 2020, year <= 2100) %>%
      group_by(Model_Scenario, Path) %>%
      arrange(year, .by_group = TRUE) %>%
      summarise(
        cumulative_value = sum(diff(year) * (head(value, -1) + tail(value, -1)) / 2),
        .groups = "drop"
      ) -> sequestration_cumulative_total
    
    #Medians
    medians <- sequestration_cumulative_total %>%
      group_by(Path) %>%
      summarise(
        median_value = round(median(cumulative_value, na.rm = TRUE), 1),
        P5 = round(quantile(cumulative_value, 0.05, na.rm = TRUE), 1),
        P95 = round(quantile(cumulative_value, 0.95, na.rm = TRUE), 1)
      )

#### 4. Percentage split by type: fossil CCS, land CDR, novel CDR ####
    #First make sure we are keeping the same number of scenarios
    ar6_reanalysis_output_sequestration_categories %>%
      filter(Variable == "fossil CCS") %>%
      distinct(Model_Scenario)-> ar6_reanalysis_output_sequestration_scenarios
    
    #Get the mean across pathways (C1, C2, C3) and Variable
    total_CO2_emissions_mean <- ar6_reanalysis_output_sequestration_categories %>%
      semi_join(ar6_reanalysis_output_sequestration_scenarios, by = "Model_Scenario") %>%
      group_by(Path, Variable, year) %>%
      summarise(mean = mean(value, na.rm = TRUE),
                median = median(value, na.rm = TRUE))
    
    #Get percentages
    total_CO2_emissions_mean %>%
      group_by(Path, year) %>%
      mutate(total_year = sum(mean)) %>%
      ungroup() %>%
      mutate(percentage = (mean * 100) / total_year) %>%
      mutate(percentage_round = round(percentage))-> ar6_reanalysis_output_sequestration_categories_areas
    
    # Define the pathways to loop over
    pathways <- c("C1", "C2", "C3")
    
    # Loop through each pathway
    for (path in pathways) {
      # Filter data for the current pathway
      filtered_data <- ar6_reanalysis_output_sequestration_categories_areas %>% filter(Path == path)
      
      # Create the plot
      p <- ggplot(filtered_data, aes(x = year, y = percentage, fill = Variable)) +
        geom_area(alpha = 0.9, size = 0.5, colour = "black") +
        scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        scale_color_manual(values = colors_variable) +
        scale_fill_manual(values = colors_variable) +
        labs(
          title = paste("Carbon Capture Split by Categories Over Time (", path, " pathways)", sep = ""),
          x = "Year",
          y = "Percentage",
          fill = "Category"
        ) +
        figure_theme +
        theme(
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          legend.position = "right"
        )
      #ggsave(paste0("figures/paper_figures/V3/1c.percent_split_timeline_", path, ".png"),p, width = 12, height = 12, units = "in", dpi = 600)
      #ggsave(paste0("figures/paper_figures/V3/1c.percent_split_timeline_", path, ".svg"),p, width = 12, height = 12, units = "in", dpi = 600)
      
      # Create the bar plot for 2100
      bar_data <- filtered_data %>% filter(year == 2100)
      
      bar_plot <- ggplot(bar_data, aes(x = as.factor(year), y = percentage, fill = Variable)) +
        geom_col(position = "stack", color = "black", alpha = 0.9) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        scale_fill_manual(values = colors_variable) +
        labs(
          title = paste("Carbon Capture Split in 2100 (", path, " pathways)", sep = ""),
          x = "Year",
          y = "Percentage",
          fill = "Category"
        ) +
        figure_theme +
        theme(
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          legend.position = "right"
        )
      
      # Save the bar plot
      # ggsave(paste0("figures/paper_figures/V3/1c.percent_split_2100_", path, ".png"),
      #        bar_plot, width = 8, height = 6, units = "in", dpi = 600)
      # ggsave(paste0("figures/paper_figures/V3/1c.percent_split_2100_", path, ".svg"),
      #        bar_plot, width = 8, height = 6, units = "in", dpi = 600)
      
    }
    
    ### Now get cumulative amount 
    ar6_reanalysis_output_sequestration_cumulative <- ar6_reanalysis_output_sequestration_categories %>%
      filter(year >= 2020, year <= 2100) %>%
      group_by(Model_Scenario, Model, Scenario, Region, Variable, Path, Unit) %>%
      arrange(year) %>%
      summarise(cumulative_value = sum(diff(year) * (head(value, -1) + tail(value, -1)) / 2), .groups = "drop")
    
    medians <- ar6_reanalysis_output_sequestration_cumulative %>%
      group_by(Path, Variable) %>%
      summarise(median_value = median(cumulative_value, na.rm = TRUE),
                P5 = quantile(cumulative_value, 0.05, na.rm = TRUE),
                P95 = quantile(cumulative_value, 0.95, na.rm = TRUE), .groups = "drop")
    
    # Create boxplot for total CDR cumulative sequestration
    p_total_CDR_boxplot <- ggplot(ar6_reanalysis_output_sequestration_cumulative, 
                                  aes(x = Variable, y = cumulative_value, fill = Path)) +
      geom_boxplot(outlier.shape = NA) +
      # Add median text labels
      geom_text(data = medians, 
                aes(x = Variable, y = median_value, label = sprintf("%.1f", median_value)), 
                vjust = -0.5, size = 4, color = "white") +
      labs(
        title = "Cumulative 2020–2100 Total Sequestration by Category",
        subtitle = "Total Carbon Sequestration (GtCO2)",
        x = "Pathway",
        y = "Cumulative Sequestration (GtCO2)",
        fill = "Pathway"
      ) +
      facet_wrap(~Path)+
      scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 300)) +
      scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
    #ggsave("figures/paper_figures/V3/1c.cumulative_type_2020-2100.png", p_total_CDR_boxplot, width = 12, height = 12, units = "in", dpi = 600)
   # ggsave("figures/paper_figures/V3/1c.cumulative_type_2020-2100.svg", p_total_CDR_boxplot, width = 12, height = 12, units = "in", dpi = 600)
    p_total_CDR_boxplot
    
    
#### 5. Percentage split by function: fossil categories, offsetting, restoring ####
    #We need CO2 emissions from AR6 and reanalysis data from Gidden et al
  #### Energy CO2 emissions (AR6)
    # Step 1: Filter for the key variables
    AR6_data %>%
      filter(Variable %in% c("Emissions|CO2|Other", 
                             "Emissions|CO2|Waste", 
                             "Emissions|CO2|Energy and Industrial Processes", 
                             "Emissions|CO2|Energy", 
                             "Emissions|CO2|Industrial Processes")) %>%
      gather(year, value, -Path, -Model, -Scenario, -Region, -Variable, -Unit) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year %in% years_to_keep) %>%
      group_by(Model, Scenario, Path, Region, Variable, Unit) %>%
      arrange(year) %>%
      ungroup() %>%
      group_by(Model, Scenario, Region, Variable, Unit, Path) %>%
      mutate(value = ifelse(year == 2010 & is.na(value),
                            value[year == 2015],
                            value)) %>% # Set 2 NAS in 2010 to 2015 values
      ungroup() %>%
      unite(Model_Scenario, c(Model, Scenario), sep = "_", remove = FALSE) %>%
      mutate(value = value / 10^3,
             Unit = "GtCO2/yr") -> ar6_CO2_energy
    
    # Step 2: Conditionally add "Emissions|CO2|Energy" and "Emissions|CO2|Industrial Processes" for missing scenarios
    ar6_CO2_energy %>%
      group_by(Model, Scenario) %>%
      # Create a flag to check if the combined variable is present
      mutate(Has_Combined_Variable = any(Variable == "Emissions|CO2|Energy and Industrial Processes")) %>%
      ungroup() %>%
      filter(Has_Combined_Variable == FALSE) %>%
      spread(Variable, value) %>%
      mutate(Variable = "Emissions|CO2|Energy and Industrial Processes",
             value = `Emissions|CO2|Energy` + `Emissions|CO2|Industrial Processes`) %>%
      select(-`Emissions|CO2|Energy`, -`Emissions|CO2|Industrial Processes`, -Has_Combined_Variable)-> ar6_CO2_energy_missing 
    #^These scenarios have missing "Emissions|CO2|Energy and Industrial Processes" but they have
    #"Emissions|CO2|Energy" and "Emissions|CO2|Industrial Processes", which with AFOLU add up to 
    #total CO2. So we create the missing category by adding its two components
    
    #Step 3: join both tables
    ar6_CO2_energy %>%
      filter(Variable %in% c("Emissions|CO2|Energy and Industrial Processes", "Emissions|CO2|Other", "Emissions|CO2|Waste")) %>%
      bind_rows(ar6_CO2_energy_missing) %>%
      group_by(Model_Scenario, Model, Scenario, Region, Unit, Path, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() -> ar6_CO2_energy_final
    
    #Step 4: Now fix 2015
    ar6_CO2_energy_final %>% 
      mutate(value = value *1000,
             Unit = "Mt CO2/yr") %>%
      spread(year, value) %>%
      mutate(Variable = "Emissions|CO2|Energy and Industrial Processes")-> ar6_CO2_energy_final_wide
    
    ar6_CO2_energy_final_wide %>%
      filter(is.na(`2015`)) %>%
      left_join(history_ar6_fixed, by = c ("Variable", "Unit")) %>%
      mutate(`2010_diff` = (`2010` - `2010_history`) / `2010_history`,
             `2015_fixed` = (`2010_diff` * `2015_history`) + `2015_history`) %>%
      select(-`2015`, -`2010_diff`, -`2010_history`, -`2015_history`) %>%
      rename(`2015` = `2015_fixed`)-> ar6_CO2_energy_final_wide_misisng_2015
    
    #Step 5: Bind rows
    ar6_CO2_energy_final_wide %>%
      filter(!is.na(`2015`)) %>%
      bind_rows(ar6_CO2_energy_final_wide_misisng_2015) %>%
      gather(year, value, -Model_Scenario, -Path, -Model, -Scenario, -Region, -Variable, -Unit) %>%
      mutate(year = as.numeric(year)) %>%
      group_by(Model_Scenario, Model, Scenario, Path, Region, Variable, Unit) %>%
      arrange(year) %>%
      mutate(value = zoo::na.approx(value, na.rm = FALSE)) %>% #interpolate between decades
      ungroup() %>%
      mutate(value = value / 10^3,
             Unit = "GtCO2/yr") %>%
      filter(Model_Scenario %in% scenarios_to_keep)-> ar6_CO2_energy_final_complete_filtered
    #^This table has AR6 CO2 emissions from Energy and Industrial Processes for all relevant 407 scenarios

  #### Land from reanalysis data
    gidden_land_reanalysis %>%
      filter(Variable == "AR6 Reanalysis|OSCARv3.2|Emissions|CO2|AFOLU|Direct", #NOTE: we use direct emissions from land
             Region == "World") %>%
      gather(year, value, -Model, -Scenario, -Region, -Variable, -Unit) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year >= 2010) %>%
      left_join(C_paths, by = c("Scenario", "Model")) %>% #This has the 540 AR6 scenarios
      filter(!is.na(Path)) %>% #Here we filter out the scenarios that are not in the reanalysis data
      unite(Model_Scenario, c(Model, Scenario), sep = "_", remove = FALSE) %>%
      mutate(value_Gt = value / 10^3) -> reanalysis_land_emissions
    #^table has 407 Model_Scenario
    
  #### Join energy (AR6) emissions and land (reanalysis) emissions
    reanalysis_land_emissions %>% #These are land reanalysis direct emissions
      select(-value) %>%
      mutate(Unit = "GtCO2/yr") %>%
      rename(value_land = value_Gt) %>%
      filter(year %in% years_to_keep) %>% #Keep AR6 model years 
      left_join(ar6_CO2_energy_final_complete_filtered, by = c("Model_Scenario", "Model", "Scenario",
                                                               "Region", "Unit", "Path", "year" )) %>% #These are AR6 energy emissions
      rename(value_energy = value) %>%
      mutate(total_CO2 = value_land + value_energy) -> total_CO2_emissions
    
  #### Join CO2 emissions with CO2 sequestration (for energy AR6, for land direct fluxes from reanalysis)  
    #Note: this table has AR6 sequestration from novel technologies (DAC, EW, BECCS, Other), and for fossil CCS
    #and land CDR from #This is variable "AR6 Reanalysis|OSCARv3.2|Carbon Removal|Land|Direct"
    #Note 2: we want to exclude 2 scenarios here that are missing fossil CCS data
    ar6_reanalysis_output_sequestration_categories %>% #407 Model_Scenario
      semi_join(ar6_reanalysis_output_sequestration_scenarios, by = "Model_Scenario") %>% #405 Model_Scenario
      filter(year %in% years_to_keep) %>%
      filter(Variable %in% c("Novel CDR", "Land CDR (reanalysis)")) %>%
      group_by(Model_Scenario, Model, Scenario, Region, Unit, Path, year) %>%
      summarise(total_CDR = sum(value)) %>%
      ungroup() %>%
      left_join(total_CO2_emissions %>% 
                  filter(year %in% years_to_keep) %>%
                  select(-value_energy, -value_land, -Variable.y, -Variable.x), 
                by = c("Model_Scenario", "Model", "Scenario", "Region", "Unit", "Path" , "year")) %>%
      group_by(Model_Scenario) %>%
      mutate(
        net_zero_year = ifelse(
          any(total_CO2 < 0, na.rm = TRUE),   # Check if there is any negative value
          year[which(total_CO2 < 0)[1]],      # Select the first year where CO2_emissions is negative
          2101)) %>%
      ungroup()-> emissions_removals_nzyear #405 Model_Scenario
    
 #### A. Offsetting CDR ####
    emissions_removals_nzyear %>%
      #Here we calculate offsetting CDR as follows:
      #up until net-zero year total CDR equals offsetting emissions
      #from net-zero year onward, the offsetting CDR is for residual emissions only
      #meaning you are using part of CDR to get to net-zero, and the other part to get to net-negative
      #since we just care about the offsetting part, we need to calculate how much is used for each
      #So, we calculate residual emissions as net emissions + CDR
      #gross emissions = net emissions + CDR
      mutate(offset_CDR  = if_else(year < net_zero_year, total_CDR, (total_CO2 + total_CDR))) %>%
      #FIX: here there are ~3 instances where scenarios that did not report CDR and are net-negative CO2, even though there is no carbon sequestration. Since we are using means we decide to set these as 0 so that we keep the same number of scenarios
      mutate(offset_CDR = if_else(offset_CDR <0, 0, offset_CDR)) -> emissions_removals_nzyear_offsetting
      #from any sector. For now we filter those out.
      #filter(offset_CDR >=0)->  emissions_removals_nzyear_offsetting
    
    #mean in GtCO2/yr
    emissions_removals_nzyear_offsetting %>% #405 Model_Scenario
      group_by(year, Path, Unit) %>%
      summarise(mean= mean(offset_CDR, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Function = "offsetting CDR") -> summary_stats_off

  #### B. Restoring CDR ####
    #Here restoring CDR refers to the CDR used to get to net-negative. 
    #So we keep instances after the net-zero year, meaning emissions are net-negative, and we just show net CO2 emissions, 
    #which is equivalent to the CDR removal to be net negative
    emissions_removals_nzyear %>%
      mutate(restore_CDR = if_else(year >= net_zero_year, total_CO2 * -1, 0)) %>%
      #Here there are some instances where a scenario goes from net-negative to positive again
      #since we use mean we set those instances to 0 so that we keep same number of scenarios (405)
    mutate(restore_CDR = if_else(restore_CDR <0, 0, restore_CDR)) -> emissions_removals_nzyear_restoring
      #filter(restore_CDR >= 0)->  emissions_removals_nzyear_restoring
    
    #mean in GtCO2/yr
    emissions_removals_nzyear_restoring %>% #407 Model_Scenario
      semi_join(ar6_reanalysis_output_sequestration_scenarios, by = "Model_Scenario") %>% #405 scenarios
      group_by(year, Path, Unit) %>%
      summarise(mean= mean(restore_CDR, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Function = "restoring CDR") -> summary_stats_res
    
  #### C. Fossil CCS ####
    ar6_reanalysis_output_sequestration_categories %>% 
      filter(Variable == "fossil CCS") %>% #405 Model_Scenario
      group_by(year, Path, Unit) %>%
      summarise(mean = mean(value, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Function = "fossil CCS")-> summary_stats_fossil 
    #^Table has summary statistics for fossil CCS (includes industrial processes CCS and fossil CCS)
    
    #Now we want to split fossil CCS into its components
    #Step 1: Prepare data from AR6 to be manipulated
    AR6_data_complete %>%
      #Filter for carbon sequestration, excluding BECCS (which is included in novel CDR)
      filter(grepl("Carbon Sequestration", Variable)) %>%
      filter(grepl("CCS", Variable)) %>%
      filter(!grepl("Biomass", Variable))%>%
      gather(year, value, -Model, -Scenario, -Region, -Variable, -Unit) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year %in% years_to_keep) %>%
      group_by(Model, Scenario, Region, Variable, Unit) %>%
      arrange(year) %>%
      mutate(value = zoo::na.approx(value, na.rm = FALSE)) %>% #interpolate between decades
      ungroup() %>%
      group_by(Model, Scenario, Region, Variable, Unit) %>%
      mutate(value = ifelse(year == 2010 & is.na(value),
                            value[year == 2015],
                            value)) %>% # Set few NAS in 2010 to 2015 values
      ungroup() %>%
      unite(Model_Scenario, c(Model, Scenario), sep = "_", remove = FALSE) %>%
      filter(Model_Scenario %in% scenarios_to_keep) %>%
      semi_join(ar6_reanalysis_output_sequestration_scenarios, by = "Model_Scenario") %>% #405 Model_Scenario
      left_join(C_paths, by = c("Model", "Scenario"))-> AR6_CCS_sequestration
    
    AR6_CCS_sequestration %>%
      group_by(year, Variable, Path) %>%
      summarise(count = n(), .groups = "drop") -> variable_counts
    
    #Step 2: figure out how much sequestration happens per category
    AR6_CCS_sequestration %>%
      #filter(year == 2100) %>%
      #There are 44 instances where carbon sequestration is negative. We set it to 0
      mutate(value = if_else(value < 0, 0, value)) %>% 
      # Calculate the percentage splits within the CCS Fossil category
      mutate(
        category = case_when(
          Variable == "Carbon Sequestration|CCS|Industrial Processes" ~ "Industrial Processes CCS",
          Variable == "Carbon Sequestration|CCS|Fossil|Energy|Demand|Industry" ~ "Fossil CCS (Industry)",
          Variable == "Carbon Sequestration|CCS|Fossil|Energy|Supply|Electricity" ~ "Fossil CCS (Electricity)",
          Variable %in% c(
            "Carbon Sequestration|CCS|Fossil|Energy|Supply|Gases", #only in COFFEE, IMAGE, WITCH
            "Carbon Sequestration|CCS|Fossil|Energy|Supply|Hydrogen",
            "Carbon Sequestration|CCS|Fossil|Energy|Supply|Liquids",
            "Carbon Sequestration|CCS|Fossil|Energy|Supply|Other" #only in IMAGE and POLES ADVANCE
          ) ~ "Fossil CCS (Others)",
          TRUE ~ NA_character_)) %>%
      filter(!is.na(category)) %>% #we go form 407 scenarios to 355
      group_by(Model_Scenario, Model, Scenario, Region, Unit, year, Path, category) %>%
      summarise(category_total = sum(value)) %>%
      ungroup() -> AR6_CCS_sequestration_categories 
    
    #double check how many instances per category and path
    AR6_CCS_sequestration_categories %>%
      group_by(Path, year, category) %>%
      summarise(count = n()) -> total_paths
    
    #Step 3: get the mean per category for each Path
    AR6_CCS_sequestration_categories %>%
      group_by(year, Path, category, Unit) %>%
      summarise(mean_category= mean(category_total, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(year, Path, Unit) %>%
      mutate(mean_category = (mean_category/10^3)) %>% #CHECK
      mutate(total_mean = sum(mean_category)) %>%
      ungroup()-> summary_stats_fossil_categories
    
  #### D. JOIN ALL ####
    summary_stats_off %>%##mean in GtCO2/yr (offset)
      bind_rows(summary_stats_res, summary_stats_fossil) %>% ##mean in GtCO2/yr (restore CDR and fossil CCS)
      group_by(Path, year, Unit) %>%
      mutate(total_year = sum(mean)) %>%
      ungroup() %>%
      mutate(percentage = if_else(is.na((mean * 100) / total_year), 0, (mean * 100) / total_year))-> carbon_capture_function
    #^This table has the mean capture per function (offsetting, restoring, fossil CCS) across scenarios
    #Then we get the percentage between the 3 (adding to 100%)
    
    #Split fossil into categories
    carbon_capture_function %>%
      filter(Function == "fossil CCS") %>%
      #filter(year == 2100) %>%
      left_join(summary_stats_fossil_categories, by = c("year", "Path")) %>%
      #Calculat ehte percent that each catgeory has over 100% in that path
      mutate(percentage_in_category = (mean_category * 100) / total_mean) %>%
      #rescale these percentages based on original fossil CCS percentages so that we respect those
      mutate(percent_new = (percentage_in_category * percentage) / 100) %>%
      #Replace NaN with 0 (they were created by diving 0/0)
      mutate(percent_new = ifelse(is.nan(percent_new), 0, percent_new))  %>%
      select(year, Path, mean, category, percent_new) %>% #Here mean is for fossil CCS< and not for the catgeories
      rename(Function = category,
             percentage = percent_new)-> carbon_capture_function_fossil

    #Bring back fossil
    carbon_capture_function %>%
      filter(Function != "fossil CCS") %>%
      bind_rows(carbon_capture_function_fossil) %>%
      mutate(percent_round = round(percentage))%>%
      mutate(Function = factor(Function, levels = function_order))-> carbon_capture_function_final
    
    # Define the pathways to loop over
    pathways <- c("C1", "C2", "C3")
    
    # Loop through each pathway
    for (path in pathways) {
      # Filter data for the current pathway
      filtered_data <- carbon_capture_function_final %>% filter(Path == path)
      
      # Create the plot
      p <- ggplot(filtered_data, aes(x = year, y = percentage, fill = Function)) +
        geom_area(alpha =0.9, size = 0.5, colour = "black") +
        scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        scale_color_manual(values = colors_function) +
        scale_fill_manual(values = colors_function) +
        labs(
          title = paste("Carbon Capture Split by Function Over Time (", path, " pathways)", sep = ""),
          x = "Year",
          y = "Percentage",
          fill = "Category"
        ) +
        figure_theme +
        theme(
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          legend.position = "right"
        )
      
      # Save the plot as PNG and SVG
      # ggsave(paste0("figures/paper_figures/V3/1d.percent_function_timeline_", path, "V2.png"), 
      #        p, width = 12, height = 12, units = "in", dpi = 600)
      # ggsave(paste0("figures/paper_figures/V3/1d.percent_function_timeline_", path, ".svg"),
      #        p, width = 12, height = 12, units = "in", dpi = 600)
      
      # Create the bar plot for 2100
      bar_data <- filtered_data %>% filter(year == 2100)
      
      bar_plot <- ggplot(bar_data, aes(x = as.factor(year), y = percentage, fill = Function)) +
        geom_col(position = "stack", color = "black", alpha = 0.9) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        scale_fill_manual(values = colors_function) +
        labs(
          title = paste("Carbon Capture Split by Function in 2100 (", path, " pathways)", sep = ""),
          x = "Year",
          y = "Percentage",
          fill = "Category"
        ) +
        figure_theme +
        theme(
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          legend.position = "right"
        )
      
      # Save the bar plot
      # ggsave(paste0("figures/paper_figures/V3/1d.percent_function_2100_", path, "V2.png"), 
      #        bar_plot, width = 8, height = 6, units = "in", dpi = 600)
      # ggsave(paste0("figures/paper_figures/V3/1d.percent_function_2100_", path, ".svg"),
      #        bar_plot, width = 8, height = 6, units = "in", dpi = 600)
      # Print the plot (optional)
      print(p)
    }
    
    #### Now do cumulative offsetting vs restoring
    emissions_removals_nzyear_offsetting %>%
      rename(value = offset_CDR) %>%
      mutate(Function = "offsetting CDR") %>%
      bind_rows(emissions_removals_nzyear_restoring %>%
                  rename(value = restore_CDR) %>%
                  mutate(Function = "restoring CDR")) %>%
      filter(year >= 2020, year <= 2100) %>%
      group_by(Model_Scenario, Model, Scenario, Region, Function, Path, Unit) %>%
      arrange(year) %>%
      summarise(cumulative_value = sum(diff(year) * (head(value, -1) + tail(value, -1)) / 2), .groups = "drop") -> emissions_removals_nzyear_offsetting_restoring_cumulative
    
    medians <- emissions_removals_nzyear_offsetting_restoring_cumulative %>%
      group_by(Path, Function) %>%
      summarise(median_value = median(cumulative_value, na.rm = TRUE),
                P5 = quantile(cumulative_value, 0.05, na.rm = TRUE),
                P95 = quantile(cumulative_value, 0.95, na.rm = TRUE),.groups = "drop")
    
    # Create boxplot for total CDR cumulative sequestration
    p_total_CDR_boxplot <- ggplot(emissions_removals_nzyear_offsetting_restoring_cumulative, 
                                  aes(x = Function, y = cumulative_value, fill = Path)) +
      geom_boxplot(outlier.shape = NA) +
      # Add median text labels
      geom_text(data = medians, 
                aes(x = Function, y = median_value, label = sprintf("%.1f", median_value)), 
                vjust = -0.5, size = 4, color = "white") +
      labs(
        title = "Cumulative 2020–2100 Total Sequestration by Function",
        subtitle = "Total Carbon Sequestration (GtCO2)",
        x = "Pathway",
        y = "Cumulative Sequestration (GtCO2)",
        fill = "Pathway"
      ) +
      facet_wrap(~Path)+
      scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 300)) +
      scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
    # ggsave("figures/paper_figures/V3/1d.cumulative_Function_2020-2100.png", p_total_CDR_boxplot, width = 12, height = 12, units = "in", dpi = 600)
    # ggsave("figures/paper_figures/V3/1d.cumulative_Function_2020-2100.svg", p_total_CDR_boxplot, width = 12, height = 12, units = "in", dpi = 600)
    p_total_CDR_boxplot
    
    #### 6. Growth rate by sequestration type ####
    #We use the Compound Annual Growth Rate (CAGR) formula: 
    #(Ending Value / Beginning Value)^(1/Number of Years) - 1
    # Step 1: Compute median values for each Variable, Path, and year
    medians <- ar6_reanalysis_output_sequestration_categories %>%
      group_by(Variable, Path, year) %>%
      summarise(median_value = median(value, na.rm = TRUE), .groups = "drop") %>%
      ungroup()
    
    # Step 2. Interpolate missing years (linear interpolation)
    full_years <- expand.grid(
      Variable = unique(medians$Variable),
      Path = unique(medians$Path),
      year = seq(min(medians$year), max(medians$year), by = 1)  # Full yearly sequence
    )
    
    medians_interp <- full_years %>%
      left_join(medians, by = c("Variable", "Path", "year")) %>%
      group_by(Variable, Path) %>%
      arrange(year) %>%
      mutate(median_value = approx(year, median_value, year, rule = 2)$y) %>%  # Linear interpolation
      ungroup()
    
    # Step 3. Calculate Annual Growth Rate
    annual_growth <- medians_interp %>%
      group_by(Variable, Path) %>%
      arrange(year) %>%
      mutate(
        annual_growth = (median_value / lag(median_value)) - 1,  # Year-over-year growth
        annual_growth_percent = annual_growth * 100
      ) %>%
      ungroup() %>%
      filter(!is.infinite(annual_growth_percent) & !is.na(annual_growth_percent))  # Remove invalid values
    
    # Step 4. Define Decades
    medians_interp <- medians_interp %>%
      mutate(decade = floor(year / 10) * 10)  # Grouping years into decades
    
    # Step 5. Compute Decadal Growth Rate
    decadal_growth <- medians_interp %>%
      group_by(Variable, Path, decade) %>%
      summarise(
        start_value = first(median_value),  # First year of the decade
        end_value = last(median_value),    # Last year of the decade
        .groups = "drop"
      ) %>%
      mutate(
        decadal_growth = ((end_value / start_value)^(1/10)) - 1,  # True compound decadal growth
        decadal_growth_percent = decadal_growth * 100
      ) %>%
      filter(!is.infinite(decadal_growth_percent) & !is.na(decadal_growth_percent))  # Remove invalid values
    
    #Clean and export
    decadal_growth %>%
      select(Variable, Path, decade, decadal_growth_percent) %>%
      spread(decade, decadal_growth_percent) -> decadal_growth_export #Supplementary table
    
    #write.csv(decadal_growth_export, "output/decadal_growth_carbon_management_type.csv")
    
    #Now add plot for 2022 to 2050
    #We know 2022 engeneered CDR was ~2MtCO2 and fossil CCS ~4MtCO2
    # Define start year and values
    start_year <- 2022
    end_year <- 2050
    n_years <- end_year - start_year
    
    start_values <- data.frame(
      Variable = c("Novel CDR", "fossil CCS", "Land CDR (reanalysis)"),
      start_value = c(0.002, 0.004, 1.983)
    )
    
    # Filter for 2050 values and calculate CAGR
    cagr_results <- medians %>%
      filter(year == end_year) %>%
      left_join(start_values, by = "Variable") %>%
      mutate(
        CAGR = ((median_value / start_value)^(1 / n_years)) - 1,
        rate = CAGR * 100
      ) %>%
      select(Variable, Path, median_value, start_value, CAGR, rate)
    
    #Step 6 plot
    p1 <- ggplot(annual_growth, aes(x = year, y = annual_growth_percent, color = Path)) +
      geom_line(size = 1, alpha = 0.9) +  
      facet_wrap(~ Variable, scales = "free_y") +  
      labs(
        title = "Annual Growth Rate of Sequestration Technologies",
        x = "Year",
        y = "Annual Growth Rate (%)",
        color = "Path"
      ) +
      scale_color_manual(values = colors) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        panel.grid.major = element_line(color = "grey80"),
        strip.text = element_text(size = 10, face = "bold")
      )
    
    #Plot Decadal Growth Rate
    p2 <- ggplot(decadal_growth, aes(x = decade, y = decadal_growth_percent, color = Path)) +
      geom_line(size = 1.2, alpha = 0.9) +  
      geom_point(size = 2) +  
      facet_wrap(~ Variable, scales = "free_y") +  
      labs(
        title = "Decadal Growth Rate of Sequestration Technologies",
        x = "Decade",
        y = "Decadal Growth Rate (%)",
        color = "Path"
      ) +
      scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
      scale_color_manual(values = colors) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        panel.grid.major = element_line(color = "grey80"),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
      )

    
#-------------------------------------------------- FIGURE 2 -----------------------------------------------------------------------
    #Figure 2 shows total GHG/CO2 emissions and temperature outcomes from original runs and under-delivering all together 10-100%
    #Read in GWP from AR6 available here: https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_WGI_Chapter07_SM.pdf
    #Table 7.SM.7 | Greenhouse gas lifetimes, radiative efficiencies, global warming potentials (GWPs), global temperature potentials (GTPs) and cumulative global temperature potentials (CGTPs).
    AR6_GWP <- read_csv("/Users/mariacandelariabergero/Library/CloudStorage/GoogleDrive-cande.bergero@gmail.com/My Drive/Education/2021 (UCI)/Research/3_IIASA/data/GWP_ar6.csv", skip = 1)
    
    #### 1. Prepare data ####
    original_emissions <- read_csv("/Users/mariacandelariabergero/python/gegca-prototype/data/raw/interim/15849b8/V3/original/scenarios-infilled_all.csv") %>% mutate(underdelivery_percent = 0)
    
    # Define the base file path
    base_path <- "/Users/mariacandelariabergero/python/gegca-prototype/data/raw/interim/15849b8/V4/underdeliver_allCDR/"

    # Define percentages (10 to 100 in steps of 10)
    percentages <- seq(10, 100, by = 10)
    
    # Read in all files and combine them
    all_data <- map_dfr(percentages, function(pct) {
      file_path <- paste0(base_path, pct, "/scenarios-infilled_all_underdelivered_", pct, "percent.csv")
      # Read file and add a column to track the percentage
      read_csv(file_path) %>%
        mutate(underdelivery_percent = pct)
    })
    
    #Merge all data 2015-2100
    original_emissions %>% #Original emissions
      bind_rows(all_data) %>% #under-delivering 10-100% every 10%
      gather(year, value, -model, -scenario, -region, -variable, -unit, -underdelivery_percent) %>%
      mutate(year = substr(year, 1, 4)) %>% #clean year column
      mutate(year = as.numeric(year)) %>%
      left_join(C_paths, by = c("scenario" = "Scenario", "model" = "Model")) %>%
      unite(Model_Scenario, c(model, scenario), sep = "_", remove = FALSE) %>%
      left_join(AR6_GWP, by = "variable")%>% #Bring in multipliers GWP 100 years from IPCC AR6
      filter(!is.na(GWP_100))%>% #Filter out BC, CO, NH3, NOx, OC, SUlfur and VOC (not GHGs)
      select(-Notes) %>%
      mutate(value = ifelse(grepl("kt", unit), value / 1000, value)) %>% #Convert gases in kilo ton to Mega ton
      mutate(value_CO2eq = value * GWP_100) %>%
      group_by(Model_Scenario, model, scenario, region, year, Path, GHG, underdelivery_percent) %>%
      summarise(sum = sum(value_CO2eq)) %>%
      ungroup() %>%
      mutate(unit = "MtCO2eq")-> underdelivering_all_gases
    
    #Add all GHGs
    #First history 2010-2015
    history_ar6 %>%
      select(Variable, Unit, `2010`,`2011`,`2012`,`2013`,`2014`, `2015`) %>%
      #Prepare variabels for join
      separate(Variable, c("AR6", "emissions", "Variable", "harmonized", "other"), sep = "\\|") %>%
      mutate(joined_column = if_else(is.na(other), paste(emissions, Variable, sep = "|"), 
                                     paste(emissions, Variable, harmonized, sep = "|"))) %>%
      select(-AR6, -emissions, -Variable, -harmonized, -other) %>%
      rename(variable = joined_column) %>%
      gather(year, value, -variable, -Unit) %>%
      left_join(AR6_GWP, by = "variable") %>% #Bring in multipliers GWP 100 years from IPCC AR6
      filter(!is.na(GWP_100))%>% #Filter out BC, CO, NH3, NOx, OC, SUlfur and VOC (not GHGs)
      select(-Notes) %>%
      mutate(value = ifelse(grepl("kt", Unit), value / 1000, value)) %>% #Convert gases in kilo ton to Mega ton
      mutate(value_CO2eq = (value * GWP_100) / 1000, #convert to co2 equivalent, and ot Gt
             Unit = "GtCO2eq",
             year = as.numeric(year)) %>%
      filter(!is.na(value)) %>% #filter out NAs in Emissions|HFC|HFC245ca
      group_by(Unit, year, GHG) %>%
      summarise(sum = sum(value_CO2eq)) %>%
      ungroup() %>%
      group_by(Unit, year) %>%
      mutate(total_GHG = sum(sum)) %>%
      ungroup() %>%
      #Note: history_ar6 data does not have all non-CO2, so we ignore total for
      #2015 and use under delivering_all_gases for this year (which has all those non-CO2. The issue is with f-gases)
      mutate(total_GHG = if_else(year == 2015, 55.85171, total_GHG))-> history_ar6_fixed_GHG_2010_2015
    
    #Now modeled GHGs 2015-2100
    underdelivering_all_gases %>%
      group_by(Model_Scenario, model, scenario, region, year, Path, unit, underdelivery_percent) %>%
      summarise(totalGHG = sum(sum)) %>%
      ungroup() %>%
      mutate(totalGHG = totalGHG / 1000,
             unit = "GtCO2eq/yr")-> underdelivering_all_gases_total
    #Note, these values are very similar to IPCC https://www.ipcc.ch/report/ar6/wg3/figures/summary-for-policymakers/figure-spm-1/

    #Only CO2
    #bring in 2010 CO2
    history_ar6 %>%
      select(Variable, Unit, `2010`,`2011`,`2012`,`2013`,`2014`, `2015`) %>%
      filter(Variable == "AR6 climate diagnostics|Emissions|CO2|Unharmonized") %>%
      gather(year, value, -Variable, -Unit) %>%
      mutate(year = as.numeric(year)) %>%
      mutate(totalCO2 = value/ 1000,
             unit = "GtCO2/yr") %>%
      select(-Unit, -Variable)-> history_ar6_fixed_2010_2015
    
    #Modeled under-delivering capture
    underdelivering_all_gases %>%
      filter(GHG %in% c("CO2 (energy)",  "CO2 (land)")) %>%
      group_by(Model_Scenario, model, scenario, region, year, Path, unit, underdelivery_percent) %>%
      summarise(totalCO2 = sum(sum)) %>%
      ungroup()%>%
      mutate(totalCO2 = totalCO2 / 1000,
             unit = "GtCO2/yr")-> underdelivering_all_CO2
    
    #Get CAGR for CO2
    # Create a new column for decade start year (e.g., 2020, 2030, ...)
    underdelivering_all_CO2_CAGR <- underdelivering_all_CO2 %>%
      mutate(decade = floor(year / 10) * 10)
    
    # Step 1: For each decade, get the start and end year median totalCO2 across paths
    median_by_decade <- underdelivering_all_CO2_CAGR %>%
      filter(year %% 10 == 0 | year %% 10 == 9) %>%  # Keep only start (e.g., 2020) and end (e.g., 2029) years
      group_by(Path, underdelivery_percent, decade, year) %>%
      summarise(median_CO2 = median(totalCO2, na.rm = TRUE), .groups = "drop")
    
    # Step 2: Pivot wider to get start and end values in one row
    cagr_by_decade <- median_by_decade %>%
      mutate(period = ifelse(year == decade, "start", "end")) %>%
      select(-year) %>%
      tidyr::pivot_wider(names_from = period, values_from = median_CO2) %>%
      filter(!is.na(start) & !is.na(end)) %>%
      mutate(
        mitigation_rate = 1 - (end / start)^(1 / 9),
        mitigation_rate = round(mitigation_rate * 100, 2)
      )
    
    
    #### 2. Figure CO2s and GHGs ####
    # Calculate median total CO2 for each year and Path
    plot_data <- underdelivering_all_CO2 %>%
      group_by(year, Path, underdelivery_percent) %>%
      summarise(median_CO2 = median(totalCO2, na.rm = TRUE),
                P5 = quantile(totalCO2, 0.05, na.rm = TRUE),
                P95 = quantile(totalCO2, 0.95, na.rm = TRUE),.groups = "drop")
    
    unique_paths <- unique(plot_data$Path)
    
    for (path in unique_paths) {
      
      plot_subset <- plot_data %>% filter(Path == path)
      
    # Create line plot with facets
    p<-ggplot(plot_subset, aes(x = year, y = median_CO2, group = underdelivery_percent, color = underdelivery_percent)) +
      geom_line(size = 1) +  # Line plot for median
      geom_line(data = history_ar6_fixed_2010_2015, aes(x = year, y= totalCO2), inherit.aes = FALSE, color = "black")+ #Historical timeline 2010-2015
      geom_hline(yintercept = 0, color = "black") +
      facet_wrap(~ Path) +  # Facet by Path
      labs(title = paste("Median CO2 Emissions Across Scenarios", path, "(energy + land)"),
           subtitle = "Harmonized and infilled emissions",
           x = "Year",
           y = "Total CO2 GtCO2/yr (Median across scenarios)") +
      #scale_color_manual(values = colors) +
      #scale_fill_manual(values = colors) +
      scale_color_viridis(option = "magma") +
      scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
      scale_y_continuous(limits = c(-12, 43), breaks = seq(-12, 43, by = 4)) +
      theme_minimal() +
      figure_theme+
      theme(panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            legend.position = "bottom") 
    # ggsave(paste0("figures/paper_figures/V3/2a.totalCO2_timeline_", path, ".png"), p, width = 12, height = 12, units = "in", dpi = 600)
    # ggsave(paste0("figures/paper_figures/V3/2a.totalCO2_timeline_", path, ".svg"), p, width = 12, height = 12, units = "in", dpi = 600)
    
    }
    
    # Calculate median total GHG for each year and Path
    plot_data_GHG <- underdelivering_all_gases_total %>%
      group_by(year, Path, underdelivery_percent) %>%
      summarise(median_GHG = median(totalGHG, na.rm = TRUE),
                P5 = quantile(totalGHG, 0.05, na.rm = TRUE),
                P95 = quantile(totalGHG, 0.95, na.rm = TRUE),.groups = "drop")
    
    for (path in unique_paths) {
      
      plot_subset <- plot_data_GHG %>% filter(Path == path)
      
      # Create line plot with facets
      p<-ggplot(plot_subset, aes(x = year, y = median_GHG, group = underdelivery_percent, color = underdelivery_percent)) +
        geom_line(size = 1) +  # Line plot for median
        geom_line(data = history_ar6_fixed_GHG_2010_2015, aes(x = year, y = total_GHG), inherit.aes = FALSE, color = "black" ) +
        geom_hline(yintercept = 0, color = "black") +
        facet_wrap(~ Path) +  # Facet by Path
        labs(title = paste("Median GHG Emissions Across Scenarios", path, "(energy + land)"),
             subtitle = "Harmonized and infilled emissions",
             x = "Year",
             y = "Total GHG GtCO2eq/yr (Median across scenarios)") +
        scale_color_viridis(option = "turbo") +
        scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
        scale_y_continuous(limits = c(-6, 60), breaks = seq(-6, 60, by = 6)) +
        theme_minimal() +
        figure_theme+
        theme(panel.background = element_rect(fill = "white"),
              plot.background = element_rect(fill = "white"),
              legend.position = "bottom") 
      # ggsave(paste0("figures/paper_figures/V3/2a.totalGHG_timeline_", path, ".png"), p, width = 12, height = 12, units = "in", dpi = 600)
      # ggsave(paste0("figures/paper_figures/V3/2a.totalGHG_timeline_", path, ".svg"), p, width = 12, height = 12, units = "in", dpi = 600)
      
    }
    
    ### Here calculate percent reductions in emissions per decade
    # Add a decade column
    plot_data_GHG <- plot_data_GHG %>%
      mutate(decade = floor(year / 10) * 10)
    
    # Filter to start/end years of each decade you want
    target_decades <- c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)
    start_end_years <- plot_data_GHG %>%
      filter(year %in% c(target_decades, target_decades + 9))  # e.g., 2020 and 2029
    
    # Calculate CAGR for each decade -> Supplementary table
    mitigation_rates <- start_end_years %>%
      arrange(Path, underdelivery_percent, year) %>%
      group_by(Path, underdelivery_percent, decade) %>%
      summarise(
        GHG_start = median_GHG[year == decade],
        GHG_end = median_GHG[year == decade + 9],
        mitigation_rate = ifelse(!is.na(GHG_start) & !is.na(GHG_end) & GHG_end > 0,
                                 1 - (GHG_end / GHG_start)^(1 / 9),
                                 NA_real_),
        .groups = "drop"
      ) %>%
      mutate(mitigation_rate = round(mitigation_rate * 100, 2))
    
    #### 2. Cumulative emisisons ####
    underdelivering_all_gases_total %>%
      filter(year >= 2020, year <= 2100) %>%
      group_by(Model_Scenario, Path,underdelivery_percent, unit) %>%
      arrange(year) %>%
      #Apply trapezoidal rule to calculate cumulative CDR
      summarise(cumulative_GHG = sum(diff(year) * (head(totalGHG, -1) + tail(totalGHG, -1)) / 2)) %>%
      ungroup() %>%
      mutate(underdelivery_percent = as.factor(underdelivery_percent))  -> underdelivering_all_gases_total_cumulative
    
    #Calculate medians for plots
    median_values <- underdelivering_all_gases_total_cumulative %>%
      group_by(Path, underdelivery_percent) %>%
      summarise(
        median_cum_GHG = median(cumulative_GHG, na.rm = TRUE),
        P5 = quantile(cumulative_GHG, 0.05, na.rm = TRUE),
        P95 = quantile(cumulative_GHG, 0.95, na.rm = TRUE),.groups = "drop"
      ) %>%
      ungroup()
    
    #Plot
    cum_GHG <- ggplot(underdelivering_all_gases_total_cumulative, 
                      aes(x = underdelivery_percent, y = cumulative_GHG, fill = Path)) +
      geom_boxplot(position = position_dodge(width = 0.75),
                   outlier.shape = NA) + #Dont show circles wiht outliers
      scale_fill_manual(values = colors) +
      facet_wrap(.~Path)+
      scale_y_continuous(limits = c(0, 3500), breaks = seq(0, 3500, by = 500)) +
      labs(title = "Boxplot of Cumulative GHG emissions from AR6 and reanalysis (land)",
           subtitle = "One boxplot per underdelivery percent, colored by Path",
           x = "Underdelivery percent",
           y = "Cumulative GHG emissions (GtCO2e)") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            legend.position = "none",
            aspect.ratio = 1)
    #ggsave("figures/paper_figures/V3/2.boxplot_cumulative_GHG_2020_2100.png", width = 8, height = 8, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/V3/2.boxplot_cumulative_GHG_2020_2100.svg", width = 8, height = 8, units = "in", dpi = 600)
    
    #Do difference
    underdelivering_all_gases_total_cumulative %>%
      spread(underdelivery_percent, cumulative_GHG) %>%
      mutate(diff_50 = `50` - `0`,
             diff_100 = `100` - `0`)-> underdelivering_all_gases_total_cumulative_difference
    
    underdelivering_all_gases_total_cumulative_difference %>%
      group_by(Path) %>%
      summarise(
        median_diff_50 = median(diff_50, na.rm = TRUE),
        P5_diff_50 = quantile(diff_50, 0.05, na.rm = TRUE),
        P95_diff_50 = quantile(diff_50, 0.95, na.rm = TRUE),
        median_diff_100 = median(diff_100, na.rm = TRUE),
        P5_diff_100 = quantile(diff_100, 0.05, na.rm = TRUE),
        P95_diff_100 = quantile(diff_100, 0.95, na.rm = TRUE),.groups = "drop"
      ) ->summary_stat
    
    cum_GHG <- ggplot(underdelivering_all_gases_total_cumulative_difference) +
      geom_boxplot(aes(x = "50%", y = diff_50, fill = Path),
                   position = position_dodge(width = 0.75)) +
      geom_boxplot(aes(x = "100%", y = diff_100, fill = Path),
                   position = position_dodge(width = 0.75)) +
      scale_fill_manual(values = colors) +
      facet_wrap(.~Path)+
      scale_y_continuous(limits = c(0, 2600), breaks = seq(0, 2600, by = 500)) +
      labs(title = "DIFF Boxplot of Cumulative GHG emissions from AR6 and reanalysis (land)",
           subtitle = "One boxplot per underdelivery percent, colored by Path",
           x = "Underdelivery percent",
           y = "Cumulative GHG emissions (GtCO2e)") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            legend.position = "none",
            aspect.ratio = 1)
    #ggsave("figures/paper_figures/V3/2.boxplot_cumulative_GHG_2020_2100_diff.png", width = 6, height = 6, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/V3/2.boxplot_cumulative_GHG_2020_2100_diff.svg", width = 6, height = 6, units = "in", dpi = 600)
    
    
    #### 3. Temperature outcomes: underdelivering all (novel CDR, land CDR, fossil CCS)####
    #These tables have all 407 scenarios, 600 ensembles each, with the temperature in each year, as well as the summary statistics across the 600 ensembles for each scenario
    #Note: for each scenario we calculate the mean temperature increase compared to pre-industrial times
    #Then we estimate the median across the 600 ensemble members for each scenario
    #and then we plot the median for each C category across the 407 scenarios.
    #407 scenarios, 600 ensembles, 101 periods -> 24,664,200
    #We can also read in reduced version with 19 periods to speed up the process -> 4,639,800
    # Define file paths and corresponding run labels

    MAGICC_original<- read_csv( "output/V3/temperature/original/MAGICC_original_temperature_statistics.csv") %>%
      mutate(run = "0%") 
    
    MAGICC_underdeliver_all_10<- read_csv( "output/V4/temperature/underdelivered_allCDR/MAGICC_10percent_temperature_statistics_allCDR.csv") %>%
      mutate(run = "10%") 
    
    MAGICC_underdeliver_all_20<- read_csv( "output/V4/temperature/underdelivered_allCDR/MAGICC_20percent_temperature_statistics_allCDR.csv") %>%
      mutate(run = "20%") 
    
    MAGICC_underdeliver_all_30<- read_csv( "output/V4/temperature/underdelivered_allCDR/MAGICC_30percent_temperature_statistics_allCDR.csv") %>%
      mutate(run = "30%") 
    
    MAGICC_underdeliver_all_40<- read_csv( "output/V4/temperature/underdelivered_allCDR/MAGICC_40percent_temperature_statistics_allCDR.csv") %>%
      mutate(run = "40%") 
    
    MAGICC_underdeliver_all_50<- read_csv( "output/V4/temperature/underdelivered_allCDR/MAGICC_50percent_temperature_statistics_allCDR.csv") %>%
      mutate(run = "50%") 
    
    MAGICC_underdeliver_all_60<- read_csv( "output/V4/temperature/underdelivered_allCDR/MAGICC_60percent_temperature_statistics_allCDR.csv") %>%
      mutate(run = "60%") 
    
    MAGICC_underdeliver_all_70<- read_csv( "output/V4/temperature/underdelivered_allCDR/MAGICC_70percent_temperature_statistics_allCDR.csv") %>%
      mutate(run = "70%") 
    
    MAGICC_underdeliver_all_80<- read_csv( "output/V4/temperature/underdelivered_allCDR/MAGICC_80percent_temperature_statistics_allCDR.csv") %>%
      mutate(run = "80%") 
    
    MAGICC_underdeliver_all_90<- read_csv( "output/V4/temperature/underdelivered_allCDR/MAGICC_90percent_temperature_statistics_allCDR.csv") %>%
      mutate(run = "90%") 
    
    MAGICC_underdeliver_all_100<- read_csv( "output/V4/temperature/underdelivered_allCDR/MAGICC_100percent_temperature_statistics_allCDR.csv") %>%
      mutate(run = "100%") 
    
    all<- bind_rows(MAGICC_underdeliver_all_10, MAGICC_underdeliver_all_20, MAGICC_underdeliver_all_30)
    
    # List of tables
    MAGICC_tables <- list(
      "0%" = MAGICC_original,
      "10%" = MAGICC_underdeliver_all_10,
      "20%" = MAGICC_underdeliver_all_20,
      "30%" = MAGICC_underdeliver_all_30,
      "40%" = MAGICC_underdeliver_all_40,
      "50%" = MAGICC_underdeliver_all_50,
      "60%" = MAGICC_underdeliver_all_60,
      "70%" = MAGICC_underdeliver_all_70,
      "80%" = MAGICC_underdeliver_all_80,
      "90%" = MAGICC_underdeliver_all_90,
      "100%" = MAGICC_underdeliver_all_100
    )
    
    # Initialize an empty list to store results
    results_list <- list()
    
    # Loop through each table, calculate median for the path, and store results
    for (run_label in names(MAGICC_tables)) {
      df <- MAGICC_tables[[run_label]] %>%
        group_by(year, Path) %>%
        summarise(median_temperature = median(median, na.rm = TRUE), #get the median across C paths (median of median)
                  median_P95 = median(P95, na.rm = TRUE), #Here get the median for each run's 95th percentile
                  .groups = "drop") %>%
        mutate(run = run_label)  # Add run label
      
      results_list[[run_label]] <- df  # Store result
    }
    
    # Combine all results into one master table
    plot_data_temperature <- bind_rows(results_list) #101 years, 11 runs, 3 paths 3,333 (rows)
    #^This is the median of the 600 ensemble (with each ensemble being global mean temperature icnrease compared to pre-industrial),
    #and the median across scenarios
    for (path in unique_paths) {
      
      plot_subset <- plot_data_temperature %>% filter(Path == path)
      
      # Create line plot with facets
      p<-ggplot(plot_subset, aes(x = year, y = median_temperature, group = run, color = run)) +
        geom_line(size = 1) +  # Line plot for median
        geom_hline(yintercept = 0, color = "black") +
        facet_wrap(~ Path) +  # Facet by Path
        labs(title = paste("Global Mean Temperature increase vs pre-industrial in", path),
             subtitle = "Median esemble member & median across scenairos",
             x = "Year",
             y = "Mean Global Temperature Increase C") +
        #scale_color_viridis(option = "turbo") +
        scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 5)) +
        scale_y_continuous(limits = c(0, 2.2), breaks = seq(0, 2.2, by = 0.2)) +
        theme_minimal() +
        figure_theme+
        theme(panel.background = element_rect(fill = "white"),
              plot.background = element_rect(fill = "white"),
              legend.position = "bottom") 
      # ggsave(paste0("figures/paper_figures/V3/2b.temperature_timeline_", path, "V2.png"), p, width = 12, height = 12, units = "in", dpi = 600)
      # ggsave(paste0("figures/paper_figures/V3/2b.temperature_timeline_", path, "V2.svg"), p, width = 12, height = 12, units = "in", dpi = 600)
      
      p<-ggplot(plot_subset, aes(x = year, y = median_P95, group = run, color = run)) +
        geom_line(size = 1) +  # Line plot for median
        geom_hline(yintercept = 0, color = "black") +
        facet_wrap(~ Path) +  # Facet by Path
        labs(title = paste("Global Mean Temperature increase vs pre-industrial in", path),
             subtitle = "P95 esemble member & then median across scenairos",
             x = "Year",
             y = "Mean Global Temperature Increase C") +
        #scale_color_viridis(option = "turbo") +
        scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 5)) +
        scale_y_continuous(limits = c(0, 3.2), breaks = seq(0, 3.2, by = 0.2)) +
        theme_minimal() +
        figure_theme+
        theme(panel.background = element_rect(fill = "white"),
              plot.background = element_rect(fill = "white"),
              legend.position = "bottom") 
      # ggsave(paste0("figures/paper_figures/V3/2b.temperature_timeline_", path, "P95_V2.png"), p, width = 12, height = 12, units = "in", dpi = 600)
      # ggsave(paste0("figures/paper_figures/V3/2b.temperature_timeline_", path, "P95_V2.svg"), p, width = 12, height = 12, units = "in", dpi = 600)
      
    }
    
    #### 4. Now analyze 50% under-delivered all####
    #This table is for 2000-2014 temperatures (modeled scenarios begin in 2015)
    AR6_data_complete %>%
      filter(Variable == "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile" ) %>%
      gather(year, value, -Model, -Scenario, -Region, -Variable, -Unit) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year >= 2000 & year <= 2014) %>%
      select(-Model, -Scenario) %>%
      distinct()-> historical_temperatures
    
    filtered_median <- MAGICC_underdeliver_all_50 %>% #table only has 50%runs
      group_by(Path, year, run) %>%
      #Get median, min and max across the 407 scenarios using their median ensemble. 
      #So this is the distribution across scenarios for their median ensemble member
      summarize(median_temperature_median = median(median, na.rm = TRUE),
                min_temperature_median = min(median, na.rm = TRUE),
                max_temperature_median = max(median, na.rm = TRUE),
                mean_temperature_median = mean(median, na.rm = TRUE),
                n = (n()/600),
                #For 50% of the data
                P25 = quantile(median, 0.25, na.rm = TRUE),
                P75 = quantile(median, 0.75, na.rm = TRUE),
                #For 90% of data
                P5 = quantile(median, 0.05, na.rm = TRUE),
                P95 = quantile(median, 0.95, na.rm = TRUE),
                #For 70% of data
                P15 = quantile(median, 0.15, na.rm = TRUE),
                P85 = quantile(median, 0.85, na.rm = TRUE)) #Number of scenarios, ignoring ensembles 
    
    #This plot has median, min and max values across the 407 scenarios using their MEDIAN ensemble values
    #The min is the minimum across the medians of the scenarios, which have 600 ensembles.
    #Includes both original runs as well as adjusted runs
    p <- ggplot() +
      geom_ribbon(data = filtered_median %>% filter(run == "50%"), aes(x = year, ymin = P5, ymax = P95, fill = Path), alpha = 0.3) +
      geom_ribbon(data = filtered_median %>% filter(run == "50%"), aes(x = year, ymin = P15, ymax = P85, fill = Path), alpha = 0.4) +
      geom_ribbon(data = filtered_median %>% filter(run == "50%"), aes(x = year, ymin = P25, ymax = P75, fill = Path), alpha = 0.5) +
      
      geom_line(data = filtered_median %>% filter(run == "50%"), aes(x = year, y = median_temperature_median, color = Path, linetype = run), size = 1, linetype = "solid") +
      geom_text(data = filtered_median, aes(x = 2090, y = 4.8, label = paste("n =", n)), 
                size = 5, vjust = 0) +  # Adjust x, y for positioning
      geom_line(data = plot_data_temperature %>% filter(run == "100%"), aes(x = year, y = median_temperature, color = Path, linetype = run), size = 0.5, linetype = "dashed") +#This is the median value across each scenario median ensemble
      geom_line(data = plot_data_temperature %>% filter(run == "0%"), aes(x = year, y = median_temperature, color = Path, linetype = run), size = 0.5, linetype = "dashed") +#This is the median value across each scenario median ensemble
      geom_line(data = historical_temperatures, aes(x = year, y = value), color = "black", size = 1, linetype = "solid") +
      labs(title = "Temperature increase from AR6 (50% underdelivered all CDR & CCS)",
           subtitle = "For median value in ensemble member - P5, P15, P25, median, P75, P85, P95 across scenarios",
           x = "Year",
           y = "Temperature increase",
           color = "Scenario and Model",
           fill = "Scenario and Model") +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      facet_wrap (~ Path) +
      scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
      scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 0.5)) +
      theme_minimal() +
      figure_theme+
      theme(panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            legend.position = "none")  
    # ggsave("figures/paper_figures/V3/2b.temperature_spread_50percent_V2.png", p, width = 12, height = 12, units = "in", dpi = 600)
    # ggsave("figures/paper_figures/V3/2b.temperature_spread_50percent_V2.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    
    #### 5. Calculate probability exceeding threshold #### (Supplementary figures 8-10)
    #Here we calculate the probability of exceeding 1.5C and 2C for each scenario across its 600 ensmebles
    #### First we calculate for 1.5C
    # Define the exceedance threshold
    threshold <- 1.5  
    
    # Initialize an empty list to store exceedance probabilities
    exceedance_prob_list <- list()
    
    # Loop through each table to compute exceedance probabilities
    for (run_label in names(MAGICC_tables)) {
      
      df <- MAGICC_tables[[run_label]]  # Get current dataset
      
      # Compute exceedance probability
      exceedance_prob <- df %>%
        group_by(Model_Scenario, Path, year) %>%
        summarise(
          exceedance_prob = sum(temperature > threshold) / n(),  # Fraction exceeding threshold
          .groups = "drop"
        ) %>%
        mutate(run = run_label)  # Add run label
      
      exceedance_prob_list[[run_label]] <- exceedance_prob  # Store results
    }
    
    # Combine all results into one master table
    exceedance_prob_1.5 <- bind_rows(exceedance_prob_list) #407 scenarios, 101 periods, 11 runs (452,177 rows)
    
    exceedance_prob_1.5 %>%
      filter(year >= 2010) %>%
      mutate(run = factor(run, levels = underdelivered_order)) -> exceedance_prob_1.5_sort
    
    # Compute median exceedance probability across all Model_Scenarios for each year and Path
    median_exceedance <- exceedance_prob_1.5_sort %>%
      group_by(year, Path, run) %>%
      summarise(median_prob = median(exceedance_prob, na.rm = TRUE), .groups = "drop")
    
    # Get unique Paths
    paths <- unique(exceedance_prob_1.5_sort$Path)
    
    # Loop through each Path and create a single figure with faceted runs
    for (p in paths) {
      # Subset data for the specific Path
      data_subset <- exceedance_prob_1.5_sort %>%
        filter(Path == p)
        
      # Count the number of unique Model_Scenarios represented
      num_scenarios <- data_subset %>% pull(Model_Scenario) %>% unique() %>% length()
      
      # Plot
      p_plot <- ggplot(data_subset, aes(x = year, y = exceedance_prob, group = Model_Scenario, color = Model_Scenario)) +
        geom_line(size = 0.5, alpha = 0.5) +  # Reduce opacity for visibility
        geom_line(data = median_exceedance %>% filter(Path == p), aes(x = year, y = median_prob),
                  inherit.aes = FALSE, color = "black", size = 0.7) +  # Add median line
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
        facet_wrap(~ run, ncol = 4) +  # Facet by run percentage
        labs(
          title = paste("Probability of Exceeding 1.5°C Over Time for Path:", p),
          subtitle = paste("Number of Model Scenarios Represented:", num_scenarios),
          x = "Year",
          y = "Exceedance Probability"
        ) +
        scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, by = 0.2)) +
        scale_color_viridis_d(option = "magma") +
        theme_minimal() +
        theme(legend.position = "none")
      
      # ggsave(filename = paste0("figures/paper_figures/V3/2c.Exceedance_1.5C_Path_", p, "_V2.png"),plot = p_plot,width = 8, height = 6, dpi = 300)
      # ggsave(filename = paste0("figures/paper_figures/V3/2c.Exceedance_1.5C_Path_", p, "_V2.svg"),plot = p_plot,width = 8, height = 6, dpi = 300)
      print(paste("Saved plot for Path:", p))
    }
    
    
    #### now try with 2.0C
    threshold <- 2.0
    
    # Initialize an empty list to store exceedance probabilities
    exceedance_prob_list_2.0 <- list()
    
    # Loop through each table to compute exceedance probabilities
    for (run_label in names(MAGICC_tables)) {
      
      df <- MAGICC_tables[[run_label]]  # Get current dataset
      
      # Compute exceedance probability
      exceedance_prob_2.0 <- df %>%
        group_by(Model_Scenario, Path, year) %>%
        summarise(
          exceedance_prob = sum(temperature > threshold) / n(),  # Fraction exceeding threshold
          .groups = "drop"
        ) %>%
        mutate(run = run_label)  # Add run label
      
      exceedance_prob_list_2.0[[run_label]] <- exceedance_prob_2.0  # Store results
    }
    
    # Combine all results into one master table
    exceedance_prob_2.0 <- bind_rows(exceedance_prob_list_2.0) #407 scenarios, 101 periods, 11 runs (452,177 rows)
    
    exceedance_prob_2.0 %>%
      filter(year >= 2010) %>%
      mutate(run = factor(run, levels = underdelivered_order)) -> exceedance_prob_2.0_sort
    
    # Compute median exceedance probability across all Model_Scenarios for each year and Path
    median_exceedance <- exceedance_prob_2.0_sort %>%
      group_by(year, Path, run) %>%
      summarise(median_prob = median(exceedance_prob, na.rm = TRUE), .groups = "drop")
    
    # Get unique Paths
    paths <- unique(exceedance_prob_2.0_sort$Path)
    
    
    # Loop through each Path and create a single figure with faceted runs
    for (p in paths) {
      # Subset data for the specific Path
      data_subset <- exceedance_prob_2.0_sort %>%
        filter(Path == p)
      
      # Count the number of unique Model_Scenarios represented
      num_scenarios <- data_subset %>% pull(Model_Scenario) %>% unique() %>% length()
      
      # Plot
      p_plot <- ggplot(data_subset, aes(x = year, y = exceedance_prob, group = Model_Scenario, color = Model_Scenario)) +
        geom_line(size = 0.5, alpha = 0.5) +  # Reduce opacity for visibility
        geom_line(data = median_exceedance %>% filter(Path == p), aes(x = year, y = median_prob),
                  inherit.aes = FALSE, color = "black", size = 0.7) +  # Add median line
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
        facet_wrap(~ run, ncol = 4) +  # Facet by run percentage
        labs(
          title = paste("Probability of Exceeding 2.0°C Over Time for Path:", p),
          subtitle = paste("Number of Model Scenarios Represented:", num_scenarios),
          x = "Year",
          y = "Exceedance Probability"
        ) +
        scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, by = 0.2)) +
        scale_color_viridis_d(option = "magma") +
        theme_minimal() +
        theme(legend.position = "none")
      
      # ggsave(filename = paste0("figures/paper_figures/V3/2c.Exceedance_2.0C_Path_", p, "_V2.png"),plot = p_plot,width = 8, height = 6, dpi = 300)
      # ggsave(filename = paste0("figures/paper_figures/V3/2c.Exceedance_2.0C_Path_", p, "_V2.svg"),plot = p_plot,width = 8, height = 6, dpi = 300)
      print(paste("Saved plot for Path:", p))
    }
    
    
    ### Now prepare actual insert figure
    exceedance_prob_2.0_sort %>%
      rename(exceedance_prob_2p0 = exceedance_prob) %>%
      left_join(exceedance_prob_1.5_sort, by = c("Model_Scenario", "Path", "run", "year")) %>%
      rename(exceedance_prob_1p5 = exceedance_prob) %>%
      group_by(Path, year, run) %>%
      summarise(median_prob_1p5 = median(exceedance_prob_1p5),
                median_prob_2p0 = median(exceedance_prob_2p0), .groups = "drop") -> exceedance_probability_median_complete
    
    p <- ggplot(exceedance_probability_median_complete %>% filter(run == "50%"), 
                aes(x = year, y = median_prob_2p0, color = Path, group = interaction(Path, run))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      # 2.0C exceedance probability lines (colored by Path)
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "100%"), 
                aes(x = year, y = median_prob_2p0, color = Path, group = interaction(Path, run)), 
                linetype = "dashed")  +  
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "0%"), 
                aes(x = year, y = median_prob_2p0, color = Path, group = interaction(Path, run)), 
                linetype = "dotted") +
      # 1.5C exceedance probability lines (set custom colors)
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "100%"), 
                aes(x = year, y = median_prob_1p5, group = interaction(Path, run)), 
                color = "black", linetype = "dashed") +  
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "0%"), 
                aes(x = year, y = median_prob_1p5, group = interaction(Path, run)), 
                color = "black", linetype = "dotted") +
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "50%"), 
                aes(x = year, y = median_prob_1p5, group = interaction(Path, run)), 
                color = "black", linetype = "solid") +
      geom_line(aes(y = 0.5), color = "black", size = 0.5, linetype = "dashed") +  
      labs(
        title = "Probability of exceeding 1.5C and 2C (median across scenarios)",
        subtitle = "Showing 50% underdelivering (solid), 100% underdelivering (dashed), 0% underdelivered (dotted),
        color for 2.0C and black for 1.5C",
        x = "Year",
        y = "Probability of exceeding threshold"
      ) +
      facet_wrap(~Path) +
      scale_color_manual(values = colors) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
      figure_theme 
    #ggsave("figures/paper_figures/V3/2c.exceedance_probability_2C_1p5C_V2.png", p, width = 12, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/V3/2c.exceedance_probability_2C_1p5C_V2.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    
    
    #### 6. Emissions relative to original pathways ####
    # Calculate median total CO2 for each year and Path
    underdelivering_all_CO2 %>%
      group_by(year, Path, underdelivery_percent) %>%
      summarise(median_CO2 = median(totalCO2, na.rm = TRUE), .groups = "drop") -> plot_data
    
    plot_data %>%
      filter(Path == "C1",
             underdelivery_percent == "0") %>%
      select(-Path, -underdelivery_percent) %>%
      rename(median_CO2_C1_original = median_CO2)-> plot_data_C1_original 
    
    plot_data %>%
      filter(Path == "C2",
             underdelivery_percent == "0") %>%
      select(-Path, -underdelivery_percent) %>%
      rename(median_CO2_C2_original = median_CO2)-> plot_data_C2_original 
    
    plot_data %>%
      filter(Path == "C3",
             underdelivery_percent == "0") %>%
      select(-Path, -underdelivery_percent) %>%
      rename(median_CO2_C3_original = median_CO2)-> plot_data_C3_original 
    #^This has the CO2 emissions in median 2C scenarios with )% under-delivering
    
    #Join all
    plot_data %>%
      left_join(plot_data_C1_original, by = "year") %>%
      left_join(plot_data_C2_original, by = "year") %>%
      left_join(plot_data_C3_original, by = "year") %>%
      mutate(difference = if_else(Path == "C1", median_CO2 - median_CO2_C1_original,
                                  if_else(Path == "C2", median_CO2 - median_CO2_C2_original, median_CO2 - median_CO2_C3_original)
                                  )) -> underdelivering_all_CO2_differences
    #Plot all
    ggplot(underdelivering_all_CO2_differences, aes(x = year, y = difference, color = Path, group = interaction(Path, underdelivery_percent))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      geom_line(aes(y = 0), color = "black", size = 1, linetype = "solid") +  # Baseline as y=1
      labs(
        title = "Difference in emissions relative to 0% underdelivering",
        subtitle = "Each line is an underdelivering % relative to its category (c1, c2, c3)",
        x = "Year",
        y = "Difference Emissions in GtCO2 (underdelivered run - original run)"
      ) +
      facet_wrap( ~Path) +
      scale_color_manual(values = colors) +
      # scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(legend.position = "bottom")

    #Plot only 50% and 100%
    p<-ggplot(underdelivering_all_CO2_differences %>% filter(underdelivery_percent == "50"), aes(x = year, y = difference, color = Path, group = interaction(Path, underdelivery_percent))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      geom_line(data = underdelivering_all_CO2_differences %>% filter(underdelivery_percent == "100"), aes(x = year, y = difference, color = Path, group = interaction(Path, underdelivery_percent)), linetype = "dashed") +  # Lines for relative emissions in each scenario
      geom_line(aes(y = 0), color = "black", size = 0.5, linetype = "solid") +  # Baseline as y=1
      labs(
        title = "Difference in emissions relative to 0% underdelivering",
        subtitle = "Each line is an underdelivering 50% (and 100%) relative to its category (c1, c2, c3)",
        x = "Year",
        y = "Difference Emissions in GtCO2 (underdelivered run - original run)"
      ) +
      scale_color_manual(values = colors) +
      scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) +
      # scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(legend.position = "bottom")
    #ggsave("figures/paper_figures/V3/2d.CO2emissions_difference.png", p, width = 12, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/V3/2d.CO2emissions_difference.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    p
    
    # Calculate median total GHG for each year and Path
    underdelivering_all_gases_total %>%
      group_by(year, Path, underdelivery_percent) %>%
      summarise(median_GHG = median(totalGHG, na.rm = TRUE), .groups = "drop") -> plot_data
    
    plot_data %>%
      filter(Path == "C1",
             underdelivery_percent == "0") %>%
      select(-Path, -underdelivery_percent) %>%
      rename(median_GHG_C1_original = median_GHG)-> plot_data_C1_original 
    
    plot_data %>%
      filter(Path == "C2",
             underdelivery_percent == "0") %>%
      select(-Path, -underdelivery_percent) %>%
      rename(median_GHG_C2_original = median_GHG)-> plot_data_C2_original 
    
    plot_data %>%
      filter(Path == "C3",
             underdelivery_percent == "0") %>%
      select(-Path, -underdelivery_percent) %>%
      rename(median_GHG_C3_original = median_GHG)-> plot_data_C3_original 
    #^This has the CO2 emissions in median 2C scenarios with )% under-delivering
    
    #Join all
    plot_data %>%
      left_join(plot_data_C1_original, by = "year") %>%
      left_join(plot_data_C2_original, by = "year") %>%
      left_join(plot_data_C3_original, by = "year") %>%
      mutate(difference = if_else(Path == "C1", median_GHG - median_GHG_C1_original,
                                  if_else(Path == "C2", median_GHG - median_GHG_C2_original, 
                                          median_GHG - median_GHG_C3_original)
      )) -> underdelivering_all_GHG_differences
    
    #Plot all
    ggplot(underdelivering_all_GHG_differences, aes(x = year, y = difference, color = Path, group = interaction(Path, underdelivery_percent))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      geom_line(aes(y = 0), color = "black", size = 1, linetype = "solid") +  # Baseline as y=1
      labs(
        title = "Difference in GHG emissions relative to 0% underdelivering",
        subtitle = "Each line is an underdelivering % relative to its category (c1, c2, c3)",
        x = "Year",
        y = "Difference Emissions in GtCO2eq (underdelivered run - original run)"
      ) +
      facet_wrap( ~Path) +
      scale_color_manual(values = colors) +
      # scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    #Plot only 50% and 100%
    p<-ggplot(underdelivering_all_GHG_differences %>% filter(underdelivery_percent == "50"), aes(x = year, y = difference, color = Path, group = interaction(Path, underdelivery_percent))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      geom_line(data = underdelivering_all_GHG_differences %>% filter(underdelivery_percent == "100"), aes(x = year, y = difference, color = Path, group = interaction(Path, underdelivery_percent)), linetype = "dashed") +  # Lines for relative emissions in each scenario
      geom_line(aes(y = 0), color = "black", size = 0.5, linetype = "solid") +  # Baseline as y=1
      labs(
        title = "Difference in GHG emissions relative to 0% underdelivering",
        subtitle = "Each line is an underdelivering 50% (and 100% dashed) relative to its category (c1, c2, c3)",
        x = "Year",
        y = "Difference Emissions in GtCO2eq (underdelivered run - original run)"
      ) +
      scale_color_manual(values = colors) +
      scale_y_continuous(limits = c(0, 21), breaks = seq(0, 21, by = 5)) +
      # scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(legend.position = "bottom")
    #ggsave("figures/paper_figures/V3/2d.GHGemissions_difference.png", p, width = 12, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/V3/2d.GHGemissions_difference.svg", p, width = 12, height = 12, units = "in", dpi = 600)
  p
    
  #### 7. Temperature relative to 2C original ####
    # Initialize an empty list to store results
    filtered_median_list <- list()
    
    # Loop through each table and compute summary statistics
    for (run_label in names(MAGICC_tables)) {
      
      df <- MAGICC_tables[[run_label]]  # Get current dataset
      
      summary_stats <- df %>%
        group_by(Path, year) %>%
        summarize(
          median_temperature_median = median(median, na.rm = TRUE),
          min_temperature_median = min(median, na.rm = TRUE),
          max_temperature_median = max(median, na.rm = TRUE),
          mean_temperature_median = mean(median, na.rm = TRUE),
          n = (n() / 600),
          P25 = quantile(median, 0.25, na.rm = TRUE),
          P75 = quantile(median, 0.75, na.rm = TRUE),
          P5 = quantile(median, 0.05, na.rm = TRUE),
          P95 = quantile(median, 0.95, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(run = run_label)  # Add run label
      
      filtered_median_list[[run_label]] <- summary_stats  # Store result
    }
    
    # Combine all runs into one master table
    filtered_median <- bind_rows(filtered_median_list) #3 paths, 101 years, 11 runs
    
    # Compute the reference temperature for "0%" run (original scenario)
    filtered_median_original <- filtered_median %>%
      filter(run == "0%") %>%
      select(Path, year, median_temperature_median) %>%
      spread(Path, median_temperature_median)
    
    # Join the original values with all runs and compute the temperature difference
    underdelivering_all_temperature_differences <- filtered_median %>%
      select(Path, year, run, median_temperature_median) %>%
      left_join(filtered_median_original, by = "year") %>%
      mutate(difference = if_else(Path == "C1", median_temperature_median - C1,
                                  if_else(Path == "C2", median_temperature_median - C2,
                                          median_temperature_median - C3)))
    
    #Plot only 50% and 100%
    p<-ggplot(underdelivering_all_temperature_differences %>% filter(run == "50%"), aes(x = year, y = difference, color = Path, group = interaction(Path, run))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      geom_line(data = underdelivering_all_temperature_differences %>% filter(run == "100%"), aes(x = year, y = difference, color = Path, group = interaction(Path, run)), linetype = "dashed") +  # Lines for relative emissions in each scenario
      geom_line(aes(y = 0), color = "black", size = 0.5, linetype = "solid") +  # Baseline as y=1
      labs(
        title = "Difference in GMT increase relative to 0% underdelivering",
        subtitle = "Each line is an underdelivering 50% (and 100%) relative to its category (c1, c2, c3)",
        x = "Year",
        y = "Difference GMT increase in C (underdelivered run - original run)"
      ) +
      scale_color_manual(values = colors) +
      scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, by = 0.1)) +
      scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
      # scale_fill_manual(values = colors) +
      theme_minimal() +
      figure_theme 
    #ggsave("figures/paper_figures/V3/2e.temperature_difference.png", p, width = 12, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/V3/2e.temperature_difference.svg", p, width = 12, height = 12, units = "in", dpi = 600)
      p

#-------------------------------------------------- FIGURE 3 -----------------------------------------------------------------------
    #Permutation analysis: underdelivering land CDR vs underdelivering novel CDR
    ##Plot shows how much temperature increases in one category vs the other, as well as 
    #How much probability of staying below thresholds changes. 
    #### 1. Load relevant libraries ####
    library(dplyr)
    library(data.table)
    library(ggplot2)
    library(plotly)
    
    # Define file directory
    base_path <- "output/V4/temperature/underdelivered_permutations"
    
    CCS_order <- c("below or equal median CCS", "above median CCS")
    
    #### 2. Get the data and calculate summary statistics ####
    # Define percent values (assuming 0 to 100 in steps of 20)
    percent_novelCDR_values <- seq(0, 100, by = 20)
    percent_landCDR_values <- seq(0, 100, by = 20)
    
    # Create an empty list to store results
    summary_list <- list()
    
    # Loop over novelCDR and landCDR percentages
    for (percent_novelCDR in percent_novelCDR_values) {
      for (percent_landCDR in percent_landCDR_values) {
        
        # Construct file path
        file_name <- paste0("MAGICC_", percent_novelCDR, "percent_novelCDR_", percent_landCDR, "percent_landCDR_temperature_statistics.csv")
        file_path <- file.path(base_path, file_name)
        # Check if file exists
        if (file.exists(file_path)) {
          
          # Load the CSV
          temp_data <- fread(file_path, select = c("Path", "Model_Scenario", "ensemble_member", "year", "temperature")) 
          #distinct(Model_Scenario, year, Path, ensemble_member, temperature) # Keep only one instance for the median value, as it is the same across all rows for a given model and period. We are dropping column temperature and ensemble here.
          
          # Compute peak temperature per ensemble member
          #Follow approach from openscm 
          #https://github.com/openscm/scmdata/blob/112f969351c5e5e872956aecfeda5b5e332637b3/src/scmdata/processing.py#L352
          ensemble_peaks <- temp_data %>%
            group_by(Model_Scenario, Path, ensemble_member) %>%
            summarise(
              peak_temp = max(temperature, na.rm = TRUE),   # Max temperature for each ensemble
              peak_year = year[which.max(temperature)],     # Year when max temp occurs
              temp_2100 = temperature[year == 2100],        # Temperature in year 2100
              .groups = "drop"
            )
          
          # Compute median peak temperature and median peak year across ensembles
          temp_summary <- ensemble_peaks %>%
            group_by(Model_Scenario, Path) %>%
            summarise(
              median_peak_temp = median(peak_temp, na.rm = TRUE),  # Median of ensemble peaks (i.e. the median of the peak over all time)
              median_peak_year = median(peak_year, na.rm = TRUE),  # Median peak year across ensembles
              median_temp_2100 = median(temp_2100, na.rm = TRUE),  # Median temperature in 2100
              .groups = "drop"
            ) %>%
            mutate(percent_novelCDR = percent_novelCDR,
                   percent_landCDR = percent_landCDR)
          
          # Store in list
          summary_list[[paste0(percent_novelCDR, "_", percent_landCDR)]] <- temp_summary
          
          # Clean up memory
          rm(temp_data)
          gc()
          
        } else {
          warning(paste("File not found:", file_path))
        }
      }
    }
    
    # Combine all summaries into one dataframe
    final_data_permutations <- bind_rows(summary_list) 
    #^This table has our 407 scenarios, each has peak (median of the peak) and 2010 temperature (median across 600 ensembles)
    
    # Create a list of unique Model_Scenario values to keep
    scenarios_to_keep <- unique(final_data_permutations$Model_Scenario)
    
    #### 3. Bring in CCS information ####
    AR6_data %>%
      filter(Variable %in% c("Carbon Sequestration|CCS|Industrial Processes", #fossil CCS
                             "Carbon Sequestration|CCS|Fossil")) %>% #fossil CCS
      gather(year, value, -Path, -Model, -Scenario, -Region, -Variable, -Unit) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year %in% years_to_keep) %>%
      group_by(Model, Scenario, Path, Region, Variable, Unit) %>%
      arrange(year) %>%
      mutate(value = zoo::na.approx(value, na.rm = FALSE)) %>% # interpolate between decades
      ungroup() %>%
      group_by(Model, Scenario, Region, Variable, Unit, Path) %>%
      mutate(value = ifelse(year == 2010 & is.na(value),
                            value[year == 2015],
                            value)) %>% # Set 2 NAS in 2010 to 2015 values
      ungroup() %>%
      unite(Model_Scenario, c(Model, Scenario), sep = "_", remove = FALSE) %>%
      mutate(value = value / 10^3,
             Unit = "GtCO2/yr") -> ar6_output_fossilCCS_sequestration_breakdown
    
    #Rename and regroup variables into: novel CDR, land CDR, fossil CCS
    ar6_output_fossilCCS_sequestration_breakdown %>%
      mutate(Variable = "fossil CCS") %>%
      group_by(Model_Scenario, Model, Scenario, Region, Variable, Unit, Path, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      #filter out scenarios that are not in reanlaysis
      filter(Model_Scenario %in% scenarios_to_keep)-> ar6_output_fossilCCS_sequestration_breakdown_total
    
    ar6_output_fossilCCS_sequestration_breakdown_total %>%
      filter(year == 2100) %>%
      group_by(Variable, Unit, Path, year) %>%
      mutate(median_CCS = median(value)) %>%
      ungroup() %>%
      mutate(CCS = if_else(value <= median_CCS, "below or equal median CCS", "above median CCS"))%>%
      select(Model_Scenario, CCS) -> ar6_output_fossilCCS_sequestration_final
    
    #Mere two tables
    final_data_permutations %>%
      left_join(ar6_output_fossilCCS_sequestration_final, by = "Model_Scenario") %>%
      #There are 2 scenarios that do not have any CCS so they appear as NA, we replace these with below median
      #Since CCS is 0
      mutate(CCS = if_else(is.na(CCS), "below or equal median CCS", CCS))%>%
      mutate(CCS = factor(CCS, levels = CCS_order))-> final_data_permutations_CCS
    #^Note: there are 22 Model_Scenarios where land carbon sequestration is negative 
    #(representing re-releases of carbon - so emissions rather than sequestration)
    #This means that in those scenarios temperature when you underdeliver land CDR is lower than original run
    #Because you are not "emitting". Since this is confusing, we want to ignore those scenarios.
    
    #Bring in reanalysis land sequestration
    #Now load reanalysis data from Gidden for Land Sequestration
    gidden_land_reanalysis<- read_csv("data/10.5281_zenodo.10158920_gidden_et_al_2023_ar6_reanalysis_data.csv")
    
    gidden_land_reanalysis %>%
      filter(Variable == "AR6 Reanalysis|OSCARv3.2|Carbon Removal|Land|Direct", #All 407 scenarios have this variable
             Region == "World") %>%
      gather(year, value, -Model, -Scenario, -Region, -Variable, -Unit) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year >= 2010) %>%
      left_join(C_paths, by = c("Scenario", "Model")) %>% #This has the 540 AR6 scenarios
      filter(!is.na(Path)) %>% #Here we filter out the scenarios that are not in the reanalysis data
      unite(Model_Scenario, c(Model, Scenario), sep = "_", remove = FALSE) %>%
      mutate(value_Gt_reanalysis = value / 10^3,
             Unit = "GtCO2/yr") -> reanalysis_CO2AFOLU_seq
    
    reanalysis_CO2AFOLU_seq %>% #This is variable "AR6 Reanalysis|OSCARV3.2|Carbon Removal|Land|Direct"
      filter(value < 0) %>%
      distinct(Model_Scenario)-> scenarios_to_exclude
    
    final_data_permutations_CCS %>%
      filter(!Model_Scenario %in% scenarios_to_exclude$Model_Scenario) -> final_data_permutations_CCS
    #Goes form 407 scenarios to 385, thus excluding the 22 Model_Scenarios that have negative sequestration
    #These scenarios have negative sequestration meaning emissions because of releases from deforestation
    
    ####Now calculate difference
    final_data_permutations_CCS %>%
      filter(percent_novelCDR == 0 & percent_landCDR == 0) %>%
      select(Model_Scenario, median_temp_2100, median_peak_temp) %>%
      rename(original_temp_2100 = median_temp_2100,
             original_temp_peak = median_peak_temp)-> original_temepratures_2100
    
    final_data_permutations_CCS %>%
      left_join(original_temepratures_2100, by = "Model_Scenario") %>%
      mutate(temp_2100_difference = median_temp_2100 - original_temp_2100,
             temp_peak_difference = median_peak_temp - original_temp_peak)-> final_data_CCS_difference
    
    final_data_CCS_difference %>%
      #Adjust rounding errors in difference to 0
      #mutate(temp_2100_difference = if_else(temp_2100_difference < -6.618018e-08, 0, temp_2100_difference)) %>%
      select(Model_Scenario, Path, percent_landCDR, percent_novelCDR, temp_2100_difference, temp_peak_difference) %>%
      mutate(percent_novelCDR = factor(percent_novelCDR, levels = c(0, 20, 40, 60, 80, 100)),
             percent_landCDR = factor(percent_landCDR, levels = c(0, 20, 40, 60, 80, 100)))-> final_data_2100_peak_diff
    
    medians <- final_data_2100_peak_diff %>%
      group_by(Path, percent_landCDR, percent_novelCDR) %>%
      summarise(median_temp_2100 = median(temp_2100_difference, na.rm = TRUE),
                median_temp_peak = median(temp_peak_difference, na.rm = TRUE), .groups = "drop")
    
    
    p <- ggplot() +
      # Scatter points for novel CDR (when land CDR == 0%)
      # geom_point(data = final_data_2100_peak_diff %>% filter(percent_landCDR == 0), 
      #            aes(x = temp_2100_difference, y = temp_peak_difference, fill = percent_novelCDR),
      #            alpha = 0.5, size = 1, shape = 24, color = "black", stroke = 0.1) + 
      geom_line(data = medians %>% filter(percent_landCDR == 0), 
                aes(x = median_temp_2100, y = median_temp_peak, group = Path), 
                color = "black", size = 0.6, inherit.aes = FALSE) + # Connect median points
      geom_point(data = medians %>% filter(percent_landCDR == 0), 
                 aes(x = median_temp_2100, y = median_temp_peak, fill = percent_novelCDR), 
                 color = "black", shape = 21, size = 3, stroke = 0.6, inherit.aes = FALSE) +
      # Scatter points for land CDR (when novel CDR == 0%)
      # geom_point(data = final_data_2100_peak_diff %>% filter(percent_novelCDR == 0), 
      #            aes(x = temp_2100_difference, y = temp_peak_difference, fill = percent_landCDR),
      #            alpha = 0.5, size = 1, shape = 21, color = "black", stroke = 0.1) +
      geom_line(data = medians %>% filter(percent_novelCDR == 0), 
                aes(x = median_temp_2100, y = median_temp_peak, group = Path), 
                color = "black", size = 0.6, inherit.aes = FALSE) + # Connect median points
      geom_point(data = medians %>% filter(percent_novelCDR == 0), 
                 aes(x = median_temp_2100, y = median_temp_peak, fill = percent_landCDR), 
                 color = "black", shape = 24, size = 3, stroke = 0.6, inherit.aes = FALSE) +
      
      geom_abline(intercept = 0, slope = 1, color = "grey", linetype = "dashed") +
      facet_wrap(~ Path) +
      scale_fill_viridis_d(option = "magma") +
      scale_x_continuous(limits = c(0, 0.3)) +
      scale_y_continuous(limits = c(0, 0.1)) +
      labs(x = "Temperature Difference in 2100", 
           y = "Peak Temperature Difference", 
           title = "Scatterplot of 2100 vs Peak Temperature Differences",
           subtitle = "Triangles: % underdelivering Land CDR (Novel CDR = 0%),
             Circles: % underdelivering Novel CDR (Land CDR = 0%)") +
      theme_minimal() +
      theme(legend.position = "bottom", 
            legend.title = element_blank(),
            aspect.ratio = 1,  
            strip.background = element_blank(),  
            strip.text = element_text(size = 12))
    p
    #ggsave("figures/MAGICC_temperature/V4/underdelivered_permutations/10.Peakvs2100_temperature_difference_scatter_together.png", p, width = 6, height = 6, units = "in", dpi = 300)
    #ggsave("figures/MAGICC_temperature/V4/underdelivered_permutations/10.Peakvs2100_temperature_difference_scatter_together.svg", p, width = 6, height = 6, units = "in", dpi = 300)
    
    
#-------------------------------------------------- PERMUTATIONS PROBABILITIES -------------------------------------------------------------------------------------------------------
    #Here we want to calculate the probability of exceeding thresholds 
    calculate_exceedance_probabilities <- function(temp_data, threshold, output_name = NULL) {
      # Ensure only one variable (temperature) is considered
      if (!"temperature" %in% names(temp_data)) {
        stop("Column 'temperature' not found in the dataset.")
      }
      
      # Identify ensemble members that ever exceed the threshold
      exceedance_data <- temp_data %>%
        group_by(Model_Scenario, Path, ensemble_member) %>%
        summarise(exceeds = any(temperature > threshold), .groups = "drop")
      
      # Compute exceedance probability per (Model_Scenario, Path)
      exceedance_prob <- exceedance_data %>%
        group_by(Model_Scenario, Path) %>%
        summarise(prob = mean(exceeds), .groups = "drop") %>%
        mutate(unit = "dimensionless")
      
      # Assign output name
      if (is.null(output_name)) {
        output_name <- paste0(threshold, " exceedance probability")
      }
      names(exceedance_prob)[names(exceedance_prob) == "prob"] <- output_name
      
      return(exceedance_prob)
    }
    
    #### 1. Get the data and calculate summary statistics ####
    # Define percent values (assuming 0 to 100 in steps of 20)
    percent_novelCDR_values <- seq(0, 100, by = 20)
    percent_landCDR_values <- seq(0, 100, by = 20)
    
    # Create an empty list to store results
    summary_list <- list()
    
    # Loop over novelCDR and landCDR percentages
    for (percent_novelCDR in percent_novelCDR_values) {
      for (percent_landCDR in percent_landCDR_values) {
        
        # Construct file path
        file_name <- paste0("MAGICC_", percent_novelCDR, "percent_novelCDR_", percent_landCDR, "percent_landCDR_temperature_statistics.csv")
        file_path <- file.path(base_path, file_name)
        
        # Check if file exists
        if (file.exists(file_path)) {
          
          # Load the CSV
          temp_data <- fread(file_path, select = c("Model_Scenario","ensemble_member", "year", "Path", "temperature")) %>%
            distinct(Model_Scenario, ensemble_member, year, Path, temperature) # Keep each ensemble
          
          # Compute exceedance probabilities for both thresholds (1.5C and 2C)
          exceedance_prob <- calculate_exceedance_probabilities(temp_data, threshold = 2.0) %>%
            left_join(calculate_exceedance_probabilities(temp_data, threshold = 1.5), 
                      by = c("Model_Scenario", "Path", "unit")) %>%
            left_join(calculate_exceedance_probabilities(temp_data %>% filter(year == 2100), threshold = 1.5) %>% 
                        rename(`1.5 exceedance probability in 2100` = `1.5 exceedance probability`), by = c("Model_Scenario", "Path", "unit")) %>%
            left_join(calculate_exceedance_probabilities(temp_data %>% filter(year == 2100), threshold = 2.0) %>%
                        rename(`2.0 exceedance probability in 2100` = `2 exceedance probability`),#
                      by = c("Model_Scenario", "Path", "unit")) %>% #
            mutate(percent_novelCDR = percent_novelCDR,
                   percent_landCDR = percent_landCDR)
          
          # Store in list
          summary_list[[paste0(percent_novelCDR, "_", percent_landCDR)]] <- exceedance_prob
          
          # Clean up memory
          rm(temp_data)
          gc()
          
        } else {
          warning(paste("File not found:", file_path))
        }
      }
    }
    
    # Combine all summaries into one dataframe
    final_data_permutations_probabilities <- bind_rows(summary_list) 
    #^This table has our 407 scenarios and 600 ensembles
    
    final_data_permutations_probabilities %>%
      filter(!Model_Scenario %in% scenarios_to_exclude$Model_Scenario) -> final_data_permutations_probabilities
    #^We go from 407 Model_Scenario to 385
    
    #### 3. Now flip the probability ####
    #Here we want to show the probability of staying within the threshold
    #Per IPCC definitions:
    #C1: Reach or exceed 1.5C during the 21st century with a likelihood of <= 67%
    #and limit warming to 1.5C in 2100 with a likelihood of > 50%
    #C2: exceed warming of 1.5C during the 21st century with a likelihood of >67%
    #and limit warming to 1.5C in 2100 with a likelihood of > 50%
    #C3: limit peak warming to 2C thoughout the 21st century with a likelihood of > 67%
    final_data_permutations_probabilities %>%
      mutate(staying_below_1p5 = 1- `1.5 exceedance probability`,
             staying_below_1p5_2100 = 1 - `1.5 exceedance probability in 2100`,
             staying_below_2p0 = 1 - `2 exceedance probability`,
             staying_below_2p0_2100 = 1- `2.0 exceedance probability in 2100`) %>%
      group_by(Path, percent_novelCDR, percent_landCDR) %>%
      summarise(staying_below_1p5_mean = mean(staying_below_1p5),
                staying_below_2p0_mean = mean(staying_below_2p0),
                staying_below_1p5_2100_mean = mean(staying_below_1p5_2100),
                staying_below_2p0_2100_mean = mean(staying_below_2p0_2100)) %>%
      ungroup() %>%
      mutate(staying_below_1p5_mean = staying_below_1p5_mean * 100,
             staying_below_2p0_mean = staying_below_2p0_mean * 100,
             staying_below_1p5_2100_mean = staying_below_1p5_2100_mean * 100,
             staying_below_2p0_2100_mean = staying_below_2p0_2100_mean * 100) -> final_data_permutations_probabilities_plot_2
    
    # Compute global min and max across all data (already done outside the loop
    # global_min <- min(final_data_permutations_probabilities_plot_2$staying_below_1p5_mean, na.rm = TRUE)
    # global_max <- max(final_data_permutations_probabilities_plot_2$staying_below_1p5_mean, na.rm = TRUE)
    library(viridisLite)
    
    # Generate a Magma colorscale with 256 colors for smooth transitions
    #magma_colors <- magma(256)  # 256 is a good resolution for gradients
    magma_colors <- rev(magma(256))  # reverse to flip the gradient
    
    # Convert to Plotly format
    colorscale_magma <- lapply(seq(0, 1, length.out = 256), function(i) {
      list(i, magma_colors[i * 255 + 1])
    })
    
    global_min <- 10
    global_max <- 90
    # Loop through each Path (C1, C2, C3)
    for (pathway in unique(final_data_permutations_probabilities_plot_2$Path)) {
      
      # Subset data for the current Path and CCS combination
      plot_data <- final_data_permutations_probabilities_plot_2 %>% filter(Path == pathway)
      
      # Generate Plotly contour plot for the 2100 temperature
      fig <- plot_ly(data = plot_data,
                     x = ~percent_novelCDR,
                     y = ~percent_landCDR,
                     z = ~staying_below_1p5_mean,
                     type = "contour",
                     ncontours = 15,
                     zmin = global_min,  # Use global fixed color scale min
                     zmax = global_max,  # Use global fixed color scale max
                     colorbar = list(
                       bordercolor = "black",
                       title = "Probability staying below 1.5C",
                       len = 0.5,
                       tickvals = seq(global_min, global_max, length.out = 5),
                       tickformat = ".2f"
                     ),
                     colorscale = colorscale_magma,  # Apply fixed colorscale
                     line = list(color = "white"),
                     autocontour = FALSE,
                     contours = list(
                       showlabels = TRUE,
                       coloring = "heatmap",  # Coloring by heatmap
                       size = 1,  # Adjust size for smoother contours
                       start = global_min,  # Start fixed at global min
                       end = global_max,    # End fixed at global max
                       showlines = TRUE  
                     )) %>%
        layout(
          title = paste("Probability for staying below 1.5C threshold", pathway),
          xaxis = list(title = "Percent Underdelivering Novel CDR"),
          yaxis = list(title = "Percent Underdelivering Land CDR")
        )
      
      # Optionally, save it as a static PNG (if orca is installed)
      # orca(fig, file = paste0("figures/MAGICC_temperature/V4/underdelivered_permutations/10b.probability_below_1p5_", pathway, ".png"))
      # orca(fig, file = paste0("figures/MAGICC_temperature/V4/underdelivered_permutations/10b.probability_below_1p5_", pathway, ".svg"))
    }
    
    
    ### Now do for 2C
    # Loop through each Path (C1, C2, C3)
    # global_min <- min(final_data_permutations_probabilities_plot_2$staying_below_2p0_mean, na.rm = TRUE)
    # global_max <- max(final_data_permutations_probabilities_plot_2$staying_below_2p0_mean, na.rm = TRUE)
    
    for (pathway in unique(final_data_permutations_probabilities_plot_2$Path)) {
      
      # Subset data for the current Path and CCS combination
      plot_data <- final_data_permutations_probabilities_plot_2 %>% filter(Path == pathway)
      
      # Generate Plotly contour plot 
      fig <- plot_ly(data = plot_data,
                     x = ~percent_novelCDR,
                     y = ~percent_landCDR,
                     #z = ~staying_below_2p0_mean, #This is the probability of staying below 2.0C in 21st century
                     z = ~staying_below_2p0_2100_mean, #This is the probability of staying below 2.0C in the year 2100
                     type = "contour",
                     ncontours = 15,
                     zmin = global_min,  # Use global fixed color scale min
                     zmax = global_max,  # Use global fixed color scale max
                     colorbar = list(
                       bordercolor = "black",
                       #title = "Probability staying below 2.0C",
                       title = "Probability staying below 2.0C in 2100",
                       len = 0.5,
                       tickvals = seq(global_min, global_max, length.out = 6),
                       tickformat = ".2f"
                     ),
                     colorscale = colorscale_magma,  # Apply fixed colorscale
                     line = list(color = "white"),
                     autocontour = FALSE,
                     contours = list(
                       showlabels = TRUE,
                       coloring = "heatmap",  # Coloring by heatmap
                       size = 1,  # Adjust size for smoother contours
                       start = global_min,  # Start fixed at global min
                       end = global_max,    # End fixed at global max
                       showlines = TRUE  
                     )) %>%
        layout(
          title = paste("Probability for staying below 2.0C threshold", pathway),
          xaxis = list(title = "Percent Underdelivering Novel CDR"),
          yaxis = list(title = "Percent Underdelivering Land CDR")
        )
      
      # Optionally, save it as a static PNG (if orca is installed)
      # orca(fig, file = paste0("figures/MAGICC_temperature/V4/underdelivered_permutations/10b.probability_below_2.0_", pathway, ".png"))
      #orca(fig, file = paste0("figures/MAGICC_temperature/V4/underdelivered_permutations/10b.probability_below_2.0_2100", pathway, ".png"))
      # orca(fig, file = paste0("figures/MAGICC_temperature/V4/underdelivered_permutations/10b.probability_below_2.0_", pathway, ".svg"))
    }
    
    #Now do 1.5 in 2100 (as opposed to through all time)
    # Compute global min and max across all data (already done outside the loop)
    #global_min <- min(final_data_permutations_probabilities_plot_2$staying_below_1p5_2100_mean , na.rm = TRUE)
    #global_max <- max(final_data_permutations_probabilities_plot_2$staying_below_1p5_2100_mean , na.rm = TRUE)
    
    # Loop through each Path (C1, C2, C3)
    for (pathway in unique(final_data_permutations_probabilities_plot_2$Path)) {
      
      # Subset data for the current Path and CCS combination
      plot_data <- final_data_permutations_probabilities_plot_2 %>% filter(Path == pathway)
      
      # Generate Plotly contour plot for the 2100 temperature
      fig <- plot_ly(data = plot_data,
                     x = ~percent_novelCDR,
                     y = ~percent_landCDR,
                     z = ~staying_below_1p5_2100_mean ,
                     type = "contour",
                     ncontours = 15,
                     zmin = global_min,  # Use global fixed color scale min
                     zmax = global_max,  # Use global fixed color scale max
                     colorbar = list(
                       bordercolor = "black",
                       title = "Probability staying below 1.5C in 2100",
                       len = 0.5,
                       tickvals = seq(global_min, global_max, length.out = 5),
                       tickformat = ".2f"
                     ),
                     colorscale = colorscale_magma,  # Apply fixed colorscale
                     line = list(color = "white"),
                     autocontour = FALSE,
                     contours = list(
                       showlabels = TRUE,
                       coloring = "heatmap",  # Coloring by heatmap
                       size = 1,  # Adjust size for smoother contours
                       start = global_min,  # Start fixed at global min
                       end = global_max,    # End fixed at global max
                       showlines = TRUE  
                     )) %>%
        layout(
          title = paste("Probability for staying below 1.5C threshold in 2100", pathway),
          xaxis = list(title = "Percent Underdelivering Novel CDR"),
          yaxis = list(title = "Percent Underdelivering Land CDR")
        )
      
      # Optionally, save it as a static PNG (if orca is installed)
      # orca(fig, file = paste0("figures/MAGICC_temperature/V4/underdelivered_permutations/10b.probability_below_1p5_in2100_", pathway, ".png"))
      # orca(fig, file = paste0("figures/MAGICC_temperature/V4/underdelivered_permutations/10b.probability_below_1p5_in2100_", pathway, ".svg"))
    }
    
#-------------------------------------------------- SUPPLEMENTARY FIGURE 1 -----------------------------------------------------------------------
  #Here we get fossil CCS for paths C1 and C2 (1.5 C scenarios)    
    p <- ggplot() +
      geom_ribbon(data = total_CO2_emissions_median %>% filter(Path != "C3"), aes(x = year, ymin = P5, ymax = P95, fill = Path), alpha = 0.2) +
      geom_ribbon(data = total_CO2_emissions_median %>% filter(Path != "C3"), aes(x = year, ymin = P15, ymax = P85, fill = Path), alpha = 0.3) +
      geom_ribbon(data = total_CO2_emissions_median %>% filter(Path != "C3"), aes(x = year, ymin = P25, ymax = P75, fill = Path), alpha = 0.4) +
      facet_wrap(~ Path) +
      geom_line(data = total_CO2_emissions_median %>% filter(Path != "C3"), 
                aes(x = year, y = median, color = Path), size = 1.2, linetype = "solid") +
      geom_hline(yintercept = 0, color = "black") +
      labs(title = "Fossil CCS",
           subtitle = "Variables: Carbon Sequestration|CCS|Industrial Processes, Carbon Sequestration|CCS|Fossil 
           (P5, P15, P25, median, P75, P85, P95)",
           x = "Year",
           y = "CO2 sequestration (GtCO2/yr)",
           fill = "Scenario and Model") +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      #facet_wrap (~ Path) +
      scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
      scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) +
      theme_minimal() +
      figure_theme+
      theme(panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            legend.position = "bottom") 
    #ggsave("figures/paper_figures/V3/SM1.fossilCCS_timeline.png", p, width = 12, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/V3/SM1.fossilCCS_timeline.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    p
    
    #Here we get total CDR for C1 and C2
    p <- ggplot() +
      geom_ribbon(data = total_CO2_seq_median %>% filter(Path != "C3"), 
                  aes(x = year, ymin = P5, ymax = P95, fill = Path), alpha = 0.2) +
      geom_ribbon(data = total_CO2_seq_median %>% filter(Path != "C3"), 
                  aes(x = year, ymin = P15, ymax = P85, fill = Path), alpha = 0.3) +
      geom_ribbon(data = total_CO2_seq_median %>% filter(Path != "C3"), 
                  aes(x = year, ymin = P25, ymax = P75, fill = Path), alpha = 0.4) +
      geom_line(data = total_CO2_seq_median %>% filter(Path != "C3"), 
                aes(x = year, y = median, color = Path), size = 1.2, linetype = "solid") +
      geom_hline(yintercept = 0, color = "black") +
      facet_wrap(~ Path) +
      labs(title = "Total CDR",
           subtitle = "Variables: Carbon Sequestration|Direct Air Capture, Enhanced Weathering, Other, CCS|Biomass and
           AR6 Reanalysis|OSCARv3.2|Carbon Removal|Land|Direct (P5, P15, P25, median, P75, P85, P95)",
           x = "Year",
           y = "CO2 sequestration (GtCO2/yr)",
           color = "Scenario and Model",
           fill = "Scenario and Model") +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
      scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) +
      theme_minimal() +
      figure_theme+
      theme(panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            legend.position = "bottom")  # Ensure the legend is removed
    #ggsave("figures/paper_figures/V3/SM1.totalCDR_timeline.png", p, width = 12, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/V3/SM1.totalCDR_timeline.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    p
  
#-------------------------------------------------- SUPPLEMENTARY FIGURE 2 -----------------------------------------------------------------------
    #Supplementary Figure 2 shows total GHG/CO2 emissions and temperature outcomes from original runs and under-delivering 
    #fossil CCS only 10-100%
    #Read in GWP from AR6 available here: https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_WGI_Chapter07_SM.pdf
    #Table 7.SM.7 | Greenhouse gas lifetimes, radiative efficiencies, global warming potentials (GWPs), global temperature potentials (GTPs) and cumulative global temperature potentials (CGTPs).
    AR6_GWP <- read_csv("/Users/mariacandelariabergero/Library/CloudStorage/GoogleDrive-cande.bergero@gmail.com/My Drive/Education/2021 (UCI)/Research/IIASA/data/GWP_ar6.csv", skip = 1)
    
    #### 1. Prepare data ####
    original_emissions <- read_csv("/Users/mariacandelariabergero/python/gegca-prototype/data/raw/interim/15849b8/V3/original/scenarios-infilled_all.csv") %>% mutate(underdelivery_percent = 0)
    
    # Define the base file path
    base_path <- "/Users/mariacandelariabergero/python/gegca-prototype/data/raw/interim/15849b8/V3/underdeliver_fossilCCS/"
    
    # Define percentages (10 to 100 in steps of 10)
    percentages <- seq(10, 100, by = 10)
    
    # Read in all files and combine them
    all_data_fossilCCS <- map_dfr(percentages, function(pct) {
      file_path_fossilCCS <- paste0(base_path, pct, "/scenarios-infilled_all_underdelivered_", pct, "percent.csv")
      # Read file and add a column to track the percentage
      read_csv(file_path_fossilCCS) %>%
        mutate(underdelivery_percent = pct)
    })
    
    #Merge all data 2015-2100
    original_emissions %>% #Original emissions
      bind_rows(all_data_fossilCCS) %>% #under-delivering 10-100% every 10%
      gather(year, value, -model, -scenario, -region, -variable, -unit, -underdelivery_percent) %>%
      mutate(year = substr(year, 1, 4)) %>% #clean year column
      mutate(year = as.numeric(year)) %>%
      left_join(C_paths, by = c("scenario" = "Scenario", "model" = "Model")) %>%
      unite(Model_Scenario, c(model, scenario), sep = "_", remove = FALSE) %>%
      left_join(AR6_GWP, by = "variable")%>% #Bring in multipliers GWP 100 years from IPCC AR6
      filter(!is.na(GWP_100))%>% #Filter out BC, CO, NH3, NOx, OC, SUlfur and VOC (not GHGs)
      select(-Notes) %>%
      mutate(value = ifelse(grepl("kt", unit), value / 1000, value)) %>% #Convert gases in kilo ton to Mega ton
      mutate(value_CO2eq = value * GWP_100) %>%
      group_by(Model_Scenario, model, scenario, region, year, Path, GHG, underdelivery_percent) %>%
      summarise(sum = sum(value_CO2eq)) %>%
      ungroup() %>%
      mutate(unit = "MtCO2eq")-> underdelivering_all_gases_fossil
    
    #Add all modeled GHGs 2015-2100
    underdelivering_all_gases_fossil %>%
      group_by(Model_Scenario, model, scenario, region, year, Path, unit, underdelivery_percent) %>%
      summarise(totalGHG = sum(sum)) %>%
      ungroup() %>%
      mutate(totalGHG = totalGHG / 1000,
             unit = "GtCO2eq/yr")-> underdelivering_all_gases_fossil_total
    #Note, these values are very similar to IPCC https://www.ipcc.ch/report/ar6/wg3/figures/summary-for-policymakers/figure-spm-1/
    
    #### 2. Figure GHGs ####
    # Calculate median total GHG for each year and Path
    plot_data_GHG_fossil <- underdelivering_all_gases_fossil_total %>%
      group_by(year, Path, underdelivery_percent) %>%
      summarise(median_GHG = median(totalGHG, na.rm = TRUE),
                P5 = quantile(totalGHG, 0.05, na.rm = TRUE),
                P95 = quantile(totalGHG, 0.95, na.rm = TRUE),.groups = "drop")
    
    for (path in unique_paths) {
      
      plot_subset <- plot_data_GHG_fossil %>% filter(Path == path)
      
      # Create line plot with facets
      p<-ggplot(plot_subset, aes(x = year, y = median_GHG, group = underdelivery_percent, color = underdelivery_percent)) +
        geom_line(size = 1) +  # Line plot for median
        geom_line(data = history_ar6_fixed_GHG_2010_2015, aes(x = year, y = total_GHG), inherit.aes = FALSE, color = "black" ) +
        geom_hline(yintercept = 0, color = "black") +
        facet_wrap(~ Path) +  # Facet by Path
        labs(title = paste("Median GHG Emissions Across Scenarios", path, "(energy + land)"),
             subtitle = "Harmonized and infilled emissions - underdelivering fossil CCS",
             x = "Year",
             y = "Total GHG GtCO2eq/yr (Median across scenarios)") +
        scale_color_viridis(option = "turbo") +
        scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
        scale_y_continuous(limits = c(-6, 60), breaks = seq(-6, 60, by = 6)) +
        theme_minimal() +
        figure_theme+
        theme(panel.background = element_rect(fill = "white"),
              plot.background = element_rect(fill = "white"),
              legend.position = "bottom") 
      # ggsave(paste0("figures/paper_figures/V3/SM2.underdelivering_fossilCCS_totalGHG_timeline_", path, ".png"), p, width = 12, height = 12, units = "in", dpi = 600)
      # ggsave(paste0("figures/paper_figures/V3/SM2.underdelivering_fossilCCS_totalGHG_timeline_", path, ".svg"), p, width = 12, height = 12, units = "in", dpi = 600)
      
    }
    
    ### Here calculate percent reductions in emissions per decade
    # Add a decade column
    plot_data_GHG_fossil <- plot_data_GHG_fossil %>%
      mutate(decade = floor(year / 10) * 10)
    
    # Filter to start/end years of each decade you want
    target_decades <- c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)
    start_end_years <- plot_data_GHG_fossil %>%
      filter(year %in% c(target_decades, target_decades + 9))  # e.g., 2020 and 2029
    
    # Calculate CAGR for each decade
    mitigation_rates_fossil_CCS <- start_end_years %>%
      arrange(Path, underdelivery_percent, year) %>%
      group_by(Path, underdelivery_percent, decade) %>%
      summarise(
        GHG_start = median_GHG[year == decade],
        GHG_end = median_GHG[year == decade + 9],
        mitigation_rate = ifelse(!is.na(GHG_start) & !is.na(GHG_end) & GHG_end > 0,
                                 1 - (GHG_end / GHG_start)^(1 / 9),
                                 NA_real_),
        .groups = "drop"
      ) %>%
      mutate(mitigation_rate = mitigation_rate * 100)
    
    #### 3. Temperature outcomes: underdelivering fossil CCS only ####
    #These tables have all 407 scenarios, 600 ensembles each, with the temperature in each year, as well as the summary statistics across the 600 ensembles for each scenario
    #Note: for each scenario we calculate the mean temperature increase compared to pre-industrial times
    #Then we estimate the median across the 600 ensemble members for each scenario
    #and then we plot the median for each C category across the 407 scenarios.
    #407 scenarios, 600 ensembles, 101 periods -> 24,664,200
    #We can also read in reduced version with 19 periods to speed up the process -> 4,639,800
    # Define file paths and corresponding run labels
    MAGICC_original<- read_csv( "output/V3/temperature/original/MAGICC_original_temperature_statistics.csv") %>%
      mutate(run = "0%") 
    
    MAGICC_underdeliver_fossilCCS_10<- read_csv( "output/V3/temperature/underdeliver_fossilCCS/MAGICC_10percent_temperature_statistics_fossilCCS.csv") %>%
      mutate(run = "10%") 
    
    MAGICC_underdeliver_fossilCCS_20<- read_csv( "output/V3/temperature/underdeliver_fossilCCS/MAGICC_20percent_temperature_statistics_fossilCCS.csv") %>%
      mutate(run = "20%") 
    
    MAGICC_underdeliver_fossilCCS_30<- read_csv( "output/V3/temperature/underdeliver_fossilCCS/MAGICC_30percent_temperature_statistics_fossilCCS.csv") %>%
      mutate(run = "30%") 
    
    MAGICC_underdeliver_fossilCCS_40<- read_csv( "output/V3/temperature/underdeliver_fossilCCS/MAGICC_40percent_temperature_statistics_fossilCCS.csv") %>%
      mutate(run = "40%") 
    
    MAGICC_underdeliver_fossilCCS_50<- read_csv( "output/V3/temperature/underdeliver_fossilCCS/MAGICC_50percent_temperature_statistics_fossilCCS.csv") %>%
      mutate(run = "50%") 
    
    MAGICC_underdeliver_fossilCCS_60<- read_csv( "output/V3/temperature/underdeliver_fossilCCS/MAGICC_60percent_temperature_statistics_fossilCCS.csv") %>%
      mutate(run = "60%") 
    
    MAGICC_underdeliver_fossilCCS_70<- read_csv( "output/V3/temperature/underdeliver_fossilCCS/MAGICC_70percent_temperature_statistics_fossilCCS.csv") %>%
      mutate(run = "70%") 
    
    MAGICC_underdeliver_fossilCCS_80<- read_csv( "output/V3/temperature/underdeliver_fossilCCS/MAGICC_80percent_temperature_statistics_fossilCCS.csv") %>%
      mutate(run = "80%") 
    
    MAGICC_underdeliver_fossilCCS_90<- read_csv( "output/V3/temperature/underdeliver_fossilCCS/MAGICC_90percent_temperature_statistics_fossilCCS.csv") %>%
      mutate(run = "90%") 
    
    MAGICC_underdeliver_fossilCCS_100<- read_csv( "output/V3/temperature/underdeliver_fossilCCS/MAGICC_100percent_temperature_statistics_fossilCCS.csv") %>%
      mutate(run = "100%")
    
    # List of tables
    MAGICC_tables_fossilCCS <- list(
      "0%" = MAGICC_original,
      "10%" = MAGICC_underdeliver_fossilCCS_10,
      "20%" = MAGICC_underdeliver_fossilCCS_20,
      "30%" = MAGICC_underdeliver_fossilCCS_30,
      "40%" = MAGICC_underdeliver_fossilCCS_40,
      "50%" = MAGICC_underdeliver_fossilCCS_50,
      "60%" = MAGICC_underdeliver_fossilCCS_60,
      "70%" = MAGICC_underdeliver_fossilCCS_70,
      "80%" = MAGICC_underdeliver_fossilCCS_80,
      "90%" = MAGICC_underdeliver_fossilCCS_90,
      "100%" = MAGICC_underdeliver_fossilCCS_100
    )
    
    # Initialize an empty list to store results
    results_list <- list()
    
    # Loop through each table, calculate median for the path, and store results
    for (run_label in names(MAGICC_tables_fossilCCS)) {
      df <- MAGICC_tables_fossilCCS[[run_label]] %>%
        group_by(year, Path) %>%
        summarise(median_temperature = median(median, na.rm = TRUE), #get the median across C paths (median of median)
                  median_P95 = median(P95, na.rm = TRUE), #Here get the median for each run's 95th percentile
                  .groups = "drop") %>%
        mutate(run = run_label)  # Add run label
      
      results_list[[run_label]] <- df  # Store result
    }
    
    # Combine all results into one master table
    plot_data_temperature_fossilCCS <- bind_rows(results_list) #101 years (alternatively 19), 11 runs, 3 paths 3,333 rows (alternatively 627)
    #^This is the median of the 600 ensemble (with each ensemble being global mean temperature icnrease compared to pre-industrial),
    #and the median across scenarios
    for (path in unique_paths) {
      
      plot_subset <- plot_data_temperature_fossilCCS %>% filter(Path == path)
      
      # Create line plot with facets
      p<-ggplot(plot_subset, aes(x = year, y = median_temperature, group = run, color = run)) +
        geom_line(size = 1) +  # Line plot for median
        geom_hline(yintercept = 0, color = "black") +
        facet_wrap(~ Path) +  # Facet by Path
        labs(title = paste("Global Mean Temperature increase vs pre-industrial in", path),
             subtitle = "Median esemble member & median across scenairos (Underdelivering Fossil CCS)",
             x = "Year",
             y = "Mean Global Temperature Increase C") +
        #scale_color_viridis(option = "turbo") +
        scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 5)) +
        scale_y_continuous(limits = c(0, 2.2), breaks = seq(0, 2.2, by = 0.2)) +
        theme_minimal() +
        figure_theme+
        theme(panel.background = element_rect(fill = "white"),
              plot.background = element_rect(fill = "white"),
              legend.position = "bottom") 
      ggsave(paste0("figures/paper_figures/V3/SM2.fossilCCS_temperature_timeline_", path, "V2.png"), p, width = 12, height = 12, units = "in", dpi = 600)
      ggsave(paste0("figures/paper_figures/V3/SM2.fossilCCS_temperature_timeline_", path, "V2.svg"), p, width = 12, height = 12, units = "in", dpi = 600)
      
      p<-ggplot(plot_subset, aes(x = year, y = median_P95, group = run, color = run)) +
        geom_line(size = 1) +  # Line plot for median
        geom_hline(yintercept = 0, color = "black") +
        facet_wrap(~ Path) +  # Facet by Path
        labs(title = paste("Global Mean Temperature increase vs pre-industrial in", path),
             subtitle = "P95 esemble member & then median across scenairos (Underdelivering Fossil CCS)",
             x = "Year",
             y = "Mean Global Temperature Increase C") +
        #scale_color_viridis(option = "turbo") +
        scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 5)) +
        scale_y_continuous(limits = c(0, 3.2), breaks = seq(0, 3.2, by = 0.2)) +
        theme_minimal() +
        figure_theme+
        theme(panel.background = element_rect(fill = "white"),
              plot.background = element_rect(fill = "white"),
              legend.position = "bottom") 
      ggsave(paste0("figures/paper_figures/V3/SM2.fossilCCS_temperature_timeline_", path, "P95_V2.png"), p, width = 12, height = 12, units = "in", dpi = 600)
      ggsave(paste0("figures/paper_figures/V3/SM2.fossilCCS_temperature_timeline_", path, "P95_V2.svg"), p, width = 12, height = 12, units = "in", dpi = 600)
      
    }
    
    #### 4. Now analyze 50% under-delivered all####
    #This table is for 2000-2014 temperatures (modeled scenarios begin in 2015)
    AR6_data_complete %>%
      filter(Variable == "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile" ) %>%
      gather(year, value, -Model, -Scenario, -Region, -Variable, -Unit) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year >= 2000 & year <= 2014) %>%
      select(-Model, -Scenario) %>%
      distinct()-> historical_temperatures
    
    filtered_median <- MAGICC_underdeliver_fossilCCS_50 %>% #table only has 50%runs
      group_by(Path, year, run) %>%
      #Get median, min and max across the 407 scenarios using their median ensemble. 
      #So this is the distribution across scenarios for their median ensemble member
      summarize(median_temperature_median = median(median, na.rm = TRUE),
                min_temperature_median = min(median, na.rm = TRUE),
                max_temperature_median = max(median, na.rm = TRUE),
                mean_temperature_median = mean(median, na.rm = TRUE),
                n = (n()/600),
                #For 50% of the data
                P25 = quantile(median, 0.25, na.rm = TRUE),
                P75 = quantile(median, 0.75, na.rm = TRUE),
                #For 90% of data
                P5 = quantile(median, 0.05, na.rm = TRUE),
                P95 = quantile(median, 0.95, na.rm = TRUE),
                #For 70% of data
                P15 = quantile(median, 0.15, na.rm = TRUE),
                P85 = quantile(median, 0.85, na.rm = TRUE)) #Number of scenarios, ignoring ensembles 
    
    #This plot has median, min and max values across the 407 scenarios using their MEDIAN ensemble values
    #The min is the minimum across the medians of the scenarios, which have 600 ensembles.
    #Includes both original runs as well as adjusted runs
    p <- ggplot() +
      geom_ribbon(data = filtered_median %>% filter(run == "50%"), aes(x = year, ymin = P5, ymax = P95, fill = Path), alpha = 0.3) +
      geom_ribbon(data = filtered_median %>% filter(run == "50%"), aes(x = year, ymin = P15, ymax = P85, fill = Path), alpha = 0.4) +
      geom_ribbon(data = filtered_median %>% filter(run == "50%"), aes(x = year, ymin = P25, ymax = P75, fill = Path), alpha = 0.5) +
      
      geom_line(data = filtered_median %>% filter(run == "50%"), aes(x = year, y = median_temperature_median, color = Path, linetype = run), size = 1, linetype = "solid") +
      geom_text(data = filtered_median, aes(x = 2090, y = 4.8, label = paste("n =", n)), 
                size = 5, vjust = 0) +  # Adjust x, y for positioning
      geom_line(data = plot_data_temperature_fossilCCS %>% filter(run == "100%"), aes(x = year, y = median_temperature, color = Path, linetype = run), size = 0.5, linetype = "dashed") +#This is the median value across each scenario median ensemble
      geom_line(data = plot_data_temperature_fossilCCS %>% filter(run == "0%"), aes(x = year, y = median_temperature, color = Path, linetype = run), size = 0.5, linetype = "dashed") +#This is the median value across each scenario median ensemble
      geom_line(data = historical_temperatures, aes(x = year, y = value), color = "black", size = 1, linetype = "solid") +
      labs(title = "Temperature increase from AR6 (50% underdelivered fossil CCS)",
           subtitle = "For median value in ensemble member - P5, P15, P25, median, P75, P85, P95 across scenarios",
           x = "Year",
           y = "Temperature increase",
           color = "Scenario and Model",
           fill = "Scenario and Model") +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      facet_wrap (~ Path) +
      scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
      scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 0.5)) +
      theme_minimal() +
      figure_theme+
      theme(panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            legend.position = "none")  
    ggsave("figures/paper_figures/V3/SM2.fossilCCS_temperature_spread_50percent_V2.png", p, width = 12, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM2.fossilCCS_temperature_spread_50percent_V2.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    
    #### 5. Calculate probability exceeding threshold ####
    #Here we calculate the probability of exceeding 1.5C and 2C for each scenario across its 600 ensmebles
    #### First we calculate for 1.5C
    # Define the exceedance threshold
    threshold <- 1.5  
    
    # Initialize an empty list to store exceedance probabilities
    exceedance_prob_list <- list()
    
    # Loop through each table to compute exceedance probabilities
    for (run_label in names(MAGICC_tables_fossilCCS)) {
      
      df <- MAGICC_tables_fossilCCS[[run_label]]  # Get current dataset
      
      # Compute exceedance probability
      exceedance_prob <- df %>%
        group_by(Model_Scenario, Path, year) %>%
        summarise(
          exceedance_prob = sum(temperature > threshold) / n(),  # Fraction exceeding threshold
          .groups = "drop"
        ) %>%
        mutate(run = run_label)  # Add run label
      
      exceedance_prob_list[[run_label]] <- exceedance_prob  # Store results
    }
    
    # Combine all results into one master table
    exceedance_prob_1.5 <- bind_rows(exceedance_prob_list) #407 scenarios, 101 periods, 11 runs (452,177 rows)
    
    exceedance_prob_1.5 %>%
      filter(year >= 2010) %>%
      mutate(run = factor(run, levels = underdelivered_order)) -> exceedance_prob_1.5_sort
    
    # Compute median exceedance probability across all Model_Scenarios for each year and Path
    median_exceedance <- exceedance_prob_1.5_sort %>%
      group_by(year, Path, run) %>%
      summarise(median_prob = median(exceedance_prob, na.rm = TRUE), .groups = "drop")
    
    # Get unique Paths
    paths <- unique(exceedance_prob_1.5_sort$Path)
    
    # Loop through each Path and create a single figure with faceted runs
    for (p in paths) {
      # Subset data for the specific Path
      data_subset <- exceedance_prob_1.5_sort %>%
        filter(Path == p)
      
      # Count the number of unique Model_Scenarios represented
      num_scenarios <- data_subset %>% pull(Model_Scenario) %>% unique() %>% length()
      
      # Plot
      p_plot <- ggplot(data_subset, aes(x = year, y = exceedance_prob, group = Model_Scenario, color = Model_Scenario)) +
        geom_line(size = 0.8, alpha = 0.5) +  # Reduce opacity for visibility
        geom_line(data = median_exceedance %>% filter(Path == p), aes(x = year, y = median_prob),
                  inherit.aes = FALSE, color = "black", size = 1) +  # Add median line
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
        facet_wrap(~ run, ncol = 4) +  # Facet by run percentage
        labs(
          title = paste("Underdelivering fossil CCS - Probability of Exceeding 1.5°C Over Time for Path:", p),
          subtitle = paste("Number of Model Scenarios Represented:", num_scenarios),
          x = "Year",
          y = "Exceedance Probability"
        ) +
        scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, by = 0.2)) +
        scale_color_viridis_d(option = "magma") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggsave(filename = paste0("figures/paper_figures/V3/SM2.fossilCCS_Exceedance_1.5C_Path_", p, "_V2.png"),plot = p_plot,width = 8, height = 6, dpi = 300)
      print(paste("Saved plot for Path:", p))
    }
    
    #### now try with 2.0C
    threshold <- 2.0
    
    # Initialize an empty list to store exceedance probabilities
    exceedance_prob_list_2.0 <- list()
    
    # Loop through each table to compute exceedance probabilities
    for (run_label in names(MAGICC_tables_fossilCCS)) {
      
      df <- MAGICC_tables_fossilCCS[[run_label]]  # Get current dataset
      
      # Compute exceedance probability
      exceedance_prob_2.0 <- df %>%
        group_by(Model_Scenario, Path, year) %>%
        summarise(
          exceedance_prob = sum(temperature > threshold) / n(),  # Fraction exceeding threshold
          .groups = "drop"
        ) %>%
        mutate(run = run_label)  # Add run label
      
      exceedance_prob_list_2.0[[run_label]] <- exceedance_prob_2.0  # Store results
    }
    
    # Combine all results into one master table
    exceedance_prob_2.0 <- bind_rows(exceedance_prob_list_2.0) #407 scenarios, 101 periods, 11 runs (452,177 rows)
    
    exceedance_prob_2.0 %>%
      filter(year >= 2010) %>%
      mutate(run = factor(run, levels = underdelivered_order)) -> exceedance_prob_2.0_sort
    
    # Compute median exceedance probability across all Model_Scenarios for each year and Path
    median_exceedance <- exceedance_prob_2.0_sort %>%
      group_by(year, Path, run) %>%
      summarise(median_prob = median(exceedance_prob, na.rm = TRUE), .groups = "drop")
    
    # Get unique Paths
    paths <- unique(exceedance_prob_2.0_sort$Path)
    
    # Loop through each Path and create a single figure with faceted runs
    for (p in paths) {
      # Subset data for the specific Path
      data_subset <- exceedance_prob_2.0_sort %>%
        filter(Path == p)
      
      # Count the number of unique Model_Scenarios represented
      num_scenarios <- data_subset %>% pull(Model_Scenario) %>% unique() %>% length()
      
      # Plot
      p_plot <- ggplot(data_subset, aes(x = year, y = exceedance_prob, group = Model_Scenario, color = Model_Scenario)) +
        geom_line(size = 0.8, alpha = 0.5) +  # Reduce opacity for visibility
        geom_line(data = median_exceedance %>% filter(Path == p), aes(x = year, y = median_prob),
                  inherit.aes = FALSE, color = "black", size = 1) +  # Add median line
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
        facet_wrap(~ run, ncol = 4) +  # Facet by run percentage
        labs(
          title = paste("Underdelivering fossil CCS - Probability of Exceeding 2.0°C Over Time for Path:", p),
          subtitle = paste("Number of Model Scenarios Represented:", num_scenarios),
          x = "Year",
          y = "Exceedance Probability"
        ) +
        scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, by = 0.2)) +
        scale_color_viridis_d(option = "magma") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggsave(filename = paste0("figures/paper_figures/V3/SM2.fossilCCS_Exceedance_2.0C_Path_", p, "_V2.png"),plot = p_plot,width = 8, height = 6, dpi = 300)
      print(paste("Saved plot for Path:", p))
    }
    
    ### Now prepare actual insert figure
    exceedance_prob_2.0_sort %>%
      rename(exceedance_prob_2p0 = exceedance_prob) %>%
      left_join(exceedance_prob_1.5_sort, by = c("Model_Scenario", "Path", "run", "year")) %>%
      rename(exceedance_prob_1p5 = exceedance_prob) %>%
      group_by(Path, year, run) %>%
      summarise(median_prob_1p5 = median(exceedance_prob_1p5),
                median_prob_2p0 = median(exceedance_prob_2p0), .groups = "drop") -> exceedance_probability_median_complete
    
    p <- ggplot(exceedance_probability_median_complete %>% filter(run == "50%"), 
                aes(x = year, y = median_prob_2p0, color = Path, group = interaction(Path, run))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      # 2.0C exceedance probability lines (colored by Path)
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "100%"), 
                aes(x = year, y = median_prob_2p0, color = Path, group = interaction(Path, run)), 
                linetype = "dashed")  +  
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "0%"), 
                aes(x = year, y = median_prob_2p0, color = Path, group = interaction(Path, run)), 
                linetype = "dotted") +
      # 1.5C exceedance probability lines (set custom colors)
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "100%"), 
                aes(x = year, y = median_prob_1p5, group = interaction(Path, run)), 
                color = "black", linetype = "dashed") +  
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "0%"), 
                aes(x = year, y = median_prob_1p5, group = interaction(Path, run)), 
                color = "black", linetype = "dotted") +
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "50%"), 
                aes(x = year, y = median_prob_1p5, group = interaction(Path, run)), 
                color = "black", linetype = "solid") +
      geom_line(aes(y = 0.5), color = "black", size = 0.5, linetype = "dashed") +  
      labs(
        title = "Underdelivering fossil CCS - Probability of exceeding 1.5C and 2C (median across scenarios)",
        subtitle = "Showing 50% underdelivering (solid), 100% underdelivering (dashed), 0% underdelivered (dotted),
        color for 2.0C and black for 1.5C",
        x = "Year",
        y = "Probability of exceeding threshold"
      ) +
      facet_wrap(~Path) +
      scale_color_manual(values = colors) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
      figure_theme 
    ggsave("figures/paper_figures/V3/SM2.fossilCCS_exceedance_probability_2C_1p5C_V2.png", p, width = 12, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM2.fossilCCS_exceedance_probability_2C_1p5C_V2.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    
    
    #### 6. Emissions relative to original pathways ####
    # Calculate median total GHG for each year and Path
    underdelivering_all_gases_fossil_total %>%
      group_by(year, Path, underdelivery_percent) %>%
      summarise(median_GHG = median(totalGHG, na.rm = TRUE), .groups = "drop") -> plot_data
    
    plot_data %>%
      filter(Path == "C1",
             underdelivery_percent == "0") %>%
      select(-Path, -underdelivery_percent) %>%
      rename(median_GHG_C1_original = median_GHG)-> plot_data_C1_original 
    
    plot_data %>%
      filter(Path == "C2",
             underdelivery_percent == "0") %>%
      select(-Path, -underdelivery_percent) %>%
      rename(median_GHG_C2_original = median_GHG)-> plot_data_C2_original 
    
    plot_data %>%
      filter(Path == "C3",
             underdelivery_percent == "0") %>%
      select(-Path, -underdelivery_percent) %>%
      rename(median_GHG_C3_original = median_GHG)-> plot_data_C3_original 
    #^This has the CO2 emissions in median 2C scenarios with )% under-delivering
    
    #Join all
    plot_data %>%
      left_join(plot_data_C1_original, by = "year") %>%
      left_join(plot_data_C2_original, by = "year") %>%
      left_join(plot_data_C3_original, by = "year") %>%
      mutate(difference = if_else(Path == "C1", median_GHG - median_GHG_C1_original,
                                  if_else(Path == "C2", median_GHG - median_GHG_C2_original, 
                                          median_GHG - median_GHG_C3_original)
      )) -> underdelivering_all_GHG_differences
    
    #Plot all
    ggplot(underdelivering_all_GHG_differences, aes(x = year, y = difference, color = Path, group = interaction(Path, underdelivery_percent))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      geom_line(aes(y = 0), color = "black", size = 1, linetype = "solid") +  # Baseline as y=1
      labs(
        title = "Difference in GHG emissions relative to 0% underdelivering",
        subtitle = "Each line is an underdelivering % relative to its category (c1, c2, c3)",
        x = "Year",
        y = "Difference Emissions in GtCO2eq (underdelivered run - original run)"
      ) +
      facet_wrap( ~Path) +
      scale_color_manual(values = colors) +
      # scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    #Plot only 50% and 100%
    p<-ggplot(underdelivering_all_GHG_differences %>% filter(underdelivery_percent == "50"), aes(x = year, y = difference, color = Path, group = interaction(Path, underdelivery_percent))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      geom_line(data = underdelivering_all_GHG_differences %>% filter(underdelivery_percent == "100"), aes(x = year, y = difference, color = Path, group = interaction(Path, underdelivery_percent)), linetype = "dashed") +  # Lines for relative emissions in each scenario
      geom_line(aes(y = 0), color = "black", size = 0.5, linetype = "solid") +  # Baseline as y=1
      labs(
        title = "Underdelivering fossil CCS - Difference in GHG emissions relative to 0% underdelivering",
        subtitle = "Each line is an underdelivering 50% (and 100% dashed) relative to its category (c1, c2, c3)",
        x = "Year",
        y = "Difference Emissions in GtCO2eq (underdelivered run - original run)"
      ) +
      scale_color_manual(values = colors) +
      scale_y_continuous(limits = c(0, 21), breaks = seq(0, 21, by = 5)) +
      # scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(legend.position = "bottom")
    ggsave("figures/paper_figures/V3/SM2.fossilCCS_GHGemissions_difference.png", p, width = 12, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM2.fossilCCS_GHGemissions_difference.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    
    #### 7. Temperature relative to 2C original ####
    # Initialize an empty list to store results
    filtered_median_list <- list()
    
    # Loop through each table and compute summary statistics
    for (run_label in names(MAGICC_tables_fossilCCS)) {
      
      df <- MAGICC_tables_fossilCCS[[run_label]]  # Get current dataset
      
      summary_stats <- df %>%
        group_by(Path, year) %>%
        summarize(
          median_temperature_median = median(median, na.rm = TRUE),
          min_temperature_median = min(median, na.rm = TRUE),
          max_temperature_median = max(median, na.rm = TRUE),
          mean_temperature_median = mean(median, na.rm = TRUE),
          n = (n() / 600),
          P25 = quantile(median, 0.25, na.rm = TRUE),
          P75 = quantile(median, 0.75, na.rm = TRUE),
          P5 = quantile(median, 0.05, na.rm = TRUE),
          P95 = quantile(median, 0.95, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(run = run_label)  # Add run label
      
      filtered_median_list[[run_label]] <- summary_stats  # Store result
    }
    
    # Combine all runs into one master table
    filtered_median <- bind_rows(filtered_median_list) #3 paths, 101 years, 11 runs
    
    # Compute the reference temperature for "0%" run (original scenario)
    filtered_median_original <- filtered_median %>%
      filter(run == "0%") %>%
      select(Path, year, median_temperature_median) %>%
      spread(Path, median_temperature_median)
    
    # Join the original values with all runs and compute the temperature difference
    underdelivering_all_temperature_differences <- filtered_median %>%
      select(Path, year, run, median_temperature_median) %>%
      left_join(filtered_median_original, by = "year") %>%
      mutate(difference = if_else(Path == "C1", median_temperature_median - C1,
                                  if_else(Path == "C2", median_temperature_median - C2,
                                          median_temperature_median - C3)))
    
    #Plot only 50% and 100%
    p<-ggplot(underdelivering_all_temperature_differences %>% filter(run == "50%"), aes(x = year, y = difference, color = Path, group = interaction(Path, run))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      geom_line(data = underdelivering_all_temperature_differences %>% filter(run == "100%"), aes(x = year, y = difference, color = Path, group = interaction(Path, run)), linetype = "dashed") +  # Lines for relative emissions in each scenario
      geom_line(aes(y = 0), color = "black", size = 0.5, linetype = "solid") +  # Baseline as y=1
      labs(
        title = "underdelivering fossil CCS - Difference in GMT increase relative to 0% underdelivering",
        subtitle = "Each line is an underdelivering 50% (and 100%) relative to its category (c1, c2, c3)",
        x = "Year",
        y = "Difference GMT increase in C (underdelivered run - original run)"
      ) +
      scale_color_manual(values = colors) +
      scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, by = 0.1)) +
      scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
      # scale_fill_manual(values = colors) +
      theme_minimal() +
      figure_theme 
    ggsave("figures/paper_figures/V3/SM2.fossilCCS_temperature_difference.png", p, width = 12, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM2.fossilCCS_temperature_difference.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    
#-------------------------------------------------- SUPPLEMENTARY FIGURE 3 -----------------------------------------------------------------------
    #Supplementary Figure 3shows total GHG/CO2 emissions and temperature outcomes from original runs and under-delivering 
    #land CDR only 10-100%
    #Read in GWP from AR6 available here: https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_WGI_Chapter07_SM.pdf
    #Table 7.SM.7 | Greenhouse gas lifetimes, radiative efficiencies, global warming potentials (GWPs), global temperature potentials (GTPs) and cumulative global temperature potentials (CGTPs).
    AR6_GWP <- read_csv("/Users/mariacandelariabergero/Library/CloudStorage/GoogleDrive-cande.bergero@gmail.com/My Drive/Education/2021 (UCI)/Research/IIASA/data/GWP_ar6.csv", skip = 1)
    
    #### 1. Prepare data ####
    original_emissions <- read_csv("/Users/mariacandelariabergero/python/gegca-prototype/data/raw/interim/15849b8/V3/original/scenarios-infilled_all.csv") %>% mutate(underdelivery_percent = 0)
    
    # Define the base file path
    base_path <- "/Users/mariacandelariabergero/python/gegca-prototype/data/raw/interim/15849b8/V3/underdeliver_landCDR/"
    
    # Define percentages (10 to 100 in steps of 10)
    percentages <- seq(10, 100, by = 10)
    
    # Read in all files and combine them
    all_data_landCDR <- map_dfr(percentages, function(pct) {
      file_path_landCDR <- paste0(base_path, pct, "/scenarios-infilled_all_underdelivered_", pct, "percent.csv")
      # Read file and add a column to track the percentage
      read_csv(file_path_landCDR) %>%
        mutate(underdelivery_percent = pct)
    })
    
    #Merge all data 2015-2100
    original_emissions %>% #Original emissions
      bind_rows(all_data_landCDR) %>% #under-delivering 10-100% every 10%
      gather(year, value, -model, -scenario, -region, -variable, -unit, -underdelivery_percent) %>%
      mutate(year = substr(year, 1, 4)) %>% #clean year column
      mutate(year = as.numeric(year)) %>%
      left_join(C_paths, by = c("scenario" = "Scenario", "model" = "Model")) %>%
      unite(Model_Scenario, c(model, scenario), sep = "_", remove = FALSE) %>%
      left_join(AR6_GWP, by = "variable")%>% #Bring in multipliers GWP 100 years from IPCC AR6
      filter(!is.na(GWP_100))%>% #Filter out BC, CO, NH3, NOx, OC, SUlfur and VOC (not GHGs)
      select(-Notes) %>%
      mutate(value = ifelse(grepl("kt", unit), value / 1000, value)) %>% #Convert gases in kilo ton to Mega ton
      mutate(value_CO2eq = value * GWP_100) %>%
      group_by(Model_Scenario, model, scenario, region, year, Path, GHG, underdelivery_percent) %>%
      summarise(sum = sum(value_CO2eq)) %>%
      ungroup() %>%
      mutate(unit = "MtCO2eq")-> underdelivering_all_gases_land
    
    #Add all modeled GHGs 2015-2100
    underdelivering_all_gases_land %>%
      group_by(Model_Scenario, model, scenario, region, year, Path, unit, underdelivery_percent) %>%
      summarise(totalGHG = sum(sum)) %>%
      ungroup() %>%
      mutate(totalGHG = totalGHG / 1000,
             unit = "GtCO2eq/yr")-> underdelivering_all_gases_land_total
    #Note, these values are very similar to IPCC https://www.ipcc.ch/report/ar6/wg3/figures/summary-for-policymakers/figure-spm-1/
    
    #### 2. Figure GHGs ####
    # Calculate median total GHG for each year and Path
    plot_data_GHG_land <- underdelivering_all_gases_land_total %>%
      group_by(year, Path, underdelivery_percent) %>%
      summarise(median_GHG = median(totalGHG, na.rm = TRUE),
                P5 = quantile(totalGHG, 0.05, na.rm = TRUE),
                P95 = quantile(totalGHG, 0.95, na.rm = TRUE),.groups = "drop")
    
    for (path in unique_paths) {
      
      plot_subset <- plot_data_GHG_land %>% filter(Path == path)
      
      # Create line plot with facets
      p<-ggplot(plot_subset, aes(x = year, y = median_GHG, group = underdelivery_percent, color = underdelivery_percent)) +
        geom_line(size = 1) +  # Line plot for median
        geom_line(data = history_ar6_fixed_GHG_2010_2015, aes(x = year, y = total_GHG), inherit.aes = FALSE, color = "black" ) +
        geom_hline(yintercept = 0, color = "black") +
        facet_wrap(~ Path) +  # Facet by Path
        labs(title = paste("Median GHG Emissions Across Scenarios", path, "(energy + land)"),
             subtitle = "Harmonized and infilled emissions - underdelivering land CDR",
             x = "Year",
             y = "Total GHG GtCO2eq/yr (Median across scenarios)") +
        scale_color_viridis(option = "turbo") +
        scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
        scale_y_continuous(limits = c(-6, 60), breaks = seq(-6, 60, by = 6)) +
        theme_minimal() +
        figure_theme+
        theme(panel.background = element_rect(fill = "white"),
              plot.background = element_rect(fill = "white"),
              legend.position = "bottom") 
      ggsave(paste0("figures/paper_figures/V3/SM3.underdelivering_landCDR_totalGHG_timeline_", path, ".png"), p, width = 12, height = 12, units = "in", dpi = 600)
      ggsave(paste0("figures/paper_figures/V3/SM3.underdelivering_landCDR_totalGHG_timeline_", path, ".svg"), p, width = 12, height = 12, units = "in", dpi = 600)
      
    }
    
    ### Here calculate percent reductions in emissions per decade
    # Add a decade column
    plot_data_GHG_land <- plot_data_GHG_land %>%
      mutate(decade = floor(year / 10) * 10)
    
    # Filter to start/end years of each decade you want
    target_decades <- c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)
    start_end_years <- plot_data_GHG_land %>%
      filter(year %in% c(target_decades, target_decades + 9))  # e.g., 2020 and 2029
    
    # Calculate CAGR for each decade
    mitigation_rates_land_CDR <- start_end_years %>%
      arrange(Path, underdelivery_percent, year) %>%
      group_by(Path, underdelivery_percent, decade) %>%
      summarise(
        GHG_start = median_GHG[year == decade],
        GHG_end = median_GHG[year == decade + 9],
        mitigation_rate = ifelse(!is.na(GHG_start) & !is.na(GHG_end) & GHG_end > 0,
                                 1 - (GHG_end / GHG_start)^(1 / 9),
                                 NA_real_),
        .groups = "drop"
      ) %>%
      mutate(mitigation_rate = mitigation_rate * 100)
    
    #### 3. Temperature outcomes: underdelivering land CCS only ####
    #These tables have all 407 scenarios, 600 ensembles each, with the temperature in each year, as well as the summary statistics across the 600 ensembles for each scenario
    #Note: for each scenario we calculate the mean temperature increase compared to pre-industrial times
    #Then we estimate the median across the 600 ensemble members for each scenario
    #and then we plot the median for each C category across the 407 scenarios.
    #407 scenarios, 600 ensembles, 101 periods -> 24,664,200
    #We can also read in reduced version with 19 periods to speed up the process -> 4,639,800
    # Define file paths and corresponding run labels
    MAGICC_original<- read_csv( "output/V3/temperature/original/MAGICC_original_temperature_statistics.csv") %>%
      mutate(run = "0%") 
    
    MAGICC_underdeliver_landCDR_10<- read_csv( "output/V3/temperature/underdelivered_landCDR/MAGICC_10percent_temperature_statistics_landCDR.csv") %>%
      mutate(run = "10%") 
    
    MAGICC_underdeliver_landCDR_20<- read_csv( "output/V3/temperature/underdelivered_landCDR/MAGICC_20percent_temperature_statistics_landCDR.csv") %>%
      mutate(run = "20%") 
    
    MAGICC_underdeliver_landCDR_30<- read_csv( "output/V3/temperature/underdelivered_landCDR/MAGICC_30percent_temperature_statistics_landCDR.csv") %>%
      mutate(run = "30%") 
    
    MAGICC_underdeliver_landCDR_40<- read_csv( "output/V3/temperature/underdelivered_landCDR/MAGICC_40percent_temperature_statistics_landCDR.csv") %>%
      mutate(run = "40%") 
    
    MAGICC_underdeliver_landCDR_50<- read_csv( "output/V3/temperature/underdelivered_landCDR/MAGICC_50percent_temperature_statistics_landCDR.csv") %>%
      mutate(run = "50%") 
    
    MAGICC_underdeliver_landCDR_60<- read_csv( "output/V3/temperature/underdelivered_landCDR/MAGICC_60percent_temperature_statistics_landCDR.csv") %>%
      mutate(run = "60%") 
    
    MAGICC_underdeliver_landCDR_70<- read_csv( "output/V3/temperature/underdelivered_landCDR/MAGICC_70percent_temperature_statistics_landCDR.csv") %>%
      mutate(run = "70%") 
    
    MAGICC_underdeliver_landCDR_80<- read_csv( "output/V3/temperature/underdelivered_landCDR/MAGICC_80percent_temperature_statistics_landCDR.csv") %>%
      mutate(run = "80%") 
    
    MAGICC_underdeliver_landCDR_90<- read_csv( "output/V3/temperature/underdelivered_landCDR/MAGICC_90percent_temperature_statistics_landCDR.csv") %>%
      mutate(run = "90%") 
    
    MAGICC_underdeliver_landCDR_100<- read_csv( "output/V3/temperature/underdelivered_landCDR/MAGICC_100percent_temperature_statistics_landCDR.csv") %>%
      mutate(run = "100%")
    
    # List of tables
    MAGICC_tables_landCDR <- list(
      "0%" = MAGICC_original,
      "10%" = MAGICC_underdeliver_landCDR_10,
      "20%" = MAGICC_underdeliver_landCDR_20,
      "30%" = MAGICC_underdeliver_landCDR_30,
      "40%" = MAGICC_underdeliver_landCDR_40,
      "50%" = MAGICC_underdeliver_landCDR_50,
      "60%" = MAGICC_underdeliver_landCDR_60,
      "70%" = MAGICC_underdeliver_landCDR_70,
      "80%" = MAGICC_underdeliver_landCDR_80,
      "90%" = MAGICC_underdeliver_landCDR_90,
      "100%" = MAGICC_underdeliver_landCDR_100
    )
    
    # Initialize an empty list to store results
    results_list <- list()
    
    # Loop through each table, calculate median for the path, and store results
    for (run_label in names(MAGICC_tables_landCDR)) {
      df <- MAGICC_tables_landCDR[[run_label]] %>%
        group_by(year, Path) %>%
        summarise(median_temperature = median(median, na.rm = TRUE), #get the median across C paths (median of median)
                  median_P95 = median(P95, na.rm = TRUE), #Here get the median for each run's 95th percentile
                  .groups = "drop") %>%
        mutate(run = run_label)  # Add run label
      
      results_list[[run_label]] <- df  # Store result
    }
    
    # Combine all results into one master table
    plot_data_temperature_landCDR <- bind_rows(results_list) #101 years (alternatively 19), 11 runs, 3 paths 3,333 rows (alternatively 627)
    #^This is the median of the 600 ensemble (with each ensemble being global mean temperature icnrease compared to pre-industrial),
    #and the median across scenarios
    for (path in unique_paths) {
      
      plot_subset <- plot_data_temperature_landCDR %>% filter(Path == path)
      
      # Create line plot with facets
      p<-ggplot(plot_subset, aes(x = year, y = median_temperature, group = run, color = run)) +
        geom_line(size = 1) +  # Line plot for median
        geom_hline(yintercept = 0, color = "black") +
        facet_wrap(~ Path) +  # Facet by Path
        labs(title = paste("Global Mean Temperature increase vs pre-industrial in", path),
             subtitle = "Median esemble member & median across scenairos (Underdelivering land CDR)",
             x = "Year",
             y = "Mean Global Temperature Increase C") +
        #scale_color_viridis(option = "turbo") +
        scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 5)) +
        scale_y_continuous(limits = c(0, 2.2), breaks = seq(0, 2.2, by = 0.2)) +
        theme_minimal() +
        figure_theme+
        theme(panel.background = element_rect(fill = "white"),
              plot.background = element_rect(fill = "white"),
              legend.position = "bottom") 
      ggsave(paste0("figures/paper_figures/V3/SM3.landCDR_temperature_timeline_", path, "V2.png"), p, width = 12, height = 12, units = "in", dpi = 600)
      ggsave(paste0("figures/paper_figures/V3/SM3.landCDR_temperature_timeline_", path, "V2.svg"), p, width = 12, height = 12, units = "in", dpi = 600)
      
      p<-ggplot(plot_subset, aes(x = year, y = median_P95, group = run, color = run)) +
        geom_line(size = 1) +  # Line plot for median
        geom_hline(yintercept = 0, color = "black") +
        facet_wrap(~ Path) +  # Facet by Path
        labs(title = paste("Global Mean Temperature increase vs pre-industrial in", path),
             subtitle = "P95 esemble member & then median across scenairos (Underdelivering land CDR)",
             x = "Year",
             y = "Mean Global Temperature Increase C") +
        #scale_color_viridis(option = "turbo") +
        scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 5)) +
        scale_y_continuous(limits = c(0, 3.2), breaks = seq(0, 3.2, by = 0.2)) +
        theme_minimal() +
        figure_theme+
        theme(panel.background = element_rect(fill = "white"),
              plot.background = element_rect(fill = "white"),
              legend.position = "bottom") 
      ggsave(paste0("figures/paper_figures/V3/SM3.landCDR_temperature_timeline_", path, "P95_V2.png"), p, width = 12, height = 12, units = "in", dpi = 600)
      ggsave(paste0("figures/paper_figures/V3/SM3.landCDR_temperature_timeline_", path, "P95_V2.svg"), p, width = 12, height = 12, units = "in", dpi = 600)
      
    }
    
    #### 4. Now analyze 50% under-delivered all####
    #This table is for 2000-2014 temperatures (modeled scenarios begin in 2015)
    AR6_data_complete %>%
      filter(Variable == "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile" ) %>%
      gather(year, value, -Model, -Scenario, -Region, -Variable, -Unit) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year >= 2000 & year <= 2014) %>%
      select(-Model, -Scenario) %>%
      distinct()-> historical_temperatures
    
    filtered_median <- MAGICC_underdeliver_landCDR_50 %>% #table only has 50%runs
      group_by(Path, year, run) %>%
      #Get median, min and max across the 407 scenarios using their median ensemble. 
      #So this is the distribution across scenarios for their median ensemble member
      summarize(median_temperature_median = median(median, na.rm = TRUE),
                min_temperature_median = min(median, na.rm = TRUE),
                max_temperature_median = max(median, na.rm = TRUE),
                mean_temperature_median = mean(median, na.rm = TRUE),
                n = (n()/600),
                #For 50% of the data
                P25 = quantile(median, 0.25, na.rm = TRUE),
                P75 = quantile(median, 0.75, na.rm = TRUE),
                #For 90% of data
                P5 = quantile(median, 0.05, na.rm = TRUE),
                P95 = quantile(median, 0.95, na.rm = TRUE),
                #For 70% of data
                P15 = quantile(median, 0.15, na.rm = TRUE),
                P85 = quantile(median, 0.85, na.rm = TRUE)) #Number of scenarios, ignoring ensembles 
    
    #This plot has median, min and max values across the 407 scenarios using their MEDIAN ensemble values
    #The min is the minimum across the medians of the scenarios, which have 600 ensembles.
    #Includes both original runs as well as adjusted runs
    p <- ggplot() +
      geom_ribbon(data = filtered_median %>% filter(run == "50%"), aes(x = year, ymin = P5, ymax = P95, fill = Path), alpha = 0.3) +
      geom_ribbon(data = filtered_median %>% filter(run == "50%"), aes(x = year, ymin = P15, ymax = P85, fill = Path), alpha = 0.4) +
      geom_ribbon(data = filtered_median %>% filter(run == "50%"), aes(x = year, ymin = P25, ymax = P75, fill = Path), alpha = 0.5) +
      
      geom_line(data = filtered_median %>% filter(run == "50%"), aes(x = year, y = median_temperature_median, color = Path, linetype = run), size = 1, linetype = "solid") +
      geom_text(data = filtered_median, aes(x = 2090, y = 4.8, label = paste("n =", n)), 
                size = 5, vjust = 0) +  # Adjust x, y for positioning
      geom_line(data = plot_data_temperature_landCDR %>% filter(run == "100%"), aes(x = year, y = median_temperature, color = Path, linetype = run), size = 0.5, linetype = "dashed") +#This is the median value across each scenario median ensemble
      geom_line(data = plot_data_temperature_landCDR %>% filter(run == "0%"), aes(x = year, y = median_temperature, color = Path, linetype = run), size = 0.5, linetype = "dashed") +#This is the median value across each scenario median ensemble
      geom_line(data = historical_temperatures, aes(x = year, y = value), color = "black", size = 1, linetype = "solid") +
      labs(title = "Temperature increase from AR6 (50% underdelivered land CDR)",
           subtitle = "For median value in ensemble member - P5, P15, P25, median, P75, P85, P95 across scenarios",
           x = "Year",
           y = "Temperature increase",
           color = "Scenario and Model",
           fill = "Scenario and Model") +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      facet_wrap (~ Path) +
      scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
      scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 0.5)) +
      theme_minimal() +
      figure_theme+
      theme(panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            legend.position = "none")  
    ggsave("figures/paper_figures/V3/SM3.landCDR_temperature_spread_50percent_V2.png", p, width = 12, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM3.landCDR_temperature_spread_50percent_V2.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    
    #### 5. Calculate probability exceeding threshold ####
    #Here we calculate the probability of exceeding 1.5C and 2C for each scenario across its 600 ensmebles
    #### First we calculate for 1.5C
    # Define the exceedance threshold
    threshold <- 1.5  
    
    # Initialize an empty list to store exceedance probabilities
    exceedance_prob_list <- list()
    
    # Loop through each table to compute exceedance probabilities
    for (run_label in names(MAGICC_tables_landCDR)) {
      
      df <- MAGICC_tables_landCDR[[run_label]]  # Get current dataset
      
      # Compute exceedance probability
      exceedance_prob <- df %>%
        group_by(Model_Scenario, Path, year) %>%
        summarise(
          exceedance_prob = sum(temperature > threshold) / n(),  # Fraction exceeding threshold
          .groups = "drop"
        ) %>%
        mutate(run = run_label)  # Add run label
      
      exceedance_prob_list[[run_label]] <- exceedance_prob  # Store results
    }
    
    # Combine all results into one master table
    exceedance_prob_1.5 <- bind_rows(exceedance_prob_list) #407 scenarios, 101 periods, 11 runs (452,177 rows)
    
    exceedance_prob_1.5 %>%
      filter(year >= 2010) %>%
      mutate(run = factor(run, levels = underdelivered_order)) -> exceedance_prob_1.5_sort
    
    # Compute median exceedance probability across all Model_Scenarios for each year and Path
    median_exceedance <- exceedance_prob_1.5_sort %>%
      group_by(year, Path, run) %>%
      summarise(median_prob = median(exceedance_prob, na.rm = TRUE), .groups = "drop")
    
    # Get unique Paths
    paths <- unique(exceedance_prob_1.5_sort$Path)
    
    # Loop through each Path and create a single figure with faceted runs
    for (p in paths) {
      # Subset data for the specific Path
      data_subset <- exceedance_prob_1.5_sort %>%
        filter(Path == p)
      
      # Count the number of unique Model_Scenarios represented
      num_scenarios <- data_subset %>% pull(Model_Scenario) %>% unique() %>% length()
      
      # Plot
      p_plot <- ggplot(data_subset, aes(x = year, y = exceedance_prob, group = Model_Scenario, color = Model_Scenario)) +
        geom_line(size = 0.8, alpha = 0.5) +  # Reduce opacity for visibility
        geom_line(data = median_exceedance %>% filter(Path == p), aes(x = year, y = median_prob),
                  inherit.aes = FALSE, color = "black", size = 1) +  # Add median line
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
        facet_wrap(~ run, ncol = 4) +  # Facet by run percentage
        labs(
          title = paste("Underdelivering land CDR - Probability of Exceeding 1.5°C Over Time for Path:", p),
          subtitle = paste("Number of Model Scenarios Represented:", num_scenarios),
          x = "Year",
          y = "Exceedance Probability"
        ) +
        scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, by = 0.2)) +
        scale_color_viridis_d(option = "magma") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggsave(filename = paste0("figures/paper_figures/V3/SM3.landCDR_Exceedance_1.5C_Path_", p, "_V2.png"),plot = p_plot,width = 8, height = 6, dpi = 300)
      print(paste("Saved plot for Path:", p))
    }
    
    #### now try with 2.0C
    threshold <- 2.0
    
    # Initialize an empty list to store exceedance probabilities
    exceedance_prob_list_2.0 <- list()
    
    # Loop through each table to compute exceedance probabilities
    for (run_label in names(MAGICC_tables_landCDR)) {
      
      df <- MAGICC_tables_landCDR[[run_label]]  # Get current dataset
      
      # Compute exceedance probability
      exceedance_prob_2.0 <- df %>%
        group_by(Model_Scenario, Path, year) %>%
        summarise(
          exceedance_prob = sum(temperature > threshold) / n(),  # Fraction exceeding threshold
          .groups = "drop"
        ) %>%
        mutate(run = run_label)  # Add run label
      
      exceedance_prob_list_2.0[[run_label]] <- exceedance_prob_2.0  # Store results
    }
    
    # Combine all results into one master table
    exceedance_prob_2.0 <- bind_rows(exceedance_prob_list_2.0) #407 scenarios, 101 periods, 11 runs (452,177 rows)
    
    exceedance_prob_2.0 %>%
      filter(year >= 2010) %>%
      mutate(run = factor(run, levels = underdelivered_order)) -> exceedance_prob_2.0_sort
    
    # Compute median exceedance probability across all Model_Scenarios for each year and Path
    median_exceedance <- exceedance_prob_2.0_sort %>%
      group_by(year, Path, run) %>%
      summarise(median_prob = median(exceedance_prob, na.rm = TRUE), .groups = "drop")
    
    # Get unique Paths
    paths <- unique(exceedance_prob_2.0_sort$Path)
    
    # Loop through each Path and create a single figure with faceted runs
    for (p in paths) {
      # Subset data for the specific Path
      data_subset <- exceedance_prob_2.0_sort %>%
        filter(Path == p)
      
      # Count the number of unique Model_Scenarios represented
      num_scenarios <- data_subset %>% pull(Model_Scenario) %>% unique() %>% length()
      
      # Plot
      p_plot <- ggplot(data_subset, aes(x = year, y = exceedance_prob, group = Model_Scenario, color = Model_Scenario)) +
        geom_line(size = 0.8, alpha = 0.5) +  # Reduce opacity for visibility
        geom_line(data = median_exceedance %>% filter(Path == p), aes(x = year, y = median_prob),
                  inherit.aes = FALSE, color = "black", size = 1) +  # Add median line
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
        facet_wrap(~ run, ncol = 4) +  # Facet by run percentage
        labs(
          title = paste("Underdelivering land CDR - Probability of Exceeding 2.0°C Over Time for Path:", p),
          subtitle = paste("Number of Model Scenarios Represented:", num_scenarios),
          x = "Year",
          y = "Exceedance Probability"
        ) +
        scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, by = 0.2)) +
        scale_color_viridis_d(option = "magma") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggsave(filename = paste0("figures/paper_figures/V3/SM3.landCDR_Exceedance_2.0C_Path_", p, "_V2.png"),plot = p_plot,width = 8, height = 6, dpi = 300)
      print(paste("Saved plot for Path:", p))
    }
    
    ### Now prepare actual insert figure
    exceedance_prob_2.0_sort %>%
      rename(exceedance_prob_2p0 = exceedance_prob) %>%
      left_join(exceedance_prob_1.5_sort, by = c("Model_Scenario", "Path", "run", "year")) %>%
      rename(exceedance_prob_1p5 = exceedance_prob) %>%
      group_by(Path, year, run) %>%
      summarise(median_prob_1p5 = median(exceedance_prob_1p5),
                median_prob_2p0 = median(exceedance_prob_2p0), .groups = "drop") -> exceedance_probability_median_complete
    
    p <- ggplot(exceedance_probability_median_complete %>% filter(run == "50%"), 
                aes(x = year, y = median_prob_2p0, color = Path, group = interaction(Path, run))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      # 2.0C exceedance probability lines (colored by Path)
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "100%"), 
                aes(x = year, y = median_prob_2p0, color = Path, group = interaction(Path, run)), 
                linetype = "dashed")  +  
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "0%"), 
                aes(x = year, y = median_prob_2p0, color = Path, group = interaction(Path, run)), 
                linetype = "dotted") +
      # 1.5C exceedance probability lines (set custom colors)
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "100%"), 
                aes(x = year, y = median_prob_1p5, group = interaction(Path, run)), 
                color = "black", linetype = "dashed") +  
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "0%"), 
                aes(x = year, y = median_prob_1p5, group = interaction(Path, run)), 
                color = "black", linetype = "dotted") +
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "50%"), 
                aes(x = year, y = median_prob_1p5, group = interaction(Path, run)), 
                color = "black", linetype = "solid") +
      geom_line(aes(y = 0.5), color = "black", size = 0.5, linetype = "dashed") +  
      labs(
        title = "Underdelivering land CDR - Probability of exceeding 1.5C and 2C (median across scenarios)",
        subtitle = "Showing 50% underdelivering (solid), 100% underdelivering (dashed), 0% underdelivered (dotted),
        color for 2.0C and black for 1.5C",
        x = "Year",
        y = "Probability of exceeding threshold"
      ) +
      facet_wrap(~Path) +
      scale_color_manual(values = colors) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
      figure_theme 
    ggsave("figures/paper_figures/V3/SM3.landCDR_exceedance_probability_2C_1p5C_V2.png", p, width = 12, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM3.landCDR_exceedance_probability_2C_1p5C_V2.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    
    
    #### 6. Emissions relative to original pathways ####
    # Calculate median total GHG for each year and Path
    underdelivering_all_gases_land_total %>%
      group_by(year, Path, underdelivery_percent) %>%
      summarise(median_GHG = median(totalGHG, na.rm = TRUE), .groups = "drop") -> plot_data
    
    plot_data %>%
      filter(Path == "C1",
             underdelivery_percent == "0") %>%
      select(-Path, -underdelivery_percent) %>%
      rename(median_GHG_C1_original = median_GHG)-> plot_data_C1_original 
    
    plot_data %>%
      filter(Path == "C2",
             underdelivery_percent == "0") %>%
      select(-Path, -underdelivery_percent) %>%
      rename(median_GHG_C2_original = median_GHG)-> plot_data_C2_original 
    
    plot_data %>%
      filter(Path == "C3",
             underdelivery_percent == "0") %>%
      select(-Path, -underdelivery_percent) %>%
      rename(median_GHG_C3_original = median_GHG)-> plot_data_C3_original 
    #^This has the CO2 emissions in median 2C scenarios with )% under-delivering
    
    #Join all
    plot_data %>%
      left_join(plot_data_C1_original, by = "year") %>%
      left_join(plot_data_C2_original, by = "year") %>%
      left_join(plot_data_C3_original, by = "year") %>%
      mutate(difference = if_else(Path == "C1", median_GHG - median_GHG_C1_original,
                                  if_else(Path == "C2", median_GHG - median_GHG_C2_original, 
                                          median_GHG - median_GHG_C3_original)
      )) -> underdelivering_all_GHG_differences
    
    #Plot all
    ggplot(underdelivering_all_GHG_differences, aes(x = year, y = difference, color = Path, group = interaction(Path, underdelivery_percent))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      geom_line(aes(y = 0), color = "black", size = 1, linetype = "solid") +  # Baseline as y=1
      labs(
        title = "Difference in GHG emissions relative to 0% underdelivering",
        subtitle = "Each line is an underdelivering % relative to its category (c1, c2, c3)",
        x = "Year",
        y = "Difference Emissions in GtCO2eq (underdelivered run - original run)"
      ) +
      facet_wrap( ~Path) +
      scale_color_manual(values = colors) +
      # scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    #Plot only 50% and 100%
    p<-ggplot(underdelivering_all_GHG_differences %>% filter(underdelivery_percent == "50"), aes(x = year, y = difference, color = Path, group = interaction(Path, underdelivery_percent))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      geom_line(data = underdelivering_all_GHG_differences %>% filter(underdelivery_percent == "100"), aes(x = year, y = difference, color = Path, group = interaction(Path, underdelivery_percent)), linetype = "dashed") +  # Lines for relative emissions in each scenario
      geom_line(aes(y = 0), color = "black", size = 0.5, linetype = "solid") +  # Baseline as y=1
      labs(
        title = "Underdelivering land CDR - Difference in GHG emissions relative to 0% underdelivering",
        subtitle = "Each line is an underdelivering 50% (and 100% dashed) relative to its category (c1, c2, c3)",
        x = "Year",
        y = "Difference Emissions in GtCO2eq (underdelivered run - original run)"
      ) +
      scale_color_manual(values = colors) +
      scale_y_continuous(limits = c(0, 21), breaks = seq(0, 21, by = 5)) +
      # scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(legend.position = "bottom")
    ggsave("figures/paper_figures/V3/SM3.landCDR_GHGemissions_difference.png", p, width = 12, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM3.landCDR_GHGemissions_difference.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    
    #### 7. Temperature relative to 2C original ####
    # Initialize an empty list to store results
    filtered_median_list <- list()
    
    # Loop through each table and compute summary statistics
    for (run_label in names(MAGICC_tables_landCDR)) {
      
      df <- MAGICC_tables_landCDR[[run_label]]  # Get current dataset
      
      summary_stats <- df %>%
        group_by(Path, year) %>%
        summarize(
          median_temperature_median = median(median, na.rm = TRUE),
          min_temperature_median = min(median, na.rm = TRUE),
          max_temperature_median = max(median, na.rm = TRUE),
          mean_temperature_median = mean(median, na.rm = TRUE),
          n = (n() / 600),
          P25 = quantile(median, 0.25, na.rm = TRUE),
          P75 = quantile(median, 0.75, na.rm = TRUE),
          P5 = quantile(median, 0.05, na.rm = TRUE),
          P95 = quantile(median, 0.95, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(run = run_label)  # Add run label
      
      filtered_median_list[[run_label]] <- summary_stats  # Store result
    }
    
    # Combine all runs into one master table
    filtered_median <- bind_rows(filtered_median_list) #3 paths, 101 years, 11 runs
    
    # Compute the reference temperature for "0%" run (original scenario)
    filtered_median_original <- filtered_median %>%
      filter(run == "0%") %>%
      select(Path, year, median_temperature_median) %>%
      spread(Path, median_temperature_median)
    
    # Join the original values with all runs and compute the temperature difference
    underdelivering_all_temperature_differences <- filtered_median %>%
      select(Path, year, run, median_temperature_median) %>%
      left_join(filtered_median_original, by = "year") %>%
      mutate(difference = if_else(Path == "C1", median_temperature_median - C1,
                                  if_else(Path == "C2", median_temperature_median - C2,
                                          median_temperature_median - C3)))
    
    #Plot only 50% and 100%
    p<-ggplot(underdelivering_all_temperature_differences %>% filter(run == "50%"), aes(x = year, y = difference, color = Path, group = interaction(Path, run))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      geom_line(data = underdelivering_all_temperature_differences %>% filter(run == "100%"), aes(x = year, y = difference, color = Path, group = interaction(Path, run)), linetype = "dashed") +  # Lines for relative emissions in each scenario
      geom_line(aes(y = 0), color = "black", size = 0.5, linetype = "solid") +  # Baseline as y=1
      labs(
        title = "underdelivering land CDR - Difference in GMT increase relative to 0% underdelivering",
        subtitle = "Each line is an underdelivering 50% (and 100%) relative to its category (c1, c2, c3)",
        x = "Year",
        y = "Difference GMT increase in C (underdelivered run - original run)"
      ) +
      scale_color_manual(values = colors) +
      scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, by = 0.1)) +
      scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
      # scale_fill_manual(values = colors) +
      theme_minimal() +
      figure_theme 
    ggsave("figures/paper_figures/V3/SM3.landCDR_temperature_difference.png", p, width = 12, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM3.landCDR_temperature_difference.svg", p, width = 12, height = 12, units = "in", dpi = 600)

    
#-------------------------------------------------- SUPPLEMENTARY FIGURE 4 -----------------------------------------------------------------------
    #Supplementary Figure 4 shows total GHG/CO2 emissions and temperature outcomes from original runs and under-delivering 
    #engineered/novel CDR only 10-100%
    #Read in GWP from AR6 available here: https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_WGI_Chapter07_SM.pdf
    #Table 7.SM.7 | Greenhouse gas lifetimes, radiative efficiencies, global warming potentials (GWPs), global temperature potentials (GTPs) and cumulative global temperature potentials (CGTPs).
    AR6_GWP <- read_csv("/Users/mariacandelariabergero/Library/CloudStorage/GoogleDrive-cande.bergero@gmail.com/My Drive/Education/2021 (UCI)/Research/IIASA/data/GWP_ar6.csv", skip = 1)
    
    #### 1. Prepare data ####
    original_emissions <- read_csv("/Users/mariacandelariabergero/python/gegca-prototype/data/raw/interim/15849b8/V3/original/scenarios-infilled_all.csv") %>% mutate(underdelivery_percent = 0)
    
    # Define the base file path
    base_path <- "/Users/mariacandelariabergero/python/gegca-prototype/data/raw/interim/15849b8/V3/underdeliver_novelCDR/"
    
    # Define percentages (10 to 100 in steps of 10)
    percentages <- seq(10, 100, by = 10)
    
    # Read in all files and combine them
    all_data_novelCDR <- map_dfr(percentages, function(pct) {
      file_path_novelCDR <- paste0(base_path, pct, "/scenarios-infilled_all_underdelivered_", pct, "percent.csv")
      # Read file and add a column to track the percentage
      read_csv(file_path_novelCDR) %>%
        mutate(underdelivery_percent = pct)
    })
    
    #Merge all data 2015-2100
    original_emissions %>% #Original emissions
      bind_rows(all_data_novelCDR) %>% #under-delivering 10-100% every 10%
      gather(year, value, -model, -scenario, -region, -variable, -unit, -underdelivery_percent) %>%
      mutate(year = substr(year, 1, 4)) %>% #clean year column
      mutate(year = as.numeric(year)) %>%
      left_join(C_paths, by = c("scenario" = "Scenario", "model" = "Model")) %>%
      unite(Model_Scenario, c(model, scenario), sep = "_", remove = FALSE) %>%
      left_join(AR6_GWP, by = "variable")%>% #Bring in multipliers GWP 100 years from IPCC AR6
      filter(!is.na(GWP_100))%>% #Filter out BC, CO, NH3, NOx, OC, SUlfur and VOC (not GHGs)
      select(-Notes) %>%
      mutate(value = ifelse(grepl("kt", unit), value / 1000, value)) %>% #Convert gases in kilo ton to Mega ton
      mutate(value_CO2eq = value * GWP_100) %>%
      group_by(Model_Scenario, model, scenario, region, year, Path, GHG, underdelivery_percent) %>%
      summarise(sum = sum(value_CO2eq)) %>%
      ungroup() %>%
      mutate(unit = "MtCO2eq")-> underdelivering_all_gases_novel
    
    #Add all modeled GHGs 2015-2100
    underdelivering_all_gases_novel %>%
      group_by(Model_Scenario, model, scenario, region, year, Path, unit, underdelivery_percent) %>%
      summarise(totalGHG = sum(sum)) %>%
      ungroup() %>%
      mutate(totalGHG = totalGHG / 1000,
             unit = "GtCO2eq/yr")-> underdelivering_all_gases_novel_total
    #Note, these values are very similar to IPCC https://www.ipcc.ch/report/ar6/wg3/figures/summary-for-policymakers/figure-spm-1/
    
    #### 2. Figure GHGs ####
    # Calculate median total GHG for each year and Path
    plot_data_GHG_novel <- underdelivering_all_gases_novel_total %>%
      group_by(year, Path, underdelivery_percent) %>%
      summarise(median_GHG = median(totalGHG, na.rm = TRUE),
                P5 = quantile(totalGHG, 0.05, na.rm = TRUE),
                P95 = quantile(totalGHG, 0.95, na.rm = TRUE),.groups = "drop")
    
    for (path in unique_paths) {
      
      plot_subset <- plot_data_GHG_novel %>% filter(Path == path)
      
      # Create line plot with facets
      p<-ggplot(plot_subset, aes(x = year, y = median_GHG, group = underdelivery_percent, color = underdelivery_percent)) +
        geom_line(size = 1) +  # Line plot for median
        geom_line(data = history_ar6_fixed_GHG_2010_2015, aes(x = year, y = total_GHG), inherit.aes = FALSE, color = "black" ) +
        geom_hline(yintercept = 0, color = "black") +
        facet_wrap(~ Path) +  # Facet by Path
        labs(title = paste("Median GHG Emissions Across Scenarios", path, "(energy + land)"),
             subtitle = "Harmonized and infilled emissions - underdelivering novel CDR",
             x = "Year",
             y = "Total GHG GtCO2eq/yr (Median across scenarios)") +
        scale_color_viridis(option = "turbo") +
        scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
        scale_y_continuous(limits = c(-6, 60), breaks = seq(-6, 60, by = 6)) +
        theme_minimal() +
        figure_theme+
        theme(panel.background = element_rect(fill = "white"),
              plot.background = element_rect(fill = "white"),
              legend.position = "bottom") 
      ggsave(paste0("figures/paper_figures/V3/SM4.underdelivering_novelCDR_totalGHG_timeline_", path, ".png"), p, width = 12, height = 12, units = "in", dpi = 600)
      ggsave(paste0("figures/paper_figures/V3/SM4.underdelivering_novelCDR_totalGHG_timeline_", path, ".svg"), p, width = 12, height = 12, units = "in", dpi = 600)
      
    }
    
    ### Here calculate percent reductions in emissions per decade
    # Add a decade column
    plot_data_GHG_novel <- plot_data_GHG_novel %>%
      mutate(decade = floor(year / 10) * 10)
    
    # Filter to start/end years of each decade you want
    target_decades <- c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)
    start_end_years <- plot_data_GHG_novel %>%
      filter(year %in% c(target_decades, target_decades + 9))  # e.g., 2020 and 2029
    
    # Calculate CAGR for each decade
    mitigation_rates_novel_CDR <- start_end_years %>%
      arrange(Path, underdelivery_percent, year) %>%
      group_by(Path, underdelivery_percent, decade) %>%
      summarise(
        GHG_start = median_GHG[year == decade],
        GHG_end = median_GHG[year == decade + 9],
        mitigation_rate = ifelse(!is.na(GHG_start) & !is.na(GHG_end) & GHG_end > 0,
                                 1 - (GHG_end / GHG_start)^(1 / 9),
                                 NA_real_),
        .groups = "drop"
      ) %>%
      mutate(mitigation_rate = mitigation_rate * 100)
    
    #### 3. Temperature outcomes: underdelivering novel CCS only ####
    #These tables have all 407 scenarios, 600 ensembles each, with the temperature in each year, as well as the summary statistics across the 600 ensembles for each scenario
    #Note: for each scenario we calculate the mean temperature increase compared to pre-industrial times
    #Then we estimate the median across the 600 ensemble members for each scenario
    #and then we plot the median for each C category across the 407 scenarios.
    #407 scenarios, 600 ensembles, 101 periods -> 24,664,200
    #We can also read in reduced version with 19 periods to speed up the process -> 4,639,800
    # Define file paths and corresponding run labels
    MAGICC_original<- read_csv( "output/V3/temperature/original/MAGICC_original_temperature_statistics.csv") %>%
      mutate(run = "0%") 
    
    MAGICC_underdeliver_novelCDR_10<- read_csv( "output/V3/temperature/underdelivered_novelCDR/MAGICC_10percent_temperature_statistics.csv") %>%
      mutate(run = "10%") 
    
    MAGICC_underdeliver_novelCDR_20<- read_csv( "output/V3/temperature/underdelivered_novelCDR/MAGICC_20percent_temperature_statistics.csv") %>%
      mutate(run = "20%") 
    
    MAGICC_underdeliver_novelCDR_30<- read_csv( "output/V3/temperature/underdelivered_novelCDR/MAGICC_30percent_temperature_statistics.csv") %>%
      mutate(run = "30%") 
    
    MAGICC_underdeliver_novelCDR_40<- read_csv( "output/V3/temperature/underdelivered_novelCDR/MAGICC_40percent_temperature_statistics.csv") %>%
      mutate(run = "40%") 
    
    MAGICC_underdeliver_novelCDR_50<- read_csv( "output/V3/temperature/underdelivered_novelCDR/MAGICC_50percent_temperature_statistics.csv") %>%
      mutate(run = "50%") 
    
    MAGICC_underdeliver_novelCDR_60<- read_csv( "output/V3/temperature/underdelivered_novelCDR/MAGICC_60percent_temperature_statistics.csv") %>%
      mutate(run = "60%") 
    
    MAGICC_underdeliver_novelCDR_70<- read_csv( "output/V3/temperature/underdelivered_novelCDR/MAGICC_70percent_temperature_statistics.csv") %>%
      mutate(run = "70%") 
    
    MAGICC_underdeliver_novelCDR_80<- read_csv( "output/V3/temperature/underdelivered_novelCDR/MAGICC_80percent_temperature_statistics.csv") %>%
      mutate(run = "80%") 
    
    MAGICC_underdeliver_novelCDR_90<- read_csv( "output/V3/temperature/underdelivered_novelCDR/MAGICC_90percent_temperature_statistics.csv") %>%
      mutate(run = "90%") 
    
    MAGICC_underdeliver_novelCDR_100<- read_csv( "output/V3/temperature/underdelivered_novelCDR/MAGICC_100percent_temperature_statistics.csv") %>%
      mutate(run = "100%")
    
    # List of tables
    MAGICC_tables_novelCDR <- list(
      "0%" = MAGICC_original,
      "10%" = MAGICC_underdeliver_novelCDR_10,
      "20%" = MAGICC_underdeliver_novelCDR_20,
      "30%" = MAGICC_underdeliver_novelCDR_30,
      "40%" = MAGICC_underdeliver_novelCDR_40,
      "50%" = MAGICC_underdeliver_novelCDR_50,
      "60%" = MAGICC_underdeliver_novelCDR_60,
      "70%" = MAGICC_underdeliver_novelCDR_70,
      "80%" = MAGICC_underdeliver_novelCDR_80,
      "90%" = MAGICC_underdeliver_novelCDR_90,
      "100%" = MAGICC_underdeliver_novelCDR_100
    )
    
    # Initialize an empty list to store results
    results_list <- list()
    
    # Loop through each table, calculate median for the path, and store results
    for (run_label in names(MAGICC_tables_novelCDR)) {
      df <- MAGICC_tables_novelCDR[[run_label]] %>%
        group_by(year, Path) %>%
        summarise(median_temperature = median(median, na.rm = TRUE), #get the median across C paths (median of median)
                  median_P95 = median(P95, na.rm = TRUE), #Here get the median for each run's 95th percentile
                  .groups = "drop") %>%
        mutate(run = run_label)  # Add run label
      
      results_list[[run_label]] <- df  # Store result
    }
    
    # Combine all results into one master table
    plot_data_temperature_novelCDR <- bind_rows(results_list) #101 years (alternatively 19), 11 runs, 3 paths 3,333 rows (alternatively 627)
    #^This is the median of the 600 ensemble (with each ensemble being global mean temperature icnrease compared to pre-industrial),
    #and the median across scenarios
    for (path in unique_paths) {
      
      plot_subset <- plot_data_temperature_novelCDR %>% filter(Path == path)
      
      # Create line plot with facets
      p<-ggplot(plot_subset, aes(x = year, y = median_temperature, group = run, color = run)) +
        geom_line(size = 1) +  # Line plot for median
        geom_hline(yintercept = 0, color = "black") +
        facet_wrap(~ Path) +  # Facet by Path
        labs(title = paste("Global Mean Temperature increase vs pre-industrial in", path),
             subtitle = "Median esemble member & median across scenairos (Underdelivering novel CDR)",
             x = "Year",
             y = "Mean Global Temperature Increase C") +
        #scale_color_viridis(option = "turbo") +
        scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 5)) +
        scale_y_continuous(limits = c(0, 2.2), breaks = seq(0, 2.2, by = 0.2)) +
        theme_minimal() +
        figure_theme+
        theme(panel.background = element_rect(fill = "white"),
              plot.background = element_rect(fill = "white"),
              legend.position = "bottom") 
      ggsave(paste0("figures/paper_figures/V3/SM4.novelCDR_temperature_timeline_", path, "V2.png"), p, width = 12, height = 12, units = "in", dpi = 600)
      ggsave(paste0("figures/paper_figures/V3/SM4.novelCDR_temperature_timeline_", path, "V2.svg"), p, width = 12, height = 12, units = "in", dpi = 600)
      
      p<-ggplot(plot_subset, aes(x = year, y = median_P95, group = run, color = run)) +
        geom_line(size = 1) +  # Line plot for median
        geom_hline(yintercept = 0, color = "black") +
        facet_wrap(~ Path) +  # Facet by Path
        labs(title = paste("Global Mean Temperature increase vs pre-industrial in", path),
             subtitle = "P95 esemble member & then median across scenairos (Underdelivering novel CDR)",
             x = "Year",
             y = "Mean Global Temperature Increase C") +
        #scale_color_viridis(option = "turbo") +
        scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 5)) +
        scale_y_continuous(limits = c(0, 3.2), breaks = seq(0, 3.2, by = 0.2)) +
        theme_minimal() +
        figure_theme+
        theme(panel.background = element_rect(fill = "white"),
              plot.background = element_rect(fill = "white"),
              legend.position = "bottom") 
      ggsave(paste0("figures/paper_figures/V3/SM4.novelCDR_temperature_timeline_", path, "P95_V2.png"), p, width = 12, height = 12, units = "in", dpi = 600)
      ggsave(paste0("figures/paper_figures/V3/SM4.novelCDR_temperature_timeline_", path, "P95_V2.svg"), p, width = 12, height = 12, units = "in", dpi = 600)
      
    }
    
    #### 4. Now analyze 50% under-delivered all####
    #This table is for 2000-2014 temperatures (modeled scenarios begin in 2015)
    AR6_data_complete %>%
      filter(Variable == "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile" ) %>%
      gather(year, value, -Model, -Scenario, -Region, -Variable, -Unit) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year >= 2000 & year <= 2014) %>%
      select(-Model, -Scenario) %>%
      distinct()-> historical_temperatures
    
    filtered_median <- MAGICC_underdeliver_novelCDR_50 %>% #table only has 50%runs
      group_by(Path, year, run) %>%
      #Get median, min and max across the 407 scenarios using their median ensemble. 
      #So this is the distribution across scenarios for their median ensemble member
      summarize(median_temperature_median = median(median, na.rm = TRUE),
                min_temperature_median = min(median, na.rm = TRUE),
                max_temperature_median = max(median, na.rm = TRUE),
                mean_temperature_median = mean(median, na.rm = TRUE),
                n = (n()/600),
                #For 50% of the data
                P25 = quantile(median, 0.25, na.rm = TRUE),
                P75 = quantile(median, 0.75, na.rm = TRUE),
                #For 90% of data
                P5 = quantile(median, 0.05, na.rm = TRUE),
                P95 = quantile(median, 0.95, na.rm = TRUE),
                #For 70% of data
                P15 = quantile(median, 0.15, na.rm = TRUE),
                P85 = quantile(median, 0.85, na.rm = TRUE)) #Number of scenarios, ignoring ensembles 
    
    #This plot has median, min and max values across the 407 scenarios using their MEDIAN ensemble values
    #The min is the minimum across the medians of the scenarios, which have 600 ensembles.
    #Includes both original runs as well as adjusted runs
    p <- ggplot() +
      geom_ribbon(data = filtered_median %>% filter(run == "50%"), aes(x = year, ymin = P5, ymax = P95, fill = Path), alpha = 0.3) +
      geom_ribbon(data = filtered_median %>% filter(run == "50%"), aes(x = year, ymin = P15, ymax = P85, fill = Path), alpha = 0.4) +
      geom_ribbon(data = filtered_median %>% filter(run == "50%"), aes(x = year, ymin = P25, ymax = P75, fill = Path), alpha = 0.5) +
      
      geom_line(data = filtered_median %>% filter(run == "50%"), aes(x = year, y = median_temperature_median, color = Path, linetype = run), size = 1, linetype = "solid") +
      geom_text(data = filtered_median, aes(x = 2090, y = 4.8, label = paste("n =", n)), 
                size = 5, vjust = 0) +  # Adjust x, y for positioning
      geom_line(data = plot_data_temperature_novelCDR %>% filter(run == "100%"), aes(x = year, y = median_temperature, color = Path, linetype = run), size = 0.5, linetype = "dashed") +#This is the median value across each scenario median ensemble
      geom_line(data = plot_data_temperature_novelCDR %>% filter(run == "0%"), aes(x = year, y = median_temperature, color = Path, linetype = run), size = 0.5, linetype = "dashed") +#This is the median value across each scenario median ensemble
      geom_line(data = historical_temperatures, aes(x = year, y = value), color = "black", size = 1, linetype = "solid") +
      labs(title = "Temperature increase from AR6 (50% underdelivered novel CDR)",
           subtitle = "For median value in ensemble member - P5, P15, P25, median, P75, P85, P95 across scenarios",
           x = "Year",
           y = "Temperature increase",
           color = "Scenario and Model",
           fill = "Scenario and Model") +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      facet_wrap (~ Path) +
      scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
      scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 0.5)) +
      theme_minimal() +
      figure_theme+
      theme(panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            legend.position = "none")  
    ggsave("figures/paper_figures/V3/SM4.novelCDR_temperature_spread_50percent_V2.png", p, width = 12, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM4.novelCDR_temperature_spread_50percent_V2.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    
    #### 5. Calculate probability exceeding threshold ####
    #Here we calculate the probability of exceeding 1.5C and 2C for each scenario across its 600 ensmebles
    #### First we calculate for 1.5C
    # Define the exceedance threshold
    threshold <- 1.5  
    
    # Initialize an empty list to store exceedance probabilities
    exceedance_prob_list <- list()
    
    # Loop through each table to compute exceedance probabilities
    for (run_label in names(MAGICC_tables_novelCDR)) {
      
      df <- MAGICC_tables_novelCDR[[run_label]]  # Get current dataset
      
      # Compute exceedance probability
      exceedance_prob <- df %>%
        group_by(Model_Scenario, Path, year) %>%
        summarise(
          exceedance_prob = sum(temperature > threshold) / n(),  # Fraction exceeding threshold
          .groups = "drop"
        ) %>%
        mutate(run = run_label)  # Add run label
      
      exceedance_prob_list[[run_label]] <- exceedance_prob  # Store results
    }
    
    # Combine all results into one master table
    exceedance_prob_1.5 <- bind_rows(exceedance_prob_list) #407 scenarios, 101 periods, 11 runs (452,177 rows)
    
    exceedance_prob_1.5 %>%
      filter(year >= 2010) %>%
      mutate(run = factor(run, levels = underdelivered_order)) -> exceedance_prob_1.5_sort
    
    # Compute median exceedance probability across all Model_Scenarios for each year and Path
    median_exceedance <- exceedance_prob_1.5_sort %>%
      group_by(year, Path, run) %>%
      summarise(median_prob = median(exceedance_prob, na.rm = TRUE), .groups = "drop")
    
    # Get unique Paths
    paths <- unique(exceedance_prob_1.5_sort$Path)
    
    # Loop through each Path and create a single figure with faceted runs
    for (p in paths) {
      # Subset data for the specific Path
      data_subset <- exceedance_prob_1.5_sort %>%
        filter(Path == p)
      
      # Count the number of unique Model_Scenarios represented
      num_scenarios <- data_subset %>% pull(Model_Scenario) %>% unique() %>% length()
      
      # Plot
      p_plot <- ggplot(data_subset, aes(x = year, y = exceedance_prob, group = Model_Scenario, color = Model_Scenario)) +
        geom_line(size = 0.8, alpha = 0.5) +  # Reduce opacity for visibility
        geom_line(data = median_exceedance %>% filter(Path == p), aes(x = year, y = median_prob),
                  inherit.aes = FALSE, color = "black", size = 1) +  # Add median line
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
        facet_wrap(~ run, ncol = 4) +  # Facet by run percentage
        labs(
          title = paste("Underdelivering novel CDR - Probability of Exceeding 1.5°C Over Time for Path:", p),
          subtitle = paste("Number of Model Scenarios Represented:", num_scenarios),
          x = "Year",
          y = "Exceedance Probability"
        ) +
        scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, by = 0.2)) +
        scale_color_viridis_d(option = "magma") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggsave(filename = paste0("figures/paper_figures/V3/SM4.novelCDR_Exceedance_1.5C_Path_", p, "_V2.png"),plot = p_plot,width = 8, height = 6, dpi = 300)
      ggsave(filename = paste0("figures/paper_figures/V3/SM4.novelCDR_Exceedance_1.5C_Path_", p, "_V2.svg"),plot = p_plot,width = 8, height = 6, dpi = 300)
      print(paste("Saved plot for Path:", p))
    }
    
    #### now try with 2.0C
    threshold <- 2.0
    
    # Initialize an empty list to store exceedance probabilities
    exceedance_prob_list_2.0 <- list()
    
    # Loop through each table to compute exceedance probabilities
    for (run_label in names(MAGICC_tables_novelCDR)) {
      
      df <- MAGICC_tables_novelCDR[[run_label]]  # Get current dataset
      
      # Compute exceedance probability
      exceedance_prob_2.0 <- df %>%
        group_by(Model_Scenario, Path, year) %>%
        summarise(
          exceedance_prob = sum(temperature > threshold) / n(),  # Fraction exceeding threshold
          .groups = "drop"
        ) %>%
        mutate(run = run_label)  # Add run label
      
      exceedance_prob_list_2.0[[run_label]] <- exceedance_prob_2.0  # Store results
    }
    
    # Combine all results into one master table
    exceedance_prob_2.0 <- bind_rows(exceedance_prob_list_2.0) #407 scenarios, 101 periods, 11 runs (452,177 rows)
    
    exceedance_prob_2.0 %>%
      filter(year >= 2010) %>%
      mutate(run = factor(run, levels = underdelivered_order)) -> exceedance_prob_2.0_sort
    
    # Compute median exceedance probability across all Model_Scenarios for each year and Path
    median_exceedance <- exceedance_prob_2.0_sort %>%
      group_by(year, Path, run) %>%
      summarise(median_prob = median(exceedance_prob, na.rm = TRUE), .groups = "drop")
    
    # Get unique Paths
    paths <- unique(exceedance_prob_2.0_sort$Path)
    
    # Loop through each Path and create a single figure with faceted runs
    for (p in paths) {
      # Subset data for the specific Path
      data_subset <- exceedance_prob_2.0_sort %>%
        filter(Path == p)
      
      # Count the number of unique Model_Scenarios represented
      num_scenarios <- data_subset %>% pull(Model_Scenario) %>% unique() %>% length()
      
      # Plot
      p_plot <- ggplot(data_subset, aes(x = year, y = exceedance_prob, group = Model_Scenario, color = Model_Scenario)) +
        geom_line(size = 0.8, alpha = 0.5) +  # Reduce opacity for visibility
        geom_line(data = median_exceedance %>% filter(Path == p), aes(x = year, y = median_prob),
                  inherit.aes = FALSE, color = "black", size = 1) +  # Add median line
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
        facet_wrap(~ run, ncol = 4) +  # Facet by run percentage
        labs(
          title = paste("Underdelivering novel CDR - Probability of Exceeding 2.0°C Over Time for Path:", p),
          subtitle = paste("Number of Model Scenarios Represented:", num_scenarios),
          x = "Year",
          y = "Exceedance Probability"
        ) +
        scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, by = 0.2)) +
        scale_color_viridis_d(option = "magma") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggsave(filename = paste0("figures/paper_figures/V3/SM4.novelCDR_Exceedance_2.0C_Path_", p, "_V2.png"),plot = p_plot,width = 8, height = 6, dpi = 300)
      ggsave(filename = paste0("figures/paper_figures/V3/SM4.novelCDR_Exceedance_2.0C_Path_", p, "_V2.svg"),plot = p_plot,width = 8, height = 6, dpi = 300)
      print(paste("Saved plot for Path:", p))
    }
    
    ### Now prepare actual insert figure
    exceedance_prob_2.0_sort %>%
      rename(exceedance_prob_2p0 = exceedance_prob) %>%
      left_join(exceedance_prob_1.5_sort, by = c("Model_Scenario", "Path", "run", "year")) %>%
      rename(exceedance_prob_1p5 = exceedance_prob) %>%
      group_by(Path, year, run) %>%
      summarise(median_prob_1p5 = median(exceedance_prob_1p5),
                median_prob_2p0 = median(exceedance_prob_2p0), .groups = "drop") -> exceedance_probability_median_complete
    
    p <- ggplot(exceedance_probability_median_complete %>% filter(run == "50%"), 
                aes(x = year, y = median_prob_2p0, color = Path, group = interaction(Path, run))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      # 2.0C exceedance probability lines (colored by Path)
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "100%"), 
                aes(x = year, y = median_prob_2p0, color = Path, group = interaction(Path, run)), 
                linetype = "dashed")  +  
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "0%"), 
                aes(x = year, y = median_prob_2p0, color = Path, group = interaction(Path, run)), 
                linetype = "dotted") +
      # 1.5C exceedance probability lines (set custom colors)
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "100%"), 
                aes(x = year, y = median_prob_1p5, group = interaction(Path, run)), 
                color = "black", linetype = "dashed") +  
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "0%"), 
                aes(x = year, y = median_prob_1p5, group = interaction(Path, run)), 
                color = "black", linetype = "dotted") +
      geom_line(data = exceedance_probability_median_complete %>% filter(run == "50%"), 
                aes(x = year, y = median_prob_1p5, group = interaction(Path, run)), 
                color = "black", linetype = "solid") +
      geom_line(aes(y = 0.5), color = "black", size = 0.5, linetype = "dashed") +  
      labs(
        title = "Underdelivering novel CDR - Probability of exceeding 1.5C and 2C (median across scenarios)",
        subtitle = "Showing 50% underdelivering (solid), 100% underdelivering (dashed), 0% underdelivered (dotted),
        color for 2.0C and black for 1.5C",
        x = "Year",
        y = "Probability of exceeding threshold"
      ) +
      facet_wrap(~Path) +
      scale_color_manual(values = colors) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
      figure_theme 
    ggsave("figures/paper_figures/V3/SM4.novelCDR_exceedance_probability_2C_1p5C_V2.png", p, width = 12, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM4.novelCDR_exceedance_probability_2C_1p5C_V2.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    
    
    #### 6. Emissions relative to original pathways ####
    # Calculate median total GHG for each year and Path
    underdelivering_all_gases_novel_total %>%
      group_by(year, Path, underdelivery_percent) %>%
      summarise(median_GHG = median(totalGHG, na.rm = TRUE), .groups = "drop") -> plot_data
    
    plot_data %>%
      filter(Path == "C1",
             underdelivery_percent == "0") %>%
      select(-Path, -underdelivery_percent) %>%
      rename(median_GHG_C1_original = median_GHG)-> plot_data_C1_original 
    
    plot_data %>%
      filter(Path == "C2",
             underdelivery_percent == "0") %>%
      select(-Path, -underdelivery_percent) %>%
      rename(median_GHG_C2_original = median_GHG)-> plot_data_C2_original 
    
    plot_data %>%
      filter(Path == "C3",
             underdelivery_percent == "0") %>%
      select(-Path, -underdelivery_percent) %>%
      rename(median_GHG_C3_original = median_GHG)-> plot_data_C3_original 
    #^This has the CO2 emissions in median 2C scenarios with )% under-delivering
    
    #Join all
    plot_data %>%
      left_join(plot_data_C1_original, by = "year") %>%
      left_join(plot_data_C2_original, by = "year") %>%
      left_join(plot_data_C3_original, by = "year") %>%
      mutate(difference = if_else(Path == "C1", median_GHG - median_GHG_C1_original,
                                  if_else(Path == "C2", median_GHG - median_GHG_C2_original, 
                                          median_GHG - median_GHG_C3_original)
      )) -> underdelivering_all_GHG_differences
    
    #Plot all
    ggplot(underdelivering_all_GHG_differences, aes(x = year, y = difference, color = Path, group = interaction(Path, underdelivery_percent))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      geom_line(aes(y = 0), color = "black", size = 1, linetype = "solid") +  # Baseline as y=1
      labs(
        title = "Difference in GHG emissions relative to 0% underdelivering",
        subtitle = "Each line is an underdelivering % relative to its category (c1, c2, c3)",
        x = "Year",
        y = "Difference Emissions in GtCO2eq (underdelivered run - original run)"
      ) +
      facet_wrap( ~Path) +
      scale_color_manual(values = colors) +
      # scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    #Plot only 50% and 100%
    p<-ggplot(underdelivering_all_GHG_differences %>% filter(underdelivery_percent == "50"), aes(x = year, y = difference, color = Path, group = interaction(Path, underdelivery_percent))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      geom_line(data = underdelivering_all_GHG_differences %>% filter(underdelivery_percent == "100"), aes(x = year, y = difference, color = Path, group = interaction(Path, underdelivery_percent)), linetype = "dashed") +  # Lines for relative emissions in each scenario
      geom_line(aes(y = 0), color = "black", size = 0.5, linetype = "solid") +  # Baseline as y=1
      labs(
        title = "Underdelivering novel CDR - Difference in GHG emissions relative to 0% underdelivering",
        subtitle = "Each line is an underdelivering 50% (and 100% dashed) relative to its category (c1, c2, c3)",
        x = "Year",
        y = "Difference Emissions in GtCO2eq (underdelivered run - original run)"
      ) +
      scale_color_manual(values = colors) +
      scale_y_continuous(limits = c(0, 21), breaks = seq(0, 21, by = 5)) +
      # scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(legend.position = "bottom")
    ggsave("figures/paper_figures/V3/SM4.novelCDR_GHGemissions_difference.png", p, width = 12, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM4.novelCDR_GHGemissions_difference.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    
    #### 7. Temperature relative to 2C original ####
    # Initialize an empty list to store results
    filtered_median_list <- list()
    
    # Loop through each table and compute summary statistics
    for (run_label in names(MAGICC_tables_novelCDR)) {
      
      df <- MAGICC_tables_novelCDR[[run_label]]  # Get current dataset
      
      summary_stats <- df %>%
        group_by(Path, year) %>%
        summarize(
          median_temperature_median = median(median, na.rm = TRUE),
          min_temperature_median = min(median, na.rm = TRUE),
          max_temperature_median = max(median, na.rm = TRUE),
          mean_temperature_median = mean(median, na.rm = TRUE),
          n = (n() / 600),
          P25 = quantile(median, 0.25, na.rm = TRUE),
          P75 = quantile(median, 0.75, na.rm = TRUE),
          P5 = quantile(median, 0.05, na.rm = TRUE),
          P95 = quantile(median, 0.95, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(run = run_label)  # Add run label
      
      filtered_median_list[[run_label]] <- summary_stats  # Store result
    }
    
    # Combine all runs into one master table
    filtered_median <- bind_rows(filtered_median_list) #3 paths, 101 years, 11 runs
    
    # Compute the reference temperature for "0%" run (original scenario)
    filtered_median_original <- filtered_median %>%
      filter(run == "0%") %>%
      select(Path, year, median_temperature_median) %>%
      spread(Path, median_temperature_median)
    
    # Join the original values with all runs and compute the temperature difference
    underdelivering_all_temperature_differences <- filtered_median %>%
      select(Path, year, run, median_temperature_median) %>%
      left_join(filtered_median_original, by = "year") %>%
      mutate(difference = if_else(Path == "C1", median_temperature_median - C1,
                                  if_else(Path == "C2", median_temperature_median - C2,
                                          median_temperature_median - C3)))
    
    #Plot only 50% and 100%
    p<-ggplot(underdelivering_all_temperature_differences %>% filter(run == "50%"), aes(x = year, y = difference, color = Path, group = interaction(Path, run))) +
      geom_line(size = 1) +  # Lines for relative emissions in each scenario
      geom_line(data = underdelivering_all_temperature_differences %>% filter(run == "100%"), aes(x = year, y = difference, color = Path, group = interaction(Path, run)), linetype = "dashed") +  # Lines for relative emissions in each scenario
      geom_line(aes(y = 0), color = "black", size = 0.5, linetype = "solid") +  # Baseline as y=1
      labs(
        title = "underdelivering novel CDR - Difference in GMT increase relative to 0% underdelivering",
        subtitle = "Each line is an underdelivering 50% (and 100%) relative to its category (c1, c2, c3)",
        x = "Year",
        y = "Difference GMT increase in C (underdelivered run - original run)"
      ) +
      scale_color_manual(values = colors) +
      scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, by = 0.1)) +
      scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
      # scale_fill_manual(values = colors) +
      theme_minimal() +
      figure_theme 
    ggsave("figures/paper_figures/V3/SM4.novelCDR_temperature_difference.png", p, width = 12, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM4.novelCDR_temperature_difference.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    
    
    
#-------------------------------------------------- SUPPLEMENTARY FIGURE 5-7 ------------------------------------------------
    #Here we compare the temperature outcomes from underdelivering each category alone and underdelivering all together.
    #### 1. Load underdelivering novel CDR ####
    underdelivery_levels <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
    file_paths <- paste0("output/V3/temperature/underdelivered_novelCDR/MAGICC_", underdelivery_levels, "percent_temperature_statistics.csv")
    
    # Initialize an empty list to store data frames
    extracted_data_list <- list()
    
    # Loop through each underdelivery level and process the data
    for (i in 1:length(underdelivery_levels)) {
      
      # Load the corresponding CSV file
      data <- read_csv(file_paths[i]) #each table has 4,639,800 rows
      
      # Clean and extract the relevant columns
      data_cleaned_pre <- data %>%
        select(Model_Scenario, run, year, Path, ensemble_member, temperature) %>%
        #distinct(Model_Scenario, run, year, Path, median, .keep_all = TRUE) %>%
        # Compute peak temperature per ensemble member
        #Follow approach from openscm 
        #https://github.com/openscm/scmdata/blob/112f969351c5e5e872956aecfeda5b5e332637b3/src/scmdata/processing.py#L352
        group_by(Model_Scenario, run, Path, ensemble_member) %>%
        summarise(
          peak_temperature = max(temperature, na.rm = TRUE), # Max temperature for each ensemble
          peak_year = year[which.max(temperature)], # Year when max temp occurs
          temp_2100 = temperature[year == 2100], # Temperature in year 2100
          .groups = "drop"
        ) 
      
      # Compute median peak temperature and median peak year across ensembles
      data_cleaned <- data_cleaned_pre %>%
        group_by(Model_Scenario, Path, run) %>%
        summarise(
          median_peak_temp = median(peak_temperature, na.rm = TRUE),  # Median of ensemble peaks (i.e. the median of the peak over all time)
          median_peak_year = median(peak_year, na.rm = TRUE),  # Median peak year across ensembles
          median_temp_2100 = median(temp_2100, na.rm = TRUE),  # Median temperature in 2100
          .groups = "drop"
        ) %>%
        mutate(set = paste0("novel CDR"))
      
      # Store the processed data in the list
      extracted_data_list[[i]] <- data_cleaned
    }
    
    # Combine all extracted data frames into one
    final_data_novelCDR <- bind_rows(extracted_data_list) #Table should have 407 scenarios, 10 times
    
    #### 2. Load underdelivering fossil CCS ####
    underdelivery_levels <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
    file_paths <- paste0("output/V3/temperature/underdeliver_fossilCCS/MAGICC_", underdelivery_levels, "percent_temperature_statistics_fossilCCS.csv")
    
    # Initialize an empty list to store data frames
    extracted_data_list <- list()
    
    # Loop through each underdelivery level and process the data
    for (i in 1:length(underdelivery_levels)) {
      
      # Load the corresponding CSV file
      data <- read_csv(file_paths[i])
      
      # Clean and extract the relevant columns
      data_cleaned_pre <- data %>%
        select(Model_Scenario, run, year, Path, ensemble_member, temperature) %>%
        #distinct(Model_Scenario, run, year, Path, median, .keep_all = TRUE) %>%
        # Compute peak temperature per ensemble member
        #Follow approach from openscm 
        #https://github.com/openscm/scmdata/blob/112f969351c5e5e872956aecfeda5b5e332637b3/src/scmdata/processing.py#L352
        group_by(Model_Scenario, run, Path, ensemble_member) %>%
        summarise(
          peak_temperature = max(temperature, na.rm = TRUE), # Max temperature for each ensemble
          peak_year = year[which.max(temperature)], # Year when max temp occurs
          temp_2100 = temperature[year == 2100], # Temperature in year 2100
          .groups = "drop"
        ) 
      
      # Compute median peak temperature and median peak year across ensembles
      data_cleaned <- data_cleaned_pre %>%
        group_by(Model_Scenario, Path, run) %>%
        summarise(
          median_peak_temp = median(peak_temperature, na.rm = TRUE),  # Median of ensemble peaks (i.e. the median of the peak over all time)
          median_peak_year = median(peak_year, na.rm = TRUE),  # Median peak year across ensembles
          median_temp_2100 = median(temp_2100, na.rm = TRUE),  # Median temperature in 2100
          .groups = "drop"
        ) %>%
        mutate(set = paste0("fossil CCS"))
      
      # Store the processed data in the list
      extracted_data_list[[i]] <- data_cleaned
    }
    
    # Combine all extracted data frames into one
    final_data_fossilCCS <- bind_rows(extracted_data_list) #Table should have 407 scenarios, 10 times
    
    #### 3. Load underdelivering land CDR ####
    underdelivery_levels <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
    file_paths <- paste0("output/V3/temperature/underdelivered_landCDR/MAGICC_", underdelivery_levels, "percent_temperature_statistics_landCDR.csv")
    
    # Initialize an empty list to store data frames
    extracted_data_list <- list()
    
    # Loop through each underdelivery level and process the data
    for (i in 1:length(underdelivery_levels)) {
      
      # Load the corresponding CSV file
      data <- read_csv(file_paths[i])
      
      # Clean and extract the relevant columns
      data_cleaned_pre <- data %>%
        select(Model_Scenario, run, year, Path, ensemble_member, temperature) %>%
        #distinct(Model_Scenario, run, year, Path, median, .keep_all = TRUE) %>%
        # Compute peak temperature per ensemble member
        #Follow approach from openscm 
        #https://github.com/openscm/scmdata/blob/112f969351c5e5e872956aecfeda5b5e332637b3/src/scmdata/processing.py#L352
        group_by(Model_Scenario, run, Path, ensemble_member) %>%
        summarise(
          peak_temperature = max(temperature, na.rm = TRUE), # Max temperature for each ensemble
          peak_year = year[which.max(temperature)], # Year when max temp occurs
          temp_2100 = temperature[year == 2100], # Temperature in year 2100
          .groups = "drop"
        ) 
      
      # Compute median peak temperature and median peak year across ensembles
      data_cleaned <- data_cleaned_pre %>%
        group_by(Model_Scenario, Path, run) %>%
        summarise(
          median_peak_temp = median(peak_temperature, na.rm = TRUE),  # Median of ensemble peaks (i.e. the median of the peak over all time)
          median_peak_year = median(peak_year, na.rm = TRUE),  # Median peak year across ensembles
          median_temp_2100 = median(temp_2100, na.rm = TRUE),  # Median temperature in 2100
          .groups = "drop"
        ) %>%
        mutate(set = paste0("land CDR"))
      
      # Store the processed data in the list
      extracted_data_list[[i]] <- data_cleaned
    }
    
    # Combine all extracted data frames into one
    final_data_landCDR <- bind_rows(extracted_data_list) #Table should have 407 scenarios, 10 times
    
    #### 4. Load underdelivering all CDR & CCS ####
    # List of underdelivery levels and file paths
    underdelivery_levels <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
    file_paths <- paste0("output/V4/temperature/underdelivered_allCDR/MAGICC_", underdelivery_levels, "percent_temperature_statistics_allCDR.csv")
    
    # Initialize an empty list to store data frames
    extracted_data_list <- list()
    
    # Loop through each underdelivery level and process the data
    for (i in 1:length(underdelivery_levels)) {
      
      # Load the corresponding CSV file
      data <- read_csv(file_paths[i])
      
      # Clean and extract the relevant columns
      data_cleaned_pre <- data %>%
        select(Model_Scenario, run, year, Path, ensemble_member, temperature) %>%
        #distinct(Model_Scenario, run, year, Path, median, .keep_all = TRUE) %>%
        # Compute peak temperature per ensemble member
        #Follow approach from openscm 
        #https://github.com/openscm/scmdata/blob/112f969351c5e5e872956aecfeda5b5e332637b3/src/scmdata/processing.py#L352
        group_by(Model_Scenario, run, Path, ensemble_member) %>%
        summarise(
          peak_temperature = max(temperature, na.rm = TRUE), # Max temperature for each ensemble
          peak_year = year[which.max(temperature)], # Year when max temp occurs
          temp_2100 = temperature[year == 2100], # Temperature in year 2100
          .groups = "drop"
        ) 
      
      # Compute median peak temperature and median peak year across ensembles
      data_cleaned <- data_cleaned_pre %>%
        group_by(Model_Scenario, Path, run) %>%
        summarise(
          median_peak_temp = median(peak_temperature, na.rm = TRUE),  # Median of ensemble peaks (i.e. the median of the peak over all time)
          median_peak_year = median(peak_year, na.rm = TRUE),  # Median peak year across ensembles
          median_temp_2100 = median(temp_2100, na.rm = TRUE),  # Median temperature in 2100
          .groups = "drop"
        ) %>%
        mutate(set = paste0("all CDR & CCS"))
      
      # Store the processed data in the list
      extracted_data_list[[i]] <- data_cleaned
    }
    
    # Combine all extracted data frames into one
    final_data_allCDR <- bind_rows(extracted_data_list) #Table should have 407 scenarios, 10 times
    
    #### 5. Combine all 4 tables into 1 and graph####
    set_order <- c("land CDR", "novel CDR", "fossil CCS", "all CDR & CCS")
    
    final_data_complete <- bind_rows(final_data_novelCDR, 
                                     #final_data_fossilCCS, 
                                     #final_data_landCDR,
                                     #final_data_allCDR
                                     ) %>%
      mutate(run = factor(run, levels = underdelivered_order)) %>%
      mutate(set = factor(set, levels = set_order))
    
    # Get unique paths
    unique_paths <- unique(final_data_complete$Path)
    
    # Loop through each unique Path and create a scatter plot
    for (path in unique_paths) {
      
      # Filter data for the current Path
      data_subset <- final_data_complete %>% filter(Path == path)
      
      # Count unique Model_Scenario per set
      set_counts <- data_subset %>%
        group_by(set) %>%
        summarise(n_models = n_distinct(Model_Scenario)) %>%
        ungroup()
      
      # Merge the count into the data for labeling
      data_subset <- data_subset %>%
        left_join(set_counts, by = "set") %>%
        mutate(set_label = factor(paste0(set, " (n=", n_models, ")"), levels = unique(paste0(set_order, " (n=", set_counts$n_models, ")"))))  # Format facet labels with correct order
      
      # Calculate median temp_2100 per facet (set) and Path
      median_values <- data_subset %>%
        group_by(run,set_label, Path) %>%
        summarise(median_temp = median(median_temp_2100, na.rm = TRUE)) %>%
        ungroup()
      
      # Create the plot
      p <- ggplot(data_subset, aes(x = run, y = median_temp_2100, color = as.factor(run))) +
        geom_point(size = 3, alpha = 0.7) +
        geom_point(data = median_values, aes(x = run, y = median_temp), color = "black", size = 2, shape = 16) +
        scale_color_manual(values = rev(RColorBrewer::brewer.pal(n = length(unique(data_subset$run)), "RdYlBu"))) +
        scale_color_viridis_d(option = "magma") +
        scale_y_continuous(limits = c(1, 2.7), breaks = seq(1, 2.7, by = 0.5)) +
        labs(title = paste("Scatterplot for Path:", path),
             subtitle = "Each point is the median ensemble per Model_Scenario in 2100",
             x = "Run", 
             y = "Global Mean Temperature Increase in 2100 (°C)", 
             color = "Run") +
        facet_wrap(~set_label) +  # Use updated facet labels
        theme_minimal() +
        #figure_theme+
        theme(legend.position = "right")
      ggsave(filename = paste0("figures/paper_figures/V3/SM5.underdeliver_comparisons_scatterplot_", path, ".png"), plot = p, width = 5, height = 5)
      ggsave(filename = paste0("figures/paper_figures/V3/SM5.underdeliver_comparisons_scatterplot_", path, ".svg"), plot = p, width = 5, height = 5)
    }
    
    #### 6. Now compare with original runs ####      
    original_runs <- read_csv("output/V3/temperature/original/MAGICC_original_temperature_statistics.csv")
    
    final_data_original <- original_runs %>%
      select(Model_Scenario, run, year, Path, ensemble_member, temperature) %>%
      # Get the peak temperature and corresponding year for each Model_Scenario and run
      group_by(Model_Scenario, run, Path, ensemble_member) %>%
      summarise(
        peak_temperature = max(temperature, na.rm = TRUE), # Max temperature for each ensemble
        peak_year = year[which.max(temperature)], # Year when max temp occurs
        temp_2100 = temperature[year == 2100], # Temperature in year 2100
        .groups = "drop"
      ) 
    
    # Compute median peak temperature and median peak year across ensembles
    final_data_original <- final_data_original %>%
      group_by(Model_Scenario, Path, run) %>%
      summarise(
        median_peak_temp_original = median(peak_temperature, na.rm = TRUE),  # Median of ensemble peaks (i.e. the median of the peak over all time)
        median_peak_year_original = median(peak_year, na.rm = TRUE),  # Median peak year across ensembles
        median_temp_2100_original = median(temp_2100, na.rm = TRUE),  # Median temperature in 2100
        .groups = "drop"
      ) 
    
    final_data_complete %>%
      left_join(final_data_original, by = c("Model_Scenario", "Path")) %>%
      mutate(difference = median_temp_2100 - median_temp_2100_original)%>%
      rename(run = run.x)-> temperature_difference # 16,280 rows (407 scenarios, 4 techs, 10 sets)
    
    for (path in unique_paths) {
      
      # Filter data for the current Path
      data_subset <- temperature_difference %>% filter(Path == path)
      
      # Compute median difference per facet (set) and run
      median_values <- data_subset %>%
        group_by(set, run, Path) %>%
        summarise(median_diff = median(difference, na.rm = TRUE), .groups = "drop")
      
      # Create the plot
      p <- ggplot(data_subset, aes(x = run, y = difference, color = as.factor(run))) +
        geom_point(size = 3, alpha = 0.7) +
        geom_point(data = median_values, aes(x = run, y = median_diff), color = "black", size = 2, shape = 16) +  # Median in black
        scale_color_manual(values = rev(RColorBrewer::brewer.pal(n = length(unique(data_subset$run)), "RdYlBu"))) +
        scale_color_viridis_d(option = "magma") +
        scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1.2, by = 0.2)) +
        labs(title = paste("Difference in GMT increase in 2100 vs original run for Path:", path),
             subtitle = "Each point is the difference between median ensemble per Model_Scenario in 2100",
             x = "Run", 
             y = "Difference GMT Increase in 2100 original vs underdelivered (°C)", 
             color = "Run") +
        facet_wrap(~set) +  # Use updated facet labels
        theme_minimal() +
        #figure_theme+
        theme(legend.position = "right")
      ggsave(filename = paste0("figures/paper_figures/V3/SM5.underdeliver_comparisons_diffrence_scatterplot_", path, ".png"), plot = p, width = 5, height = 5)
      ggsave(filename = paste0("figures/paper_figures/V3/SM5.underdeliver_comparisons_diffrence_scatterplot_", path, ".svg"), plot = p, width = 5, height = 5)
    }
    
#-------------------------------------------------- SUPPLEMENTARY FIGURE 8-10 ------------------------------------------------
    #These figures were creates above with the probabilities of crossing thresholds per scenario
#-------------------------------------------------- SUPPLEMENTARY FIGURE 11 ------------------------------------------------
    #### 1. Plot contours for 2100 temperature ####
    library(plotly)
    library(htmlwidgets)
    
    #NOTES:
    # plotly performs bilinear interpolation for missing values
    #Plotly takes the values at the closest surrounding grid points 
    #(e.g., (0, 0), (20, 0), (0, 20), and (20, 20)) and computes the 
    #value of z at intermediate points (like (10, 10)) based on these.
    final_data_permutations_CCS %>%
      group_by(Path,percent_novelCDR, percent_landCDR) %>%
      summarise(peak_year_median = median(median_peak_temp),
                temp_2100_median = median(median_temp_2100),
                temp_2100_mean = mean(median_temp_2100)) %>%
      ungroup() -> final_data_CCS_summary
    
    # Compute global min and max across all data (already done outside the loop)
    global_min <- min(final_data_CCS_summary$temp_2100_mean, na.rm = TRUE)
    global_max <- max(final_data_CCS_summary$temp_2100_mean, na.rm = TRUE)
    
    global_min <- 1.3
    global_max <- 1.92
    # Loop through each Path (C1, C2, C3)
    for (pathway in unique(final_data_CCS_summary$Path)) {
      
      # Subset data for the current Path and CCS combination
      plot_data <- final_data_CCS_summary %>% filter(Path == pathway)
      
      # Generate Plotly contour plot for the 2100 temperature
      fig <- plot_ly(data = plot_data,
                     x = ~percent_novelCDR,
                     y = ~percent_landCDR,
                     z = ~temp_2100_mean,
                     type = "contour",
                     ncontours = 13,
                     zmin = global_min,  # Use global fixed color scale min
                     zmax = global_max,  # Use global fixed color scale max
                     colorbar = list(
                       bordercolor = "black",
                       title = "GMT increase",
                       len = 0.5,
                       tickvals = seq(global_min, global_max, length.out = 6),
                       tickformat = ".2f"
                     ),
                     colorscale = colorscale_magma,  # Apply fixed colorscale
                     line = list(color = "white"),
                     autocontour = TRUE,
                     contours = list(
                       showlabels = TRUE,
                       coloring = "heatmap",  # Coloring by heatmap
                       size = 0.2,  # Adjust size for smoother contours
                       start = global_min,  # Start fixed at global min
                       end = global_max,    # End fixed at global max
                       showlines = TRUE  
                     )) %>%
        layout(
          title = paste("Contour Plot of 2100 Temperature for Path:", pathway, "(mean across scenarios)"),
          xaxis = list(title = "Percent Underdelivering Novel CDR"),
          yaxis = list(title = "Percent Underdelivering Land CDR")
        )
      orca(fig, file = paste0("figures/paper_figures/V3/SM11.2100_contour_plotly_", pathway, "_mean.png"))
      orca(fig, file = paste0("figures/paper_figures/V3/SM11.2100_contour_plotly_", pathway, "_mean.svg"))
    }
    
    #### Now do contour plot for the difference
    final_data_permutations_CCS %>%
      filter(percent_novelCDR == 0 & percent_landCDR == 0) %>%
      select(Model_Scenario, median_temp_2100) %>%
      rename(temp_2100_original = median_temp_2100)-> final_data_permutations_CCS_original_2100
    
    final_data_permutations_CCS %>%
      left_join(final_data_permutations_CCS_original_2100, by = "Model_Scenario") %>%
      mutate(temp_2100_difference = median_temp_2100 - temp_2100_original)-> final_data_permutation_difference
    
    final_data_permutation_difference %>%
      group_by(Path,percent_novelCDR, percent_landCDR) %>%
      summarise(temp_2100_difference_median = median(temp_2100_difference),
                temp_2100_difference_mean = mean(temp_2100_difference)) %>%
      ungroup() -> final_data_CCS_difference_summary
    
    # Compute global min and max across all data (already done outside the loop)
    global_min <- min(final_data_CCS_difference_summary$temp_2100_difference_mean, na.rm = TRUE)
    global_max <- max(final_data_CCS_difference_summary$temp_2100_difference_mean, na.rm = TRUE)
    global_max <- 0.4
    # Loop through each Path (C1, C2, C3)
    for (pathway in unique(final_data_CCS_difference_summary$Path)) {
      
      # Subset data for the current Path and CCS combination
      plot_data <- final_data_CCS_difference_summary %>% filter(Path == pathway)
      
      # Generate Plotly contour plot for the 2100 temperature
      fig <- plot_ly(data = plot_data,
                     x = ~percent_novelCDR,
                     y = ~percent_landCDR,
                     z = ~temp_2100_difference_mean,
                     type = "contour",
                     ncontours = 12,
                     zmin = global_min,  # Use global fixed color scale min
                     zmax = global_max,  # Use global fixed color scale max
                     colorbar = list(
                       bordercolor = "black",
                       title = "GMT increase",
                       len = 0.5,
                       tickvals = seq(global_min, global_max, length.out = 5),
                       tickformat = ".2f"
                     ),
                     colorscale = colorscale_magma,  # Apply fixed colorscale
                     line = list(color = "white"),
                     autocontour = TRUE,
                     contours = list(
                       showlabels = TRUE,
                       coloring = "heatmap",  # Coloring by heatmap
                       size = 0.2,  # Adjust size for smoother contours
                       start = global_min,  # Start fixed at global min
                       end = global_max,    # End fixed at global max
                       showlines = TRUE  
                     )) %>%
        layout(
          title = paste("Contour Plot of 2100 Temperature Difference (run - original) for Path:", pathway, "(mean)"),
          xaxis = list(title = "Percent Underdelivering Novel CDR"),
          yaxis = list(title = "Percent Underdelivering Land CDR")
        )
      orca(fig, file = paste0("figures/paper_figures/V3/SM11.2100_contour_plotly_difference_", pathway, "_mean_color.png"))
      orca(fig, file = paste0("figures/paper_figures/V3/SM11.2100_contour_plotly_difference_", pathway, "_mean_color.svg"))
    }

#-------------------------------------------------- SUPPLEMENTARY FIGURE 12 ------------------------------------------------
    #### 1. Countour with peak temperature ####
    final_data_permutations_CCS %>%
      group_by(Path, percent_novelCDR, percent_landCDR) %>%
      summarise(peak_year_mean = round(mean(median_peak_year)),
                peak_temp_mean= mean(median_peak_temp)) %>%
      ungroup() -> final_data_CCS_summary_peak
    
    # Compute global min and max across all data (already done outside the loop)
    global_min <- min(final_data_CCS_summary_peak$peak_temp_mean, na.rm = TRUE)
    global_max <- max(final_data_CCS_summary_peak$peak_temp_mean, na.rm = TRUE)
    
    global_min<- 1.5
    global_max <- 1.95
    # Loop through each Path (C1, C2, C3)
    for (pathway in unique(final_data_CCS_summary_peak$Path)) {
      
      # Subset data for the current Path and CCS combination
      plot_data <- final_data_CCS_summary_peak %>% filter(Path == pathway)
      
      # Generate Plotly contour plot for the peak temperature
      fig <- plot_ly(data = plot_data,
                     x = ~percent_novelCDR,
                     y = ~percent_landCDR,
                     z = ~peak_temp_mean,
                     type = "contour",
                     ncontours = 13,
                     zmin = global_min,  # Use global fixed color scale min
                     zmax = global_max,  # Use global fixed color scale max
                     colorbar = list(
                       bordercolor = "black",
                       title = "GMT increase",
                       len = 0.5,
                       tickvals = seq(global_min, global_max, length.out = 6),
                       tickformat = ".2f"
                     ),
                     colorscale = colorscale_magma,  # Apply fixed colorscale
                     line = list(color = "white"),
                     autocontour = TRUE,
                     contours = list(
                       showlabels = TRUE,
                       coloring = "heatmap",  # Coloring by heatmap
                       size = 0.2,  # Adjust size for smoother contours
                       start = global_min,  # Start fixed at global min
                       end = global_max,    # End fixed at global max
                       showlines = TRUE  
                     )) %>%
        layout(
          title = paste("Contour Plot of Peak Temperature for Path:", pathway, "(mean across scenarios)"),
          xaxis = list(title = "Percent Underdelivering Novel CDR"),
          yaxis = list(title = "Percent Underdelivering Land CDR")
        )
      orca(fig, file = paste0("figures/paper_figures/V3/SM12.Peak_contour_plotly_", pathway, "_mean.png"))
      orca(fig, file = paste0("figures/paper_figures/V3/SM12.Peak_contour_plotly_", pathway, "_mean.svg"))
    }
    
    #### Now do contour plot for the difference
    final_data_permutations_CCS %>%
      filter(percent_novelCDR == 0 & percent_landCDR == 0) %>%
      select(Model_Scenario, median_peak_temp) %>%
      rename(temp_peak_original = median_peak_temp)-> final_data_permutations_CCS_original_peak
    
    final_data_permutations_CCS %>%
      left_join(final_data_permutations_CCS_original_peak, by = "Model_Scenario") %>%
      mutate(temp_peak_difference = median_peak_temp - temp_peak_original)-> final_data_permutation_peak_difference
    
    final_data_permutation_peak_difference %>%
      group_by(Path,percent_novelCDR, percent_landCDR) %>%
      summarise(temp_peak_difference_mean = mean(temp_peak_difference)) %>%
      ungroup() -> final_data_permutation_peak_difference_summary
    
    # Compute global min and max across all data (already done outside the loop)
    global_min <- min(final_data_permutation_peak_difference_summary$temp_peak_difference_mean, na.rm = TRUE)
    global_max <- max(final_data_permutation_peak_difference_summary$temp_peak_difference_mean, na.rm = TRUE)
    global_max <- 0.2
    
    # Loop through each Path (C1, C2, C3)
    for (pathway in unique(final_data_permutation_peak_difference_summary$Path)) {
      
      # Subset data for the current Path and CCS combination
      plot_data <- final_data_permutation_peak_difference_summary %>% filter(Path == pathway)
      
      # Generate Plotly contour plot for the 2100 temperature
      fig <- plot_ly(data = plot_data,
                     x = ~percent_novelCDR,
                     y = ~percent_landCDR,
                     z = ~temp_peak_difference_mean,
                     type = "contour",
                     ncontours = 12,
                     zmin = global_min,  # Use global fixed color scale min
                     zmax = global_max,  # Use global fixed color scale max
                     colorbar = list(
                       bordercolor = "black",
                       title = "GMT increase",
                       len = 0.5,
                       tickvals = seq(global_min, global_max, length.out = 5),
                       tickformat = ".2f"
                     ),
                     colorscale = colorscale_magma,  # Apply fixed colorscale
                     line = list(color = "white"),
                     autocontour = TRUE,
                     contours = list(
                       showlabels = TRUE,
                       coloring = "heatmap",  # Coloring by heatmap
                       size = 0.2,  # Adjust size for smoother contours
                       start = global_min,  # Start fixed at global min
                       end = global_max,    # End fixed at global max
                       showlines = TRUE  
                     )) %>%
        layout(
          title = paste("Contour Plot of Peak Temperature Difference (run - original) for Path:", pathway, "(mean)"),
          xaxis = list(title = "Percent Underdelivering Novel CDR"),
          yaxis = list(title = "Percent Underdelivering Land CDR")
        )
      orca(fig, file = paste0("figures/paper_figures/V3/SM12.Peak_contour_plotly_difference_", pathway, "_mean.png"))
      orca(fig, file = paste0("figures/paper_figures/V3/SM12.Peak_contour_plotly_difference_", pathway, "_mean.svg"))
    }
    
#-------------------------------------------------- SUPPLEMENTARY FIGURES 13-15  -------------------------------------------------------------------------------------------------------
    #Density plots for 2100 temeprature
    #### 1. Get the data and calculate summary statistics ####
    # Define percent values (assuming 0 to 100 in steps of 20)
    percent_novelCDR_values <- seq(0, 100, by = 20)
    percent_landCDR_values <- seq(0, 100, by = 20)
    
    # Create an empty list to store results
    base_path <- "output/V4/temperature/underdelivered_permutations"
    summary_list <- list()
    
    # Loop over novelCDR and landCDR percentages
    for (percent_novelCDR in percent_novelCDR_values) {
      for (percent_landCDR in percent_landCDR_values) {
        
        # Construct file path
        file_name <- paste0("MAGICC_", percent_novelCDR, "percent_novelCDR_", percent_landCDR, "percent_landCDR_temperature_statistics.csv")
        file_path <- file.path(base_path, file_name)
        
        # Check if file exists
        if (file.exists(file_path)) {
          
          # Load the CSV
          temp_data <- fread(file_path, select = c("Model_Scenario","ensemble_member", "year", "Path", "temperature")) %>%
            distinct(Model_Scenario, ensemble_member, year, Path, temperature) %>% # Keep each ensemble
            filter(year == 2100) %>%
            mutate(percent_novelCDR = percent_novelCDR,
                   percent_landCDR = percent_landCDR)
          
          # Store in list
          summary_list[[paste0(percent_novelCDR, "_", percent_landCDR)]] <- temp_data
          
          # Clean up memory
          rm(temp_data)
          gc()
          
        } else {
          warning(paste("File not found:", file_path))
        }
      }
    }
    
    # Combine all summaries into one dataframe
    final_data_permutations_densities <- bind_rows(summary_list) 
    #^This table has our 407 scenarios, each has 2010 temperature for each ensemble
    
    #### Figure Loop over each unique Path
    for (path_value in unique(final_data_permutations_densities$Path)) {
      
      # Filter data for the current Path
      path_data <- final_data_permutations_densities %>%
        filter(Path == path_value)
      
      # Compute the peak (mode) of the density curve for the median values
      density_peaks <- path_data %>%
        group_by(percent_novelCDR, percent_landCDR) %>%
        summarise(peak_temp = density(temperature)$x[which.max(density(temperature)$y)], .groups = "drop")  # Find peak of density
      
      # Generate the density plot
      p <- ggplot() +
        geom_density(data = path_data, aes(x=temperature, fill = Path), alpha = 0.8) +
        geom_vline(data = density_peaks, aes(xintercept = peak_temp), color = "black", linetype = "dashed", linewidth = 0.5) +  # Peak vertical line
        geom_text(data = density_peaks, aes(x = peak_temp, y = 1, label = sprintf("%.2f°C", peak_temp)), 
                  color = "black", size = 3, hjust = -0.5) +  # Add text label near the vertical line
        scale_color_manual(values = colors) +
        scale_fill_manual(values = colors) +
        facet_grid(
          rows = vars(percent_novelCDR),  # Novel CDR facets as rows
          cols = vars(percent_landCDR),   # Land CDR facets as columns
          switch = "y",  # Moves row facet labels to the left & column facet labels to the top
        ) +
        theme_minimal() +
        labs(
          title = paste("Density Plot for Temperature Increase in 2100 Path:", path_value),
          subtitle = "Rows: underdelivering novel CDR. Columns: underdelivering land CDR. Black line represents mode across scenarios for each ensemble",
          x = "Temperature",
          y = "Density",
          color = "Model Scenario"
        ) +
        theme(
          legend.position = "none",
          strip.text = element_text(size = 10, face = "bold"),  # Make facet labels clearer
          aspect.ratio = 1  # Makes each facet square
        )
      
      # Save the plot
      ggsave(filename = paste0("figures/paper_figures/V3/SM13.density_plot_one_line_", path_value, "_2100.png"), plot = p, width = 10, height = 6)
      ggsave(filename = paste0("figures/paper_figures/V3/SM13.density_plot_one_line_", path_value, "_2100.svg"), plot = p, width = 10, height = 6)
    }
    
#-------------------------------------------------- SUPPLEMENTARY FIGURES 16-18 -------------------------------------------------------------------------------------------------------
    #### 2. Distribution for peak temperature ####
    # Create an empty list to store results
    summary_list <- list()
    
    # Loop over novelCDR and landCDR percentages
    for (percent_novelCDR in percent_novelCDR_values) {
      for (percent_landCDR in percent_landCDR_values) {
        
        # Construct file path
        file_name <- paste0("MAGICC_", percent_novelCDR, "percent_novelCDR_", percent_landCDR, "percent_landCDR_temperature_statistics.csv")
        file_path <- file.path(base_path, file_name)
        
        # Check if file exists
        if (file.exists(file_path)) {
          
          # Load the CSV
          temp_data <- fread(file_path, select = c("Model_Scenario","ensemble_member", "year", "Path", "temperature")) %>%
            distinct(Model_Scenario, ensemble_member, year, Path, temperature) # Keep each ensemble
          
          # #### Approach 1: we look at max temperature in a scenario, and then keep the
          # #600 ensembles for that year
          # # Find the peak temperature and corresponding year for each Model_Scenario
          # peak_years <- temp_data %>%
          #   group_by(Model_Scenario) %>%
          #   filter(temperature == max(temperature, na.rm = TRUE)) %>%
          #   select(Model_Scenario, year) %>%
          #   distinct()
          # 
          # # Keep all ensemble members for the identified peak years
          # temp_data_filtered <- temp_data %>%
          #   inner_join(peak_years, by = c("Model_Scenario", "year")) %>%
          #   mutate(percent_novelCDR = percent_novelCDR,
          #          percent_landCDR = percent_landCDR)
          # 
          #### Approach 2: we look into each scenario and pull the peak temperature 
          #in each of the 600 ensembles, regardless fo the year.
          # Find the peak temperature and corresponding year for each Model_Scenario
          peak_temp <- temp_data %>%
            group_by(Model_Scenario, ensemble_member) %>%
            filter(temperature == max(temperature, na.rm = TRUE)) %>%
            mutate(percent_novelCDR = percent_novelCDR,
                   percent_landCDR = percent_landCDR)
          
          
          # Store in list
          summary_list[[paste0(percent_novelCDR, "_", percent_landCDR)]] <- peak_temp
          
          # Clean up memory
          rm(temp_data, peak_temp)
          gc()
          
        } else {
          warning(paste("File not found:", file_path))
        }
      }
    }
    
    # Combine all summaries into one dataframe
    final_data_permutations_densities_peak <- bind_rows(summary_list)
    
    #### Figure Loop over each unique Path
    for (path_value in unique(final_data_permutations_densities_peak$Path)) {
      
      # Filter data for the current Path
      path_data <- final_data_permutations_densities_peak %>%
        filter(Path == path_value)
      
      # Compute the peak (mode) of the density curve for the median values
      density_peaks <- path_data %>%
        group_by(percent_novelCDR, percent_landCDR) %>%
        summarise(peak_temp = density(temperature)$x[which.max(density(temperature)$y)], .groups = "drop")  # Find peak of density
      
      # Generate the density plot
      p <- ggplot() +
        geom_density(data = path_data, aes(x=temperature, fill = Path), alpha = 0.8) +
        geom_vline(data = density_peaks, aes(xintercept = peak_temp), color = "black", linetype = "dashed", linewidth = 0.5) +  # Peak vertical line
        geom_text(data = density_peaks, aes(x = peak_temp, y = 1, label = sprintf("%.2f°C", peak_temp)), 
                  color = "black", size = 3, hjust = -0.5) +  # Add text label near the vertical line
        scale_color_manual(values = colors) +
        scale_fill_manual(values = colors) +
        facet_grid(
          rows = vars(percent_novelCDR),  # Novel CDR facets as rows
          cols = vars(percent_landCDR),   # Land CDR facets as columns
          switch = "y",  # Moves row facet labels to the left & column facet labels to the top
        ) +
        theme_minimal() +
        labs(
          title = paste("Density Plot for Peak Temperature Path:", path_value),
          subtitle = "Rows: underdelivering novel CDR. Columns: underdelivering land CDR. Black line represents mode across scenarios for each ensemble",
          x = "Temperature",
          y = "Density",
          color = "Model Scenario"
        ) +
        theme(
          legend.position = "none",
          strip.text = element_text(size = 10, face = "bold"),  # Make facet labels clearer
          aspect.ratio = 1  # Makes each facet square
        )
      
      # Save the plot
      ggsave(filename = paste0("figures/paper_figures/V3/SM16.density_plot_one_line_", path_value, "_peak.png"), plot = p, width = 10, height = 6)
      ggsave(filename = paste0("figures/paper_figures/V3/SM16.density_plot_one_line_", path_value, "_peak.svg"), plot = p, width = 10, height = 6)
    }
  
#-------------------------------------------------- SUPPLEMENTARY FIGURE 19 -------------------------------------------------------------------------------------------------------  
    #Here we want to show how technologies scale: get cumulative value, then normalize by diving among largest value (range 0-1)
    #Then we plot with our carbon management technologies
    #Then calculate CAGR
    
    #### 1. Load technology development data ####
    US_shale_gas_data <- read_csv("data/us_shale_gas.csv", skip = 2) #https://www.eia.gov/dnav/ng/ng_prod_sum_dc_NUS_mmcf_a.htm
    HATCH_data <- read_csv("data/HATCH_v1.5_Clean.csv") #https://zenodo.org/records/10035735
    
    HATCH_data %>%
      gather(year, value, -ID, -`Spatial Scale`, -Region, -`Country Name`, -`Technology Name`,
             -Metric, -Unit, -`Data Source`, -Variable) %>%
      filter(`Spatial Scale` == "Global",
             year >= 1900)  -> HATCH_data_long
    
    HATCH_data_long %>%
      filter(grepl("Cumulative", Variable))-> HATCH_data_cumulative
    
    HATCH_data_cumulative %>%
      filter(!is.na(value)) %>%
      mutate(year = as.numeric(year)) %>%
      group_by(`Technology Name`, `Country Name`) %>%
      mutate(value_normalized = value / max(value, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(value_cumulative = value)-> HATCH_data_normalized
    
    #Graph
    ggplot(HATCH_data_normalized, aes(x = year, y = value_normalized, color = `Technology Name`)) +
      geom_line(size = 1) +
      labs(
        x = "Year",
        y = "Normalized deployment (relative to max)",
        title = "Normalized Deployment of Technologies Over Time",
        color = "Technology"
      ) +
      facet_wrap(~`Technology Name`) +
      theme_minimal() +
      theme(
        text = element_text(size = 12),
        legend.position = "right"
      )
    
    #Now calculate with technologies that were annual values
    HATCH_data_long %>%
      filter(grepl("Annual", Variable)) %>%
      filter(!is.na(value)) %>%
      mutate(year = as.numeric(year)) %>%
      group_by(`Technology Name`, `Country Name`) %>%
      arrange(year) %>%
      mutate(value_cumulative = cumsum(value)) %>%
      ungroup() -> HATCH_data_cumulative_from_annual
    
    HATCH_data_cumulative_from_annual %>%
      mutate(year = as.numeric(year)) %>%
      group_by(`Technology Name`, `Country Name`) %>%
      mutate(value_normalized = if_else(value_cumulative == 0, 0, value_cumulative / max(value_cumulative, na.rm = TRUE))) %>%
      ungroup() -> HATCH_data_normalized_from_annual
    
    #Graph
    ggplot(HATCH_data_normalized_from_annual, aes(x = year, y = value_normalized, color = `Technology Name`)) +
      geom_line(size = 1) +
      labs(
        x = "Year",
        y = "Normalized deployment (relative to max)",
        title = "Normalized Deployment of Technologies Over Time",
        color = "Technology"
      ) +
      facet_wrap(~`Technology Name`) +
      theme_minimal() +
      theme(
        text = element_text(size = 12),
        legend.position = "none"
      )
    
    #Jon botoh tables
    HATCH_data_normalized_from_annual %>%
      bind_rows(HATCH_data_normalized) -> HATCH_data_normalized_global_all
    
    chunk_size <- 20
    
    # Get unique variables
    variables <- unique(HATCH_data_normalized_global_all$`Technology Name`)
    
    # Total number of plots
    n_chunks <- ceiling(length(variables) / chunk_size)
    
    # Loop through chunks
    for (i in 1:n_chunks) {
      
      # Select current chunk of variables
      var_chunk <- variables[((i - 1) * chunk_size + 1):min(i * chunk_size, length(variables))]
      
      # Filter data for this chunk
      plot_data <- HATCH_data_normalized_global_all %>%
        filter(`Technology Name` %in% var_chunk)
      
      # Create plot
      p <- ggplot(plot_data, aes(x = year, y = value_normalized, color = `Technology Name`)) +
        geom_line(size = 1) +
        labs(
          title = paste("Normalized Deployment (Variables", ((i - 1) * chunk_size + 1), "to", min(i * chunk_size, length(variables)), ")"),
          x = "Year",
          y = "Normalized Value",
          color = "Variable"
        ) +
        theme_minimal() +
        facet_wrap(~`Technology Name`) +
        theme(
          legend.position = "none",
          legend.text = element_text(size = 7),
          plot.title = element_text(size = 14, face = "bold")
        )
      ggsave(
        filename = paste0("figures/test/normalized_plot_chunk_", i, ".png"),
        plot = p,
        width = 10,
        height = 6,
        dpi = 300
      )
    }
    
    #### 2. Bring in our carbon management data ####
    scenarios_to_exclude <- pull(scenarios_to_exclude, Model_Scenario)
    
    ar6_reanalysis_output_sequestration_categories %>%
      #Exclude the scenarios that have negative sequestration
      filter(!Model_Scenario %in% scenarios_to_exclude) %>%
      group_by(Model_Scenario, Variable, Path) %>%
      arrange(year) %>%
      mutate(value_cumulative = cumsum(value)) %>%
      ungroup() -> ar6_reanalysis_output_sequestration_categories_cumulative
    
    ar6_reanalysis_output_sequestration_categories_cumulative %>%
      group_by(Model_Scenario, Path, Variable) %>%
      mutate(value_normalized = if_else(value_cumulative == 0, 0, value_cumulative / max(value_cumulative, na.rm = TRUE))) %>%
      ungroup() -> ar6_reanalysis_output_sequestration_categories_cumulative_normalized
    
    # Summarise the data: quantiles per Variable, Path, and year
    total_CO2_managed_cum_median <- ar6_reanalysis_output_sequestration_categories_cumulative_normalized %>%
      group_by(Path, Variable, year) %>%
      summarise(
        median = median(value_normalized, na.rm = TRUE),
        P5 = quantile(value_normalized, 0.05, na.rm = TRUE),
        P15 = quantile(value_normalized, 0.15, na.rm = TRUE),
        P25 = quantile(value_normalized, 0.25, na.rm = TRUE),
        P75 = quantile(value_normalized, 0.75, na.rm = TRUE),
        P85 = quantile(value_normalized, 0.85, na.rm = TRUE),
        P95 = quantile(value_normalized, 0.95, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Plot
    p<-  ggplot(total_CO2_managed_cum_median, aes(x = year)) +
      geom_ribbon(aes(ymin = P5, ymax = P95, fill = Path), alpha = 0.2) +
      geom_ribbon(aes(ymin = P15, ymax = P85, fill = Path), alpha = 0.3) +
      geom_ribbon(aes(ymin = P25, ymax = P75, fill = Path), alpha = 0.4) +
      geom_line(aes(y = median, color = Path), size = 1.2) +
      geom_hline(yintercept = 0, color = "black") +
      labs(
        title = "Carbon management deployment",
        subtitle = "(shades: P5, P15, P25, median, P75, P85, P95)",
        x = "Year",
        y = "Normalized CO2 sequestration (GtCO2/yr)",
        fill = "Path",
        color = "Path"
      ) +
      facet_grid(Variable ~ Path) +
      scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, 30)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.position = "none"
      )
    #ggsave("figures/paper_figures/V3/SM21.cum_normalized_carbon_management.png", width = 6, height = 6, units = "in", dpi = 600)
    
    #### 3. Merge data with my tables ####
    US_shale_gas_data %>%
      rename(year = Date, value = `U.S. Natural Gas Gross Withdrawals from Shale Gas (Million Cubic Feet)`) %>%
      arrange(year) %>%
      mutate(value_cumulative = cumsum(value)) %>%
      mutate(value_normalized = if_else(value_cumulative == 0, 0, value_cumulative / max(value_cumulative, na.rm = TRUE))) %>%
      ungroup() -> US_shale_gas_cum
    
    HATCH_data_normalized_global_all %>%
      #We filter out technologies that appear for less than 30 years
      group_by(`Technology Name`) %>%
      #filter(n_distinct(year) >= 30) %>%
      ungroup() %>%
      filter(`Technology Name` %in% c("Coal Power",
                                      #"Renewable Power",
                                      "Nuclear Energy",
                                      "Lithium Mine Production",
                                      "Ethanol" #Note ethanol has only 1958-1972 data so it looks quite exponential
                                      
                                      # "Electricity",
                                      # "Ethanol",
                                      # "Hydroelectricity",
                                      # "Natural Gas Production",
                                      # "Natural Gas Power",
                                      # "Concentrated Solar Power",
                                      # "Oil Refining Capacity",
                                      # "Oil Production"
                                      #"Coal Production"
      )) -> HATCH_data_normalized_global_filtered
    
    # Step 1: Get all facet combinations
    facet_combinations <- total_CO2_managed_cum_median %>%
      distinct(Variable, Path)
    
    # Step 2: Expand HATCH data to include all facet combinations
    HATCH_for_facets <- HATCH_data_normalized_global_filtered %>%
      rename(HATCH_Variable = Variable) %>%
      crossing(facet_combinations)
    
    # Step 3: Plot
    ggplot(total_CO2_managed_cum_median, aes(x = year)) +
      geom_ribbon(aes(ymin = P5, ymax = P95, fill = Path), alpha = 0.2) +
      geom_ribbon(aes(ymin = P15, ymax = P85, fill = Path), alpha = 0.3) +
      geom_ribbon(aes(ymin = P25, ymax = P75, fill = Path), alpha = 0.4) +
      geom_line(aes(y = median, color = Path), size = 0.5) +
      geom_hline(yintercept = 0, color = "black") +
      geom_vline(xintercept = 2015, linetype = "dashed", color = "turquoise", size = 0.5)+
      # Now HATCH lines appear in every facet
      geom_line(
        data = HATCH_for_facets,
        aes(x = year, y = value_normalized, group = `Technology Name`, 
            linetype = `Technology Name`, color =  `Technology Name`),
        size = 0.3,
        inherit.aes = FALSE
      ) +
      # Now US shale gas lines appear in every facet (2007-2023)
      geom_line(
        data = US_shale_gas_cum,
        aes(x = year, y = value_normalized), color = "red",
        size = 0.3,
        inherit.aes = FALSE
      ) +
      labs(
        title = "Carbon management deployment",
        subtitle = "(shades: P5, P15, P25, median, P75, P85, P95)",
        x = "Year",
        y = "CO2 sequestration (GtCO2/yr)",
        fill = "Path",
        color = "Path",
        linetype = "Technology"
      ) +
      facet_grid(Path ~ Variable) +
      scale_x_continuous(limits = c(1900, 2100), breaks = seq(1900, 2100, 50)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.position = "bottom"
      )
    ggsave("figures/paper_figures/V3/SM19.cum_normalized_carbon_management_final.png", width = 6, height = 6, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM19.cum_normalized_carbon_management_final.svg", width = 6, height = 6, units = "in", dpi = 600)
    
    #### 4. Calculate CAGR ####
    #We use the Compound Annual Growth Rate (CAGR) formula: 
    #(Ending Value / Beginning Value)^(1/Number of Years) - 1
    # Note that we need annual values, and not cumuative here
    annual_growth <- HATCH_data_normalized_global_filtered %>%
      group_by(`Technology Name`, Variable) %>%
      arrange(year) %>%
      mutate(value_annual = value_cumulative - lag(value_cumulative)) %>%
      mutate(
        annual_growth = (value_annual / lag(value_annual)) - 1,  # Year-over-year growth
        annual_growth_percent = annual_growth * 100
      ) %>%
      ungroup()  %>%
      filter(!is.infinite(annual_growth_percent) & !is.na(annual_growth_percent))  # Remove invalid values that appear when divindn by 0 or na
    
    # Calculate Compute Decadal Growth Rate
    decadal_growth <- HATCH_data_normalized_global_filtered %>%
      group_by(`Technology Name`, Variable) %>%
      arrange(year) %>%
      mutate(value_annual = value_cumulative - lag(value_cumulative)) %>%
      filter(year %% 10 == 0 | year %% 10 == 9) %>%  # Only years ending in 0 or 9
      mutate(
        decade = paste0(floor(year / 10) * 10, "s")  # e.g., 1960s, 1970s
      ) %>%
      group_by(`Technology Name`, Variable, decade) %>%
      filter(n() == 2) %>%  # Ensure both start (0) and end (9) values exist
      arrange(year) %>%
      summarize(
        start_year = min(year),
        end_year = max(year),
        start_value = value_annual[year == min(year)],
        end_value = value_annual[year == max(year)],
        cagr = (end_value / start_value)^(1 / (end_year - start_year)) - 1,
        cagr_percent = cagr * 100,
        .groups = "drop"
      )
    
    # Calculate Compute Decadal Growth Rate
    decadal_growth_shale <- US_shale_gas_cum %>%
      filter(year %% 10 == 0 | year %% 10 == 9) %>%  # Only years ending in 0 or 9
      mutate(
        decade = paste0(floor(year / 10) * 10, "s")  # e.g., 1960s, 1970s
      ) %>%
      group_by(decade) %>%
      filter(n() == 2) %>%  # Ensure both start (0) and end (9) values exist
      arrange(year) %>%
      summarize(
        start_year = min(year),
        end_year = max(year),
        start_value = value[year == min(year)],
        end_value = value[year == max(year)],
        cagr = (end_value / start_value)^(1 / (end_year - start_year)) - 1,
        cagr_percent = cagr * 100,
        .groups = "drop"
      )
    
#-------------------------------------------------- SUPPLEMENTARY FIGURE 20 -------------------------------------------------------------------------------------------------------
    #Here get temeprature for 50% underdeliveirng for 95th percentile, as opposed to 50th
    AR6_data_complete %>%
      filter(Variable == "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|95.0th Percentile" ) %>%
      gather(year, value, -Model, -Scenario, -Region, -Variable, -Unit) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year >= 2000 & year <= 2014) %>%
      select(-Model, -Scenario) %>%
      distinct()-> historical_temperatures_95
    
    filtered_median_P95 <- MAGICC_underdeliver_all_50 %>% #table only has 50%runs
      group_by(Path, year, run) %>%
      #Get median, min and max across the 407 scenarios using their P95 ensemble. 
      #So this is the distribution across scenarios for their P95 ensemble member
      summarize(median_temperature_P95 = median(P95, na.rm = TRUE),
                min_temperature_P95 = min(P95, na.rm = TRUE),
                max_temperature_P95 = max(P95, na.rm = TRUE),
                mean_temperature_P95 = mean(P95, na.rm = TRUE),
                n = (n()/600),
                #For 50% of the data
                P25 = quantile(P95, 0.25, na.rm = TRUE),
                P75 = quantile(P95, 0.75, na.rm = TRUE),
                #For 90% of data
                P5 = quantile(P95, 0.05, na.rm = TRUE),
                P95 = quantile(P95, 0.95, na.rm = TRUE),
                #For 70% of data
                P15 = quantile(P95, 0.15, na.rm = TRUE),
                P85 = quantile(P95, 0.85, na.rm = TRUE)) #Number of scenarios, ignoring ensembles 
    
    #This plot has median, min and max values across the 407 scenarios using their MEDIAN ensemble values
    #The min is the minimum across the medians of the scenarios, which have 600 ensembles.
    #Includes both original runs as well as adjusted runs
    p <- ggplot() +
      geom_ribbon(data = filtered_median_P95 %>% filter(run == "50%"), aes(x = year, ymin = P5, ymax = P95, fill = Path), alpha = 0.3) +
      geom_ribbon(data = filtered_median_P95 %>% filter(run == "50%"), aes(x = year, ymin = P15, ymax = P85, fill = Path), alpha = 0.4) +
      geom_ribbon(data = filtered_median_P95 %>% filter(run == "50%"), aes(x = year, ymin = P25, ymax = P75, fill = Path), alpha = 0.5) +
      
      geom_line(data = filtered_median_P95 %>% filter(run == "50%"), aes(x = year, y = median_temperature_P95, color = Path, linetype = run), size = 1, linetype = "solid") +
      geom_text(data = filtered_median_P95, aes(x = 2090, y = 4.8, label = paste("n =", n)), 
                size = 5, vjust = 0) +  # Adjust x, y for positioning
      geom_line(data = plot_data_temperature %>% filter(run == "100%"), aes(x = year, y = median_P95, color = Path, linetype = run), size = 0.5, linetype = "dashed") +#This is the median of P95 value across each scenario median ensemble
      geom_line(data = plot_data_temperature %>% filter(run == "0%"), aes(x = year, y = median_P95, color = Path, linetype = run), size = 0.5, linetype = "dashed") +#This is the median of P95 value across each scenario median ensemble
      geom_line(data = historical_temperatures_95, aes(x = year, y = value), color = "black", size = 1, linetype = "solid") +
      labs(title = "Temperature increase from AR6 (50% underdelivered all CDR & CCS)",
           subtitle = "For P95 value in ensemble member - P5, P15, P25, median, P75, P85, P95 across scenarios",
           x = "Year",
           y = "Temperature increase",
           color = "Scenario and Model",
           fill = "Scenario and Model") +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      facet_wrap (~ Path) +
      scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
      scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 0.5)) +
      theme_minimal() +
      figure_theme+
      theme(panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            legend.position = "none")  
    ggsave("figures/paper_figures/V3/SM20.temperature_spread_50percent_95percentile.png", p, width = 12, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM20.temperature_spread_50percent_95percentile.svg", p, width = 12, height = 12, units = "in", dpi = 600)

#-------------------------------------------------- SUPPLEMENTARY FIGURE 21 -------------------------------------------------------------------------------------------------------   
    #Here we create a figure with carbon management by technology: land CDR, engineered CDR< fossil CCS
    #We want to add data poitns for literature values
    literature_thresholds<-read_csv("data/literature_thresholds.csv") 

    #Filter out lines
    literature_thresholds %>%
      filter(Year != "threshold",
             Variable != "C storage") %>%
      mutate(Year = as.numeric(Year)) -> literature_thresholds_year
    
    ar6_reanalysis_output_sequestration_categories %>%
      mutate(Variable = if_else(Variable == "Land CDR (reanalysis)", "Land CDR",
                                 if_else(Variable == "Novel CDR", "Engineered CDR", "Fossil CCS"))) -> ar6_reanalysis_output_sequestration_categories_new
    #Do ribbon with facets
    ar6_output_sequestration_median <- ar6_reanalysis_output_sequestration_categories_new %>%
      group_by(year, Variable, Path) %>%
      summarize(median_value = median(value, na.rm = TRUE),
                median_value_Gt = median(value, na.rm = TRUE),
                min_value_Gt = min(value, na.rm = TRUE),
                max_value_Gt = max(value, na.rm = TRUE),
                P5 = quantile(value, 0.05, na.rm = TRUE),
                P15 = quantile(value, 0.15, na.rm = TRUE),
                P25 = quantile(value, 0.25, na.rm = TRUE),
                P75 = quantile(value, 0.75, na.rm = TRUE),
                P85 = quantile(value, 0.85, na.rm = TRUE),
                P95 = quantile(value, 0.95, na.rm = TRUE))
    
    # Calculate the number of unique Model_Scenario combinations for the current path
    facet_counts <- ar6_reanalysis_output_sequestration_categories_new %>%
      group_by(Path, Variable) %>%
      summarise(unique_count = n_distinct(Model_Scenario), .groups = "drop")
    
    # Plot
    p <- ggplot() +
      geom_ribbon(data = ar6_output_sequestration_median, aes(x = year, ymin = P5, ymax = P95, fill = Path), alpha = 0.3) + #90%
      geom_ribbon(data = ar6_output_sequestration_median, aes(x = year, ymin = P15, ymax = P85, fill = Path), alpha = 0.4) + #70%
      geom_ribbon(data = ar6_output_sequestration_median, aes(x = year, ymin = P25, ymax = P75, fill = Path), alpha = 0.5) + #50%
      geom_line(data = ar6_output_sequestration_median, aes(x = year, y = median_value_Gt, color = Path), size = 1.2) +

      geom_hline(yintercept = 0, color = "black") +
      geom_text(data = facet_counts, aes(x = 2095, y = 20, label = paste0("n = ", unique_count)),
                hjust = 1, vjust = 1, color = "black", size = 5) +
      facet_grid(Path ~ Variable) +
      labs(title = "CO2 Capture in AR6 ",
           subtitle = "Variables: Direct Air Capture, Enhanced Weathering, Other, CCS|Biomass, & CCS|Industrial Processes, CCS|Fossil,
          & AR6 Reanalysis|OSCARv3.2|Carbon Removal|Land|Direct  (P5, P25, median, P75, P95)",
           x = "Year",
           y = "Capture (GtCO2/yr)",
           color = "Scenario and Model",
           fill = "Scenario and Model") +
      geom_point(data = literature_thresholds_year, aes(x = Year, y = Value)) +
      scale_x_continuous(limits = c(2010, 2100), breaks = seq(2010, 2100, by = 10)) +
      scale_y_continuous(limits = c(-2, 22), breaks = seq(-2, 22, by = 4)) +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      theme_minimal() +
      figure_theme +
      theme(panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            legend.position = "none") 
    ggsave("figures/paper_figures/V3/SM21.CO2sequestration_ribbon.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM21.CO2sequestration_ribbon.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    
    #Now get cumulative\# Filter years 2020–2100 and compute cumulative capture per group using trapezoidal rule
    ar6_reanalysis_output_sequestration_categories_new %>%
      filter(year >= 2020, year <= 2100) %>%
      group_by(Model_Scenario, Model, Scenario, Region, Variable, Path, Unit) %>%
      arrange(year) %>%
      #Apply trapezoidal rule to calculate cumulative CDR
      summarise(cumulative_capture = sum(diff(year) * (head(value, -1) + tail(value, -1)) / 2)) %>%
      ungroup() -> ar6_reanalysis_output_sequestration_categories_cumulative
    
    #Calculate medians for plots
    median_values <- ar6_reanalysis_output_sequestration_categories_cumulative %>%
      group_by(Path, Variable) %>%
      summarise(
        median_capture = median(cumulative_capture, na.rm = TRUE),
      ) %>%
      ungroup()
    
    #Get n
    facet_counts <- ar6_reanalysis_output_sequestration_categories_cumulative %>%
      group_by(Path, Variable) %>%
      summarise(n_scenarios = n_distinct(Model_Scenario), .groups = "drop")
    
    p_removals <- ggplot(ar6_reanalysis_output_sequestration_categories_cumulative, aes( y = cumulative_capture, fill = Path)) +
      geom_boxplot() +
      geom_hline(yintercept = 0, color = "black") +
      scale_fill_manual(values = colors) +
      facet_grid(Path ~ Variable) +
      scale_y_continuous(limits = c(-200, 1400), breaks = seq(-200, 1400, by = 200)) +
      labs(title = "Boxplot of Cumulative CO2 Capture from AR6 and reanalysis (land)",
           subtitle = "(Variables: Carbon Sequestration|Direct Air Capture, Enhanced Weathering, Other, CCS|Biomass, 
             & CCS|Industrial Processes, CCS|Fossil, 
           & AR6 Reanalysis|OSCARV3.2|Carbon Removal|Land|Direct",
           x = "Variable",
           y = "Cumulative capture (Gt CO2)") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            legend.position = "none")
    ggsave("figures/paper_figures/V3/SM21.boxplot_cumulative_energy_CO2_removals_2020_2100.png", width = 6, height = 6, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM21.boxplot_cumulative_energy_CO2_removals_2020_2100.svg", width = 6, height = 6, units = "in", dpi = 600)
    
#-------------------------------------------------- SUPPLEMENTARY FIGURE 22 ------------------------------------------------------------------------------------------------------- 
    #Make figure for net-zero CO2 emissions year
    #### 1. Prepare data ####
    original_emissions <- read_csv("/Users/mariacandelariabergero/python/gegca-prototype/data/raw/interim/15849b8/V3/original/scenarios-infilled_all.csv") %>% mutate(underdelivery_percent = 0)
    
    # Define the base file path
    base_path <- "/Users/mariacandelariabergero/python/gegca-prototype/data/raw/interim/15849b8/V4/underdeliver_allCDR/"
    
    # Define percentages (10 to 100 in steps of 10)
    percentages <- seq(10, 100, by = 10)
    
    # Read in all files and combine them
    all_data <- map_dfr(percentages, function(pct) {
      file_path <- paste0(base_path, pct, "/scenarios-infilled_all_underdelivered_", pct, "percent.csv")
      # Read file and add a column to track the percentage
      read_csv(file_path) %>%
        mutate(underdelivery_percent = pct)
    })
    
    #Merge all data 2015-2100
    original_emissions %>% #Original emissions
      bind_rows(all_data) %>% #under-delivering 10-100% every 10%
      gather(year, value, -model, -scenario, -region, -variable, -unit, -underdelivery_percent) %>%
      mutate(year = substr(year, 1, 4)) %>% #clean year column
      mutate(year = as.numeric(year)) %>%
      left_join(C_paths, by = c("scenario" = "Scenario", "model" = "Model")) %>%
      unite(Model_Scenario, c(model, scenario), sep = "_", remove = FALSE) %>%
      left_join(AR6_GWP, by = "variable")%>% #Bring in multipliers GWP 100 years from IPCC AR6
      filter(!is.na(GWP_100))%>% #Filter out BC, CO, NH3, NOx, OC, SUlfur and VOC (not GHGs)
      select(-Notes) %>%
      mutate(value = ifelse(grepl("kt", unit), value / 1000, value)) %>% #Convert gases in kilo ton to Mega ton
      mutate(value_CO2eq = value * GWP_100) %>%
      group_by(Model_Scenario, model, scenario, region, year, Path, GHG, underdelivery_percent) %>%
      summarise(sum = sum(value_CO2eq)) %>%
      ungroup() %>%
      mutate(unit = "MtCO2eq")-> underdelivering_all_gases
    
    #Add all GHGs
    #First history 2010-2015
    history_ar6 %>%
      select(Variable, Unit, `2010`,`2011`,`2012`,`2013`,`2014`, `2015`) %>%
      #Prepare variabels for join
      separate(Variable, c("AR6", "emissions", "Variable", "harmonized", "other"), sep = "\\|") %>%
      mutate(joined_column = if_else(is.na(other), paste(emissions, Variable, sep = "|"), 
                                     paste(emissions, Variable, harmonized, sep = "|"))) %>%
      select(-AR6, -emissions, -Variable, -harmonized, -other) %>%
      rename(variable = joined_column) %>%
      gather(year, value, -variable, -Unit) %>%
      left_join(AR6_GWP, by = "variable") %>% #Bring in multipliers GWP 100 years from IPCC AR6
      filter(!is.na(GWP_100))%>% #Filter out BC, CO, NH3, NOx, OC, SUlfur and VOC (not GHGs)
      select(-Notes) %>%
      mutate(value = ifelse(grepl("kt", Unit), value / 1000, value)) %>% #Convert gases in kilo ton to Mega ton
      mutate(value_CO2eq = (value * GWP_100) / 1000, #convert to co2 equivalent, and ot Gt
             Unit = "GtCO2eq",
             year = as.numeric(year)) %>%
      filter(!is.na(value)) %>% #filter out NAs in Emissions|HFC|HFC245ca
      group_by(Unit, year, GHG) %>%
      summarise(sum = sum(value_CO2eq)) %>%
      ungroup() %>%
      group_by(Unit, year) %>%
      mutate(total_GHG = sum(sum)) %>%
      ungroup() %>%
      #Note: history_ar6 data does not have all non-CO2, so we ignore total for
      #2015 and use under delivering_all_gases for this year (which has all those non-CO2. The issue is with f-gases)
      mutate(total_GHG = if_else(year == 2015, 55.85171, total_GHG))-> history_ar6_fixed_GHG_2010_2015
    
    #Now modeled GHGs 2015-2100
    underdelivering_all_gases %>%
      group_by(Model_Scenario, model, scenario, region, year, Path, unit, underdelivery_percent) %>%
      summarise(totalGHG = sum(sum)) %>%
      ungroup() %>%
      mutate(totalGHG = totalGHG / 1000,
             unit = "GtCO2eq/yr")-> underdelivering_all_gases_total
    #Note, these values are very similar to IPCC https://www.ipcc.ch/report/ar6/wg3/figures/summary-for-policymakers/figure-spm-1/
    
    #Only CO2
    #bring in 2010 CO2
    history_ar6 %>%
      select(Variable, Unit, `2010`,`2011`,`2012`,`2013`,`2014`, `2015`) %>%
      filter(Variable == "AR6 climate diagnostics|Emissions|CO2|Unharmonized") %>%
      gather(year, value, -Variable, -Unit) %>%
      mutate(year = as.numeric(year)) %>%
      mutate(totalCO2 = value/ 1000,
             unit = "GtCO2/yr") %>%
      select(-Unit, -Variable)-> history_ar6_fixed_2010_2015
    
    #Modeled under-delivering capture
    underdelivering_all_gases %>%
      filter(GHG %in% c("CO2 (energy)", "CO2 (land)")) %>%
      group_by(Model_Scenario, model, scenario, region, year, Path, unit, underdelivery_percent) %>%
      summarise(totalCO2 = sum(sum)) %>%
      ungroup()%>%
      mutate(totalCO2 = totalCO2 / 1000,
             unit = "GtCO2/yr")-> underdelivering_all_CO2
    
    #### 2. Find net-zero CO2 year####
    underdelivering_all_CO2 %>%
      group_by(Model_Scenario, model, scenario, region, Path, unit, underdelivery_percent) %>%    
      mutate(
        net_zero_year = ifelse(
          any(totalCO2 < 0, na.rm = TRUE),   # Check if there is any negative value
          year[which(totalCO2 < 0)[1]],      # Select the first year where CO2_emissions is negative
          2101)) %>%
      select(Model_Scenario, model, scenario, region, Path, unit, underdelivery_percent, net_zero_year) %>%
      distinct() -> closest_to_zero_year
  
    #### 3. Plot ####
    #Go to wide format
    closest_to_zero_year %>%
      #filter(underdelivery_percent %in% c(0, 100)) %>%
      spread(underdelivery_percent, net_zero_year) %>%
      mutate(year_diff = `100` - `0`)->closest_to_zero_year_wide
    #Note that here year 2100 may mean the scenairo does not reach net zero by then
    
    median_diff_by_path <- closest_to_zero_year_wide %>%
      group_by(Path) %>%
      summarise(median_0_underdelivered = median(`0`, na.rm = TRUE),
                median_100_underdelivered = median(`100`, na.rm = TRUE),
                median_year_diff = median(year_diff, na.rm = TRUE)) %>%
      arrange(Path)

    closest_to_zero_year_wide_ordered <- closest_to_zero_year_wide %>%
      group_by(Path) %>%
      mutate(Model_Scenario = fct_reorder(Model_Scenario, `0`, .desc = FALSE)) %>%
      ungroup()
    
    scenario_counts <- closest_to_zero_year_wide_ordered %>%
      group_by(Path) %>%
      summarise(n_scenarios = n_distinct(Model_Scenario))
    
    p<-ggplot(closest_to_zero_year_wide_ordered, aes(y = Model_Scenario, color = Path)) +
      geom_segment(aes(x = `0`, xend = `100`, yend = Model_Scenario), 
                   size = 1.2, alpha = 0.8) +
      geom_point(aes(x = `0`), size = 2, shape = 1, fill = "white", stroke = 1.2) +
      geom_point(aes(x = `100`), size = 2, shape = 16, fill = "white", stroke = 1.2) +
      facet_wrap(~Path, scales = "free_y") +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      scale_x_continuous(breaks = seq(2030, 2101, by = 10), limits = c(2030, 2101)) +
      labs(
        title = "Net-zero year shift with underdelivery (Lollipop Plot)",
        subtitle = "From 0% (hollow) to 100% (fill)",
        x = "Year",
        y = NULL
      ) +
      figure_theme +
      theme(panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            legend.position = "none",
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) 
    ggsave("figures/paper_figures/V3/SM22.netzero_emissions_year.png", p, width = 12, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM22.netzero_emissions_year.svg", p, width = 12, height = 12, units = "in", dpi = 600)
    
    
    
    ###TEST with heatmap
    # Step 1 — Create an ordering vector for Model_Scenario
    model_order <- closest_to_zero_year %>%
      filter(underdelivery_percent == 0) %>%
      arrange(net_zero_year) %>%
      pull(Model_Scenario)
    
    # Step 2 — Apply this ordering as a factor
    closest_to_zero_year_heat <- closest_to_zero_year %>%
      mutate(Model_Scenario = factor(Model_Scenario, levels = model_order))
    
    # Calculate median net-zero year per Path and underdelivery_percent
    median_net_zero <- closest_to_zero_year %>%
      group_by(Path, underdelivery_percent) %>%
      summarise(median_net_zero_year = round(median(net_zero_year, na.rm = TRUE))) %>%
      ungroup()
    
    median_net_zero %>%
      spread(underdelivery_percent, median_net_zero_year) -> median_net_zero_wide
    
    # Step 3 — Make your plot
    p_heatmap <- ggplot(closest_to_zero_year_heat, aes(x = underdelivery_percent, y = Model_Scenario, fill = net_zero_year)) +
      geom_tile(color = "white", size = 0.1) +
      facet_wrap(~Path, scales = "free_y") +
      scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
      labs(
        title = "Net-zero year across underdelivery percentages",
        x = "Underdelivery (%)",
        y = NULL,
        fill = "Net-zero year"
      ) +
      figure_theme +
      theme(
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text.y = element_blank(),  # show text if desired!
        axis.ticks.y = element_blank()
      )
    ggsave("figures/paper_figures/V3/SM22.netzero_emissions_year_all.png", p_heatmap, width = 12, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/V3/SM22.netzero_emissions_year_all.svg", p_heatmap, width = 12, height = 12, units = "in", dpi = 600)
    
    
    
    
