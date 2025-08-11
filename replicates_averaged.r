#specify your sampleid and treatmentid below
my_cellline <- "COLO 205"
my_drug <- "Afatinib"

#specify your PSet
screen <- GDSC_PSet

sensitivity_data <- sensitivityRaw(screen)

#this program prints variance between replicates at each dose and a plot of the average dose response curve

#here, edit the pattern based on how replicate experiments are named in the PSet you are using
pattern <- paste0("^", my_cellline, "_", my_drug, "_")

pattern <- gsub("\\[", "\\\\[", pattern)
pattern <- gsub("\\]", "\\\\]", pattern)
pattern <- gsub("\\.", "\\\\.", pattern)
pattern <- gsub("\\(", "\\\\(", pattern)
pattern <- gsub("\\)", "\\\\)", pattern)

print(pattern)
experiment_ids <- dimnames(sensitivityRaw(screen))[[1]]

matches <- grepl(pattern, experiment_ids)

matching_experiment_ids <- experiment_ids[matches]
#print(matching_experiment_ids)
print(paste("Number of experiments:", length(matching_experiment_ids)))

if (length(matching_experiment_ids) > 1) {
  replicates_dose_viability_list <- list()
  
  for (experiment_id in matching_experiment_ids) {
    current_experiment_df <- as.data.frame(sensitivity_data[experiment_id, , ])
    
    replicates_dose_viability_list[[experiment_id]] <- current_experiment_df
  }
  matches_dose_viability <- na.omit(do.call(rbind, replicates_dose_viability_list))
  #rounding can be changed but makes sure that extremely close doses don't show up twice
  matches_dose_viability$Dose <- round(matches_dose_viability$Dose, 3)
  average_viability <- aggregate(Viability ~ Dose, data = matches_dose_viability, FUN = mean)
  statistical_variance <- aggregate(Viability ~ Dose, data = matches_dose_viability, FUN = var)
  variance_df <- statistical_variance
  
  colnames(variance_df)[colnames(variance_df) == "Viability"] <- "Variance"
  average_viability$Viability <- average_viability$Viability/100
  graph_data <- average_viability[order(average_viability$Dose), ]
  
  message("\nAverage Viabilities per Dose")
  print(graph_data)
  print(variance_df)
  
  plot(log10(graph_data$Dose), graph_data$Viability,
       xlab = "Log10 Concentration", ylab = "Viability",
       main = paste0("Average Dose-Response for ", pattern),
       pch = 16,
       ylim = c(min(0, graph_data$Viability), max(1.1, graph_data$Viability)))
  lines(log10(graph_data$Dose), graph_data$Viability)
} else if(length(matching_experiment_ids) == 1) {
  drugDoseResponseCurve(screen, drug = my_drug, cellline = my_cellline)
}

  



