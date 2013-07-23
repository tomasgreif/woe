
# Get summary IV for data frame
iv.mult(german_data,"gb",summary=TRUE)

# Plot summary IV for data frame
iv.plot.summary(iv.mult(german_data,"gb",summary=TRUE))

# Plot WoE patterns for individual variables
iv.plot.woe(iv=iv.mult(german_data,"gb",vars=c("age","duration","ca_status")))

# Plot WoE patterns for all variables
iv.plot.woe(iv=iv.mult(german_data,"gb"))

# Transform raw variables to WoE (creates new columns [original_column_name]_woe)
str(iv.replace.woe(german_data,iv.mult(german_data,"gb")))