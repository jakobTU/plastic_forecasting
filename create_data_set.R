### THIS FILE CREATES AN EXAMPLE DATA SET. THE NUMBERS ARE PURELY MADE UP AND
### ARE ONLY FOR EXEMPLARY PURPOSES.

# Means and standard deviations for the 13 fractions and the input material.
means <- c(10, 20, 30, 20, 15, 10, 5, 5, 10, 25, 30, 10, 1, 100)
sds <- means / 2

# Sample Gaussian distributed data, set the minimum to 0 and save it in a data.frame.
set.seed(123)
ts_materials <- as.data.frame(pmax(mapply(function(i) rnorm(1375, mean=means[i], sd=sds[i]), i=1:14), 0))

# The variable names
var_names <- c('GTK', 'Folie', 'Hohlkörper', 'MKh', 'MKw', 'NE.metall...ALU',
               'PPK', 'PE', 'PET.flaschen', 'PET.schale', 'PP', 'PS',
               'Sortierreste', 'tonnes')
names(ts_materials) <- var_names

# Get the sum of the fractions.
ts_materials$sum_materials <- rowSums(ts_materials[ , 1:13])

# Randomly sample the 24 holidays and bridging days on which the plant was out of production.
ts_materials[sample(1375, 24), ] <- 0

# Add dates to the data.
ts_materials$time <- seq(as.Date('2020/03/02'), by='day', length.out=1375)

# Save the data.frame.
write.csv(ts_materials, 'input_output_data.csv', row.names=FALSE)