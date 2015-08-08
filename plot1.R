# Get Feb.1st & 2nd of 2007 and write it to a separate file.

plot1 <- function(typ="h") {

	# Get the data
	if (!file.exists("hpc_Feb_2002.csv")) {
		# Read and subset data, then save the subset
		df <- read.table("household_power_consumption.txt",sep=";",na.strings="?",header=TRUE)
		df1 <- df[df$Date=="1/2/2007" | df$Date=="2/2/2007",]
		write.csv(df1, file="hpc_Feb_2002.csv")
	}
	df <- read.csv("hpc_Feb_2002.csv")

	# Set up plot device as png
	png(filename = "plot1.png",
		width = 480, height = 480,
		units = "px", pointsize = 12,
		bg = "white",  res = NA,
		type = "cairo")

	# Plot 1
	hist(	df$Global_active_power,
		breaks=12,
		col="red",
		main="Global Active Power",
		xlab="Global Active Power (kilowatts)"
	)
	dev.off()

	# Set plot device back to windows
	windows()

}
