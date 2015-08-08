plot2 <- function() {

	# Get the data
	if (!file.exists("hpc_Feb_2002.csv")) {
		# Read and subset data, then save the subset
		df <- read.table("household_power_consumption.txt",sep=";",na.strings="?",header=TRUE)
		df1 <- df[df$Date=="1/2/2007" | df$Date=="2/2/2007",]
		write.csv(df1, file="hpc_Feb_2002.csv")
	}
	df <- read.csv("hpc_Feb_2002.csv")

	# Add timestamp column based on Date and Time columns
	df$timestamp <- apply(
		df,
		1,
		function(x)
			as.numeric( strptime( paste(x[2],x[3]), "%d/%m/%Y %H:%M:%S") )

	)

	# Omit any rows with NA columns
	df <- df[complete.cases(df),]

	# Set up plot device as png
	png(filename = "plot2.png",
		width = 480, height = 480,
		units = "px", pointsize = 12,
		bg = "white",  res = NA,
		type = "cairo")

	# Calculate x-axis tick mark locations
	tick1 = as.numeric( strptime( "1/2/2007", "%d/%m/%Y"))  #Thursday
	tick2 = as.numeric( strptime( "2/2/2007", "%d/%m/%Y"))  #Friday
	tick3 = as.numeric( strptime( "3/2/2007", "%d/%m/%Y"))  #Saturday

	# Plot 2
	plot(	df$timestamp, df$Global_active_power,
		pch=NA,  xaxt="n",
		xlab="",
		ylab="Global Active Power (kilowatts)"
	)
	lines(df$timestamp, df$Global_active_power, type="l")
	axis(1, tick=TRUE, at=c(tick1,tick2,tick3), labels=c("Thu","Fri","Sat") )

	dev.off()

	# Set plot device back to windows
	windows()
 
}
