plot3 <- function() {

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
	png(filename = "plot3.png",
		width = 480, height = 480,
		units = "px", pointsize = 12,
		bg = "white",  res = NA,
		type = "cairo")

	# Calculate x-axis tick mark locations
	tick1 = as.numeric( strptime( "1/2/2007", "%d/%m/%Y"))  #Thursday
	tick2 = as.numeric( strptime( "2/2/2007", "%d/%m/%Y"))  #Friday
	tick3 = as.numeric( strptime( "3/2/2007", "%d/%m/%Y"))  #Saturday

	# Determine minimum and maximum of sub metering observations
	ymin = min(c(
			min(df$Sub_metering_1),
			min(df$Sub_metering_2),
			min(df$Sub_metering_3)
			)
		)

	ymax = max(c(
			max(df$Sub_metering_1),
			max(df$Sub_metering_2),
			max(df$Sub_metering_3)
			)
		)

	# Plot 3
	plot(	df$timestamp, df$Global_active_power,
		pch=NA,  xaxt="n",
		ylim=c(ymin, ymax),
		xlab="",
		ylab="Energy sub metering"
	)

	lines(df$timestamp, df$Sub_metering_1, type="l", col="black")
	axis(1, tick=TRUE, at=c(tick1,tick2,tick3), labels=c("Thu","Fri","Sat") )

	lines(df$timestamp, df$Sub_metering_2, type="l", col="red")
	axis(1, tick=TRUE, at=c(tick1,tick2,tick3), labels=c("Thu","Fri","Sat") )

	lines(df$timestamp, df$Sub_metering_3, type="l", col="blue")
	axis(1, tick=TRUE, at=c(tick1,tick2,tick3), labels=c("Thu","Fri","Sat") )

	legend("topright"
		, legend = c("Sub metering 1","Sub metering 2", "Sub metering 3")
		, lty = 1
		, lwd = 2
		, col = c("black","red","blue")
	)

	dev.off()

	# Set plot device back to windows
	windows()
 
}
