#!/usr/bin/env Rscript
library("gdata") #Needed for the read.xls function

pcls = rep_len(c("black", "blue", "green", "red", "yellow", "darkblue", "magenta", "gold", "forestgreen", "darkorange3", "darkorchid", "darkmagenta", "darkolivegreen", "brown1", "azure4", "chartreuse4", "cadetblue4", "coral", "deeppink4", "honeydew4", "greenyellow"), 28) #Random list of colours, which wraps around so that the list is 28 items long, and chosen to not have similar colours next to each other hopefully
titles = c("Day 2", "Day 4", "Day 6", "Day 9", "Day 11", "Day 13") #Titles for the first set of graphs
daynos = c( 2, 4, 6, 9, 11, 13) #The numbers for the days in integer form
s8 = read.xls("CLS76_dataset from Matt Kaeberlein(1).xls", sheet=8) #The names of the columns are in sheet 8, column D

daylist = list() #We need a list of data frames to store each days worth of stuff
for (sheet in 1:6) #There are six sheets of data
{
	s1 = read.xls("CLS76_dataset from Matt Kaeberlein(1).xls", sheet=sheet) #read the sheet
	s1 = s1[-c(1), ] #Remove the first row, as this is noise
	times = s1[1] #Grab the times column
	#The following is a hackish way to convert this column into a floating point of hours
	times2 = sprintf("%s", times[[1]]) 
	times3 = strptime(times2, format = "%H:%M:%S")
	times4 = times3$hour + ((times3$sec / 60) + times3$min) / 60
	daylist[[sheet]] = data.frame(times4) #begin the data frame for day 2 by adding our new and improved Time column
	names(daylist[[sheet]])[1] = "Time" #Name the column while we are at it
	blanks = (s1[86] + s1[87] + s1[88]) / 3 # Get blanks column for normalisation
	for (i in 1:28) #Loop through the non-blank columns
	{
		daylist[[sheet]][i + 1] = (s1[i * 3 - 1] + s1[i * 3] + s1[i * 3 + 1]) / 3 - blanks #Create columns from averaging three columns
		names(daylist[[sheet]])[i + 1] = toString(s8[i * 3 - 2, 4]) #Modify name of column
	}
}
mutants = names(daylist[[1]])[-1] #The names of all the mutants. The first item is "Time" so that is excluded

xax = c(0, 25) #X-axis parameters
yax = c(0, 1.7) #Y-axis parameters

#Make SVG of first set of six graphs
svg("Growth of all mutants (part 1).svg")
par(mfrow=c(2,3)) #Three rows, two columns
par(oma = c(0,0,0,4)) #Add some blank space on the right for the legend
for (sheet in 1:6) #Loop through the days again
{
	par(col="black") #Base colour is black
	par(pty="s") #Square plotting regions
	plot(1, type="n", xlab="Time (hours)", ylab="Absorbency at OD600", xlim=xax, ylim=yax, main=titles[sheet]) #Blank plot, with labels but not data
	for (i in 1:28) #Loop through columns
	{
		par(col=pcls[i], pch=(i %% 25)) #Set colour and symbol (currently unused)
		lines(daylist[[sheet]][[1]], daylist[[sheet]][[i + 1]], type="l") #Plot the data
	}
}
#Create legend
par(oma = c(0,0,0,0), new=TRUE, fig=c(0, 1, 0, 1), mar = c(0,0,0,0), col="black")
plot(0,0,type="n",bty="n",xaxt="n",yaxt="n")
legend("right", bty="n", mutants, lty=c(1,1), horiz=FALSE, lwd=c(2.5, 2.5), col=pcls, cex=0.80, xpd=TRUE)
#Chart done!
dev.off()

#Make SVGs of second set of 28 graphs
svg("Impact of each mutation (part 2) 01 to 12.svg") #First 12
par(mfrow=c(3,4)) #3 rows, 4 columns
par(oma = c(1,0,0,0)) #Space for legend
for (i in 1:12) #loop through first 12
{
	par(col="black") #Default colour should be black
	plot(1, type="n", xlab="Time (hours)", ylab="Absorbency at OD600", xlim=xax, ylim=yax, main=mutants[i]) #Blank plot, with labels but not data
	for (sheet in 1:6) #loop through the sheets also
	{
		par(col=pcls[sheet], pch=(sheet %% 25)) #Set colour and symbol (currently unused)
		lines(daylist[[sheet]][[1]], daylist[[sheet]][[i + 1]], type="l") #Plot the data
	}
}
#Create legend
par(oma = c(0,0,0,0), new=TRUE, fig=c(0,1,0,1), mar = c(0,0,0,0), col="black")
plot(0,0,type="n",bty="n",xaxt="n",yaxt="n")
legend("bottom", bty="n", titles, lty=c(1,1), horiz=TRUE, lwd=c(2.5, 2.5), col=pcls, cex=0.80, xpd=TRUE)
#Chart done!
dev.off()

svg("Impact of each mutation (part 2) 13 to 20.svg") #Second 12
par(mfrow=c(3,4))
par(oma = c(1,0,0,0))
for (i in 13:24)
{
	par(col="black")
	plot(1, type="n", xlab="Time (hours)", ylab="Absorbency at OD600", xlim=xax, ylim=yax, main=mutants[i]) #Blank plot, with labels but not data
	for (sheet in 1:6)
	{
		par(col=pcls[sheet], pch=(sheet %% 25)) #Set colour and symbol (currently unused)
		lines(daylist[[sheet]][[1]], daylist[[sheet]][[i + 1]], type="l") #Plot the data
	}
}
par(oma = c(0,0,0,0), new=TRUE, fig=c(0,1,0,1), mar = c(0,0,0,0), col="black")
plot(0,0,type="n",bty="n",xaxt="n",yaxt="n")
legend("bottom", bty="n", titles, lty=c(1,1), horiz=TRUE, lwd=c(2.5, 2.5), col=pcls, cex=0.80, xpd=TRUE)
#Chart done!
dev.off()

svg("Impact of each mutation (part 2) 21 to 28.svg") #Final 4
par(mfrow=c(3,4)) #NB: This will make plots of the same dimensions as before, for consistency. This SVG file will need to be edited slightly
par(oma = c(1,0,0,0))
for (i in 25:28)
{
	par(col="black")
	plot(1, type="n", xlab="Time (hours)", ylab="Absorbency at OD600", xlim=xax, ylim=yax, main=mutants[i]) #Blank plot, with labels but not data
	for (sheet in 1:6)
	{
		par(col=pcls[sheet], pch=(sheet %% 25)) #Set colour and symbol (currently unused)
		lines(daylist[[sheet]][[1]], daylist[[sheet]][[i + 1]], type="l") #Plot the data
	}
}
par(oma = c(0,0,0,0), new=TRUE, fig=c(0,1,0,1), mar = c(0,0,0,0), col="black")
plot(0,0,type="n",bty="n",xaxt="n",yaxt="n")
legend("bottom", bty="n", titles, lty=c(1,1), horiz=TRUE, lwd=c(2.5, 2.5), col=pcls, cex=0.80, xpd=TRUE)
#Chart done!
dev.off()

numtimes = length(daylist[[1]][[1]])

#Calculate inflection doubling time
inflection = data.frame(daynos) # First column is the day numbers
names(inflection)[1] = "Day" # Name of first column is "Day"
for (i in 1:28) # Loop through the different strains
{
	dtimes = vector(length = 6) # Vector to temporarily store the column
	for (sheet in 1:6) # Loop through the sheets/days
	{
		diffs = vector(length = (numtimes - 1)) # Vector for the diffs (one less than the number of data points)
		for (d in 2:numtimes) # Loop through data points
		     {
			     diffs[d - 1] = (log(daylist[[sheet]][[i + 1]][d]) - log(daylist[[sheet]][[i + 1]][d - 1]))/(daylist[[sheet]][[1]][d] - daylist[[sheet]][[1]][d - 1]) # Calculate differentials
		     }
		dtimes[sheet] = log(2)/max(diffs) # log(2) / maximum diff goes into data frame
	}
	inflection[i + 1] = dtimes # Insert column into data frame
	names(inflection)[1 + i] = mutants[i] # Name column
}

#Calculate interval doubling time
intervald = data.frame(daynos)
names(intervald)[1] = "Day"
for (i in 1:28)
{
	dtimes = vector(length = 6)
	for (sheet in 1:6) #Loop through sheets/days
	{
		bt = 0 #Represents the lower bound of the .2->.5 gap
		tp = 0 #Represents upper bound
		for (q in 1:numtimes) #Loop through column
		{
			if (daylist[[sheet]][[i + 1]][q] < 0.5) {
				tp = q #Will end up with the index of the highest value under .5
			}
			if (daylist[[sheet]][[i + 1]][numtimes - q + 1] > 0.2) {
				bt = numtimes - q + 1 #index of lowest over .2
			}
		}
		if (((bt == 0) | (tp == 0)) | (bt == tp)) {
			dtimes[sheet] = NA #Certain situations are not usable
		} else {
			dtimes[sheet] = log(2) / ((log(daylist[[sheet]][[i+1]][tp]) - log(daylist[[sheet]][[i+1]][bt])) / (daylist[[sheet]][[1]][tp] - daylist[[sheet]][[1]][bt])) #Set value
		}
	}
	intervald[i + 1] = dtimes # Insert column
	names(intervald)[1 + i] = mutants[i] # Name column
}

#Make charts of doubling time

dxarr = c(0, 14)
dyarr = c(0, 200)

svg("Doubling time (part 3) 01 to 12.svg") #First 12
par(mfrow=c(3,4)) #3 rows, 4 columns
par(oma = c(1,0,0,0)) #Space for legend
for (i in 1:12) #loop through first 12
{
	par(col="black") #Default colour should be black
	plot(1, type="n", xlab="Age (days)", ylab="Doubling time (minutes)", xlim=dxarr, ylim=dyarr, main=mutants[i]) #Blank plot, with labels but not data
	par(col=pcls[1], pch=1) #Set colour and symbol (currently unused)
	lines(inflection[[1]], inflection[[i + 1]]*60, type="l") #Plot the data
	par(col=pcls[2], pch=2)
	lines(intervald[[1]], intervald[[i + 1]]*60, type="l")
}
#Create legend
par(oma = c(0,0,0,0), new=TRUE, fig=c(0,1,0,1), mar = c(0,0,0,0), col="black")
plot(0,0,type="n",bty="n",xaxt="n",yaxt="n")
legend("bottom", bty="n", c("Inflection doubling time", "Interval doubling time"), lty=c(1,1), horiz=TRUE, lwd=c(2.5, 2.5), col=pcls, cex=0.80, xpd=TRUE)
#Chart done!
dev.off()

svg("Doubling time (part 3) 13 to 24.svg") #Second 12
par(mfrow=c(3,4)) #3 rows, 4 columns
par(oma = c(1,0,0,0)) #Space for legend
for (i in 13:24) #loop through first 12
{
	par(col="black") #Default colour should be black
	plot(1, type="n", xlab="Age (days)", ylab="Doubling time (minutes)", xlim=dxarr, ylim=dyarr, main=mutants[i]) #Blank plot, with labels but not data
	par(col=pcls[1], pch=1) #Set colour and symbol (currently unused)
	lines(inflection[[1]], inflection[[i + 1]]*60, type="l") #Plot the data
	par(col=pcls[2], pch=2)
	lines(intervald[[1]], intervald[[i + 1]]*60, type="l")
}
#Create legend
par(oma = c(0,0,0,0), new=TRUE, fig=c(0,1,0,1), mar = c(0,0,0,0), col="black")
plot(0,0,type="n",bty="n",xaxt="n",yaxt="n")
legend("bottom", bty="n", c("Inflection doubling time", "Interval doubling time"), lty=c(1,1), horiz=TRUE, lwd=c(2.5, 2.5), col=pcls, cex=0.80, xpd=TRUE)
#Chart done!
dev.off()

svg("Doubling time (part 3) 25 to 28.svg") #Final 4
par(mfrow=c(3,4)) #3 rows, 4 columns
par(oma = c(1,0,0,0)) #Space for legend
for (i in 25:28) #loop through first 12
{
	par(col="black") #Default colour should be black
	plot(1, type="n", xlab="Age (days)", ylab="Doubling time (minutes)", xlim=dxarr, ylim=dyarr, main=mutants[i]) #Blank plot, with labels but not data
	par(col=pcls[1], pch=1) #Set colour and symbol (currently unused)
	lines(inflection[[1]], inflection[[i + 1]]*60, type="l") #Plot the data
	par(col=pcls[2], pch=2)
	lines(intervald[[1]], intervald[[i + 1]]*60, type="l")
}
#Create legend
par(oma = c(0,0,0,0), new=TRUE, fig=c(0,1,0,1), mar = c(0,0,0,0), col="black")
plot(0,0,type="n",bty="n",xaxt="n",yaxt="n")
legend("bottom", bty="n", c("Inflection doubling time", "Interval doubling time"), lty=c(1,1), horiz=TRUE, lwd=c(2.5, 2.5), col=pcls, cex=0.80, xpd=TRUE)
#Chart done!
dev.off()


#Time shift calculation


tshift = data.frame(daynos)
names(tshift)[1] = "Day"
for (i in 1:28)
{
	reach = vector(length = 6)
	for (sheet in 1:6) #Loop through sheets/days
	{
		tlow = 0
		for (l in 1:numtimes) {
			if (daylist[[sheet]][[i + 1]][l] > 0.3) {
				tlow = l - 1
				break;
			}
		}
		if (tlow == 0) {
			reach[sheet] = NA
		} else {
			ya = log(daylist[[sheet]][[i + 1]][tlow])
			yb = log(daylist[[sheet]][[i + 1]][tlow + 1])
			xa = daylist[[sheet]][[1]][tlow]
			xb = daylist[[sheet]][[1]][tlow + 1]
			slope = (ya - yb) / (xa - xb)
			reach[sheet] = (0.2 - ya) / slope + xa
		}
	}
	tshift[i + 1] = reach - reach[1]
	names(tshift)[i + 1] = mutants[i]
}

#Graph time shifts

txa = c(0, 13)
tya = c(0, 20)

svg("Time Shift (part 4a) 01 to 12.svg") #First 12
par(mfrow=c(3,4)) #3 rows, 4 columns
par(oma = c(0,0,0,0)) #Space for legend
for (i in 1:12) #loop through first 12
{
	par(col="black") #Default colour should be black
	plot(1, type="n", xlab="Age (days)", ylab="Time shift (hours)", xlim=txa, ylim=tya, main=mutants[i]) #Blank plot, with labels but not data
	par(col=pcls[1], pch=1) #Set colour and symbol (currently unused)
	lines(tshift[[1]], tshift[[i + 1]], type="l") #Plot the data
}
#Chart done!
dev.off()

svg("Time Shift (part 4a) 13 to 24.svg") #Second 12
par(mfrow=c(3,4)) #3 rows, 4 columns
par(oma = c(0,0,0,0)) #Space for legend
for (i in 13:24) #loop through first 12
{
	par(col="black") #Default colour should be black
	plot(1, type="n", xlab="Age (days)", ylab="Time shift (hours)", xlim=txa, ylim=tya, main=mutants[i]) #Blank plot, with labels but not data
	par(col=pcls[1], pch=1) #Set colour and symbol (currently unused)
	lines(tshift[[1]], tshift[[i + 1]], type="l") #Plot the data
}
#Chart done!
dev.off()

svg("Time Shift (part 4a) 25 to 28.svg") #Final 4
par(mfrow=c(3,4)) #3 rows, 4 columns
par(oma = c(0,0,0,0)) 
for (i in 25:28) #loop through first 12
{
	par(col="black") #Default colour should be black
	plot(1, type="n", xlab="Age (days)", ylab="Time shift (hours)", xlim=txa, ylim=tya, main=mutants[i]) #Blank plot, with labels but not data
	par(col=pcls[1], pch=1) #Set colour and symbol (currently unused)
	lines(tshift[[1]], tshift[[i + 1]], type="l") #Plot the data
}
#Chart done!
dev.off()

svg("Time Shift (all).svg") #All together
par(col = "black", oma=c(0, 0, 0, 0))
plot(1, type="n", xlab="Age (days)", ylab="Time shift (hours)", xlim = txa, ylim = tya, main="Time Shift of all strains\n(Relative time to reach an OD of 0.3)")
for (i in 1:28)
{
	par(col=pcls[i], pch=((i %% 25) + 1))
	lines(tshift[[1]], tshift[[i + 1]], type="l")
}
par(col="black")
legend("left", bty="n", mutants, lty=c(1,1), horiz=FALSE, lwd=c(2.5, 2.5), col=pcls, cex=0.80, xpd=TRUE)
dev.off()

