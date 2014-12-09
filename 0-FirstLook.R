# efg, 24 April 2014

setwd("C:/Data/US-Government/Census-Bureau/Missouri-Census-Data-Center/")

d <- read.csv("xtract-2014-04-18.csv", colClasses="character")
dim(d)

# Remove commas or $ from all fields
for (i in 1:ncol(d))
{
  d[,i] <- gsub(",|\\$", "", d[,i])
}

d <- d[-1,]  # Remove first row

# Convert % fields to numeric
pct.name <- grep("^(P|p)ct", names(d), value=TRUE)
median.name <- grep("^(M|m)edian", names(d), value=TRUE)

all.name <- c(pct.name, median.name)
#all.name

# Convert to numeric
for (i in 1:length(all.name))
{
  d[,all.name[i]] <- as.numeric(d[,all.name[i]])
}

# after manual inspection, reduce to this list
keep.list <- c(
  "pctUnder18",        "pctOver65",
  "pctWhite1",         "pctBlack1",
  "pctAsian1",         "pctHispanicPop",
  "pctpoor",           "pctInCollege",
  "pctBachelorsormore","pctForeignBorn",
  "pctRenterOcc",      "MedianAge",
  "MedianHHInc",       "MedianFamInc",
  "MedianHValue",      "MedianGrossRent")

### State subsets
d.US <- d
d.KS <- d[d$state == "20", ]
d.MO <- d[d$state == "29", ]

options(scipen=6)
pdf(paste0("0-FirstLook-Charts.pdf"))

par(mfcol=c(3,1), mar=c(2.5,4.1,2.5,2.1), oma=c(1,0,1,0))

plot.comparison <- function(TARGET, THIS, d, BASE, target.data, XLIM)
{
  hist(d[,THIS], main="", xlab="", col="gray", xlim=XLIM, breaks=40)
  mtext(paste(nrow(d), BASE, "ZCTAs"), adj=0, cex=0.75)
  abline(v=target.data[1,THIS], col="blue", lwd=2)

  x <- target.data[1,THIS]
  #y <- par()$usr[3] + 0.75*strheight("X")
  y <- par()$usr[4]
  q.level <- round(100* sum(target.data[1,THIS] >= d[,THIS], na.rm=TRUE) /
                   (nrow(d)-sum(is.na(d[,THIS]))), 1)
  note <- paste0(target.data[1,THIS], " (", BASE, " ZCTA quantile ",
          q.level, "%)")
  if (x < mean(par()$usr[1:2]))
  { # text on right of vertical line
    x <- x + 2*strwidth("X")
    text(x,y, note, cex=1, col="blue", adj=0.05, xpd=TRUE)
  } else {
    # text on left of vertical line
    x <- x - 2*strwidth("X")
    text(x,y, note, cex=1, col="blue", adj=0.95, xpd=TRUE)
  }

}

plot.TARGET.THIS <- function(TARGET, THIS, XLIM, show.footer=TRUE)
{
  target.data <- d.US[d.US$zcta5 == TARGET,]

  plot.comparison(TARGET, THIS, d.US, "US", target.data, XLIM)
  mtext(paste0("ZCTA ", TARGET, " (", target.data$ZIPName, "):  ", THIS),
        TOP<-3, adj=0.5, line=-0.5, col="blue", outer=TRUE)

  plot.comparison(TARGET, THIS, d.MO, "MO", target.data, XLIM)
  plot.comparison(TARGET, THIS, d.KS, "KS", target.data, XLIM)

  if (show.footer)
  {
    mtext("Sources: 2010 US Census; Missouri Census Data Center (2013)",
          BOTTOM<-1, adj=0.05, line=0, cex=0.75, col="blue", outer=TRUE)
    mtext(expression(italic("UMKC Center for Health Insights")),
          BOTTOM,    adj=0.95, line=0, cex=0.75, col="blue", outer=TRUE)
  }
}

plot.TARGET.THIS("64109", "pctpoor",         c( 0,100))
plot.TARGET.THIS("64112", "pctpoor",         c( 0,100))
plot.TARGET.THIS("64109", "pctHispanicPop",  c( 0,100))
plot.TARGET.THIS("64112", "pctHispanicPop",  c( 0,100))
plot.TARGET.THIS("64109", "MedianAge",       c(10, 80))
plot.TARGET.THIS("64112", "MedianAge",       c(10, 80))
plot.TARGET.THIS("64109", "pctUnder18",      c( 0,100))
plot.TARGET.THIS("64112", "pctOver65",       c( 0,100))

dev.off()

