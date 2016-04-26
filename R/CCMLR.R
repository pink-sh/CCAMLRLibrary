plotQuantitiesInTonnes <- function(species=c(), start=1946, end=2016, file="qryTable02.csv", chart="Bar") {
  library(rCharts)
  library(dplyr)
  library(jsonlite)
  myData <- read.csv(file)
  if (length(species > 0)) {
    reduced <- filter(myData, SpeciesCode %in% species)
  } else {
    reduced <- myData
    species <- unique(reduced$SpeciesCode)
  }
  OUT <- data.frame("SeasonYear"=(start:end))
  for (sp in species) {
    aggr0 <- filter(reduced, SpeciesCode == sp)
    aggr01 <- aggregate(aggr0$CatchWeight.t., by=list(SeasonYear=aggr0$SeasonYear, ScientificName=aggr0$ScientificName), FUN=sum)
    aggr02 <- transform(aggr01, SeasonYear = as.character(SeasonYear), CatchWeight.t. = as.numeric(x))
    aggr03 <- filter(aggr02, SeasonYear >= start, SeasonYear <= end)
    ifelse(lengths(aggr03, use.names = TRUE) == 0,next,1)
    aggr03$x <- NULL


    vector <- c()
    i = 1;
    scientificName <- ""
    apply(OUT, 1, function(row1) {
      yrOut = row1['SeasonYear']
      value <- 0
      apply(aggr03, 1, function(row2) {
        yrIn = row2['SeasonYear']

        scientificName <<- row2['ScientificName']
        if (!is.na(yrIn)) {
          if (yrOut == yrIn) {
            value <<- row2['CatchWeight.t.']
          }
        }
      })
      vector <<- c(vector, value)
      i <<- i + 1
    })
    OUT[[scientificName]] <- vector
  }
  OUT <- transform(OUT, SeasonYear = as.character(SeasonYear))
  m1 <- mPlot(x = "SeasonYear", y = names(OUT), type = chart, data = OUT, stacked = "TRUE")
  m1$set(hoverCallback = "#! function(index, options, content) {
         var row = options.data[index];
         var tuples = [];
         var YEAR = '';
         for (var key in row) {
         	if (key != 'SeasonYear') {
         		tuples.push([key, parseInt(row[key])]);
         	} else {
         		year = row[key]
         	}
         }
         tuples.sort(function(a, b) {return b[1] - a[1]});
		     var ret = [];
		     ret.push('<div style=\"color: red;\">' + year + '</div>');
		     for (var i = 0; i < tuples.length; i++) {
		 	      if (i > 10) break;
		 	      ret.push(tuples[i][0].replace('.', ' ') + ':' + tuples[i][1]);
		     }
		     return ret.join('<br />'); } !#")
  m1$save('output.html', standalone = TRUE)
  return (toJSON(OUT))
}

plotQuantitiesInTonnesByCountry <- function(start=1946, end=2016, file="qryTable03_1.csv", chart="Bar") {
  library(rCharts)
  library(dplyr)
  library(jsonlite)
  myData <- read.csv(file)
  aggr0 <- aggregate(myData$CatchWeight.t., by=list(SeasonYear=myData$SeasonYear, Country=myData$Country), FUN=sum)
  aggr01 <- transform(aggr0, SeasonYear = as.character(SeasonYear), CatchWeight.t. = as.numeric(x))
  aggr01$x <- NULL
  aggr02 <- filter(aggr01, SeasonYear >= start, SeasonYear <= end)
  aggr03 <- aggregate(aggr02$CatchWeight.t., by=list(Country=aggr02$Country), FUN=sum)
  aggr04 <- transform(aggr03, CatchWeight.t. = as.numeric(x))
  aggr04$x <- NULL
  m1 <- mPlot(x = "Country", y = "CatchWeight.t.", type = chart, data = aggr04, stacked = "TRUE", xLabelAngle = 85)
  m1$save('output.html', standalone = TRUE)
  return (toJSON(aggr04))
}

plotCatchEfforts <- function(start=1946, end=2016, file="QueryEffortsCatch.csv", chart="Bar") {
  library(rCharts)
  library(dplyr)
  library(jsonlite)
  myData <- read.csv(file)
  aggr0 <- filter(myData, SeasonYear >= start, SeasonYear <= end)
  aggr01 <- transform(aggr0, mean = CatchWeight/FishingHours)
  m1 <- mPlot(x = "SeasonYear", y = "mean", type = chart, data = aggr01, stacked = "TRUE", xLabelAngle = 85)
  m1$save('output.html', standalone = TRUE)
  return (toJSON(aggaggr01r04))
}

getSpecies <- function(file="qryTable02.csv") {
  library(jsonlite)
  library(plyr)
  df <- read.csv(file)
  plyed <- ddply(df, c("ScientificName","SpeciesCode"), head, 1)
  ret <- data.frame("NAME" = plyed$ScientificName, "ALPHA" = plyed$SpeciesCode)
  ret <- ret[!apply(ret, 1, function(x) any(x=="")),]
  return (toJSON(ret))
}
