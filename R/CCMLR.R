vector.is.empty <- function(x) return(length(x) ==0 )

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
  namesOut <- c()
  for (name in names(OUT)) {
    if (name != 'SeasonYear') {
      OUT[, name] = as.double(OUT[, name])
      namesOut <- c(namesOut, name)
    }
  }
  m1 <- mPlot(x = "SeasonYear", y = namesOut, type = chart, data = OUT, stacked = "TRUE")
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
  aggr01 <- transform(aggr0, Mean = CatchWeight/FishingHours)
  m1 <- mPlot(x = "SeasonYear", y = "Mean", type = chart, data = aggr01, stacked = "TRUE", xLabelAngle = 85)
  m1$save('output.html', standalone = TRUE)
  json <- toJSON(aggr01, na = "null", pretty = FALSE)
  return (json)
}

getSpecies <- function(file="qryTable02.csv") {
  library(jsonlite)
  library(plyr)
  df <- read.csv(file)
  plyed <- ddply(df, c("ScientificName","SpeciesCode"), head, 1)
  #ret <- data.frame("NAME" = plyed$ScientificName, "ALPHA" = plyed$SpeciesCode)
  ret <- data.frame("id" = plyed$SpeciesCode, "name" = plyed$ScientificName)
  ret <- ret[!apply(ret, 1, function(x) any(x=="")),]
  return (toJSON(ret))
}

getUnique <- function(file="qryTable12.csv", colVal = "", colKey = "") {
  library(jsonlite)
  library(plyr)
  df <- read.csv(file)
  plyed <- ddply(df, c(colVal,colKey), head, 1)
  #ret <- data.frame("VALUE" = plyed[colVal], "KEY" = plyed[colKey])
  ret <- data.frame("id" = plyed[colKey], "name" = plyed[colVal])
  ret <- ret[!apply(ret, 1, function(x) any(x=="")),]
  ret[with(ret, order(ret[,1])), ]
  return (toJSON(ret))
}

catchByFishingMethods <- function(start=1946, end=2016, species=c(), gear=c(), asd=c(), file="qryTable12.csv", chart="Bar", removeZero=FALSE) {
  library(rCharts)
  library(dplyr)
  library(jsonlite)
  myData <- read.csv(file)
  aggr0 <- filter(myData, SeasonYear >= start, SeasonYear <= end)
  if (!vector.is.empty(species)) {
    aggr0 <- filter(aggr0, SpeciesCode %in% species)
  }
  if (!vector.is.empty(gear)) {
    aggr0 <- filter(aggr0, GearCode %in% gear)
  }
  if (!vector.is.empty(asd)) {
    aggr0 <- filter(aggr0, ASD %in% asd)
  }
  aggr01 <- transform(aggr0, tonnes = as.numeric(CatchWeight.t.))
  aggr01[,10] <- as.numeric(as.character( aggr01[,10] ))
  aggr01 <- aggregate(aggr01$CatchWeight.t., by=list(aggr01$SpeciesCode), FUN=sum, na.rm=TRUE)
  aggr01 <- transform(aggr01, Tonnes = as.numeric(x))
  aggr01 <- transform(aggr01, SpeciesCode = as.character(Group.1))
  aggr01$Group.1 = NULL
  aggr01$x = NULL
  if (removeZero) {
    aggr01 <- aggr01[apply(aggr01["Tonnes"],1,function(z) !any(z==0)),]
  }
  json <- toJSON(aggr01)
  m1 <- mPlot(x = "SpeciesCode", y = "Tonnes", type = chart, data = aggr01, stacked = "TRUE", xLabelAngle = 65)
  m1$save('output.html', standalone = TRUE)
  return (json)
}

FishingDays <- function(start=1946, end=2016, species=c(), gear=c(), asd=c(), area=c(), months=c(), file="qryTable11_5.csv", chart="Bar", removeZero=FALSE) {
  library(rCharts)
  library(dplyr)
  library(jsonlite)

  myData <- read.csv(file)
  aggr0 <- filter(myData, SeasonYear >= start, SeasonYear <= end)
  if (!vector.is.empty(species)) {
    aggr0 <- filter(aggr0, TargetSpeciesCode %in% species)
  }
  if (!vector.is.empty(months)) {
    aggr0 <- filter(aggr0, MonthNm %in% months)
  }
  if (!vector.is.empty(gear)) {
    aggr0 <- filter(aggr0, GearCode %in% gear)
  }
  if (!vector.is.empty(asd)) {
    aggr0 <- filter(aggr0, ASD %in% asd)
  }
  if (!vector.is.empty(area)) {
    aggr0 <- filter(aggr0, Area %in% area)
  }
  
  aggr01 <- aggregate(aggr0$FishingDays, by=list(aggr0$SeasonYear, aggr0$MonthNm, aggr0$ScientificName, aggr0$TargetSpeciesCode, aggr0$Area), FUN=sum, na.rm=TRUE)
  aggr01 <- transform(aggr01, Year = as.character(Group.1))
  aggr01 <- transform(aggr01, Month = as.character(Group.2))
  aggr01 <- transform(aggr01, ScientificName = as.character(Group.3))
  aggr01 <- transform(aggr01, SpeciesCode = as.character(Group.4))
  aggr01 <- transform(aggr01, Area = as.character(Group.5))
  aggr01 <- transform(aggr01, FishingDays = as.numeric(x))
  aggr01$Group.1 = NULL
  aggr01$Group.2 = NULL
  aggr01$Group.3 = NULL
  aggr01$Group.4 = NULL
  aggr01$Group.5 = NULL
  aggr01$x = NULL
  aggr01$monthNum <- match(aggr01$Month, month.name)
  aggr01 <- aggr01[apply(aggr01["monthNum"],1,function(z) !any(is.na(z))),]
  aggr01 <- aggr01[ order(aggr01$Year, aggr01$monthNum), ]
  tspecies <- unique(aggr01$SpeciesCode)
  cspecies <- c()
  for (sp in tspecies) {
    print(sp)
    if (!is.null(sp) && sp != "NULL") {
      cspecies <- c(cspecies, sp)
    }
  }
  aggr02 <- aggr01
  apply(aggr02, 1, function(row1) {
    ScientificName <- row1['ScientificName'];
    SpeciesCode <- row1['SpeciesCode'];
    lista <- c()
    apply(aggr02, 1, function(row2) {
      if (SpeciesCode == row2['SpeciesCode']) {
        lista <<- c(lista, row2['FishingDays'])
      } else {
        lista <<- c(lista, 0)
      }
    })
    aggr02[[SpeciesCode]] <<- as.numeric(unlist(lista));
  })
  json <- toJSON(aggr02)
  aggr03 <- setDT(aggr02)[, lapply(.SD, sum), by=.(Year, Month, monthNum), .SDcols=cspecies]
  aggr03$out <- paste(aggr03$Year, aggr03$monthNum, sep="/")

  if (chart == "Stacked") {
    m1 <- mPlot(x = c("out"), y = species, type = "Bar", data = aggr03, stacked = "true", xLabelAngle = 65, pointSize='0')
  } else {
    m1 <- mPlot(x = c("out"), y = species, type = chart, data = aggr03, xLabelAngle = 65, pointSize='0')
  }
  m1$save('output.html', standalone = TRUE)
  
  return (json)
}
