#Windgeschwindigkeit	Anstieg des Wasserspiegels
#                  Knoten	mph	km/h	m

#Tropisches Tief < 34	< 39	< 63	≈ 0
#Tropischer Sturm	34 bis 64	39 bis 73	63 bis 118	0,1 bis 1,1
#Hurrikan Kategorie 1	64 bis 82	74 bis 95	119 bis 153	1,2 bis 1,6
#Hurrikan Kategorie 2	83 bis 95	96 bis 110	154 bis 177	1,7 bis 2,5
#Hurrikan Kategorie 3	96 bis 113	111 bis 130	178 bis 209	2,6 bis 3,8
#Hurrikan Kategorie 4	114 bis 135	131 bis 155	210 bis 249	3,9 bis 5,5
#Hurrikan Kategorie 5	> 135	> 155	> 249	über 5,5


classifyHurricane <- function(knots) {
  if(is.na(knots)) {
    return("NA")
  }
  if(knots >= 135 ) {
    return("H5")
  } else if (knots >= 114 & knots < 135) {
    return("H4")
  } else if (knots >= 96 & knots < 113) {
    return("H3")
  }else if (knots >= 83 & knots < 96) {
    return("H2")
  }else if (knots >= 64 & knots < 82) {
    return("H1")
  }else if (knots >= 34 & knots < 64) {
    return("TS")
  } else {
    return("TT")
  }
}


###
highestClassification4Storm <- function(x) {
  resultDatFram <- data.frame("Key"= character(),
                   "YEAR" = character(),
                   "CATEGORY" = character(),
                   "Wind" = integer(),
                   stringsAsFactors=FALSE)
  result <- ""
  if(is.tibble(x)) {
    result <- paste(result, "istibble", sep = "")
    if("Key" %in% colnames(x)) {
      result <- paste(result, "has Key", sep = ":")
    }
    if("CATEGORY" %in% colnames(x)) {
      result <- paste(result, "has Category", sep = ":")
    }
  }
  print(result)
  for( i in 1:nrow(x) ) {
    if(x$Key[i] %in% resultDatFram$Key ){
      #print(paste("found key", x$Key[i], sep = ":"))
      tmpindex = which(resultDatFram$Key==x$Key[i])
      #print(paste(i, tmpindex, sep = ":"))
      tmp2 <- compareCategory(x[i,], resultDatFram[tmpindex,])
      resultDatFram$CATEGORY[tmpindex] <- tmp2$CATEGORY
      resultDatFram$Wind[tmpindex] <- tmp2$Wind
    } else {
      #add row
      resultDatFram <- addRow2Df(resultDatFram, x[i,])
    }
  }
  return(resultDatFram)
}

###################
addRow2Df <- function(datfram, newrow) {
  tmpR11 <- data.frame(newrow$Key[1], newrow$YEAR[1], newrow$CATEGORY[1], newrow$Wind[1], stringsAsFactors=FALSE)
  names(tmpR11) <- c("Key", "YEAR","CATEGORY", "Wind" ) 
  resultfram <- rbind(datfram, tmpR11)
  return(resultfram)
}

#####################
compareCategory <- function(inputfram, resultfram) {
  if(is.na(inputfram$Wind) ){
    inputfram$Wind <- 0
  }
  if(is.na(resultfram$Wind)) {
    resultfram$Wind <- 0
  }
  if(inputfram$Wind > resultfram$Wind) {
    return(inputfram)
  } else {
    return(resultfram)
  }
}

#for(i in 1:nrow(df20)) {print(df20$Key[i])}
#df20$CATEGORY[df20$Key=="AL021851"]
#"AL021851" %in% df20$Key
#return index 
#which(df20$Key=="AL021851")
#length(which(aaa$CATEGORY=="H5"))
