#
# Auxiliary functions
#
# Package: mtrank
# Authors: Guido Schwarzer <guido.schwarzer@uniklinik-freiburg.de>,
#          Theodoros.Evrenoglou <theodoros.evrenoglou@uniklinik-freiburg.de>
# License: GPL (>= 2)
#


# A functions specifying the opposite of %in%
'%!in%' <- function(x,y)!('%in%'(x,y))

# A function that "cleans" long arm level data from:
# for binary data: (i) all-zero event studies and (ii) studies with missing number of event/sample sizes
# for continuous data: studies with missing arm level means, sample size and standard deviation

clean <- function(data, type) {
  # Get rid of warning 'no visible binding for global variable'
  studlab <- NULL
  #
  if (type == "binary") {  
    # Remove NAs    
    data <- data[complete.cases(data$event), , drop = FALSE]
    data <- data[complete.cases(data$n), , drop = FALSE]
    # Remove 0-0 studies  
    events.study <- tapply(data$event, data$studlab, sum)
    #
    if (any(events.study == 0))
      data <- data[data$studlab %in%
                     names(events.study)[events.study != 0], , drop = FALSE]
  }
  else if (type == "continuous") {
    # Remove NAs    
    data <- data[complete.cases(data$mean), , drop = FALSE]
    data <- data[complete.cases(data$sd), , drop = FALSE]
    data <- data[complete.cases(data$n), , drop = FALSE]
  }
  # Remove single arm studies
  count <- as.data.frame(table(data$studlab))
  #
  data %>% filter(studlab %!in% count$Var1[count$Freq == 1])
}


# A function transforming wide to long format data

go_long <- function(treat, event, mean, sd, n, studlab, type) {
  studyid <- unique(studlab)
  studyid_long <- rep(studyid, length(treat))
  #
  id <- seq_len(length(studyid))
  id_long <- rep(id, length(treat))
  #
  treat <- unlist(treat)
  
  if (type == "binary") {
    event <- unlist(event)
    n <- unlist(n)
    #
    dat <- data.frame(studlab = studyid_long, id = id_long, treat, event, n)
  }
  else if (type == "continuous") {
    mean <- unlist(mean)  
    sd <- unlist(sd)
    n <- unlist(n)
    #
    dat <- data.frame("studlab" = studyid_long, "id" = id_long,
                      treat, mean, sd, n)
  }
  #
  dat <- dat %>% arrange(id) %>% select(-id)
  #
  dat
}


allNA <- function(x)
  all(is.na(x))


catch <- function(argname, matchcall, data, encl)
  eval(matchcall[[match(argname, names(matchcall))]], data, enclos = encl)


int2num <- function(x) {
  #
  # Convert integer to numeric
  #
  if (is.integer(x))
    res <- as.numeric(x)
  else
    res <- x
  #
  res
}


npn <- function(x) {
  #
  # Check for non-positive values in vector
  #
  selNA <- is.na(x)
  res <- selNA
  if (sum(!selNA) > 0)
    x[!selNA] <- x[!selNA] <= 0
  #
  res
}


replaceNULL <- function(x, replace = NA) {
  if (is.null(x))
    return(replace)
  x
}


replaceNA <- function(x, replace = NA) {
  if (is.null(x))
    return(x)
  else
    x[is.na(x)] <- replace
  x
}


replaceVal <- function(x, old, new) {
  if (is.null(x))
    return(x)
  else
    x[x == old] <- new
  x
}


extrVar <- function(x, name)
  x[[name]]


calcPercent <- function(x)
  100 * x / sum(x, na.rm = TRUE)


list2vec <- function(x) {
  if (is.list(x))
    return(do.call("c", x))
  else
    return(x)
}


setsv <- function(x) {
  if (is.null(x))
    res <- "desirable"
  else {
    res <- setchar(x, c("good", "bad"), stop.at.error = FALSE)
    #
    if (!is.null(res))
      res <- switch(res, good = "desirable", bad = "undesirable")
    else
      res <- x
  }
  #
  setchar(res, c("desirable", "undesirable"))
}
