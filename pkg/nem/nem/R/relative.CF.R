relative.CF <- function(CF, random.CF, type = "mean"){
  return((mean(random.CF) - CF)/(mean(random.CF)))
}
