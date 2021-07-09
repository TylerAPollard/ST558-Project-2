weekday <- c(1,2,3,4,5,6,0)
output_file <- paste0("Weekday", weekday, "Analysis.md")
params = lapply(weekday, 
                FUN = function(x){
                  list(weekday = x)
                })
reports <- tibble(output_file, params)

apply(reports, MARGIN = 1,
      FUN = function(x){
        rmarkdown::render(input = "Project2_Tyler_Lucy.Rmd", output_file = x[[1]], params = x[[2]])
      })
