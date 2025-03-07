make_yaml_header <- function(rmd_string){
  rmd_string <- paste0(rmd_string,"\n",
                       "---", "\n",
                       "title: 'template'", "\n",
                       "output: html_document", "\n",
                       "date: '2025-03-07'","\n",
                       "---", "\n","\n"
  )
  return(rmd_string)
}

make_string <- function(rmd_string, variable){
  rmd_string <- paste0(rmd_string,
                       paste0(
                         "\n",
                         "\n",
                         paste0("## `r variables[",which(variables==variable),"]`"),
                         "\n",
                         "\n",
                         "```{r, results='asis'}",
                         "\n",
                         paste0("plot_list[['",variable,"']]"),
                         "\n",
                         "```",
                         "\n"
                       )
  )
  rmd_string
}

add_model_output <- function(rmd_string){
  rmd_string <- paste0(rmd_string,
                       paste0(
                         "\n",
                         "# Model Output",
                         "\n",
                         "\n",
                         "```{r, results='asis'}",
                         "\n",
                         "stargazer::stargazer(m1,m2,m3,m4, type='html')",
                         "\n",
                         "```"
                       )
  )
  rmd_string
}

make_tabsets <- function(){
  rmd_string <- ""
  rmd_string <- make_yaml_header(rmd_string)
  rmd_string <- paste0(rmd_string, "# plots {.tabset} \n")
  
  for (variable in variables){
    rmd_string <- make_string(rmd_string, variable)
  }
  
  rmd_string <- add_model_output(rmd_string)
  return(rmd_string)
}
