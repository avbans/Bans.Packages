#' Render markdown report
#'
#'This function renders and saves a markdown directly from a R script
#' to any desired location. `
#' @param input_location Location of R script, in quotes.
#' @param input_name Script name , in quotes.
#' @param output_location Location where markdown will be saved, in quotes.
#' @param output_name File name, in quotes
#'
#' @return Saved markdown file.
#'
#' @examples report_compiler(input_location = "scripts",input_namne = "mymarkdownreport.R",output_location = "output",output_name = "Report.html")
#' @export
report_compiler<-function(input_location,input_name,output_location,output_name){
  project_directory = here()
  file_input <-paste(project_directory,input_location,input_name, sep = "/")
  file_output = paste(project_directory,output_location, output_name, sep = "/")

  rmarkdown::render(input = file_input,
                    output_file = file_output)
}
