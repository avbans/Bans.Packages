#THIS SCRIPT ASSISTS WITH COMPILING R SCRIPTS DIRECTLY AS A MARKDOWN
#VARIABLES ENTERED NEED TO BE ENTERED AS QUOTES (ex:input_name = "myscript.r")
#INPUT_NAME NEEDS TO BE ".R" AND OUTPUT FILE NEEDS TO BE ".HTML/PDF/DOC/MISC."

Imports:
  here,
  markdown

report_compiler<-function(input_location,input_name,output_location,output_name){
  project_directory = here()
  file_input <-paste(project_dir,input_location,input_name, sep = "/")
  file_output = paste(project_dir,output_location, output_name, sep = "/")

  rmarkdown::render(input = file_input,
                    output_file = file_output)
}
