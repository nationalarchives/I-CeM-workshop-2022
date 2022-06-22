getExtension <- function(file){ 
    # Thanks to https://stackoverflow.com/questions/7779037/extract-file-extension-from-file-path
    ex <- strsplit(basename(file), split="\\.")[[1]]
    return(ex[-1])
} 

as_numeric_def <- function(x) {
    x_num <- as.numeric(x)
    ifelse(is.na(x_num), -1, x_num)
}

fix_header <- function(source_file, output_file) {

    if (getExtension(source_file) == 'zip') {
    SOURCE_ZIP_FILE = source_file
    SOURCE_DATA_FILE = unzip(SOURCE_ZIP_FILE, list=TRUE)
    SOURCE_DATA_FILE = SOURCE_DATA_FILE[1,]$Name
    unzip(SOURCE_ZIP_FILE, SOURCE_DATA_FILE)
    } else {
        SOURCE_DATA_FILE = source_file
    }

    fr <- file(SOURCE_DATA_FILE,open="rt") #open file connection to read
    fw <- file(output_file, open="wt") #open file connection to write 
    header_row <- readLines(fr, n=1) #read in header
    second_row <- readLines(fr, n=1) # read the next row
    second_row
    header_row <- unlist(strsplit(header_row, "|", fixed=TRUE))
    header_length <- length(header_row)
    second_length <- length(unlist(strsplit(second_row, "|", fixed=TRUE)))
    if (header_length != second_length) {
        header_row <- append(header_row, 'BLANK_1', after = 84)
        header_row <- append(header_row, 'BLANK_2', after = 85)
        header_row <- paste0(header_row, collapse = '|') #modify header    
    }
    writeLines(header_row,con=fw) #write header to file
    writeLines(second_row,con=fw) #write header to file
    while(length(body <- readLines(fr,n=10000)) > 0) {
    writeLines(body,fw) #pass rest of file in chunks of 10000
    }

    close(fr);close(fw) #close connections
}


load_census <- function(census_file, census_variables, column_code) {
    source_data <- read_delim(census_file,delim = '|', col_names = TRUE, show_col_types = FALSE)
    nrow(source_data)

    
    column_labels <- census_variables %>%
    select("VariableName", "Label","Coded","AnonymisedVersion") %>%
    mutate(Coded = (Coded == "x"), AnonymisedVersion = (AnonymisedVersion == "x"))

    columns_by_census <- variables %>%
    select("VariableName", EW51:SCOT01) %>%
    pivot_longer(EW51:SCOT01, names_to = "census_name", values_to = "has_variable") %>%
    mutate(has_variable = (has_variable == "x"))

    selected_columns <- columns_by_census %>%
    filter(census_name == 'EW51' & has_variable) %>%
    left_join(column_labels, by = 'VariableName') %>%
    filter(AnonymisedVersion) %>%
    select("VariableName", "Label")

    code_columns <- columns_by_census %>%
    filter(census_name == 'EW51' & has_variable) %>%
    filter(!(VariableName %in% c("H_Sex", "Sex", "Parish"))) %>%
    left_join(column_labels, by = 'VariableName') %>%
    filter(AnonymisedVersion & Coded) %>%
    select("VariableName")
    
    not_numeric <- c("Cage", "Std_Par", "Cnti", "Alt_Cnti", "Ctry", "Alt_Ctry")
    
    numeric_columns <- union(as.vector(str_trim(code_columns$VariableName)), c("ParType", "H_Occ"))
    print(numeric_columns)
    
    numeric_columns <- setdiff(numeric_columns, not_numeric)

    census_data <- source_data %>% select(all_of(str_trim(selected_columns$VariableName))) %>%
        mutate_at(vars(all_of(numeric_columns)), as_numeric_def)

    source_data <- NULL
    return(census_data)
}


