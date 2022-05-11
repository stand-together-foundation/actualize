#' Declare a new environment
#' @description when package is loaded, create new environment to store needed variables
actualizeEnv <- new.env()

actualizeEnv$line_regex <- paste0(
  "^\\s*", # leading whitespace
  "(?<export>export\\s+)?", # export, if given
  "(?<key>[^=]+)", # variable name
  "=", # equals sign
  "(?<q>['\"]?)", # quote if present
  "(?<value>.*)", # value
  "\\g{q}", # the same quote again
  "\\s*", # trailing whitespace
  "$" # end of line
)

#' Ignore comments in an environment file
#' @description an internal function from the dotenv package
#' @param lines lines of textt read from a `.env` file
ignore_comments <- function(lines) {
  grep("^#", lines, invert = TRUE, value = TRUE)
}

#' Ignore empty lines in an environment file
#' @description an internal function from the dotenv package
#' @param lines lines of textt read from a `.env` file
ignore_empty_lines <- function (lines) {
  grep("^\\s*$", lines, invert = TRUE, value = TRUE)
}

#' Find variables from a `.env` file
#' @description an internal function from the dotenv package
#' @param line lines of textt read from a `.env` file
#' @param match An integer vector giving the position in table
#' of the first match if there is a match, otherwise nomatch.
extract_match <- function(line, match) {
  tmp <- mapply(
    attr(match, "capture.start"),
    attr(match, "capture.length"),
    FUN = function(start, length) {
      tmp <- substr(line, start, start + length - 1)
    }
  )
  names(tmp) <- attr(match, "capture.names")
  tmp
}

#' Find variables from a `.env` file
#' @description an internal function from the dotenv package
#' @param line lines of textt read from a `.env` file
#' @param envir where to look for the object
parse_dot_line <- function(line, envir) {
  line_regex <- get(line_regex, envir = envir)
  match <- regexpr(line_regex, line, perl = TRUE)
  if (match == -1)
    stop("Cannot parse dot-env line: ", substr(line, 1, 40),
         call. = FALSE)
  as.list(extract_match(line, match)[c("key", "value")])
}


#' Set token cache from googledrive::drive_auth()
#'
#' @param secret_cache The file path for a secrets file stored to disk.
#' @export
set_project_user_token <- function(secret_cache = '.secrets') {
  # designate project-specific cache
  options(gargle_oauth_cache = secret_cache)

  # trigger auth on purpose --> store a token in the specified cache
  googledrive::drive_auth()

}


#' Load environment variables from the specified file
#'
#' @description
#'
#' Load variables defined in the given file, as environment variables, and
#' also return a vector of variable names or keys.
#'
#' @param file The location of your data science environment variable file.
#' @export
load_env_vars <- function(file = '.env-data-science') {
  dotenv::load_dot_env(file = file)

  tmp <- readLines(file)
  tmp <- ignore_comments(tmp)
  tmp <- ignore_empty_lines(tmp)
  tmp <- lapply(tmp, actualizeEnv$parse_dot_line)
  tmp <- unlist(tmp)[names(unlist(tmp)) == "key"]
  keys_index <- stringr::str_starts(tmp, "I360", negate = TRUE)
  actualizeEnv$keys <- unlist(tmp)[keys_index]

}

#' Assign Google Drive metadata to a Named value
#'
#' @param key Something that identifies the file of interest on your
#' Google Drive. Can be a name or path, a file id or URL marked with
#' as_id(), or a dribble
#' @param envir where to look for the object
#' @export
assign_google_meta <- function(key, envir = actualizeEnv) {
  assign(
    x = tolower(key),
    value = googledrive::drive_get(id = Sys.getenv(key)),
    envir = envir
  )
}

#' Download file metadata from Google Drive
#'
#' @param keys a vector of something that identifies the file of
#' interest on your Google Drive. Can be a name or path, a file
#' id or URL marked with as_id(), or a dribble.
#' @param ... other arguments to be passed on to `googledrive::drive_auth()`.
#' In particular, the token parameter for that function should be passed to any
#' use of this function in production since the function is currently designed
#' to equire "out of band" authentication.
#' @export
load_metadata <- function(keys, ...) {
  googledrive::drive_auth(cache = ".secrets", ...)

  lapply(X = keys,
         FUN = assign_google_meta)

}

#' Download a named object from Google Drive
#'
#' @param obj An object name (given as a character string).
#' @param envir where to look for the object
#' @export
load_google_drive_file <- function(obj = "dbg1", envir = actualizeEnv) {
  googledrive::drive_download(
    get(x = obj, envir = envir),
    path = glue::glue("~/actualize/data-raw/",
                      tolower(obj)),
    overwrite = TRUE
  )
}

#' Download several named objects from Google Drive
#'
#' @param keys a vector of object names (given as a character string).
#' @param ... other arguments to be passed on to `googledrive::drive_auth()`.
#' In particular, the token parameter for that function should be passed to any
#' use of this function in production since the function is currently designed
#' to equire "out of band" authentication.
#' @export
load_data <- function(keys, ...) {
  googledrive::drive_auth(cache = ".secrets", ...)

  lapply(X = tolower(keys),
         FUN = load_google_drive_file)

}


#' Parse PAWS bucket contents for most recent file
#'
#' @param bucket_contents a list returned from the `list_buckets` method in
#' PAWS S3 tool.
#' @export
get_max_file_location <- function(bucket_contents) {
  rowname = NULL
  value = NULL
  . = NULL

  bucket_contents_names <- paste0('X', 1:length(bucket_contents))

  names(bucket_contents) <- bucket_contents_names

  max_file_location <-
    as.data.frame(lapply(bucket_contents, unlist)) %>%
    tibble::rownames_to_column(.) %>%
    dplyr::bind_rows(.) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(bucket_contents_names)) %>%
    dplyr::filter(rowname == "LastModified") %>%
    dplyr::mutate(value = as.integer(value)) %>%
    dplyr::filter(value == max(value)) %>%
    .$name %>%
    readr::parse_number(.)

  return(max_file_location)
}

#' Parse PAWS bucket contents for most recent file
#'
#' @param local_dest_path The place where the file should be downloaded.
#' @param file_name The name of the flat you are looking for in the S3 bucket.
#' @param env_file The location of your data science environment variable file.
#'
#' @description
#'
#' This function grabs the most recent file in the S3 bucket named in the
#' `I360_S3_BUCKET` environment variable. It stores the file in the team's
#' Google Drive and also keeps a local copy for immediate analysis.
#' @export
download_most_recent_bucket_file <- function(local_dest_path =  '~/actualize/data-raw/',
                                             file_name = 'local1',
                                             env_file = '.env-data-science') {
  . = NULL

  local_data <- googledrive::drive_mkdir(name = "Local Data",
                                         path = "~/actualize-data/",
                                         overwrite = TRUE)

  s3_i360 <- paws::s3(config = list(
    credentials = list(
      creds = list(
        access_key_id = Sys.getenv("I360_ACCESS_KEY_ID"),
        secret_access_key = Sys.getenv("I360_SECRET_ACCESS_KEY")
      )
    ),
    region = Sys.getenv("I360_REGION")
  ))

  bucket_contents <- s3_i360$list_objects(Bucket = Sys.getenv("I360_S3_BUCKET")) %>%
    .$Contents

  bucket_contents %>%
    .[[get_max_file_location(bucket_contents)]] %>%
    .$Key %>%
    s3_i360$get_object(Bucket = Sys.getenv("I360_S3_BUCKET"),
                       Key = .) %>%
    .$Body %>%
    readr::write_file(x = .,
                      file = glue::glue(local_dest_path, file_name),)

  new_dribble <- googledrive::drive_upload(
    media = glue::glue(local_dest_path, file_name),
    path = local_data,
    name = file_name,
    overwrite = TRUE,
    verbose = TRUE
  )

  oldenv <- utils::read.table(env_file, stringsAsFactors = FALSE)
  newenv <-
    oldenv[stringr::str_starts(oldenv$V1, toupper(file_name), negate = TRUE), ]
  utils::write.table(
    newenv,
    env_file,
    quote = FALSE,
    sep = "\n",
    col.names = FALSE,
    row.names = FALSE
  )
  new_key <-
    paste0(toupper(file_name),
           '=',
           new_dribble$drive_resource[[1]]$webViewLink)
  write(new_key, env_file, sep = "\n", append = TRUE)


}

#' Read an Excel file and assign to environment,
#' @param file_name the location of data in your local project
#' @param envir where to look for the object
assign_excel <- function(file_name, envir = actualizeEnv) {
  parsed_file_name <- sub(".*/", "", file_name)

  assign(
    x = parsed_file_name,
    value = readxl::read_xlsx(file_name),
    envir = envir
  )

  if (names(get(parsed_file_name, envir = envir)) == 'B') {
    assign(
      x = sub(".*/", "", file_name),
      value = readxl::read_xlsx(file_name,
                                skip = 1),
      envir = envir
    )
  }

}

#' Read an CSV file and assign to environment,
#' @param file_name the location of data in your local project
#' @param envir where to look for the object
assign_csv <- function(file_name, envir = actualizeEnv) {
  assign(
    x = sub(".*/", "", file_name),
    value =  readr::read_csv(file_name),
    envir = envir
  )
}

#' Read an SPSS file and assign to environment,
#' @param file_name the location of data in your local project
#' @param envir where to look for the object
assign_sav <- function(file_name, envir = actualizeEnv) {
  assign(
    x = tolower(file_name),
    value =  haven::read_sav(file_name),
    envir = envir
  )
}

#' Create tibbles of all files in data-raw
#'
#' @param file_dir The place where downloaded files are located.
assign_tibbles <- function(file_dir = '~/actualize/data-raw') {
  . = NULL

  file_names <- list.files(
    '~/actualize/data-raw',
    full.names = TRUE
  )

  file_types <- dqmagic::file_type(file_names)

  tibble::tibble(file_names,
                 file_types) %>%
    dplyr::filter(stringr::str_detect(file_types, "Excel")) %>%
    .$file_names %>%
    purrr::map(.x = .,
               .f = assign_excel)

  tibble::tibble(file_names,
                 file_types) %>%
    dplyr::filter(stringr::str_detect(file_types, "SPSS")) %>%
    .$file_names %>%
    purrr::map(.x = .,
               .f = assign_sav)

  tibble::tibble(file_names,
                 file_types) %>%
    dplyr::filter(stringr::str_detect(file_types, "CSV")) %>%
    .$file_names %>%
    purrr::map(.x = .,
               .f = assign_csv)


}



#' Import all actualize data from various sources
#'
#' @param env_file The location of your data science environment variable file.
#' @param file_dir the designated location of local actualize data. In the production
#' repository, this is the `data-raw/` directory. This directory should be added to
#' your .gitignore file as well.
#' @export
import_actualize_data <- function(env_file = '.env-data-science',
                                  file_dir = '~/actualize/data-raw') {
  message("load environment variables...")
  actualizeEnv$keys <- load_env_vars(file = env_file)
  message("DONE")
  message("downloading most recent data from S3...")
  download_most_recent_bucket_file(
    env_file = env_file,
    local_dest_path = file_dir,
    file_name = 'local1'
  )
  message("DONE")
  message("assigning Google metadata to Global environment...")
  load_metadata(keys = actualizeEnv$keys)
  message("DONE")
  message("downloading data from Google Drive to disk...")
  load_data(keys = actualizeEnv$keys)
  message("DONE")
  message("loading data from disk to Global environment...")
  assign_tibbles(file_dir = file_dir)
  message("DONE")

}
