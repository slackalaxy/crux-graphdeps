# Which CRUX release to read. Each release is a branch on git.crux.nu.
branch <- "3.8"

# Download one collection (core, opt, ...) as a single archive from
# git.crux.nu, unpack it, and read the "# Depends on:" line of every Pkgfile.
read_collection <- function(coll, branch) {
  url <- paste0("https://git.crux.nu/ports/", coll, "/archive/", branch, ".tar.gz")
  
  # tempfile() makes a throw-away file/folder name; R cleans these up itself.
  tarfile <- tempfile(fileext = ".tar.gz")
  download.file(url, tarfile, quiet = TRUE)
  folder <- tempfile()
  untar(tarfile, exdir = folder)
  
  # Find every Pkgfile inside the unpacked archive.
  pkgfiles <- list.files(folder, pattern = "^Pkgfile$", recursive = TRUE, full.names = TRUE)
  
  result <- data.frame()
  for (f in pkgfiles) {
    # The port name is the name of the folder the Pkgfile sits in.
    port <- basename(dirname(f))
    
    lines <- readLines(f, warn = FALSE)
    dep_line <- grep("^# Depends on:", lines, value = TRUE)
    
    url_line <- grep("^# URL:", lines, value = TRUE)
    desc_line <- grep("^# Description:", lines, value = TRUE)
    
    homepage <- trimws(sub("^# URL:", "", url_line[1]))
    desc <- trimws(sub("^# Description:", "", desc_line[1]))
    
    deps <- character(0)
    if (length(dep_line) > 0) {
      deps <- sub("^# Depends on:", "", dep_line[1])
      deps <- gsub(",", " ", deps)
      deps <- strsplit(trimws(deps), "[[:space:]]+")[[1]]
      deps <- deps[deps != ""]
    }
    
    # No dependencies: one row with an empty dependency, like ",port,coll".
    if (length(deps) == 0) {
      deps <- ""
    }
    
    result <- rbind(result, data.frame(dependency = deps, port = port, collection = coll, url = homepage, description = desc))
  }
  return(result)
}

# Read collections and stack them into one table.
fetch_all <- function() {
  all_df <- data.frame()
  for (coll in c("core", "opt", "xorg", "contrib", "compat-32")) {
    all_df <- rbind(all_df, read_collection(coll, branch))
  }
  return(all_df)
}

# Save collections to disk
collections <- fetch_all()
write.csv(collections, file = "all.csv", row.names = FALSE)

# Save update timestamp to disk
write.table(format(Sys.time(), tz = "UTC", usetz = TRUE), file = "timeUTC.txt",
            quote = FALSE, row.names = FALSE, col.names = FALSE)

### POSIT conenction to Gitea tos not work. ####################################

# Local copy of the data, and how old it may be before we download again.
# While testing, the app restarts often; this way it only contacts
# git.crux.nu when the saved copy is older than max_age_hours.
# cache <- "all.csv"
# max_age_hours <- 6

# Is there a saved copy, and is it recent enough?
# file.mtime() is when the file was last written.
# fresh <- file.exists(cache) &&
#   difftime(Sys.time(), file.mtime(cache), units = "hours") < max_age_hours
# 
# if (fresh) {
#   # Recent copy: use it, no download.
#   collections <- read.csv(cache, stringsAsFactors = FALSE)
# } else {
#   # Too old or missing: try to download. If that fails, NULL means "didn't work".
#   collections <- tryCatch(fetch_all(), error = function(e) NULL)
#   
#   if (is.null(collections)) {
#     # Download failed: fall back to the old copy, even if it's stale.
#     collections <- read.csv(cache, stringsAsFactors = FALSE)
#   } else {
#     # Download worked: save it, so the next start can reuse it.
#     write.csv(collections, cache, row.names = FALSE)
#   }
# }
