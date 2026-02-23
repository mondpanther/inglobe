# find_dropbox.R
# Detect the local Dropbox folder, including enterprise/business Dropbox installs.
#
# Dropbox stores its sync-folder location in a JSON config file:
#   Windows: %APPDATA%/Dropbox/info.json  or  %LOCALAPPDATA%/Dropbox/info.json
#   macOS/Linux: ~/.dropbox/info.json
#
# The JSON contains either a "business" key (enterprise), a "personal" key, or both.
# This module exposes:
#   get_dropbox_path()       – root Dropbox folder (prefers business over personal)
#   get_dropbox_path_personal() – personal folder only (NULL if absent)
#   get_dropbox_path_business() – business folder only (NULL if absent)
#   get_dropbox_info()       – full parsed info.json list

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required by find_dropbox.R")
}

# ---------------------------------------------------------------------------
# Internal: locate and read info.json
# ---------------------------------------------------------------------------
.dropbox_info_paths <- function() {
  if (.Platform$OS.type == "windows") {
    c(
      file.path(Sys.getenv("LOCALAPPDATA"), "Dropbox", "info.json"),
      file.path(Sys.getenv("APPDATA"), "Dropbox", "info.json")
    )
  } else {
    file.path(Sys.getenv("HOME"), ".dropbox", "info.json")
  }
}

.read_dropbox_info <- function() {
  for (p in .dropbox_info_paths()) {
    if (file.exists(p)) {
      return(jsonlite::fromJSON(p, simplifyVector = FALSE))
    }
  }
  NULL
}

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Return the full parsed contents of Dropbox's info.json.
#' NULL if Dropbox is not installed / info.json not found.
get_dropbox_info <- function() {
  .read_dropbox_info()
}

#' Return the local Dropbox sync-folder path.
#'
#' @param prefer Character, either "business" (default) or "personal".
#'   When both accounts exist, the preferred type is returned.
#' @return A single character path, or NULL if not found.
get_dropbox_path <- function(prefer = c("business", "personal")) {
  prefer <- match.arg(prefer)
  info <- .read_dropbox_info()
  if (is.null(info)) return(NULL)

  first  <- if (prefer == "business") "business" else "personal"
  second <- if (prefer == "business") "personal" else "business"

  if (!is.null(info[[first]]$path))  return(info[[first]]$path)
  if (!is.null(info[[second]]$path)) return(info[[second]]$path)
  NULL
}

#' Return the business (enterprise) Dropbox path, or NULL.
get_dropbox_path_business <- function() {
  info <- .read_dropbox_info()
  if (!is.null(info$business$path)) info$business$path else NULL
}

#' Return the personal Dropbox path, or NULL.
get_dropbox_path_personal <- function() {
  info <- .read_dropbox_info()
  if (!is.null(info$personal$path)) info$personal$path else NULL
}
