# SQLite helper (Phase 0+). Tables are created lazily.

get_db_path <- function() {
  get_cfg("paths.sqlite_path", "database/app.sqlite")
}

db_connect <- function() {
  path <- get_db_path()
  ensure_dir(dirname(path))
  DBI::dbConnect(RSQLite::SQLite(), path)
}

db_init <- function(con) {
  stopifnot(inherits(con, "DBIConnection"))

  DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS jobs (
  job_id TEXT PRIMARY KEY,
  created_at TEXT,
  updated_at TEXT,
  status TEXT,
  group_var TEXT,
  tax_level TEXT,
  job_dir TEXT,
  n_samples INTEGER,
  n_features INTEGER,
  has_ai INTEGER,
  has_report INTEGER
);
")

  DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS job_files (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  job_id TEXT,
  file_type TEXT,
  original_name TEXT,
  stored_path TEXT,
  md5 TEXT
);
")

  DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS job_results (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  job_id TEXT,
  result_type TEXT,
  file_path TEXT,
  created_at TEXT
);
")

  invisible(TRUE)
}

db_upsert_job <- function(job_id, job_dir, status = "created") {
  assert_non_empty_string(job_id, "job_id")
  assert_non_empty_string(job_dir, "job_dir")

  con <- db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  db_init(con)

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  existing <- DBI::dbGetQuery(con, "SELECT job_id FROM jobs WHERE job_id = ?", params = list(job_id))
  if (nrow(existing) == 0) {
    DBI::dbExecute(
      con,
      "INSERT INTO jobs (job_id, created_at, updated_at, status, job_dir, has_ai, has_report) VALUES (?, ?, ?, ?, ?, 0, 0)",
      params = list(job_id, now, now, status, job_dir)
    )
  } else {
    DBI::dbExecute(
      con,
      "UPDATE jobs SET updated_at = ?, status = ?, job_dir = ? WHERE job_id = ?",
      params = list(now, status, job_dir, job_id)
    )
  }
}

db_insert_job_file <- function(job_id, file_type, original_name, stored_path, md5) {
  assert_non_empty_string(job_id, "job_id")
  assert_non_empty_string(file_type, "file_type")
  assert_non_empty_string(original_name, "original_name")
  assert_non_empty_string(stored_path, "stored_path")
  assert_non_empty_string(md5, "md5")

  con <- db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  db_init(con)

  DBI::dbExecute(
    con,
    "INSERT INTO job_files (job_id, file_type, original_name, stored_path, md5) VALUES (?, ?, ?, ?, ?)",
    params = list(job_id, file_type, original_name, stored_path, md5)
  )
}

