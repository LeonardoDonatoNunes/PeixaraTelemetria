source('scripts/utils_settings.R')

dir_root = "scripts/sql"
arquivos = list.files(dir_root)



if (!dir.exists('dados/arquivos_csv_app')) {
  dir.create('dados/arquivos_csv_app')
}


for (arquivo in arquivos) {
  file_name = paste0("dados/arquivos_csv_app/", strsplit(arquivo, "\\.")[[1]][1], ".csv")
  logger::log_info(file_name)
  path = paste(dir_root, arquivo, sep = '/')
  stmt = paste(readLines(path), collapse = "\n")
  df_i = get_sql(stmt)
  write.csv(df_i, file = file_name, row.names = FALSE)
}
