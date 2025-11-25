list.files(
  path = "source/docu_report/",
  #path = ".",
  pattern = "\\.png|\\.jpg|\\.eps|\\.pdf|\\.sty|\\.tex|\\.aux|\\.log|site_libs",
  full.names = TRUE
) |>
  unlink(x = _, recursive = TRUE)
