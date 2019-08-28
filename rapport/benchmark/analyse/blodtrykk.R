## Blodtykk

dt1[stage  %in% c("stage1", "stage2"), bpress := 2] %>%
  .[stage == "elevated", bpress := 1] %>%
  .[stage == "normal", bpress := 0]


tabBt <- rollup(dt1,
  j = .(
    n90 = sum(bpress == 1, na.rm = T),
    n95 = sum(bpress == 2, na.rm = T),
    N = sum(!is.na(bpress))
  ), by = "hosKort")


tabRaw <- tabBt[, {
  p90 = n90 / N * 100;
  p95 = n95 / N * 100;
  u90 = sprintf("%s (%0.1f)", n90, p90);
  u95 = sprintf("%s (%0.1f)", n95, p95);
  list(
    hosKort = hosKort,
    u90 = u90,
    u95 = u95,
    N = N
  )
}]

tabRaw[is.na(hosKort), hosKort := "Hele landet"]

for (j in seq_len(ncol(tabRaw))){
  set(tabRaw, which(tabRaw[[j]] == '0 (0.0)'), j = j, value = " -")
}

tabNavn <- c("", "90-94 %tile", ">=95 %tile", "N")
setnames(tabRaw, names(tabRaw), tabNavn)

tabOut <- exp.tabel(tabRaw, "Blodtykk persentil: n(%)", ncol = 4,
  size = 0.8, total = 2, valgCol = 2:3, valgAlign = "right",
  rowHeight = .025, mixCol = 2:3)
