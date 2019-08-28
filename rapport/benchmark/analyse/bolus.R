## Boluskalkulator

bolVar <- c("beh_ins_boluswizzard", "beh_ins_boluswizzard_frekv")
var1 <- bolVar[2]
dt1[get(var1) %like% "^> 50", bolkod := 1] %>%
  .[get(var1) %like% "^< 50", bolkod := 2] %>%
  .[get(var1) %like% "Vet", bolkod := 3]

var2 <- bolVar[1]
## dt1[get(var2) == "Ja", .N]


bolJa <- rollup(dt1[get(var2) == "Ja"],
  j = .(
    nja = .N,
    n50mer = sum(bolkod == 1, na.rm = TRUE),
    n50und = sum(bolkod == 2, na.rm = TRUE),
    nvk = sum(is.na(bolkod)) + sum(bolkod == 3, na.rm = TRUE)
  ),
  by = "hosKort"
)

bolJa[is.na(hosKort), hosKort := "Hele landet"]


bolvetikke <- rollup(dt1,
  j = .(
    N = sum(!is.na(get(var2))) - sum(get(var2) == "Vet ikke", na.rm = TRUE),
    nik = sum(get(var2) == "Nei", na.rm = TRUE)
  ),
  by = "hosKort")

bolvetikke[is.na(hosKort), hosKort := "Hele landet"]

dtAlle <- bolJa[bolvetikke, on = "hosKort"]

## dtAlle

tabRaw <- dtAlle[, {
  p50mer = n50mer / nja * 100;
  p50und = n50und / nja * 100;
  pvk = nvk / nja * 100;
  pikke = nik / N * 100;
  u50mer = sprintf("%s (%0.1f)", n50mer, p50mer);
  u50und = sprintf("%s (%0.1f)", n50und, p50und);
  uvk = sprintf("%s (%0.1f)", nvk, pvk);
  uik = sprintf("%s (%0.1f)", nik, pikke);
  list(
    hosp = hosKort,
    u50mer = u50mer,
    u50und = u50und,
    uvk = uvk,
    uik = uik
  )
}]

for (j in seq_len(ncol(tabRaw))){
  set(tabRaw, which(tabRaw[[j]] == '0 (0.0)'), j = j, value = " -")
}

giNavn <- c("", "> 50%", "< 50%", "ikke oppgitt", "bruker ikke")
setnames(tabRaw, names(tabRaw), giNavn)

tabOut <- exp.tabel(tabRaw, "Bruk av boluskalkulater: n(%)", ncol = 5, size = 0.9, total = 1, rowHeight = 0.025, mixCol = 2:5)
