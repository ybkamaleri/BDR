## Aldersfordeling

tabAlder <- rollup(dt1,
  j = list(
    a1 = sum(agekat == 1),
    a2 = sum(agekat == 2),
    a3 = sum(agekat == 3),
    a4 = sum(agekat == 4)),
  by = c("hosKort"))

## Totallen
tabTot <- cube(dt1, .(n = sum(!is.na(agekat))), by = "hosKort")

## merge begge DT
tabAge <- tabAlder[tabTot, on = "hosKort"]

## reorder

tabAge[is.na(hosKort), hosKort := "Totalt"]
nyNavn <- c("", "<5 år", "5-9 år", "10-14 år", ">14 år", "Totalt")
setnames(tabAge, names(tabAge), nyNavn)

tabOut <- exp.tabel(
  tabAge,
  name = "Alder",
  ncol = 6,
  size = 0.9,
  total = 2,
  del = c(.2, .15, .15, .15, .15, .2),
  rowHeight = .015,
  mixCol = 2:5
  )
