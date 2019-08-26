## Registrerte pasienter

pas2018 <- DT[, .N, by = .(hospID, yr)]
pas2017 <- dtB4[yr == 2017, .N, by = .(hospid, yr)]

pas.all <- merge(pas2018, pas2017, by.x = "hospID", by.y = "hospid", all.x = TRUE)
setkey(pas.all, hospID)

pasDT <- sykehusID[pas.all, on = "hospID"]
setnames(pasDT, c("yr.x", "N.x", "yr.y", "N.y"), c("yr2018", "n2018", "yr2017", "n2017"), skip_absent = TRUE)

pasFig <- raphist(pasDT, hosKort, n2018, n2017, lab1 = "(2018)", lab2 = "(2017)", leg1 = "2018", leg2 = "2017")

pasCir <- pasFig +
  annotate(geom = "rect", ymin = 100, ymax = 260, xmin = 5, xmax = 9, alpha = .1) +
  annotate(geom = "text", x = 7, y = 180, label = "Total kompletthet 98%", size = 6, color = "#002b66")
