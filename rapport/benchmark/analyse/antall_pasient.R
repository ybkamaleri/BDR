## Registrerte pasienter

pas2018 <- DT[, .N, by = .(hospID, yr)]
## pas2017 <- dtB4[yr == 2017, .N, by = .(hospid, yr)]

## pas.all <- merge(pas2018, pas2017, by.x = "hospID", by.y = "hospid", all.x = TRUE)
## setkey(pas.all, hospID)

## pasDT <- sykehusID[pas.all, on = "hospID"]
## setnames(pasDT, c("yr.x", "N.x", "yr.y", "N.y"), c("yr2018", "n2018", "yr2017", "n2017"), skip_absent = TRUE)

## pasFig <- raphist(pasDT, hosKort, n2018, n2017, lab1 = "(2018)", lab2 = "(2017)", leg1 = "2018", leg2 = "2017")

## pasCir <- pasFig +
##   annotate(geom = "rect", ymin = 100, ymax = 260, xmin = 5, xmax = 9, alpha = .1) +
##   annotate(geom = "text", x = 7, y = 180, label = "Total kompletthet 98%", size = 6, color = "#002b66")


### Bare 2018 pasienter
pasN2018 <- DT[, .N, by = .(hosKort, yr)]
pasN2018[, pro := round(N / nrow(DT) * 100, digits = 1)]

pasFig <- rreg::regbar(pasN2018, hosKort, pro, num = "N", ylab = "Andel (%)",
  title = "Registrerte pasienter i 2018")
pasFig <- pasFig +
   annotate(geom = "rect", ymin = 3, ymax = 9, xmin = 3, xmax = 7, alpha = .1) +
   annotate(geom = "text", x = 5, y = 6, label = "Total kompletthet 98%", size = 6, color = "#002b66")


pasFigN <- rreg::regbar(pasN2018, hosKort, N, ylab = "Antall pasienter",
  title = "Registrerte pasienter i 2018")
pasFigN <- pasFigN +
  annotate(geom = "rect", ymin = 100, ymax = 260, xmin = 5, xmax = 9, alpha = .1) +
  annotate(geom = "text", x = 7, y = 180, label = "Total kompletthet 98%", size = 6, color = "#002b66")

pasCir <- pasFig

## ggsave("antallPasientNtot.jpg", plot = pasFigN, width = 18, height = 20, units = "cm")
## ggsave("antallPasientProsTot.jpg", plot = pasFig, width = 18, height = 20, units = "cm")
