## Etnisitet

## Nordiske land
nordLand <- c("Norge", "Danmark", "Finland", "Sverige", "Island")

## Nasjonalitet variabler - Nasjonalitet, mor- og farfødeland
dt1[!is.na(fodelandMor) , nordiskMor := ifelse(fodelandMor %in% nordLand, 1L, 0L)]
dt1[!is.na(fodelandFar) , nordiskFar := ifelse(fodelandFar %in% nordLand, 1L, 0L)]

## nordiskBarn
nordBarn <- dt1[, {
  mor = ifelse(is.na(nordiskMor), 0, nordiskMor);
  far = ifelse(is.na(nordiskFar), 0L, nordiskFar);
  barn = mor + far;
  nordiskBarn = ifelse(is.na(nordiskMor) & is.na(nordiskFar), NA, barn);
  list(
    barn = barn,
    hosKort = hosKort,
    nordkid = as.integer(nordiskBarn),
    mor = nordiskMor,
    far = nordiskFar,
    hba1c = lab_HbA1cAkerVerdi
  )}]

nordBarn[nordkid > 0, norsk := 1] %>%
  .[nordkid == 0, norsk := 0]

## missing info om foreldre
norskNA <- nordBarn[is.na(norsk), .N]

## Hele landet
tabTotRaw <- nordBarn[, .(n = .N, mean = mean(hba1c, na.rm = T)), by = norsk]
tabTot <- data.table(hosKort = "Hele landet",
  a1 = tabTotRaw$n[tabTotRaw$norsk == 1],
  m1 = tabTotRaw$mean[tabTotRaw$norsk == 1],
  a0 = tabTotRaw$n[tabTotRaw$norsk == 0],
  m0 = tabTotRaw$mean[tabTotRaw$norsk == 0])

tabTot <- tabTot[!is.na(a1), ]

## per sykehus
norskLong <- rollup(nordBarn,
  j = .(
    n = .N,
    mean = mean(hba1c, na.rm = TRUE)
  ),
  by = c("hosKort", "norsk"))

tabMean <- dcast(norskLong, hosKort ~ norsk, value.var = "mean", subset = .(!is.na(norsk)))
tabAntall <- dcast(norskLong, hosKort ~ norsk, value.var = "n", subset = .(!is.na(norsk)))

meanName <- paste0("m", names(tabMean)[-1])
setnames(tabMean, names(tabMean)[-1], meanName)
antallName <- paste0("a", names(tabAntall)[-1])
setnames(tabAntall, names(tabAntall)[-1], antallName)

tabEth <- tabAntall[tabMean, on = "hosKort"]
## merge med total
tabAll <- rbindlist(list(tabEth, tabTot), use.names = TRUE)

## rename variables
setcolorder(tabAll, c("hosKort", "a1", "m1", "a0", "m0"))

tabAll[, m1 := sprintf("  %0.2f", m1)]
tabAll[, m0 := sprintf("  %0.2f", m0)]
tabAll[, a0 := as.character(`a0`)][is.na(a0), a0 := "-"]
tabAll[m0 == 'NA', m0 := "-"]

nyNavn <- c("", "Antall", "Gj.snitt", "Antall", "Gj.snitt")
setnames(tabAll, names(tabAll), nyNavn)

## Tabell

tabXX <- as_hux(tabAll, add_colnames = TRUE)
tabXX <- map_background_color(tabXX, by_rows("grey90", "white"))

lastRow <- nrow(tabXX)
lastCol <- ncol(tabXX)

tabXX <- tabXX %>%
  map_background_color(by_rows("grey90", "white")) %>%
  set_bold(lastRow,, TRUE)

tabXX <- rbind(c("", "Nordisk", "", "Annen", ""), tabXX)

tabOut <- tabXX %>%
  set_top_border(3,, TRUE) %>%
  set_bottom_border(1,2:5, 0.5) %>%
  set_align(, c(2,4), "left") %>%
  set_width(0.9) %>%
  set_row_height(.025)

## quick_pdf(tabOut, file = "test.pdf")
