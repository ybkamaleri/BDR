## Etnisitet

nasDT <- ars2018dt1[hospID==12, ] #Stavanger

## Nordiske land
nordLand <- c("Norge", "Danmark", "Finland", "Sverige", "Island")

## Nasjonalitet variabler - Nasjonalitet, mor- og farfødeland
nasDT[!is.na(fodelandMor) , nordiskMor := ifelse(fodelandMor %in% nordLand, 1L, 0L)]
nasDT[!is.na(fodelandFar) , nordiskFar := ifelse(fodelandFar %in% nordLand, 1L, 0L)]


nordBarn <- nasDT[, {mor = ifelse(is.na(nordiskMor), 0, nordiskMor);
  far = ifelse(is.na(nordiskFar), 0L, nordiskFar);
  barn = mor + far;
  nordiskBarn = ifelse(is.na(nordiskMor) & is.na(nordiskFar), NA, barn);
  list(barn = barn,
    nasj = Nasjonalitet,
    barnland = FodeLand,
    nordkid = as.integer(nordiskBarn),
    landMor = fodelandMor,
    landFar = fodelandFar,
    mor = nordiskMor,
    far = nordiskFar,
    kjonn = Kjonn,
    Pnr = Pnr,
    ENavn = ENavn,
    FNavn = FNavn,
    sykehus = hosKort
  )}]

barnDT <- nordBarn[is.na(nordkid), ]

library(openxlsx)
write.xlsx(barnDT, "nasj_stvg.xlsx")
