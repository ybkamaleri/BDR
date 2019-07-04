## nasjonalitet og kjønn
## ---------------------

nordLand <- c("Norge", "Danmark", "Finland", "Sverige", "Island")

ars2018[!is.na(fodelandMor) , nordiskMor := ifelse(fodelandMor %in% nordLand, 1L, 0L)]
ars2018[!is.na(fodelandFar) , nordiskFar := ifelse(fodelandFar %in% nordLand, 1L, 0L)]

## nordisk barn
## Minst en av foreldrene er født i de nordiske landene
ars2018[, .N, by=.(nordiskFar)]

nordBarn <- ars2018[, {mor = ifelse(is.na(nordiskMor), 0, nordiskMor);
             far = ifelse(is.na(nordiskFar), 0, nordiskFar);
             barn = mor + far;
             nordiskBarn = ifelse(is.na(nordiskMor) & is.na(nordiskFar), NA, barn);
             list(barn = barn, 
                  nordiskBarn = nordiskBarn,
                  mor = nordiskMor,
                  far = nordiskFar,
                  kjonn = Kjonn,
                  Pnr = Pnr
                  )}]

nordBarn[.(nordiskBarn = 1:2, to = 1), on = "nordiskBarn", nordiskBarn := i.to]
# nordBarn[nordiskBarn %in% 1:2, nordiskBarn := 1]

nordBarn[, .N, by = .(nordiskBarn, kjonn)]

