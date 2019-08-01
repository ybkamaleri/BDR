## Type pumper

lokalDT <- lok2018dt1

#valgtfritt som har blitt sortert av Siv Janne (pumpe_txt
## lokalDT[, str(pumpe_txt)]
lokalDT[, .N, by = .(pumpe_txt)]



## Alle som er registeret som "Annet" under 'beh_ins_pumpe_type' doublesjekkes mot
## 'beh_ins_pumpe_annet' og registerert riktig hvis det finnes
## Lager nye variabel for merging av disse
lokalDT[, pumpeType := beh_ins_pumpe_type]
lokalDT[beh_ins_pumpe_type == "Annet" | is.na(beh_ins_pumpe_type),
  pumpeType := pumpe_txt]

##bort med whitespace leading og trailing
lokalDT[, pumpeType := trimws(pumpeType, which = "both")]

#Antall
innTab <- lokalDT[!is.na(pumpeType), .N, by = pumpeType]

## Andel
nPump <- length(which(!is.na(lokalDT$pumpeType)))
innTab[, tot := nPump]
innTab[, pos := round((N / tot) * 100, digits = 1)]

innTab[, tot := NULL]

## Antal missing
mispump <- length(which(is.na(lokalDT$pumpeType)))

## Tabell
setnames(innTab, names(innTab), c("Pumpe type", "Antall", "Andel"))
outTab <- tabHux(innTab)



## Sjekker
## ---------
## lokalDT[, .N, by = .(beh_ins_pumpe_type, pumpe_txt, pumpeType)]
