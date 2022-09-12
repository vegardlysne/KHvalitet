# KHvalitet

Kvalitetskontroll av KUBE-filer 

# Bruk

For � kontrollere en KUBE brukes filen `Kvalitetskontroll.Rmd`. Denne henter inn de n�dvendige funksjonene som gjennomf�rer de ulike stegene av kvalitetskontrollen. Disse kj�res i rekef�lge, og output kan inspiseres stegvis. Dersom ett steg er ok, g�r du videre til neste. 

Plott lagres i egne mapper p� forskningsserver. 

Til slutt kan hele kvalitetskontrollen printes som en rapport, og lagres som dokumentasjon, ved � trykke p� `Knit` �verst i RStudio.

**INPUT** skjer hovedsakelig i f�rste kodechunk. Her defineres f�lgende:

- Sti til ny KUBE som skal kontrolleres
- Sti til gammel KUBE som brukes som sammenligningsgrunnlag for deler av kvalitetskontrollen
- Prikkegrense