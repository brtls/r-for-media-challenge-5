# B. Kießling
# Challenge 5: Visualisierung von Stadtteilsdaten
# Achtung: Die Challenge hat keinen Code zum überprüfen aber dafür eine Plot als png wie euer Ergebnis aussehen sollte

# Dot-Plot über das Verhältnis von Einkünften und Wohnungsgröße

# 1. Lade den Datensatz StadtteilprofileBerichtsjahr2017.xlsx und speichere ihn im Objekt stadtteile
# Anmerkung: In den aktuelleren Datensätzen sind die Einkünfte leider nicht aufgelistet 
# Tipp: das Argument skip als Teil der Funktion read_xlsx könnte sehr hilfreich sein - ?read_xlsx
# Tipp: Nutze die Funktion clean_names() aus dem Janitor Package, sodass du saubere Colnames erhältst - ??clean_names()

# 2. Selektiere die Columns:
     # x1
     # gesamtbetrag_der_einkunfte_je_steuerpflichtigen_lohn_und_einkommen_steuer_im_jahr
     # durchschnittliche_wohnungsgrosse_in_m2
     # durchschnittlicher_immobilienpreis_fur_eine_eigentums_wohnung_in_eur_m2 

# 3. Nenne die columns in stadtteil, einkommen, wohnungsgroesse, kaufpreis_m2 mit der Funktion rename() um - rename(new_name = old_name)

# 4. Erstelle ein Dot-Plot mit dem Einkommen auf der x-Achse und der Wohnungsgröße auf der y-Achse

# 5. Nutze das Mapping-Argument color für den kaufpreis_m2

# 5. Beschrifte die Achsen, gebe dem Plot einen Titel und Beschrifte die Legende - labs(color = "text")

# 6. Beschreibe mit einem Satz, was dir bei der Interpretation der Daten auffällt und committe diesen mit deinen Ergebnissen

# load library 
library(tidyverse)
library(readxl)

# import data and skip first 3 rows
stadtteile <- read_excel("data/StadtteilprofileBerichtsjahr2017.xlsx", skip = 3)

# clear names with janitor and mutate "durchschnittlicher_immobilienpreis(...) from character into numeric
stadtteile <- janitor::clean_names(stadtteile)
stadtteile <- stadtteile %>% 
  mutate(durchschnittlicher_immobilienpreis_fur_eine_eigentums_wohnung_in_eur_m2 = as.numeric(durchschnittlicher_immobilienpreis_fur_eine_eigentums_wohnung_in_eur_m2))

# selecting and renaming the required variables 
stadtteile <- stadtteile %>% 
  select(x1, gesamtbetrag_der_einkunfte_je_steuerpflichtigen_lohn_und_einkommen_steuer_im_jahr, durchschnittliche_wohnungsgrosse_in_m2, durchschnittlicher_immobilienpreis_fur_eine_eigentums_wohnung_in_eur_m2) %>% 
  rename(stadtteil = x1,
         einkommen = gesamtbetrag_der_einkunfte_je_steuerpflichtigen_lohn_und_einkommen_steuer_im_jahr,
         wohnungsgroesse = durchschnittliche_wohnungsgrosse_in_m2, 
         kaufpreis_m2 = durchschnittlicher_immobilienpreis_fur_eine_eigentums_wohnung_in_eur_m2)

# ploting the data in a dotplot and adding the linear regression line (lm() = linear model)
ggplot(data = stadtteile, aes(x = einkommen, y = wohnungsgroesse, color = kaufpreis_m2)) +
  geom_point() +
  ggtitle("Verhältnis des Einkommens und der Wohnungsgröße in Hamburg") +
  xlab("Jährliches Einkommen in €") + 
  ylab("Wohnungsgröße in m²") +
  labs(color = "Kaufpreis pro m²") +
  geom_smooth(method = lm, se = FALSE, color = "red") + # which straight line has the smallest distance to all points
  theme_minimal()

# checking F-statistic, whether there is a significant correlation between the "einkommen" (independent variable) and the "wohnungsgroesse" (dependent variable)
model <- lm(wohnungsgroesse~einkommen, data = stadtteile)
summary(model)

# Ergebnis: der p-Wert (p<0,05) liegt (weit) unter 0,05 (6.219e-14 = 0,00000000006219), somit wird die 0 Hypothese verworfen und die alternativ Hypothese gilt.
# Es besteht somit ein signifikanter Zusammenhang zwischen dem Einkommen und der Wohnungsgroesse
# Der R² Wert ist 0,4165, somit besteht ein mäßiger Zusammenhang zwischen dem Einkommen und der Wohnungsgröße. 0 = schlecht, 1 = perfekt.
# Koeffizienten: Einkommen = die Steigung beträgt 0,000594. Daraus folgt: Steigt das Einkommen um 1 Einheit, ändert sich die Wohungsgroesse um 0,000594 Einheiten.
# Bsp: Schätzung für ein jährliches Einkommen von 75000€ 
y = 5.879e+01 + 5.940e-04 * 75000
y
# Schätzungsweise liegt bei einem jährlichen Einkommen von 75000€ die Wohnungsgröße bei 103.34 m²