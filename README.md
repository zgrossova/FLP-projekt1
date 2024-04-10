## FLP 2024/2025 Projekt 1 
###  Autor: Žaneta Grossová \<xgross11\>
## Struktura odevzdaného adresáře:

**makefile**

**Main.hs** - zpracuje příkaz spuštění, na základě kterého spustí buď tvorbu stromu a klasifikaci (Classification.hs) nebo trénování (TreeTraining.hs)

**Helper.hs** - obsahuje vlastní implementaci funkcí,které se nenachází v dostupných/povolených knihovnách a které jsou využívány ve více modulech

**Classification.hs** - obsahuje implementaci budování rozhodovacího stromu a klasifikaci vstupních dat dle vytvořeného stromu

**TreeTraining.hs** - obsahuje implementaci trénování/tvorby rozhodovacího stromu na základě anotovaných trénovacích dat

**DecisionTree.hs** - obsahuje definici datové struktury rozhodovacího stromu a vlastní implementaci instance Show pro takovýto strom

**README.md** - stručný popis řešení

Pro překlad je nutné v kořenovém adresáři použít **make**.

## Informace k řešení:
Řešení předpokládá, že vstupy budou *korektní* dle zadání. Počet a formát parametrů spuštění je kontrolován, v případě jiných než očekávaných dvou forem spuštění program končí chybou v doprovodu s výpisem nápovědy. Implemetován je i parametr --help pro zobrazení nápovědy. Hodnoty parametrů (například existence souborů) již kontrolován není. 

Implementován je celý rozsah zadání včetně vlastní implementace instance Show pro rozhodovací strom. Všechny zdrojové soubory jsou opatřeny hlavičkami a kód je řádně komentován v anglickém jazyce. 