A shiny app that accepts dose-response data for three investigational drugs, computes IC50s and produces dose-response curves with between-drug IC50 tests.  
link: https://claireshz.shinyapps.io/IC50Application/

**Input** - Upload a spreadsheet containing the absolute absorbances of all wells on one plate (MTT is used) in the following manner:
- Column 1: drug concentrations from high to low
- Columns 2-4: absorbance of cells after being treated by drug M
- Columns 5-7: absorbance of cells after being treated by drug P
- Columns 8-10: absorbance of cells after being treated by combined drugs M+P
- Columns 11-13: absorbance of cells presented with DMSO only

_See TestData.xlsx for an example of input data format._

**Output**: view and download the IC50 curves and comparisons.


Note: models and parameters are chosen for some specific studies and may not be able to directly apply to other studies.
