# Install DiagrammeR package if not already installed
if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
  install.packages("DiagrammeR")
}

# Load the DiagrammeR package
library(DiagrammeR)

grViz("
digraph flowchart {
  # Node definitions
  node [shape = box, style = filled, fillcolor = lightblue]
  TaxCalculator_Subset1 [label = 'TaxCalculator_Subset1.R']
  TaxCalculator_Subset2 [label = 'TaxCalculator_Subset2.R']
  TaxCalculator_Subset3 [label = 'TaxCalculator_Subset3.R']
  PIT_Module [label = 'PIT-Module.R']
  Calc_AggregationOfData [label = 'Calc-AggregationOfData.R']
  Calc_Structure [label = 'Calc-Structure.R']
  Calc_Distribution_Effects [label = 'Calc-Distribution-Effects.R']
  Calc_Redistribution_Effects [label = 'Calc-Redistribution-Effects.R']
  Charts_StructureGrossIncome [label = 'Charts-StructureGrossIncome.R']
  Charts_PIT_Revenues [label = 'Charts-PIT_Revenues.R']
  Charts_Distribution [label = 'Charts-Distribution.R']

  # Edge definitions
  PIT_Module -> TaxCalculator_Subset1
  PIT_Module -> TaxCalculator_Subset2
  PIT_Module -> TaxCalculator_Subset3
  TaxCalculator_Subset1 -> Calc_AggregationOfData
  TaxCalculator_Subset2 -> Calc_AggregationOfData
  TaxCalculator_Subset3 -> Calc_AggregationOfData
  Calc_AggregationOfData -> Calc_Structure
  Calc_Structure -> Calc_Distribution_Effects
  Calc_Distribution_Effects -> Calc_Redistribution_Effects
  Calc_Redistribution_Effects -> Charts_PIT_Revenues
  Calc_Redistribution_Effects -> Charts_StructureGrossIncome
  Calc_Redistribution_Effects -> Charts_Distribution
}
")
