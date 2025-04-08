Steps to replicate Table A10:

For each A_InputParameters_X.txt, with X = 1, 2, 3, 4, 5:
1) Rename A_InputParameters_X.txt as A_inputParameters.txt and run the baseline executable
2) Rename A_InputParameters.txt as A_inputParameters_X.txt
3) Rename A_convResults.txt as A_convResults_X.txt, and do the same with A_ec.txt, A_irToBR.txt,
A_irToNash.txt, A_qg.txt, A_res.txt

At the end, run the R scripts: fist load the data, then compute the table

