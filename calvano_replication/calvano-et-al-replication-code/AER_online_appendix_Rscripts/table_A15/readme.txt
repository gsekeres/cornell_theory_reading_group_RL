Steps to replicate Table A15:

1) Rename A_InputParameters_1.txt as A_inputParameters.txt and run the baseline executable
2) Rename A_InputParameters.txt as A_inputParameters_1.txt
3) Rename A_convResults.txt as A_convResults_1.txt, and do the same with A_ec.txt, A_irToBR.txt,
A_irToNash.txt, A_qg.txt, A_res.txt
4) Store all the Q_1_XXXX.txt files in a new subfolder, called trained_Q
5) Rename A_InputParameters_2.txt as A_inputParameters.txt and run the baseline executable
6) Rename A_InputParameters.txt as A_inputParameters_2.txt
7) Rename A_convResults.txt as A_convResults_2.txt, and do the same with A_ec.txt, A_irToBR.txt,
A_irToNash.txt, A_qg.txt, A_res.txt

At the end, run the R scripts: fist load the data, then compute the table

