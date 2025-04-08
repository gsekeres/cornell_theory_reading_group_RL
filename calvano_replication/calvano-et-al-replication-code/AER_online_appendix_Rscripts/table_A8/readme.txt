Steps to replicate Table A8:

1) Rename A_InputParameters_mu.txt as A_inputParameters.txt and run the baseline executable
2) Rename A_InputParameters.txt as A_inputParameters_mu.txt
3) Rename A_convResults.txt as A_convResults_mu.txt, and do the same with A_ec.txt, A_irToBR.txt,
A_irToNash.txt, A_qg.txt, A_res.txt
4) Rename A_InputParameters_mu0.txt as A_inputParameters.txt and run the baseline executable
5) Rename InfoExperiment_1.txt as InfoExperiment_12.txt
6) Rename A_det_1.txt as A_det_12.txt
7) Append the second row of A_conv_results.txt at the end of A_conv_results_mu.txt, 
and do the same with A_ec.txt, A_irToBR.txt, A_irToNash.txt, A_qg.txt, A_res.txt
8) Delete A_convResults.txt, A_ec.txt, A_irToBR.txt, A_irToNash.txt, A_qg.txt, A_res.txt
9) Rename A_convResults_mu.txt as A_convResults.txt, and do the same with 
A_ec_mu.txt, A_irToBR_mu.txt, A_irToNash_mu.txt, A_qg_mu.txt, A_res_mu.txt
10) Run the R scripts: fist load the data, then compute the table

