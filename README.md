# Geologija data analysis
Data analysis and visualization of $\delta^{18}O$, $\delta^{2}H$ and $^{3}H$ and meteorological data for precipitation isotopes in Ljubljana, period 2011-2024. The results obtained using this analysis are presented in Gačnik et al. (_under review_), "Isotopic composition of hydrogen and oxygen in precipitation at the station Ljubljana (Reaktor), Slovenia: period 2011–2024"

## Instructions:
1) Open the _"Data analysis GEOLOGIJA.R"_ in the _"scripts"_ directory, preferably with RStudio as the text editor.
2) The first line of code in the opened script should be ran only once and then uncommented, as it installs all the needed R packages for running the script.
3) Set path parameters (_path_scripts_, _path_data_, and _path_figures_) to match the paths of your downloaded GitHub repository. The path parameters are located in the lines 11-13 of the _"Data analysis GEOLOGIJA.R"_ script.
Pay attention to structure paths with "/" and not with "\\"  (the default for Windows paths).
4) Run the code line by line with CTRL + ENTER from line 11 onward.
    Note: 
    - The plot viewer in Rstudio will not show the plots in the intended aspect ratio and size. The actual figures in the intended format are saved into the directory specified by the "path_figures" parameter at the start of the script

Prepared by Jan Gačnik, Marko Štrok, Klara Žagar, and Polona Vreča. 
