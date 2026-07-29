# Geologija data analysis
Data analysis and visualization of $\delta^{18}O$, $\delta^{2}H$ and $^{3}H$ and meteorological data for precipitation isotopes in Ljubljana, period 2011-2024. The results obtained using this analysis are presented in Gačnik et al. (_under review_), "Isotopic composition of hydrogen and oxygen in precipitation at the station Ljubljana (Reaktor), Slovenia: period 2011–2024"

## Instructions:
1) Clone or download the repository, then open _"Geologija_data_analysis.Rproj"_ in RStudio. This sets the repository root as the working directory.
2) Open _"Data analysis GEOLOGIJA.R"_ from the _"scripts"_ directory.
3) Run the `install.packages(...)` command only the first time, then comment it out.
4) The paths in the script are relative to the project root and do not need to be changed. Keep the _data_, _figures_, _scripts_, and _tables_ directories in their existing locations.
5) Run the remaining code line by line with CTRL + ENTER.
    Note: 
    - The plot viewer in Rstudio will not show the plots in the intended aspect ratio and size. The actual figures in the intended format are saved into the directory specified by the "path_figures" parameter at the start of the script

Prepared by Jan Gačnik, Marko Štrok, Klara Žagar, and Polona Vreča. 
