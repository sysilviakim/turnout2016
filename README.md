# turnout2016

Replication material for Kim, Seo‐young Silvia, R. Michael Alvarez, and Christina M. Ramirez. 2020. ["Who Voted in 2016? Using Fuzzy Forests to Understand Voter Turnout."](https://onlinelibrary.wiley.com/doi/abs/10.1111/ssqu.12777) Social Science Quarterly 101(2): 978–88.

* The makefile requires that the \*.tab file in [CCES Common Content, 2016](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910/DVN/GDF6Z0) (version: Feb 10, 2018) be downloaded in the subdirectory `/data/`. 
* We provide `Rda` files for random and fuzzy forests, since the runtime of these depending on the hardware specification can be long. The logit and the CART we ask the user to run herself---due to the file size limits we cannot push these to the repository. The user can create the same random and fuzzy forests objects using the given makefile.
* To undergo the entire build, type `make all` into the command line. The build process has been last tested on Red Hat Enterprise Linux Workstation 7.5 (Maipo), Intel(R) Xeon(R) Gold 6130 CPU @ 2.10GHz.
