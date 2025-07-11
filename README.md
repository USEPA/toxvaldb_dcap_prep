# toxvaldb_dcap_prep

# Background
ToxValDB is a large compilation of in vivo toxicology data and risk assessment values. The database originated in response to the need for consistently annotated and computable toxicology data for use in the development and validation of non-animal new approach methods (NAMs). The database has two major components. The first, ToxValDB Stage contains data that closely match data from each source, in both structure and terminology. The second (the main ToxValDB database) maps all source data to a consistent structure and set of vocabularies. The current version of the database (9.6.1) contains 255,417 records covering 41,769 chemicals from 36 sources (55 source tables).

# Repository Content
This repository contains the input files and R scripts used to generate the files used for the Database Calibrated Assessment Process (DCAP) analysis using ToxValDB v9.6.1 data.

# Where to access ToxValDB data
- [CompTox Chemicals Dashboard](https://comptox.epa.gov/dashboard/)
	 - Navigate to the "Hazard Data" sidebar tab from a chemical page
	 - Note: Current CCD version is ToxValDB v9.5.0, it will be updated in Fall 2025
- [US EPA FigShare Dataset](https://doi.org/10.23645/epacomptox.20394501)
	- Versioned releases of ToxValDB in XLSX and MySQL dump file format with associated documentation
	- Note, the FigShare DOI link will land on the most recent version of the FigShare posting. Use the version dropdown menu to navigate to the desired release version based on the dataset title.
	- Version v9.6.1 was also released as a [Zenodo Dataset](https://zenodo.org/records/15231010)
	
# Where to access DCAP results
- [US EPA FigShare Dataset](https://doi.org/10.23645/epacomptox.28780757)
  - The DCAP results were also released as a [Zenodo Dataset](https://zenodo.org/uploads/15357834)

# ToxValDB Repository Links
- [toxvaldbstage](https://github.com/usepa/toxvaldbstage)
- [toxvaldbmain](https://github.com/usepa/toxvaldbmain/)

# Run Workflow to Generate Export from ToxValDB
  1. Clone repository
    >`> git clone`
  2. Set up a .Renviron file based on the "Example_Renviron.txt" file
    > - `datapath`
      - Directory path to where the "data" folder is being read from/written to. Default is the repository folder itself.
    > - `db_user`
      - Username to an instance of the ToxValDB database
      - **Note:** At this time, the ToxValDB database is only available for internal EPA users or from setting up a local copy using the SQL Database Dump file in the linked Clowder repository above.
    > - `db_pass`
      - Password to an instance of the ToxValDB database
    > - `db_server`
      - Host/server for an instance of the ToxValDB database
    > - `db_port`
      - Port for the host/server
    >- `toxval.db`
      - Name of the ToxValDB database (default of res_toxval_v96)

  3. Select a version of ToxValDB to use.
      - Full database
        - *TBD instructions*
      - SQLite database
        - *TBD instructions*
  4. Open an RStudio session using the `toxvaldb_dcap_prep.Rproj` file
  5. Load the package using `devtools::load_all()`
    - Install missing packages as needed. See `NAMESPACE` file. Users can also optionally use the included `renv`. See [renv](https://rstudio.github.io/renv/) package documentation for use.
  6. Open the `R/run_toxvaldb_dcap_prep.R` script
  7. Run the `run_toxvaldb_dcap_prep()` function with desired parameters
  8. Wait for export to run (~10-20 minutes depending on how many DTXSID values are included or filtered out)
  9. Review `data/results/*run_name*` subfolder that is produced. Notable files are:
    - /results/DCAP_export_toxval_type.xlsx
      - Summary of effect types by source that were exported from ToxValDB for DCAP
	  - /results/ToxValDB for DCAP res_toxval_v96.xlsx
		  - Full dataset pulled from ToxValDB as initially qualifying records
	  - /results/ToxValDB for DCAP res_toxval_v96 POD filtered.xlsx
		  - Full dataset filtered to a single POD per study_group. This is the finalized prepped file used as input for the DCAP.
	  - /results/ToxValDB for DCAP res_toxval_v96 removed entries.xlsx
		  - Records filtered out from the POD filtered file with removal reason
	  - /results/ToxValDB for DCAP res_toxval_v96_1_cancer_removed.xlsx
  	  - Records filtered out due to only having a toxicological effect category of "cancer"
	  - results/ToxValDB for DCAP res_toxval_v96_1 ECOTOX NOEL filtered.xlsx
	    - Records from ECOTOX filtered out due to the N(OA)EL being greater than the minimum L(OA)EL in a study group
	  - results/DCAP_ToxVal_res_toxval_v96_1_input.xlsx
	    - Additional processing of "POD filtered" file to remove intermediate fields and recode select fields.

> Submit any questions to Taylor Wall (wall.taylor@epa.gov) or Chelsea Weitekamp (weitekamp.chelsea@epa.gov)

# Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use.  EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information.  Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA.  The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
