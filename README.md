
# toxvaldbBMDh

# Background
ToxValDB is a large compilation of in vivo toxicology data and risk assessment values. The database originated in response to the need for consistently annotated and computable toxicology data for use in the development and validation of non-animal new approach methods (NAMs). The database has two major components. The first, ToxValDB Stage contains data that closely match data from each source, in both structure and terminology. The second (the main ToxValDB database) maps all source data to a consistent structure and set of vocabularies. The current version of the database (9.6.0) contains 238,617 records covering 39,669 chemicals from over 40 sources.

# Repository Content
This repository contains the iput files and R scripts used to generate the files used for DCAP analysis using ToxValDB v9.6.0 data.

# Where to access ToxValDB data

- [CompTox Chemicals Dashboard](https://comptox.epa.gov/dashboard/)
	 - Navigate to the "Hazard Data" sidebar tab from a chemical page
	 - Note: Current CCD version is ToxValDB v9.5.0, it will be updated in Fall 2024
- [US EPA Clowder Repository](https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b#folderId=62e184ebe4b055edffbfc22b)
	- Versioned releases of ToxValDB in XLSX and MySQL dump file format with associated documentation

# ToxValDB Repository Links
- [toxvaldbstage](https://github.com/usepa/toxval_stage)
- [toxvaldbmain](https://github.com/usepa/toxvaldbmain/)

# Run Workflow to Generate Export from ToxValDB
 - Clone repository
	 >`> git clone`
 - Set up a .Renviron file based on the "Example_Renviron.txt" file
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

 3. Open an RStudio session using the `toxvaldbBMDh.Rproj` file
 4. Load the package using `devtools::load_all()`
	- Install missing packages as needed. See `NAMESPACE` file
 5. Open the `R/driver.R` script
 6. Run the `driver()` function with desired parameters
 7. Wait for export to run (~10-20 minutes depending on how many DTXSID values are included or filtered out)
 8. Review `data/results/*run_name*` subfolder that is produced. Notable files are:
	- /results/ToxValDB for BMDh res_toxval_v96.xlsx
		- Full dataset pulled from ToxValDB as initially qualifying records
	- /results/ToxValDB for BMDh res_toxval_v96 POD filtered.xlsx
		- Full dataset filtered to a single POD per study_group
	- /results/ToxValDB for BMDh res_toxval_v96 removed entries.xlsx
		- List of records filtered out from the POD filtered file with removal reason

> Submit any questions to Taylor Wall (wall.taylor@epa.gov) or Chelsea Weitekamp (weitekamp.chelsea@epa.gov)