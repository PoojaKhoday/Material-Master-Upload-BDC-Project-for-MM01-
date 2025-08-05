Material Master Upload BDC Program (ZBDC_REC_PPK1)

Project Overview
This SAP ABAP project is a BDC (Batch Data Communication) program to automate the creation of Material Master records via Transaction MM01.
The program reads material details from a tab-separated text file (.txt) and simulates MM01 screen entries to create materials.

Uploaded fields include:

- Material Number (MATNR)

- Material Type (MTART)

- Industry Sector (MBRSH)

- Base Unit of Measure (MEINS)

SAP Table Used
MARA – General Material Data 

Features
- Selection screen to upload the Notepad file containing material details
- BDC recording for MM01 to automate material creation
- Reads tab separated fields using GUI_UPLOAD
- Automatically fills Basic Data 1 with a default description
- Supports bulk material creation in one execution
- Displays success or error messages for each record processed

Learning Outcomes
- By implementing this project, you will learn:
- How to perform BDC programming using CALL TRANSACTION
- Uploading and processing flat files with GUI_UPLOAD
- Constructing BDCDATA internal tables for SAP screen navigation
- Automating MM01 Material Master creation
- Handling bulk data uploads and basic error handling
