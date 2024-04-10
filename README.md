# generateICAN-MNDData

Code to generate up to date drug list with data from relisyr - and combine this with data from in vitro screening, pathway analysis, chembl, bnf, admetsar - to upload to googlesheets. ICAN-MND will then pull data from googlesheets for the shinyapp (https://camarades.shinyapps.io/ICAN-MND)

Raw data sources
1. DrugLibrary.csv - Drug Screening Library from Alessandra. 6651 compounds, columns: Library, Original_Supplier_Name, Name, Compound_ID, CAS, SMILES, Target
2. CompoundList-inSilicoPrediction_MND_CAS.csv - List of compounds identified as candidates from WGCNA using TargetALS consortium RNAseq data from pALS post mortem brains. Contains Coumpound and gene
3. MCB_retest_results[1].csv - James Longden's astrocyte MCB assay screen results
4. HitList_ProteinAggregationAssay_drc-retests-activity.csv - Alessandra's protein aggregation assay screen results
5. ReLiSyR list - pull up-to-date drug list of drugs with included clinical publications, drugs meeting MND drug/disease logic (>=1 clinical study in MND or clinical studies in >=2 other diseases of interest)from AWS database.
6. ReLiSyR current prioritised (core) drugs - googlesheet
7. RegEx Drug dictionary to match drug names from above developed by Jing using DrugBank data, stored in drugdictionary googlesheet

Other data sources
8. BNF drug list - drug listed in the British National Formulary (https://bnf.nice.org.uk/)
9. ChEMBL (https://www.ebi.ac.uk/chembl/) - availability in oral formulation, prescription only status, rule of 5, rule of 3 
10. AdmetSAR (http://lmmd.ecust.edu.cn/admetsar2/) - for blood brain penetrance data 


Steps: 
1. 01getAdmetSarBBB.R 
(original file name getAdmetSarBBB.R in OneDrive > ReLiSyR-MND/Drug Lists/mcbHitsSmiles)
Input: DrugLibrary.csv
Function: Get BBB penetrance data from http://lmmd.ecust.edu.cn/admetsar2/ for drug screening library (DrugLibrary.csv) drugs using the listed SMILES. Done via docker/RSelenium (done on CW's Mac). 
Output: drugLibrary_AdnetSARBBB2021-08-18.csv

2. 02generateAllDrugsList.R
Input: Drug Dictionary (drugBankDictionary and drugVocabulary - googlesheet); MCB_retest_results[1].csv (local); HitList_ProteinAggregationAssay_drc-retests-activity.csv (local); CompoundList-inSilicoPrediction_MND_CAS.csv; drugLibrary_AdnetSARBBB2021-08-18.csv (local); ReLiSyR list (from AWS RDS database)
Function: First, generate combine drug dictionary and drugVocabulary to get CAS, then match CAS with Druglibrary with BBB permeability, astrocyte MCB screen, protein aggregation screen, WGCNA hits. Then use drug dictionary and autoannotation to match by name with ReLiSyR list to produce mergedList2. Use the names in mergedList2 to get pharmacological data from ChEMBL. 
Output: output/Sys.Date()allDrugsListWithChemblData.csv; writes copy to ICANGooglesheet, sheet = AllDrugsList


3. 03getBNFDrugList.R
Function: Reads all drugs listed in the BNF by scraping https://bnf.nice.org.uk/drugs/ from drug names, medicinal forms and special order forms. Then use Drug Dictionary to match to regex terms used to create allDrugListWithChemblData.csv in step 2. 
Output: output/bnf/Sys.Date()BNFData.csv, writes copy to ICANGooglesheet, sheet = BNFData


4. 04getICTRPData.R
Function: reads in MND/ALS clinical trials data downloaded manually from ICTRP frontend to generate list of drugs, number of MND/ALS clinical trials registered in one of the listed trial registries, and trial registration number. NB: there may be duplicates across trial registries. 
ICTRP data sources as of 2022-06-29:
Australian New Zealand Clinical Trials Registry, last data file imported on 30 May 2022
Chinese Clinical Trial Registry, last data file imported on 30 May 2022
ClinicalTrials.gov, last data file imported on 30 May 2022
EU Clinical Trials Register (EU-CTR), last data file imported on 30 May 2022 
ISRCTN, last data file imported on 30 May 2022 
The Netherlands National Trial Register, last data file imported on 30 May 2022
Brazilian Clinical Trials Registry (ReBec), last data file imported on 31 May 2022 
Clinical Trials Registry - India, last data file imported on 30 May 2022
Clinical Research Information Service - Republic of Korea, last data file imported on 30 May 2022 
Cuban Public Registry of Clinical Trials, last data file imported on 30 May 2022 
German Clinical Trials Register, last data file imported on 30 May 2022 
Iranian Registry of Clinical Trials, last data file imported on 30 May 2022 
Japan Registry of Clinical Trials (jRCT), last data file imported on 2 May 2022 
Pan African Clinical Trial Registry, last data file imported on 30 May 2022 
Sri Lanka Clinical Trials Registry, last data file imported on 30 May 2022 
Thai Clinical Trials Registry (TCTR), last data file imported on 31 May 2022 
Peruvian Clinical Trials Registry (REPEC), last data file imported on 30 May 2022 
Lebanese Clinical Trials Registry (LBCTR), last data file imported on 31 May 2022
Input: 2022-06-27IctrpResults.csv, AllDrugsList from ICANGooglesheet
Output: 2022-06-28ICTRPClinicalTrialsList.csv, writes copy to ICANGooglesheet, sheet = ICTRPList;
2022-06-28AllDrugsList.csv, writes copy to ICANGooglesheet, sheet = AllDrugsList 

5. 05cleanRegexDrugNames.R
Function: Several drug names in drug dictionary were erroneous and wrongly matched. We clean the data to remove the erroneous matches. 
Input: ICANGooglesheet, sheet = AllDrugsList 
Output: 2022-06-28AllDrugsList.csv, writes copy to ICANGooglesheet, sheet = AllDrugsList 

6. 06getMissingCAS.R
Function: Some drugs are missing CAS numbers from drugVocabulary/drugDictionary/Drug screening library. We parse drug names to https://commonchemistry.cas.org/api and ChemIDPlus (https://chem.nlm.nih.gov/) to obtain CAS numbers where available. 
Input: ICANGooglesheet, sheet = AllDrugsList 
Output: 2022-06-29AllDrugsList.csv, writes copy to ICANGooglesheet, sheet = AllDrugsList 
