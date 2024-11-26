# Files Description:
## COVID_intext_ChatGPT.R
The Inline text data extraction function.
Put your own OPEN-API-KEY in the function `openai_api_key()`.
## COVID_Table.R
The Table data extraction function.
## COVID_Meta_Analysis.R
Data Extraction Process of using both methods and the Meta-Analyis Process.

# Data Extraction Process
1. First download articles according to the DOIs in "Files DOI".
2. Create two seperate folders, and put all articles used for table data extraction method into one folder, and put the rest articles into another one which used for in-line text extraction method.
3. Download all three R files and put them into a same folder, and also put two article folders into this folder.
4. Open the file "COVID_intext_ChatGPT.R" and find the function 'openai_api_key()' and put your own OPEN-API-KEY in this function and save the change.
5. Open the file "COVID_Meta_Analysis.R", and change the folder's name in source() function and list.files() function based on the created folder's name.
6. Run updated "COVID_Meta_Analysis.R" and get the extraction result and Meta-analysis.
