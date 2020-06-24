# ethnicity_read_codes

For my PhD project I'm using primary care data from the [Optimum Patient Care Research Database](https://opcrd.co.uk). In order to derive a variable for patient ethnicity, I wanted a comprehensive Read code list meeting the following criteria:  
1. Includes both version 2 and version 3 Read codes. 
2. Includes both the 2001 and 2011 censuses. 
3. Is mapped to validated ethnicity categories.  

There are several great Read code resources (e.g. [OpenSafely](https://opensafely.org), [CALIBER](https://www.caliberresearch.org/portal), [Phenotype Library](https://phenotype.id), [ClinicalCodes](https://clinicalcodes.rss.mhs.man.ac.uk), [Vision](http://help.visionhealth.co.uk/visiondatahub/clinical%20portal/Content/G_Full%20Help%20Topics/Reporting/Ethnicity%20Definitions.htm)), but I couldn't find any that satisfy all three criteria for ethnicity.

I've constructed a Read code list that (hopefully!) meets the above criteria. This repository includes:  
1. The code I used to scrape the ethnicity Read code lists from the above websites [read_tables.R](https://github.com/elsie-h/ethnicity_read_codes/blob/master/read_tables.R). 
2. The scraped and cleaned data [rc_ethnicity.csv](https://github.com/elsie-h/ethnicity_read_codes/blob/master/rc_ethnicity.csv). 
3. A markdown file [prepare_list.R](https://github.com/elsie-h/ethnicity_read_codes/blob/master/prepare_list.md) documenting the methods that I used to map the ethnicity codes in the OpenSafely list to the codes in the CALIBER, Phenotype Library, Clinical Codes and Vision lists. 
4. The final code list [rc_ethnicity_final.csv](https://github.com/elsie-h/ethnicity_read_codes/blob/master/rc_ethnicity_final.csv). 

Please feel free to comment/suggest changes/use for your own research!
