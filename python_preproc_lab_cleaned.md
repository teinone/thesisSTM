

```python
import sys
import pandas as pd
import numpy as np
import json
import os
import re
import pprint
# import random
import string
import sys
#import pathlib2
from pathlib import Path
#from pathlib2 import *

#Add garbage collection:
import gc

print("Libraries loaded")
```

    Libraries loaded



```python
!mkdir /home/jupyter/content/

```


```python
!pwd
!mkdir  /home/jupyter/content/PDF/
!mkdir  /home/jupyter/content/PDF/TXT/

```

    /home/jupyter



```python
!mkdir  /home/jupyter/content/PDF/TXT/2015
```


```python
!gsutil -m cp -r gs://rsm-thesis-bucket/TXT_prod/TXT/2015/ /home/jupyter/content/PDF/TXT/2015/

```

    Copying gs://rsm-thesis-bucket/TXT_prod/TXT/2015/AFM_303151_thesis_id2323.txt...
    Copying gs://rsm-thesis-bucket/TXT_prod/TXT/2015/AFM_311740_thesisresit_id2261.txt...
    Copying gs://rsm-thesis-bucket/TXT_prod/TXT/2015/AFM_319367_thesis_id2335.txt...
    Copying gs://rsm-thesis-bucket/TXT_prod/TXT/2015/AFM_322831_thesis_id2273.txt...
    ....
    Copying gs://rsm-thesis-bucket/TXT_prod/TXT/2015/SM_403619_thesis_id2649.txt...
    \ [976/976 files][140.9 MiB/140.9 MiB] 100% Done                                
    Operation completed over 976 objects/140.9 MiB.                                  



```python
%cd /home/jupyter/content/PDF/TXT/2015/2015
```

    /home/jupyter/content/PDF/TXT/2015/2015



```python
!ls -la
```

    total 146328
    drwxr-xr-x 2 jupyter jupyter  69632 Jun 10 13:20 .
    drwxr-xr-x 3 jupyter jupyter   4096 Jun 10 13:20 ..
    -rw-r--r-- 1 jupyter jupyter 133732 Jun 10 13:20 AFM_303151_thesis_id2323.txt
    -rw-r--r-- 1 jupyter jupyter 107335 Jun 10 13:20 AFM_311740_thesisresit_id2261.txt
    -rw-r--r-- 1 jupyter jupyter  88678 Jun 10 13:20 AFM_319367_thesis_id2335.txt
    ....
    -rw-r--r-- 1 jupyter jupyter  63620 Jun 10 13:20 SM_403619_thesis_id2649.txt



```python
!mkdir %cd /home/jupyter/content/PDF/TXT/2015/2015/CSV/
```


```python
################################################################################
# WORKING FILE-LEVEL CLOUD VERSION ###########################################
################################################################################

# Much of this chunk has been adapted from Levente Csibi's (2019) work.

# Count index for looping files
cnt = 1

# File directories
DIR_INPUT_TXTs = "/home/jupyter/content/PDF/TXT/2015/2015/" #Input
OUTPUTDIR = "/home/jupyter/content/PDF/TXT/2015/2015/CSV/"  #Output

# Loop filenames
for filename in os.listdir(DIR_INPUT_TXTs):
  
  # Empty dataframe to loop
  df = pd.DataFrame(columns=('Program', "Student", "Resit", "FileID", "Year"))

  # Only take .txt files
  if filename.endswith(".txt"): 
    
    # Read txt files
    fulltext = open(str(Path(DIR_INPUT_TXTs)) + "/" + str(Path(filename)), "r").read()
    
    # Obtaining info from filenames
    # First param is offset from the file name end. Clips file extension. 
    # The second is offset from substring index.
    # The last is the index of the element.
    program  = filename[:-4].split('_')[0:][0]
    student  = filename[:-4].split('_')[0:][1]
    resit    = filename[:-4].split('_')[0:][2]
    fileID   = filename[:-4].split('_')[0:][3]
    year     = 2015                                #used for filename
    savename = (str(student) + str(year) + '.csv') #used for filename

    # Preprocessing doc contents ===============================================
    
    #Load the fulltext into a list, split it at the sentence breaks
    fulltext_splitted = list(fulltext.split('. '))
    
    #Take only lines with over 35 chrs
    fulltext_splitted = [line for line in fulltext_splitted if len(line) >= 35]
    
    #Drop all non-alphabetic characters using RegEx
    fulltext_splitted = [re.sub('[^a-zA-Z ]+', ' ', line) for line in fulltext_splitted]
    
    #Drop lines without actual text (less than 35 characters after stripping)
    fulltext_splitted = [line for line in fulltext_splitted if len(line.strip()) >= 35]
    
    #Lowercase everything
    fulltext_splitted = [line.lower() for line in fulltext_splitted]
      
    # Writing and saving results to the gs bucket ==============================

    # Writing into dataframe
    # Loop the entire fulltext dictionary
    for each_line in range(len(fulltext_splitted)):
      newrow = {'Program': program, 'Student': student, 'Resit': resit, 'FileID': fileID, 'Year': year, 'Fulltext': fulltext_splitted[each_line]}
      df = df.append(newrow, ignore_index = True)

    # Save the df as csv  
    df.to_csv(Path(OUTPUTDIR)/savename, index = False)
    
    # Upload to gs bucket
    !gsutil cp /home/jupyter/content/PDF/TXT/$year/$year/CSV/$savename gs://rsm-thesis-bucket/CSV_prod/$year/ 
    
    # CLEANUP ==================================================================
    
    # Clear bash variable from python so it can be reset in the next loop  
    !unset savename
    !unset year
    # Delete df so it can be recreated for the next loop
    del df
    
    # Finish loop, increment count.
    print("Loop " + str(cnt) + " done.")
    cnt = cnt+1
    continue
        
#   else:
#     continue
    
########################################################################################

#Save to csv
# df.to_csv(Path(OUTPUTDIR)/'2017_new_sentences.csv', index = False)
print("All files stripped successfully.")
```

    Copying file:///home/jupyter/content/PDF/TXT/2015/2015/CSV/3565092015.csv [Content-Type=text/csv]...
    / [1 files][ 85.3 KiB/ 85.3 KiB]                                                
    Operation completed over 1 objects/85.3 KiB.                                     
    Loop 1 done.
    Copying file:///home/jupyter/content/PDF/TXT/2015/2015/CSV/3397292015.csv [Content-Type=text/csv]...
    / [1 files][ 94.7 KiB/ 94.7 KiB]                                                
    Operation completed over 1 objects/94.7 KiB.                                     
    Loop 2 done.
    Copying file:///home/jupyter/content/PDF/TXT/2015/2015/CSV/3764272015.csv [Content-Type=text/csv]...
    / [1 files][213.9 KiB/213.9 KiB]                                                
    Operation completed over 1 objects/213.9 KiB.                                    
    Loop 3 done.
    Copying file:///home/jupyter/content/PDF/TXT/2015/2015/CSV/4028872015.csv [Content-Type=text/csv]...
    / [1 files][136.9 KiB/136.9 KiB]                                                
    Operation completed over 1 objects/136.9 KiB.                                    
    Loop 4 done.
    Copying file:///home/jupyter/content/PDF/TXT/2015/2015/CSV/4002732015.csv [Content-Type=text/csv]...
    -
    Operation completed over 1 objects/276.5 KiB.                                    
    Loop 5 done.
    Copying file:///home/jupyter/content/PDF/TXT/2015/2015/CSV/3341912015.csv [Content-Type=text/csv]...
    / [1 files][149.6 KiB/149.6 KiB]                                                
    Operation completed over 1 objects/149.6 KiB.                                    
    Loop 6 done.
    Copying file:///home/jupyter/content/PDF/TXT/2015/2015/CSV/3300592015.csv [Content-Type=text/csv]...
    / [1 files][111.4 KiB/111.4 KiB]                                                
    Operation completed over 1 objects/111.4 KiB.                                    
    Loop 7 done.
    Copying file:///home/jupyter/content/PDF/TXT/2015/2015/CSV/3574632015.csv [Content-Type=text/csv]...
    / [1 files][100.5 KiB/100.5 KiB]                                                
    Operation completed over 1 objects/100.5 KiB.                                    
    Loop 8 done.
    Copying file:///home/jupyter/content/PDF/TXT/2015/2015/CSV/3292982015.csv [Content-Type=text/csv]...
    / [1 files][239.5 KiB/239.5 KiB]                                                
    Operation completed over 1 objects/239.5 KiB.                                    
    Loop 9 done.
    Copying file:///home/jupyter/content/PDF/TXT/2015/2015/CSV/3880052015.csv [Content-Type=text/csv]...
    
    ....
    Copying file:///home/jupyter/content/PDF/TXT/2015/2015/CSV/3363632015.csv [Content-Type=text/csv]...
    / [1 files][427.3 KiB/427.3 KiB]                                                
    Operation completed over 1 objects/427.3 KiB.                                    
    Loop 973 done.
    Copying file:///home/jupyter/content/PDF/TXT/2015/2015/CSV/3393132015.csv [Content-Type=text/csv]...
    / [1 files][124.7 KiB/124.7 KiB]                                                
    Operation completed over 1 objects/124.7 KiB.                                    
    Loop 974 done.
    Copying file:///home/jupyter/content/PDF/TXT/2015/2015/CSV/3372492015.csv [Content-Type=text/csv]...
    / [1 files][188.6 KiB/188.6 KiB]                                                
    Operation completed over 1 objects/188.6 KiB.                                    
    Loop 975 done.
    Copying file:///home/jupyter/content/PDF/TXT/2015/2015/CSV/3247582015.csv [Content-Type=text/csv]...
    / [1 files][112.6 KiB/112.6 KiB]                                                
    Operation completed over 1 objects/112.6 KiB.                                    
    Loop 976 done.
    All files stripped successfully.



```python
!mkdir /home/jupyter/content/PDF/TXT/2014/
```


```python
!gsutil -m cp -r gs://rsm-thesis-bucket/TXT_prod/TXT/2014/* /home/jupyter/content/PDF/TXT/2014/

```

    Copying gs://rsm-thesis-bucket/TXT_prod/TXT/2014/AFM_279657_thesisresit_id977.txt...
    Copying gs://rsm-thesis-bucket/TXT_prod/TXT/2014/AFM_294575_thesis_id1030.txt...
    ....
    Copying gs://rsm-thesis-bucket/TXT_prod/TXT/2014/SM_404289_thesis_id624.txt...
    Copying gs://rsm-thesis-bucket/TXT_prod/TXT/2014/SM_404263_thesisresit_id606.txt...
    Copying gs://rsm-thesis-bucket/TXT_prod/TXT/2014/SM_404326_thesis_id573.txt...
    | [1.5k/1.5k files][204.9 MiB/204.9 MiB] 100% Done                              
    Operation completed over 1.5k objects/204.9 MiB.                                 



```python
%cd /home/jupyter/content/PDF/TXT/2014/CSV
!ls -la
```

    /home/jupyter/content/PDF/TXT/2014/CSV
    total 696
    drwxr-xr-x 2 jupyter jupyter   4096 Jun 10 15:49 .
    drwxr-xr-x 3 jupyter jupyter  90112 Jun 10 15:48 ..
    -rw-r--r-- 1 jupyter jupyter 228795 Jun 10 15:49 3103362014.csv
    -rw-r--r-- 1 jupyter jupyter  61242 Jun 10 15:48 3454122014.csv
    -rw-r--r-- 1 jupyter jupyter 201161 Jun 10 15:49 3632342014.csv
    -rw-r--r-- 1 jupyter jupyter  24614 Jun 10 15:48 4009612014.csv
    -rw-r--r-- 1 jupyter jupyter  91276 Jun 10 15:49 4029602014.csv
    ....



```python
!mkdir /home/jupyter/content/PDF/TXT/2014/CSV/
```


```python
################################################################################
# WORKING FILE-LEVEL CLOUD VERSION ###########################################
################################################################################

# Much of this chunk has been adapted from Levente Csibi's (2019) work.

# Count index for looping files
cnt = 1

# File directories
DIR_INPUT_TXTs = "/home/jupyter/content/PDF/TXT/2014/" #Input
OUTPUTDIR = "/home/jupyter/content/PDF/TXT/2014/CSV/"  #Output

# Loop filenames
for filename in os.listdir(DIR_INPUT_TXTs):
  
  # Empty dataframe to loop
  df = pd.DataFrame(columns=('Program', "Student", "Resit", "FileID", "Year"))

  # Only take .txt files
  if filename.endswith(".txt"): 
    
    # Read txt files
    fulltext = open(str(Path(DIR_INPUT_TXTs)) + "/" + str(Path(filename)), "r").read()
    
    # Obtaining info from filenames
    # First param is offset from the file name end. Clips file extension. 
    # The second is offset from substring index.
    # The last is the index of the element.
    program  = filename[:-4].split('_')[0:][0]
    student  = filename[:-4].split('_')[0:][1]
    resit    = filename[:-4].split('_')[0:][2]
    fileID   = filename[:-4].split('_')[0:][3]
    year     = 2014                                #used for filename
    savename = (str(student) + str(year) + '.csv') #used for filename

    # Preprocessing doc contents ===============================================
    
    #Load the fulltext into a list, split it at the sentence breaks
    fulltext_splitted = list(fulltext.split('. '))
    
    #Take only lines with over 35 chrs
    fulltext_splitted = [line for line in fulltext_splitted if len(line) >= 35]
    
    #Drop all non-alphabetic characters using RegEx
    fulltext_splitted = [re.sub('[^a-zA-Z ]+', ' ', line) for line in fulltext_splitted]
    
    #Drop lines without actual text (less than 35 characters after stripping)
    fulltext_splitted = [line.strip() for line in fulltext_splitted if len(line.strip()) >= 35]
    
    #Lowercase everything
    fulltext_splitted = [line.lower() for line in fulltext_splitted]
      
    # Writing and saving results to the gs bucket ==============================

    # Writing into dataframe
    # Loop the entire fulltext dictionary
    for each_line in range(len(fulltext_splitted)):
      newrow = {'Program': program, 'Student': student, 'Resit': resit, 'FileID': fileID, 'Year': year, 'Fulltext': fulltext_splitted[each_line]}
      df = df.append(newrow, ignore_index = True)

    # Save the df as csv  
    df.to_csv(Path(OUTPUTDIR)/savename, index = False)
    
    # Upload to gs bucket
#     !gsutil cp /home/jupyter/content/PDF/TXT/$year/CSV/$savename gs://rsm-thesis-bucket/CSV_prod/$year/ 
    
    # CLEANUP ==================================================================
    
    # Clear bash variable from python so it can be reset in the next loop  
    !unset savename
    !unset year
    # Delete df so it can be recreated for the next loop
    del df
    
    # Finish loop, increment count.
    print("Loop " + str(cnt) + " done.")
    cnt = cnt+1
    continue
        
#   else:
#     continue
    
########################################################################################

#Save to csv
# df.to_csv(Path(OUTPUTDIR)/'2017_new_sentences.csv', index = False)
print("All files stripped successfully.")
!gsutil cp /home/jupyter/content/PDF/TXT/2014/CSV/* gs://rsm-thesis-bucket/CSV_prod/2014/ 

```

    Loop 1 done.
    Loop 2 done.
    Loop 3 done.
    ...
    Loop 1458 done.
    Loop 1459 done.
    Loop 1460 done.
    Loop 1461 done.
    Loop 1462 done.
    All files stripped successfully.
    Copying file:///home/jupyter/content/PDF/TXT/2014/CSV/2600412014.csv [Content-Type=text/csv]...
    Copying file:///home/jupyter/content/PDF/TXT/2014/CSV/2631962014.csv [Content-Type=text/csv]...
    Copying file:///home/jupyter/content/PDF/TXT/2014/CSV/2664032014.csv [Content-Type=text/csv]...
    Copying file:///home/jupyter/content/PDF/TXT/2014/CSV/2693962014.csv [Content-Type=text/csv]...
    - [4 files][576.0 KiB/576.0 KiB]                                                
    ==> NOTE: You are performing a sequence of gsutil operations that may
    run significantly faster if you instead use gsutil -m cp ... Please
    see the -m section under "gsutil help options" for further information
    about when gsutil -m can be advantageous.
    
    Copying file:///home/jupyter/content/PDF/TXT/2014/CSV/2733352014.csv [Content-Type=text/csv]...
    Copying file:///home/jupyter/content/PDF/TXT/2014/CSV/2739552014.csv [Content-Type=text/csv]...
    ...
    Copying file:///home/jupyter/content/PDF/TXT/2014/CSV/4042892014.csv [Content-Type=text/csv]...
    Copying file:///home/jupyter/content/PDF/TXT/2014/CSV/4043042014.csv [Content-Type=text/csv]...
    Copying file:///home/jupyter/content/PDF/TXT/2014/CSV/4043102014.csv [Content-Type=text/csv]...
    Copying file:///home/jupyter/content/PDF/TXT/2014/CSV/4043262014.csv [Content-Type=text/csv]...
    Copying file:///home/jupyter/content/PDF/TXT/2014/CSV/4044072014.csv [Content-Type=text/csv]...
    \ [1.4k files][204.3 MiB/204.3 MiB]  741.4 KiB/s                                
    ==> NOTE: You are performing a sequence of gsutil operations that may
    run significantly faster if you instead use gsutil -m cp ... Please
    see the -m section under "gsutil help options" for further information
    about when gsutil -m can be advantageous.
    
    
    Operation completed over 1.4k objects/204.3 MiB.                                 



```python
!mkdir /home/jupyter/content/PDF/TXT/2014/CSV/ALL/ 

```


```python
# Consolidate a year's theses into a single csv file

input_dir = "/home/jupyter/content/PDF/TXT/2014/CSV/" #Input
output_dir = "/home/jupyter/content/PDF/TXT/2014/CSV/ALL/"  #Output

consolidation_df = pd.DataFrame(columns = ('Program', 'Student', 'Resit', 'FileID', 'Year', 'Fulltext')) 

cnt = 1

for filename in os.listdir(input_dir):
    if filename.endswith(".csv"):
        read_df = pd.read_csv(input_dir + "/" + str(Path(filename)))
        consolidation_df = consolidation_df.append(read_df, ignore_index = True)
        
        print("Loop " + str(cnt) + " done.")
        cnt = cnt +1
#         consolidation_df.iloc[0:2]
        
consolidation_df.to_csv((str(Path(output_dir)) + "/2014.csv"), index = False)
print("CSV done")


```

    Loop 1 done.
    Loop 2 done.
    Loop 3 done.
    Loop 4 done.
    Loop 5 done.
    Loop 6 done.
    ...
    Loop 1423 done.
    Loop 1424 done.
    Loop 1425 done.
    Loop 1426 done.
    Loop 1427 done.
    Loop 1428 done.
    Loop 1429 done.
    Loop 1430 done.
    Loop 1431 done.
    Loop 1432 done.
    Loop 1433 done.
    CSV done



```python
%cd /home/jupyter/content/PDF/TXT/2014/CSV/ALL/
!ls -la

```

    /home/jupyter/content/PDF/TXT/2014/CSV/ALL
    total 209160
    drwxr-xr-x 2 jupyter jupyter      4096 Jun 10 17:55 .
    drwxr-xr-x 3 jupyter jupyter     61440 Jun 10 17:38 ..
    -rw-r--r-- 1 jupyter jupyter 214108807 Jun 10 17:55 2014.csv



```python
!gsutil cp /home/jupyter/content/PDF/TXT/2014/CSV/ALL/2014.csv gs://rsm-thesis-bucket/CSV_prod/2014/2014.csv 

```

    Copying file:///home/jupyter/content/PDF/TXT/2014/CSV/ALL/2014.csv [Content-Type=text/csv]...
    ==> NOTE: You are uploading one or more large file(s), which would run
    significantly faster if you enable parallel composite uploads. This
    feature can be enabled by editing the
    "parallel_composite_upload_threshold" value in your .boto
    configuration file. However, note that if you do this large files will
    be uploaded as `composite objects
    <https://cloud.google.com/storage/docs/composite-objects>`_,which
    means that any user who downloads such objects will need to have a
    compiled crcmod installed (see "gsutil help crcmod"). This is because
    without a compiled crcmod, computing checksums on composite objects is
    so slow that gsutil disables downloads of composite objects.
    
    |
    Operation completed over 1 objects/204.2 MiB.                                    



```python

```


```python
!mkdir /home/jupyter/content/PDF/TXT/2015/2015/CSV/ALL/
```


```python

input_dir = "/home/jupyter/content/PDF/TXT/2015/2015/CSV/" #Input
output_dir = "/home/jupyter/content/PDF/TXT/2015/2015/CSV/ALL/"  #Output

consolidation_df = pd.DataFrame(columns = ('Program', 'Student', 'Resit', 'FileID', 'Year', 'Fulltext')) 

cnt = 1

for filename in os.listdir(input_dir):
    if filename.endswith(".csv"):
        read_df = pd.read_csv(input_dir + "/" + str(Path(filename)))
        consolidation_df = consolidation_df.append(read_df, ignore_index = True)
        
        print("Loop " + str(cnt) + " done.")
        cnt = cnt +1
#         consolidation_df.iloc[0:2]
        
consolidation_df.to_csv((str(Path(output_dir)) + "/2015.csv"), index = False)
print("CSV done")

```

    Loop 1 done.
    Loop 2 done.
    Loop 3 done.
    Loop 4 done.
    Loop 5 done.
    Loop 6 done.
    Loop 7 done.
    Loop 8 done.
    Loop 9 done.
    ...
    Loop 960 done.
    Loop 961 done.
    Loop 962 done.
    Loop 963 done.
    Loop 964 done.
    Loop 965 done.
    Loop 966 done.
    CSV done



```python
!gsutil cp /home/jupyter/content/PDF/TXT/2015/2015/CSV/ALL/2015.csv gs://rsm-thesis-bucket/CSV_prod/2015/2015.csv 

```

    Copying file:///home/jupyter/content/PDF/TXT/2015/2015/CSV/ALL/2015.csv [Content-Type=text/csv]...
    \
    Operation completed over 1 objects/141.6 MiB.                                    



```python
!mkdir /home/jupyter/content/PDF/TXT/2016/
!gsutil -m cp gs://rsm-thesis-bucket/CSV_prod/2016/*.csv /home/jupyter/content/PDF/TXT/2016/ 
!mkdir /home/jupyter/content/PDF/TXT/2016/ALL/

```

    Copying gs://rsm-thesis-bucket/CSV_prod/2016/1717242016.csv...
    Copying gs://rsm-thesis-bucket/CSV_prod/2016/2988862016.csv...
    Copying gs://rsm-thesis-bucket/CSV_prod/2016/3203562016.csv...
    ...
    Copying gs://rsm-thesis-bucket/CSV_prod/2016/4076252016.csv...
    | [809/809 files][114.2 MiB/114.2 MiB] 100% Done                                
    Operation completed over 809 objects/114.2 MiB.                                  



```python
# Consolidate a year's theses into a single csv file

input_dir = "/home/jupyter/content/PDF/TXT/2016/" #Input
output_dir = "/home/jupyter/content/PDF/TXT/2016/ALL/"  #Output

consolidation_df = pd.DataFrame(columns = ('Program', 'Student', 'Resit', 'FileID', 'Year', 'Fulltext')) 

cnt = 1

for filename in os.listdir(input_dir):
    if filename.endswith(".csv"):
        read_df = pd.read_csv(input_dir + "/" + str(Path(filename)))
        consolidation_df = consolidation_df.append(read_df, ignore_index = True)
        
        print("Loop " + str(cnt) + " done.")
        cnt = cnt +1
#         consolidation_df.iloc[0:2]
        
consolidation_df.to_csv((str(Path(output_dir)) + "/2016.csv"), index = False)
print("CSV done")
```

    Loop 1 done.
    Loop 2 done.
    Loop 3 done.
    ...
    Loop 803 done.
    Loop 804 done.
    Loop 805 done.
    Loop 806 done.
    Loop 807 done.
    Loop 808 done.
    Loop 809 done.
    CSV done



```python
!gsutil cp /home/jupyter/content/PDF/TXT/2016/ALL/2016.csv gs://rsm-thesis-bucket/CSV_prod/2016/2016.csv 

```

    Copying file:///home/jupyter/content/PDF/TXT/2016/ALL/2016.csv [Content-Type=text/csv]...
    \
    Operation completed over 1 objects/114.2 MiB.                                    



```python
!mkdir /home/jupyter/content/PDF/TXT/2017/
!mkdir /home/jupyter/content/PDF/TXT/2017/ALL/
!gsutil -m cp gs://rsm-thesis-bucket/CSV_prod/2017/*.csv /home/jupyter/content/PDF/TXT/2017/ 
```

    Copying gs://rsm-thesis-bucket/CSV_prod/2017/3056832017.csv...
    Copying gs://rsm-thesis-bucket/CSV_prod/2017/3059462017.csv...
    Copying gs://rsm-thesis-bucket/CSV_prod/2017/3061902017.csv...
    Copying gs://rsm-thesis-bucket/CSV_prod/2017/3130812017.csv...
    Copying gs://rsm-thesis-bucket/CSV_prod/2017/3222262017.csv...
    ...
    Copying gs://rsm-thesis-bucket/CSV_prod/2017/4037112017.csv...
    Copying gs://rsm-thesis-bucket/CSV_prod/2017/4040472017.csv...
    Copying gs://rsm-thesis-bucket/CSV_prod/2017/4042762017.csv...
    Copying gs://rsm-thesis-bucket/CSV_prod/2017/4059082017.csv...
    Copying gs://rsm-thesis-bucket/CSV_prod/2017/4047042017.csv...
    Copying gs://rsm-thesis-bucket/CSV_prod/2017/4066762017.csv...
    / [857/857 files][122.6 MiB/122.6 MiB] 100% Done                                
    Operation completed over 857 objects/122.6 MiB.                                  



```python
# Consolidate a year's theses into a single csv file

input_dir = "/home/jupyter/content/PDF/TXT/2017/" #Input
output_dir = "/home/jupyter/content/PDF/TXT/2017/ALL/"  #Output

consolidation_df = pd.DataFrame(columns = ('Program', 'Student', 'Resit', 'FileID', 'Year', 'Fulltext')) 

cnt = 1

for filename in os.listdir(input_dir):
    if filename.endswith(".csv"):
        read_df = pd.read_csv(input_dir + "/" + str(Path(filename)))
        consolidation_df = consolidation_df.append(read_df, ignore_index = True)
        
        print("Loop " + str(cnt) + " done.")
        cnt = cnt +1
#         consolidation_df.iloc[0:2]
        
consolidation_df.to_csv((str(Path(output_dir)) + "/2017.csv"), index = False)
print("CSV done")

!gsutil cp /home/jupyter/content/PDF/TXT/2017/ALL/2017.csv gs://rsm-thesis-bucket/CSV_prod/2017/2017.csv 

```

    Loop 1 done.
    Loop 2 done.
    Loop 3 done.
    Loop 4 done.
    Loop 5 done.
    ...
    Loop 852 done.
    Loop 853 done.
    Loop 854 done.
    Loop 855 done.
    Loop 856 done.
    Loop 857 done.
    CSV done
    Copying file:///home/jupyter/content/PDF/TXT/2017/ALL/2017.csv [Content-Type=text/csv]...
    \
    Operation completed over 1 objects/122.6 MiB.                                    



```python
# As the next step the files are downloaded from the gs bucket to the local machine
```

