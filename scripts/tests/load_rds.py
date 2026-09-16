# Source - https://stackoverflow.com/a/53956614
# Posted by Otto Fajardo
# Retrieved 2026-09-16, License - CC BY-SA 4.0

import pyreadr

result = pyreadr.read_r("/Volumes/T7 Shield/FRES/fuels_data/tests/chunk_0_2.RDS") # also works for RData

# done! 
# result is a dictionary where keys are the name of objects and the values python
# objects. In the case of Rds there is only one object with None as key
df = result[None] # extract the pandas data frame 

print(df.head())
