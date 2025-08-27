DATA

sparrow_data.csv - raw data used in the 'data manipulation' section to create the files used for analysis

- bird ID
- brood reference
- sex estimate (0 - female, 1 - male)
- rearing brood
- cohort (year of birth)
- death status (0 - no death record, 1 - dead in nest box, 2 - seen dead, 3 - not seen for a while)
- last live record
- last stage (last stage seen before death: 0 - egg, 1 - half-hatched, 2 - chick, 3 - post-fledgling)
- lifespan
- genetic pedigree (social mother ID, social father ID, genetic mother ID, genetic father ID)
- offspring of EP (if the bird is a result of EPR: 0 - no, 1 - yes)

sparrow_delifing.csv - file produced from MakeDelifingTable.rb to be able to extract recruits

sparrow_female.csv - used in all models for analysis 

  - social mother ID 
  - offspring (lifetime total)
  - EP offspring (liftime extra-pair offspring)
  - recruits (lifetime total)
  - cohort (year of birth of mother)
  - dead (0 - alive, 1 - dead)
  - lifespan
  - mating strategy (0 - monogamous, 1 - polygamous)

sparrow_mate.csv - used to calculate transitions between ages 

  - social mother ID
  - cohort M (year of birth of mother)
  - cohort O (year of birth of offspring)
  - Age
  - mating strategy (0 - monogamous, 1 - polygamous)

CODE

MakeDelifingTable.rb - code used to calculate recruits 

sparrow_code.csv - code used in the 'statisical analysis' section
