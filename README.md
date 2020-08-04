# knb-lter-cap.617

Regional drinking water quality monitoring program: [data](https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-cap&identifier=617) publishing workflow

### data access

Source data reside in an Access database. Use mdbtools to extract tables as csv files:

- sudo apt install mdbtools
- export all tables: `mdb-tables -d ',' ASU\ Regional\ Water\ Quality\ database.mdb | xargs -L1 -d ',' -I{} bash -c 'mdb-export ASU\ Regional\ Water\ Quality\ database.mdb' "$1" > "$1".csv' -- {}`
