# knb-lter-cap.617

Regional drinking water quality monitoring program: [data](https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-cap&identifier=617) publishing workflow

## project overview

Arizona Statue University (ASU) has been working with regional water providers (Salt River Project (SRP), Central Arizona Project (CAP)) and metropolitan Phoenix cities since 1998 on algae-related issues affecting drinking water supplies, treatment, and distribution. The results have improved the understanding of taste and odor (T&O) occurrence, control, and treatment, improved the understanding of dissolved organic and algae dynamics, and initiated a forum to discuss and address regional water quality issues. The monitoring benefits local Water Treatment Plants (WTPs) by optimizing ongoing operations (i.e., reducing operating costs), improving the quality of municipal water for consumers, facilitating long-term water quality planning, and providing information on potentially future-regulated compounds. ASU has been monitoring water quality in terminal reservoirs (Lake Pleasant, Saguaro Lake, and Bartlett Lake) continuously from 1998 to the present for algae-related constituents (taste and odors, and more recently cyanotoxins), nutrients, and disinfection by-product precursors (i.e., total and dissolved organic carbon and organic nitrogen). Additional monitoring has been conducted in the SRP and CAP canal systems and in water treatment plants in Phoenix, Tempe and Peoria. During this work the Valley has been in a prolonged drought and recently one above average wet year, and this data provides important baseline data for development of new or expanded WTPs and management of existing WTPs in the future. The current work has improved the understanding of T&O sources and treatment, but additional research and monitoring into the future is necessary.

Reservoir monitoring is conducted once per month at Bartlett Lake, Saguaro Lake, and Lake Pleasant, and quarterly at Roosevelt, Apache, and Canyon Lakes. Samples are depth integrated in the epilimnion and hypolimnion. CAP will collect samples from Lake Pleasant. SRP will collect samples from Bartlett, Saguaro, Roosevelt, Apache, and Canyon Lakes (at no cost to ASU or cities). Field measurements for temperature with depth will also be collected. River samples (Salt River below Saguaro Lake @ Blue Point Bridge and Verde River at the Beeline Highway) will be collected once per month. Samples will be analyzed for carbon (TOC/DOC), total nitrogen, total phosphorous, arsenic, conductance and T&O compounds (MIB, Geosmin, Cyclocitrol). The purpose of the lake sampling is to provide early warning information on potentially large changes in water quality – due to algae production, lake destratification, and forest fire or other runoff events. Additional monthly sampling will be coordinated with USGS (Salt River above Roosevelt, Verde River at Tangle) and CAP (Lake Havasu).

Canal monitoring is conducted once per month (January through June) and twice per month as needed during periods of higher T&O production (i.e., July-December). Field measurements for temperature and pH will be made. Sampling will include the CAP, Arizona, and South canals at multiple locations. Monthly samples will be analyzed for carbon (TOC/DOC), total nitrogen, arsenic, conductance, and T&O compounds (MIB, Geosmin, Cyclocitrol). Bi-weekly samples will be analyzed only for T&O compounds. The purpose of the canal sampling is to identify hot-spots of T&O production and to make recommendations to the cities/SRP/CAP to perform some type of treatment (brushing, copper, etc.). Additional canal sampling will be scheduled to further identify canal hot spots or to provide more frequent process control information.

WTP raw and finished water is collected once per month (January through June) and twice per month as needed during periods of higher T&O production (e.g., July-December). WTP sampling will be conducted at two Tempe WTPs, one Peoria WTP, Glendale WTPs and other selected WTPs. Monthly samples will be analyzed for carbon (TOC/DOC), total nitrogen, arsenic, conductance, and T&O compounds (MIB, Geosmin, Cyclocitrol). Bi-weekly samples will be analyzed only for T&O compounds. The purpose of the WTP sampling is to provide continued evaluation of water quality produced at the WTPs.

## data processing notes

Source data reside in an Access database. Use [mdbtools](https://askubuntu.com/questions/342925/opening-an-accdb-file-in-ubuntu) to extract tables as csv files:

- `sudo apt install mdbtools`
- export all tables: `mdb-tables -d ',' ASU\ Regional\ Water\ Quality\ database.mdb | xargs -L1 -d ',' -I{} bash -c 'mdb-export ASU\ Regional\ Water\ Quality\ database.mdb "$1" > "$1".csv' -- {}`
- note that the above command does result in the error `Error: Table does not exist in database` but all relevant tables are exported.

## versioning

### knb-lter-cap.617.6 *2023-01-17*

- data refresh
- expanded .gitignore to ignore files within data directory

### knb-lter-cap.617.5 *2022-11-29*

- data refresh
- update attributes metadata files to yaml format
- update config.yaml to reflect updates to capeml
- pared repository of old data (and adding data directory to ignore)

### knb-lter-cap.617.4 *2022-02-30*

- data refresh
- fix units of total phosphorus and total dissolved phosphorus that were listed as nanograms per liter but in fact are micrograms per liter
