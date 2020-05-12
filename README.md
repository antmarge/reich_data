# Text parsing & plotting to check Reich Lab aDNA annotation data

Empirical observation that dates sometimes get flipped and geographical coordinates are not within bounds of reported country. Attempt at a systematic check for data inconsistencies.

1. Check dates. There is a "Average Date BP" (single integer value) and a free text date column "Date...". Parsed the free text date column to get upper and lower date bounds. Then checked if the reported "Average Date BP" was consistent with that date range. 
- RESULT: 11 individuals have an inconsistent date
- REASON: commonly because the Date is reported as years from 0 CE instead of years from 1950 (BP definition)

2. Check coordinates. Best can do is to check that the reported country is consistent with reported coordinates. Very difficult to do since countries are complicated polygons. For now, decided to just do this visually. Draw a polygon around individuals of the same reported country. Then plot those polygons onto a map of the world. Irregular polygons, where one vertex is in another country, should be obvious upon visual inspection
- RESULT: ~3 individuals with misspecified coordinates
- REASON: the longitude coordinate was not scaled to be with respect to 0, the Prime Meridian (e.g. 10W should be -10). With respect to Eurasian coordinates, this happens in Spain and Portugal. 
- Load plotly html document in browser to mouse over coordinates
