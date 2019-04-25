
```r
json_data <- jsonlite::fromJSON(json_file)
```

- `json_data$geographies` > A data.frame with 1 rows and 7 columns
  - `id` chr (obsolete)
  - `name` chr (obsolete)
  - `type` chr (obsolete)
  - `feature` List of 1
  - `comparisonFeature` List of 1
  - `filters` List of 1 (obsolete)
  - `themes` List of 1

- `data.frame(json_data$geographies$features)`
  - `id`
  - `name`
  - `href`
- `data.frame(json_data$geographies$comparisonFeature)`
  - `id`
  - `name`
- `data.frame(json_data$geographies$themes)`
  - `id`
  - `name`
  - `indicators` List of N where N is the num of level 1 (N = 3 for barn, N = 5 for kols)


    - date
    - href
    - type
    - values
    - comparisonValues
    - associates
  - 
- `json_data$geographies$features` > list of one, boomr data
- 


