# CCFatigue Modules External Data Sources

## CSV Data Files description


| File | Description | Source | Used in module |
|------|-------------|--------|----------------|
| astm.csv | ? | STP313_Fittin straight lines.pdf (page 6) | S-N-Curve-LogLog.py |
| astm95.csv | ? ... P = 95% | STP313_Fittin straight lines.pdf (page 20-23) | S-N-Curve-LogLog.py |
| astm99.csv | ? ... P = 99% | STP313_Fittin straight lines.pdf (page 20-23) | S-N-Curve-LogLog.py |


## ASTM astm.csv file standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)

| Column name          | Description                                        | Unit  | Data type | Mandatory  |
|----------------------|----------------------------------------------------|-------|-----------|------------|
| n | ? (inf = infinity) | [-] | numbers | y |
| 50 | Values for P = 50% ? | ? | double | y |
| ... | | | | |
| 99.5 | Values for P = 99.5% ? | ? | double | y |

## ASTM astm95.csv & astm99.csv files standards (column names must be exact) :

Encoding format: UTF-8, Separator: ',' (comma)

| Column name          | Description                                        | Unit  | Data type | Mandatory  |
|----------------------|----------------------------------------------------|-------|-----------|------------|
| n | ? (inf = infinity) | [-] | numbers | y |
| 1 | ? | ? | double | y |
| ... | | | | |
| 120 | ? | ? | double | y |
| inf | ? | ? | double | y |

