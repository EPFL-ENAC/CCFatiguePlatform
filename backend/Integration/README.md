# Homework

## Dependencies

xlrd : manipulating excel files </br>
matplotlib : plotting the results

## Installing

Do once (the pip file from /backend has been modified)

```bash
../backend/pipenv install
```

## Executing program
### Main program
Launch the main.py with as argument the database connection
string and replace the fields with your own connection parameters ([More info](https://docs.sqlalchemy.org/en/14/core/engines.html)):

* dialect+driver://username:password@host:port/database

Example:
```bash
python main.py postgresql://username:password@localhost:5432/ENAC_Exo
```

Time for the whole execution: 10min.

Execution actions:</br>
* Connection to the database
* Creation of tables from ORM classes specified in [model.py](/Integration/model/init_db.py)
* Extraction of data with Pandas ([read_data.py](/Integration/read_data.py))
* Deletion of the entries having a missing mandatory data field ([data_quality.py](/Integration/data_quality.py))
* Query data corresponding to the homework question
* Plot the statistic and save it into png file in [/figures](/Integration/figures/)

### Unit test

Launch unittests:

```bash
python test_data_quality.py
```
## Result

This image should be built in [/figures](/Integration/figures/)
<img src="Integration/StressRatio_MaxMachload_distribution.png" style="width: 650px; max-width: 100%; height: auto" title="Click to enlarge picture" />

## Remarks

* As initial_crack_length is a mandatory field and is missing on
all given data, I purposely make it 0 so that the exercise can go on.
(Otherwise all data is removed by hierarchy == not inserted in the DB)
* By design (the exercise specify to use SQLAlchemy) the table structure
is defined using ORM classes. So columns are hard-coded into /model/
* Using other DB design like NoSQL would allow changing dynamically columns
* Units are specified in the table class definition. Inserting a non-correct
unit in the DB would throw an error

## Future work

* Optimize Pandas file reading time with different strategies:
  * Loading by chunk
  * Specify precise data type (int8 if possible, etc..) 
* Implement a date format conversion in data_quality.py
* Separate table definition (ORM classes in init_db.py) into different files
* Column names are checked in read_data.py (line 75,101,154) in order
to populate the ORM class. A modularity to the name can be implemented here.
* In the table ORM class, define the column "Run-out" with the "-" (not possible so far)
