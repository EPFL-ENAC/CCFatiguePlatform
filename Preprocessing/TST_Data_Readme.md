# TST Standard Data format

For each test, the user should provide 2 files, named
**TST _ {Date} _ {Test type} _ {###}.{csv/json}** with:

* date: YYYY-MM-DD
* Test type: {FA} = standard fatigue, {QS} = standard quasi-static, {FF} = combined fatigue/fracture, {SF} = combined quasi-static/fracture

### (1) The CSV file should contain the following variables (and exact column names) :


| Column name          | Description                                        | Unit  | Data type |   |
|----------------------|----------------------------------------------------|-------|-----------|---|
| Machine\_N_cycles     | Number of cycles counted by the machine            | [-]   | int       |   |
| Machine_Load         | Load measured by the machine                       | [MPa] | double    |   |
| Machine_Displacement | Displacement measured by the machine               | [-]   | double    |   |
| DIC_index            | Image number                                       | [-]   | int       |   |
| DIC\_N_cycles         | Number of cycles counted by the camera             | [-]   | int       |   |
| DIC_exx              | Displacement measured along main axis              | [mm]  | double    |   |
| DIC_eyy              | Displacement measured along secondary axis         | [mm]  | double    |   |
| DIC_exy              | Displacement measured along a specified axis       | [mm]  | double    |   |
| DIC_crack_length     | Crack length measurement (for fracture testings)   | [mm]  | double    |   |
| Th\_N_cycles          | Number of cycles counted by temperature monitoring | [-]   | int       |   |
| Th_time              | Time as counted by temperature monitoring          | [sec] | int       |   |
| Th\_specimen_max      | Maximum temperature monitored                      | [°C]  | single    |   |
| Th\_specimen_mean     | Mean temperature                                   | [°C]  | single    |   |
| Th_chamber           | Temperature of the test environment                | [°C]  | single    |   |
| Th_uppergrips        | Temperature of the upper grips                     | [°C]  | single    |   |
| Th_lowergrips        | Temperature of the lower grips                     | [°C]  | single    |   |



### (2) The JSON file contains the following metadata:
```
{
  "Experience": { 
    "Laboratory": "",
    "Researcher": "",
    "Experiment Type": "CA",
    "Measurement": [
      {
        "Measuring Equipment": "",
        "Reliability Level": 0,
        "Control Mode": "",
      }
    ],
    "Publications": [
      {
        "Title": "",
        "DOI": ""
      }
    ],
    "Experiment": {
      "Date": "YYYY-MM-DD",
      "Specimen number": "000",
      "Material Type": {
        "Resin": "Biresin CR83",
        "Hardener": "Sika CH83-2",
        "Mixing ratio": "3:1"
      },
      "Test Conditions": {
        "Temperature": 0,
        "Relative Humidity": "",
        "Grip Pressure": ""
      },
      "Geometry": {
        "Length": 0,
        "Width": 0,
        "Thickness": 0
      },
      "Laminates and Assemblies": {
        "Curing Time": 0,
        "Curing Temperature": 0,
        "Curing Pressure": 0,
        "Fiber Content": 0,
        "Stacking Sequence": "(±45)_2s"
      },
      "Constituent Materials": {
        "Fiber Material": "",
        "Fiber Geometry": "",
        "Area Density": 0
      },
      "Loading information": {
        "Fatigue Test Type": "Constant Amplitude",
        "Stress Ratio": 0.1,
        "Number of Cycles to Failure": 1198627,
        "Stress at Failure": 47.4,
        "Strain at Failure": 1.17
      },
      "Fracture information": {
        "Fracture analysis": 0,
        "Fracture mode": "",
        "Initial crack length": 0
      }
    },
    "Experiment Units": {
      "Stress": "MPa",
      "Temperature": "°C",
      "Dimension": "mm",
      "Pressure": "Pa",
      "Area Density": "g/m^2",
      "Adimensional": "[-]",
      "Time": "[hr]"
    }
  }
  ```