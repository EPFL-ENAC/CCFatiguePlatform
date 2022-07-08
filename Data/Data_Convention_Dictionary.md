AGG, SNC, CLD, FAF, DAS
      
| Variable name        | Description                            | Symbol    | Unit    | Data type | Used in            |
|----------------------|----------------------------------------|-----------|---------|-----------|--------------------|
| stress_ratio         | Stress ratio                           | R         | [-]     | double    | AGG, SNC, FAF      |
| reliability_level    | Reliability level                      | P(N)      | %       | double    | AGG                |
| stress_level         | Index of the cluster of stress level   |           | [-]     | int       | AGG                |
| stress_parameter     | Cyclic max stress                      | sigma_max | [MPa]   | double    | AGG, FAF           |
| stress_lowerbound    | Stress at failure (lower bound)        | sigma_max | [MPa]   | double    | SNC                |
| stress_upperbound    | Stress at failure (upper bound)        | sigma_max | [MPa]   | double    | SNC                |
| number_of_cycles     | Number of cycles at failure            | N         | [-]     | double    | AGG                |
| residual_strength    | Residual strength                      | sigma_r   | [MPa]   | double    | AGG                |
| a                    | S-N Curve parameter                    | A         | [-]     | double    | SNC                |
| b                    | S-N Curve parameter                    | B         | [1/MPa] | double    | SNC                |
| lrsq                 | ?                                      | LRSQ      | [?]     | double    | SNC                |
| fp                   | Linearity criterion                    | Fp        | [?]     | double    | SNC                |
| stress_amplitude     | Stress amplitude                       | ?         | [MPa]   | double    | CLD                |
| mean_stress          | Mean stress                            | ?         | [MPa]   | double    | CLD                |
| stress_range         | sigma_max - sigma_min                  | ?         | double  | y         | CYC                |
| stress_mean          | ?                                      | sigma_m   | double  | y         | CYC                |
| r_ratio?             | ?                                      | ?         | double  | y         | CYC                |
| n_cycles?            | ?                                      | ?         | double  | y         | CYC                |
| cum_n_cycles?        | percentage?                            | ?         | double  | y         | CYC                |
|----------------------|----------------------------------------|-----------|---------|-----------|--------------------|
| rsql                 | Reliability level                      | ?         | ?       | int       | SNC                |
| cycles_to_failure    | Number of cycles to failure            | N         | [-]     | int       | SNC, CLD, FAF, DAS |
| stress               | Stress at failure (median)             | sigma_max | [MPa]   | double    | SNC                |
| max_stress           | Max stress                             | sigma_max | [MPa]   | double    | DAS                |


