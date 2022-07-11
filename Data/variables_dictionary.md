      
| Variable name        | Description                            | Symbol    | Unit    | Data type | Used in            |
|----------------------|----------------------------------------|-----------|---------|-----------|--------------------|
| stress_ratio         | Stress ratio                           | R         | [-]     | double    | AGG, SNC, FAF, CYC |
| reliability_level    | Reliability level                      | rsql      | %       | double    | AGG, SNC           |
| stress_level         | Index of the cluster of stress level   |           | [-]     | int       | AGG                |
| stress_parameter     | Cyclic max stress                      | sigma_max | [MPa]   | double    | AGG, FAF           |
| stress_lowerbound    | Stress at failure (lower bound)        | sigma_max | [MPa]   | double    | SNC                |
| stress_upperbound    | Stress at failure (upper bound)        | sigma_max | [MPa]   | double    | SNC                |
| stress               | Stress at failure (median)             | sigma_max | [MPa]   | double    | SNC                |
| max_stress           | Max stress                             | sigma_max | [MPa]   | double    | DAS                |
| cycles_to_failure    | Number of cycles to failure            | N         | [-]     | int       | AGG, SNC, CLD, FAF, DAS |
| n_cycles             | Number of cycles                       | ?         |         | double    | CYC                |
| residual_strength    | Residual strength                      | sigma_r   | [MPa]   | double    | AGG                |
| a                    | S-N Curve parameter                    | A         | [-]     | double    | SNC                |
| b                    | S-N Curve parameter                    | B         | [1/MPa] | double    | SNC                |
| lrsq                 | ?                                      | LRSQ      | [?]     | double    | SNC                |
| fp                   | Linearity criterion                    | Fp        | [?]     | double    | SNC                |
| stress_amplitude     | Stress amplitude                       | sigma_a   | [MPa]   | double    | CLD, CYC           |
| mean_stress          | Mean stress                            | ?         | [MPa]   | double    | CLD                |
| stress_mean          | ?                                      | sigma_mean |         | double    | CYC                |
| cum_n_cycles?        | percentage?                            | ?         |         | double    | CYC                |

