| Variable name         | Description                                             | Symbol      | Unit    | Data type | Used in                 |
| --------------------- | ------------------------------------------------------- | ----------- | ------- | --------- | ----------------------- |
| a                     | S-N Curve parameter                                     | A           | [-]     | double    | SNC                     |
| b                     | S-N Curve parameter                                     | B           | [1/MPa] | double    | SNC                     |
| confidence_interval   | Confidence bounds (usually 5, 95%)                      | rsql        | %       | double    | SNC, FAF                |
| cum_n_cycles          | percentage                                              |             |         | double    | CYC                     |
| cycles_to_failure     | Number of cycles to failure                             | N           | [-]     | int       | AGG, SNC, CLD, FAF      |
| damage                | Damage                                                  | D           | [-]     | double    | DAS                     |
| fp                    | Linearity criterion                                     | Fp          | [?]     | double    | SNC                     |
| lrsq                  | ?                                                       | LRSQ        | [?]     | double    | SNC                     |
| n_cycles              | Number of cycles                                        | n           |         | double    | CYC                     |
| off_axis_angle1       | ?                                                       |             | [°]     | double    | FAF                     |
| off_axis_angle2       | ?                                                       |             | [°]     | double    | FAF                     |
| reliability_level     | Reliability level                                       | P(N)        | %       | double    | AGG                     |
| residual_strength     | Residual strength                                       | sigma_r     | [MPa]   | double    | AGG                     |
| stress_amplitude      | Stress amplitude                                        | sigma_a     | [MPa]   | double    | CLD                     |
| stress_cluster_number | Index of the cluster of stress level for Whitney method |             | [-]     | int       | AGG                     |
| stress_lowerbound     | Stress at failure (lower bound)                         | sigma_max   | [MPa]   | double    | SNC                     |
| stress_max            | Max stress                                              | sigma_max   | [MPa]   | double    | LDS, SNC, AGG, FAF, DAS |
| stress_mean           | Mean stress                                             | sigma_mean  | [MPa]   | double    | CLD, CYC                |
| stress_range          | Stress range (= 2 * sigma_a)                            | delta_sigma | [MPa]   | double    | CYC                     |
| stress_ratio          | Stress ratio                                            | R           | [-]     | double    | AGG, SNC, FAF, CYC      |
| stress_upperbound     | Stress at failure (upper bound)                         | sigma_max   | [MPa]   | double    | SNC                     |
