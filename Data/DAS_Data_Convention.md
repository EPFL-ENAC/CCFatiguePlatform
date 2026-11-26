# DAS standard data format

DAS data is the output of the Damage Summation module. The module implements
the final uniaxial workflow described in the CCfatigue paper:

1. convert one load-history spectrum (LDS) into counted cycles;
2. fit S-N curves when raw AGG data is supplied, or use supplied SNC data;
3. construct a constant-life diagram (CLD);
4. obtain the constant-amplitude life `N_i` of each counted cycle from the CLD;
5. calculate damage per spectrum pass with the Palmgren-Miner rule,
   `D_pass = sum(n_i / N_i)`;
6. calculate predicted spectrum passes to failure as `1 / D_pass`.

The spectrum is evaluated at a user-defined range of load scale factors. This
produces the paper's maximum-stress versus spectrum-passes life curve while
also retaining the actual Miner damage index for every factor.

## File naming convention

- `DAS_{load-history}_{cycle-method}_{sn-method}_{cld-method}.csv`
- `DAS_{load-history}_{cycle-method}_{sn-method}_{cld-method}.json`

## DAS CSV columns

Encoding: UTF-8. Separator: comma.

| Column | Description | Unit |
| --- | --- | --- |
| `scale_factor` | Multiplier applied to every load-history peak and valley | `[-]` |
| `stress_max` | Maximum scaled stress represented by the counted spectrum | `[MPa]` |
| `stress_min` | Minimum scaled stress represented by the counted spectrum | `[MPa]` |
| `damage_per_pass` | Palmgren-Miner damage caused by one complete spectrum pass | `[-]` |
| `spectrum_passes_to_failure` | Predicted repetitions of the complete spectrum before `D = 1` | `[-]` |
| `static_overload_bins` | Counted-cycle bins reaching a CLD static-strength boundary | `[-]` |
| `cycle_counting_method` | Cycle-counting solver used | `[-]` |
| `sn_curve_method` | S-N solver used, or `supplied SNC` | `[-]` |
| `cld_method` | Constant-life diagram solver used | `[-]` |

The JSON companion records the selected-factor summary, cycle and CLD counts,
warnings, and the ten largest cycle-bin damage contributions near scale factor
1.0.
