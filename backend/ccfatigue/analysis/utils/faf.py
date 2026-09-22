from enum import Enum


class FatigueModel(str, Enum):
    LIN_LOG = "Lin-Log"
    LOG_LOG = "Log-Log"

# 100, 200, ... , 900, 1000, 2000, ... 9000, 10000, 2000 etc
# last number is this list is 2*10^7
# [for picking starting i] i = 9 * x -> 10^x, i0 = 18 -> N0 = 100, i0 = 27 -> N0 = 1000
LIST_CYCLES_TO_FAILURE = [ (1 + i % 9) * 10 ** ( i // 9) for i in range(27,64) ]