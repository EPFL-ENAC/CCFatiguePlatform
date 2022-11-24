from enum import Enum


class FatigueModel(str, Enum):
    LIN_LOG = "Lin-Log"
    LOG_LOG = "Log-Log"
