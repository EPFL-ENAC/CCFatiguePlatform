from pydantic import BaseModel


class AppInfo(BaseModel):
    """
    App Info
    """

    name: str
    version: str


class UnitInfo(BaseModel):
    """
    Unit Info
    """

    subject: str
    unit: str
