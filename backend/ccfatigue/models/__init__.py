from pydantic import BaseModel


class AppInfo(BaseModel):
    """
    App Info
    """

    name: str
    version: str
