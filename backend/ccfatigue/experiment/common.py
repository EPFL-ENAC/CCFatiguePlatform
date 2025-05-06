import os
from typing import Any, Dict, List

from sqlalchemy import Column
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select

from ccfatigue.config import settings
from ccfatigue.models.database_v2 import Test

DATA_DIRECTORY: str = os.path.join(settings.data_path, "preprocessed")  # type: ignore


def extract_experiment_metadata(experiment: Dict[str, Any]) -> Dict[str, Dict[str, Any]]:
    return {
        "general": {
            "laboratory": experiment.get("laboratory"),
            "researcher": experiment.get("researcher"),
            "date": experiment.get("date"),
            "experiment_type": experiment.get("experiment_type"),
            "measuring_equipment": experiment.get("measuring_equipment"),
            "publication_doi": experiment.get("publication_doi"),
            "material_tested": experiment.get("material_tested"),
        },
        "materials": {
            "material_tested": experiment.get("material_tested"),
            "sample_type": experiment.get("material_type_sample_type"),
            "fiber_form": experiment.get("material_type_fiber_form"),
            "resin": experiment.get("material_type_resin"),
            "stacking_sequence": experiment.get("laminates_and_assemblies_stacking_sequence"),
        },
        "control": {
            "control_mode": experiment.get("control_mode"),
            "loading_rate": experiment.get("loading_rate"),
            "fracture_mode": experiment.get("fracture_mode_fm") if (
                experiment.get("qs_experiment_type") == "fracture"
                or experiment.get("fa_experiment_type") == "fracture"
            ) else None,
            "fatigue_r_ratio": experiment.get("fatigue_r_ratio") if experiment.get("experiment_type") == "FA" else None,
            "fatigue_frequency": experiment.get("fatigue_frequency") if experiment.get("experiment_type") == "FA" else None,
            "fatigue_loading_type": experiment.get("fatigue_loading_type_flt") if experiment.get("experiment_type") == "FA" else None,
        },
        "curing": {
            "curing_time": experiment.get("curing_time"),
            "curing_temperature": experiment.get("curing_temperature"),
            "curing_pressure": experiment.get("curing_pressure"),
            "postcuring_applied": any(
                experiment.get(key) is not None
                for key in [
                    "postcuring_time",
                    "postcuring_temperature",
                    "postcuring_pressure",
                ]
            ),
        },
    }


import logging
logger = logging.getLogger(__name__)

def flatten_metadata(metadata: Dict[str, Dict[str, Any]]) -> Dict[str, Any]:
    flat_metadata: Dict[str, Any] = {}
    for section_name, section in metadata.items():
        if not isinstance(section, dict):
            logger.warning(f"Expected dict for metadata section '{section_name}', got {type(section)}")
            continue
        flat_metadata.update(section)
    return flat_metadata




async def get_test_fields(
    session: AsyncSession, experiment_id: int, test_id: int, fields: List[Column]
) -> Dict[str, Any]:
    values: Dict[str, Any] = (
        (
            await session.execute(
                select(*fields)
                .where(Test.experiment_id == experiment_id)
                .where(Test.id == test_id)
            )
        )
        .one()  # type: ignore
        ._asdict()
    )
    return values
