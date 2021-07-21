import json
from datetime import date
from typing import Any, List

import uvicorn
from fastapi import FastAPI, Query
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel

import dashboarder

app = FastAPI()

allowed_origins = [
    '*',
]

app.add_middleware(
    CORSMiddleware,
    allow_origins=allowed_origins,
    allow_credentials=True,
    allow_methods=['*'],
    allow_headers=['*'],
)


class Experience(BaseModel):
    id: str
    laboratory: str
    researcher: str
    type: str
    date: date


class Plot(BaseModel):
    stress_strain: Any
    creep: Any
    hysteresis_area: Any
    stiffness: Any


class Test(BaseModel):
    experience: Any
    total_dissipated_energy: int
    strain_at_failure: float
    plot: Plot


@app.get('/experiences', response_model=List[Experience])
async def get_experiences() -> List[Experience]:
    return []


@app.get('/experience/test', response_model=Test)
async def get_test(
        laboratory: str,
        researcher: str,
        experience_type: str = Query(..., alias='experienceType'),
        date: date = Query(...),
        test_number: int = Query(..., alias='testNumber', ge=0, lt=1000)
) -> Test:
    experience_source_file = '../Preprocessing/vahid_CA_skel.json'
    with open(experience_source_file) as f:
        experience_data = json.load(f)

    # TODO
    # Add Strain at failure from HYS*.csv on the fly
    # Scott prepares this in Fatigue_test_dashboard/Hysteresis_loops.py
    # strain_at_failure()
    strain_at_failure = 1.17
    (experience_data['Experiment']
        ['Standard Fatigue']
        ['Strain at Failure']) = strain_at_failure

    dashboard = dashboarder.generate_dashboard(
        laboratory, researcher, experience_type, date, test_number)

    return Test(
        experience=experience_data,
        total_dissipated_energy=dashboard.total_dissipated_energy,
        strain_at_failure=strain_at_failure,
        plot=Plot(
            stress_strain=dashboard.stress_strain,
            creep=dashboard.creep,
            hysteresis_area=dashboard.hysteresis_area,
            stiffness=dashboard.stiffness,
        ),
    )


if __name__ == "__main__":
    uvicorn.run("main:app", host="0.0.0.0", port=8000, reload=True)
