import json
from datetime import date
from typing import List

from fastapi import FastAPI, File, Query, UploadFile
from fastapi.middleware.cors import CORSMiddleware

from ccfatigue import analyzer
from ccfatigue import dashboarder
from ccfatigue.model import (
    Dashboard, Experiment, Plot, SnCurveMethod, SnCurveResult, Test)
from ccfatigue.config import settings
from ccfatigue.services.database import Base, database, engine


app = FastAPI()

if settings.cors_enabled:
    print('cors enabled')
else:
    print('cors disabled')
    app.add_middleware(
        CORSMiddleware,
        allow_origins=['*'],
        allow_credentials=True,
        allow_methods=['*'],
        allow_headers=['*'],
    )


Base.metadata.create_all(engine)

@app.on_event("startup")
async def startup():
    await database.connect()


@app.on_event("shutdown")
async def shutdown():
    await database.disconnect()


@app.get('/experiments', response_model=List[Experiment])
async def get_experiments() -> List[Experiment]:
    return []


@app.get('/dashboard', response_model=Dashboard)
async def get_dashboard(
        laboratory: str,
        researcher: str,
        experiment_type: str = Query(..., alias='experimentType'),
        date: date = Query(...),
        test_numbers: List[int] = Query(...,
                                        alias='testNumbers', ge=0, lt=1000)
) -> Dashboard:
    experiment_source_file = '../Preprocessing/vahid_CA_skel.json'
    with open(experiment_source_file) as f:
        experiment_data = json.load(f)

    # TODO
    # Add Strain at failure from HYS*.csv on the fly
    # Scott prepares this in Fatigue_test_dashboard/Hysteresis_loops.py
    # strain_at_failure()
    strain_at_failure = 1.17
    (experiment_data['Experiment']
        ['Standard Fatigue']
        ['Strain at Failure']) = strain_at_failure

    dashboard = dashboarder.generate_dashboard(
        laboratory, researcher, experiment_type, date, test_numbers)

    return Dashboard(
        experiment=experiment_data,
        tests=[
            Test(
                number=test.number,
                color=test.color,
                total_dissipated_energy=test.total_dissipated_energy,
                strain_at_failure=strain_at_failure
            )
            for test in dashboard.tests
        ],
        plot=Plot(
            stress_strain=dashboard.stress_strain,
            creep=dashboard.creep,
            hysteresis_area=dashboard.hysteresis_area,
            stiffness=dashboard.stiffness,
        ),
    )


@app.post('/snCurve/file')
async def run_sn_curve_file(file: UploadFile = File(...),
                            methods: List[SnCurveMethod] = Query(...),
                            r_ratios: List[float] = Query(..., alias='rRatios')
                            ) -> SnCurveResult:
    return analyzer.run_sn_curve(file.file, methods, r_ratios)
