import json
from datetime import date
from typing import List

from fastapi import FastAPI, File, Query, UploadFile
from fastapi.middleware.cors import CORSMiddleware

from ccfatigue import analyzer
from ccfatigue import dashboarder
from ccfatigue.model import (
    Dashboard, Experience, Plot, SnCurveMethod, SnCurveResult, Test)
from ccfatigue.config import settings


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


@app.get('/experiences', response_model=List[Experience])
async def get_experiences() -> List[Experience]:
    return []


@app.get('/dashboard', response_model=Dashboard)
async def get_dashboard(
        laboratory: str,
        researcher: str,
        experience_type: str = Query(..., alias='experienceType'),
        date: date = Query(...),
        test_numbers: List[int] = Query(...,
                                        alias='testNumbers', ge=0, lt=1000)
) -> Dashboard:
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
        laboratory, researcher, experience_type, date, test_numbers)

    return Dashboard(
        experience=experience_data,
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
