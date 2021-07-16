import json
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

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


@app.get('/experience')
async def get_experience():
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

    # TODO
    # Add Total Dissipated Energy on the fly
    # Scott prepares this in Fatigue_test_dashboard/test_dashboard.py
    # calculate_tde()
    total_dissipated_energy = 23987
    (experience_data['Experiment']
        ['Standard Fatigue']
        ['Total Dissipated Energy']) = total_dissipated_energy

    return experience_data
