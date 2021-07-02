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
    return experience_data
