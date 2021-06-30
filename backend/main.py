import json
from fastapi import FastAPI

app = FastAPI()


@app.get('/experience')
async def get_experience():
    experience_source_file = '../Preprocessing/vahid_CA_skel.json'
    with open(experience_source_file) as f:
        experience_data = json.load(f)
    return experience_data
