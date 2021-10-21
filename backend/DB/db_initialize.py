#!/usr/bin/env python3

'''
This will initialize MongoDB :
+ Database Name : CCFatigue
+ Collection Name : Tests

and inject into it a single (for now) experiment
found in /Preprocessing/vahid_CA_skel.json
'''

import os
import sys
import json
from pymongo import MongoClient
from pymongo.errors import ServerSelectionTimeoutError

DB_NAME = 'CCFatigue'
COLL_NAME = 'Tests'
FILES_TO_INJECT = [
    '../../Preprocessing/vahid_CA_skel.json',
]

CWD = os.path.dirname(os.path.abspath(sys.argv[0]))

FILES_TO_INJECT = [
    os.path.abspath(os.path.join(CWD, json_file))
    for json_file in FILES_TO_INJECT
]

PASS_FILE = os.path.join(CWD, 'secrets/mongo-root.txt')
with open(PASS_FILE, 'r') as f:
    PASS = f.readline().strip()

MONGODB_INFOS = {
    'USER': 'root',
    'PASS': PASS,
    'HOST': 'localhost',
    'PORT': 27017,
    'CONN_TIMEOUT_MS': 2000,
}


if __name__ == '__main__':

    # Connect to DB Engine
    try:
        client = MongoClient(
            username=MONGODB_INFOS['USER'],
            password=MONGODB_INFOS['PASS'],
            host=MONGODB_INFOS['HOST'],
            port=MONGODB_INFOS['PORT'],
            serverSelectionTimeoutMS=MONGODB_INFOS['CONN_TIMEOUT_MS'],
        )

        # Connect to Database + Collection
        list_of_db = client.list_database_names()
        db = client[DB_NAME]
        if DB_NAME not in list_of_db:
            print(f'Created Database { DB_NAME }')

        list_of_coll = db.list_collection_names()
        collection = db.get_collection(COLL_NAME)
        if COLL_NAME not in list_of_coll:
            print(f'Created Collection { COLL_NAME }')

        # Loop all JSON and inject them into DB if not already present
        for json_file in FILES_TO_INJECT:
            print(f'* { json_file } : ', end='')
            with open(json_file, 'r') as f:
                data = json.load(f)
                resp = collection.find_one({}, data)
                if resp is None:
                    collection.insert_one(data)
                    print('injected')
                else:
                    print('already present')

        print()
    except ServerSelectionTimeoutError:
        sys.exit('Could not connect to Database')
