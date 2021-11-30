"""initial inject data to DB

Revision ID: ea8898a5d8e7
Revises: c88178579df5
Create Date: 2021-11-25 15:20:50.589997

"""
from alembic import op
from sqlalchemy import MetaData, Table


# revision identifiers, used by Alembic.
revision = 'ea8898a5d8e7'
down_revision = 'c88178579df5'
branch_labels = None
depends_on = None


def upgrade():
    # get metadata from current connection
    meta = MetaData(bind=op.get_bind())

    # pass in tuple with tables we want to reflect,
    # otherwise whole database will get reflected
    meta.reflect(only=('experiment', 'test',))

    # inject into experiment table
    experiment = Table('experiment', meta)
    op.bulk_insert(
        experiment,
        [
            {
                'laboratory': 'CCLAB',
                'researcher': 'Abdolvahid Movahedirad',
                'experiment_type': 'Standard Fatigue',
                'publication_title':
                    ('Fatigue damage in angle-ply GFRP'
                     ' laminates under tension-tension fatigue'),
                'publication_doi':
                    ('https://doi.org/10.1016/'
                     'j.ijfatigue.2017.12.015'),
                'geometry_length': 250,
                'geometry_width': 25,
                'geometry_thickness': 2.25,
                'images_repository': None,
                'laminates_and_assemblies_curing_time': 8,
                'laminates_and_assemblies_curing_temperature': 70,
                'laminates_and_assemblies_curing_pressure': None,
                'laminates_and_assemblies_fiber_content': 62,
                'laminates_and_assemblies_stacking_sequence': '(±45)_2s',
            },
        ]
    )

    # inject into test table
    test = Table('test', meta)
    op.bulk_insert(
        test,
        [
            {
                'experiment_id': 1,
                'specimen_number': '002',
                'number_of_cycles_to_failure': 1198627,
            },
        ]
    )


def downgrade():
    pass
