"""initial inject data to DB

Revision ID: e8da76ec51b8
Revises: 14c4bbf64e6c
Create Date: 2021-11-12 15:54:13.438864

"""
from alembic import op
from sqlalchemy import MetaData, Table


# revision identifiers, used by Alembic.
revision = 'e8da76ec51b8'
down_revision = '14c4bbf64e6c'
branch_labels = None
depends_on = None


def upgrade():
    # get metadata from current connection
    meta = MetaData(bind=op.get_bind())

    # pass in tuple with tables we want to reflect,
    # otherwise whole database will get reflected
    meta.reflect(only=('experience', 'test',))

    # inject into experience table
    experience = Table('experience', meta)
    op.bulk_insert(
        experience,
        [
            {
                'laboratory': 'CCLAB',
                'researcher': 'Abdolvahid Movahedirad',
                'experiment_type': 'Standard Fatigue',
                'publication_title': 'Fatigue damage in angle-ply GFRP laminates under tension-tension fatigue',
                'publication_doi': 'https://doi.org/10.1016/j.ijfatigue.2017.12.015',
                'geometry_length': 250,
                'geometry_width': 25,
                'geometry_thickness': 2.25,
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
                'experience_id': 1,
                'specimen_number': '002',
                'number_of_cycles_to_failure': 1198627,
            },
        ]
    )


def downgrade():
    pass
