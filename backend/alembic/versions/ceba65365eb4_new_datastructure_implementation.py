"""Your message that will be used

Revision ID: ceba65365eb4
Revises: e01704da37a7
Create Date: 2025-04-29 14:41:36.257917

"""
from alembic import op
import sqlalchemy as sa
from sqlalchemy import inspect, text


# revision identifiers, used by Alembic.
revision = 'ceba65365eb4'
down_revision = 'e01704da37a7'
branch_labels = None
depends_on = None

def upgrade() -> None:
    bind = op.get_bind()
    inspector = inspect(bind)

    existing_columns = {col["name"] for col in inspector.get_columns("experiment")}
    existing_test_columns = {col["name"] for col in inspector.get_columns("test")}

    # ENUMs
    fracture_mode_enum = sa.Enum("Mode I", "Mode II", "Mode III", "Mixed-Mode", name="fracture_mode_fm")
    fatigue_loading_enum = sa.Enum("CA", "VA", "BL", "Other", name="fatigue_loading_type_flt")
    control_mode_enum = sa.Enum("Load Controlled", "Displacement Controlled", "Strain controlled", name="control_mode")
    experiment_type_enum = sa.Enum("FA", "QS", "OT", name="experiment_type")

    fracture_mode_enum.create(bind, checkfirst=True)
    fatigue_loading_enum.create(bind, checkfirst=True)
    control_mode_enum.create(bind, checkfirst=True)
    experiment_type_enum.create(bind, checkfirst=True)

    # Colonne esperimento
    def add_column_if_missing(table, name, column):
        if name not in existing_columns:
            op.add_column(table, column)

    add_column_if_missing('experiment', 'experiment_type', sa.Column('experiment_type', experiment_type_enum))
    add_column_if_missing('experiment', 'fa_experiment_type', sa.Column('fa_experiment_type', sa.String()))
    add_column_if_missing('experiment', 'qs_experiment_type', sa.Column('qs_experiment_type', sa.String()))
    add_column_if_missing('experiment', 'ot_experiment_type', sa.Column('ot_experiment_type', sa.String()))
    add_column_if_missing('experiment', 'ot_add_info', sa.Column('ot_add_info', sa.String()))
    add_column_if_missing('experiment', 'fracture_mode_fm', sa.Column('fracture_mode_fm', fracture_mode_enum))
    add_column_if_missing('experiment', 'fm_add_info', sa.Column('fm_add_info', sa.String()))
    add_column_if_missing('experiment', 'fatigue_loading_type_flt', sa.Column('fatigue_loading_type_flt', fatigue_loading_enum))
    add_column_if_missing('experiment', 'flt_add_info', sa.Column('flt_add_info', sa.String()))
    add_column_if_missing('experiment', 'loading_rate', sa.Column('loading_rate', sa.Float()))
    add_column_if_missing('experiment', 'material_tested', sa.Column('material_tested', sa.String()))
    add_column_if_missing('experiment', 'sample_type_add_info', sa.Column('sample_type_add_info', sa.String()))
    add_column_if_missing('experiment', 'other_polymers_add_info', sa.Column('other_polymers_add_info', sa.String()))
    add_column_if_missing('experiment', 'curing_time', sa.Column('curing_time', sa.Float()))
    add_column_if_missing('experiment', 'curing_temperature', sa.Column('curing_temperature', sa.Float()))
    add_column_if_missing('experiment', 'curing_pressure', sa.Column('curing_pressure', sa.Float()))
    add_column_if_missing('experiment', 'postcuring_time', sa.Column('postcuring_time', sa.Float()))
    add_column_if_missing('experiment', 'postcuring_temperature', sa.Column('postcuring_temperature', sa.Float()))
    add_column_if_missing('experiment', 'postcuring_pressure', sa.Column('postcuring_pressure', sa.Float()))
    add_column_if_missing('experiment', 'glue', sa.Column('glue', sa.String()))
    add_column_if_missing('experiment', 'glue_curing_time', sa.Column('glue_curing_time', sa.Float()))
    add_column_if_missing('experiment', 'glue_curing_temperature', sa.Column('glue_curing_temperature', sa.Float()))
    add_column_if_missing('experiment', 'glue_curing_pressure', sa.Column('glue_curing_pressure', sa.Float()))
    add_column_if_missing('experiment', 'fatigue_r_ratio', sa.Column('fatigue_r_ratio', sa.Float()))
    add_column_if_missing('experiment', 'fatigue_frequency', sa.Column('fatigue_frequency', sa.Float()))

    # Conversione tipo colonna
    if 'material_type_area_density' in existing_columns:
        op.alter_column('experiment', 'material_type_area_density', type_=sa.Float())

    # Colonne Test
    def add_test_column_if_missing(name, column):
        if name not in existing_test_columns:
            op.add_column('test', column)

    add_test_column_if_missing('sequential_number', sa.Column('sequential_number', sa.Integer()))
    add_test_column_if_missing('number_of_cycles', sa.Column('number_of_cycles', sa.Integer()))
    add_test_column_if_missing('maximum_load', sa.Column('maximum_load', sa.Float()))
    add_test_column_if_missing('subset_size', sa.Column('subset_size', sa.Float()))
    add_test_column_if_missing('step_size', sa.Column('step_size', sa.Float()))

    # Tentativo di rimozione colonne vecchie (se ci sono)
    for col in ['specimen_number', 'stress_ratio', 'maximum_stress', 'frequency', 'displacement_controlled_loading_rate', 'load_controlled_loading_rate']:
        if col in existing_test_columns:
            op.drop_column('test', col)


def downgrade() -> None:
    # Reverse changes made in upgrade()
    op.drop_column('experiment', 'fa_experiment_type')
    op.drop_column('experiment', 'qs_experiment_type')
    op.drop_column('experiment', 'ot_experiment_type')
    op.drop_column('experiment', 'ot_add_info')
    op.drop_column('experiment', 'fracture_mode_fm')
    op.drop_column('experiment', 'fm_add_info')
    op.drop_column('experiment', 'fatigue_loading_type_flt')
    op.drop_column('experiment', 'flt_add_info')
    op.drop_column('experiment', 'loading_rate')
    op.drop_column('experiment', 'material_tested')
    op.drop_column('experiment', 'sample_type_add_info')
    op.drop_column('experiment', 'other_polymers_add_info')
    op.drop_column('experiment', 'curing_time')
    op.drop_column('experiment', 'curing_temperature')
    op.drop_column('experiment', 'curing_pressure')
    op.drop_column('experiment', 'postcuring_time')
    op.drop_column('experiment', 'postcuring_temperature')
    op.drop_column('experiment', 'postcuring_pressure')
    op.drop_column('experiment', 'glue')
    op.drop_column('experiment', 'glue_curing_time')
    op.drop_column('experiment', 'glue_curing_temperature')
    op.drop_column('experiment', 'glue_curing_pressure')
    op.drop_column('experiment', 'fatigue_r_ratio')
    op.drop_column('experiment', 'fatigue_frequency')
    op.alter_column('experiment', 'material_type_area_density', type_=sa.String())

    op.add_column('test', sa.Column('specimen_number', sa.Integer()))
    op.add_column('test', sa.Column('stress_ratio', sa.Float()))
    op.add_column('test', sa.Column('maximum_stress', sa.Float()))
    op.add_column('test', sa.Column('frequency', sa.Float()))
    op.add_column('test', sa.Column('displacement_controlled_loading_rate', sa.Float()))
    op.add_column('test', sa.Column('load_controlled_loading_rate', sa.Float()))
    op.drop_column('test', 'sequential_number')
    op.drop_column('test', 'number_of_cycles')
    op.drop_column('test', 'maximum_load')
    op.drop_column('test', 'subset_size')
    op.drop_column('test', 'step_size')
