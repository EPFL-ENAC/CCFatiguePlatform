"""Your message that will be used

Revision ID: ceba65365eb4
Revises: e01704da37a7
Create Date: 2025-04-29 14:41:36.257917

"""
from alembic import op
import sqlalchemy as sa


# revision identifiers, used by Alembic.
revision = 'ceba65365eb4'
down_revision = 'e01704da37a7'
branch_labels = None
depends_on = None


def upgrade() -> None:
    # Add new columns to Experiment
    op.add_column('experiment', sa.Column('fa_experiment_type', sa.String()))
    op.add_column('experiment', sa.Column('qs_experiment_type', sa.String()))
    op.add_column('experiment', sa.Column('ot_experiment_type', sa.String()))
    op.add_column('experiment', sa.Column('ot_add_info', sa.String()))
    op.add_column('experiment', sa.Column('fracture_mode_fm', sa.Enum("Mode I", "Mode II", "Mode III", "Mixed-Mode", name="fracture_mode_fm")))
    op.add_column('experiment', sa.Column('fm_add_info', sa.String()))
    op.add_column('experiment', sa.Column('fatigue_loading_type_flt', sa.Enum("CA", "VA", "BL", "Other", name="fatigue_loading_type_flt")))
    op.add_column('experiment', sa.Column('flt_add_info', sa.String()))
    op.add_column('experiment', sa.Column('loading_rate', sa.Float()))
    op.add_column('experiment', sa.Column('material_tested', sa.String()))
    op.add_column('experiment', sa.Column('sample_type_add_info', sa.String()))
    op.add_column('experiment', sa.Column('other_polymers_add_info', sa.String()))
    op.add_column('experiment', sa.Column('curing_time', sa.Float()))
    op.add_column('experiment', sa.Column('curing_temperature', sa.Float()))
    op.add_column('experiment', sa.Column('curing_pressure', sa.Float()))
    op.add_column('experiment', sa.Column('postcuring_time', sa.Float()))
    op.add_column('experiment', sa.Column('postcuring_temperature', sa.Float()))
    op.add_column('experiment', sa.Column('postcuring_pressure', sa.Float()))
    op.add_column('experiment', sa.Column('glue', sa.String()))
    op.add_column('experiment', sa.Column('glue_curing_time', sa.Float()))
    op.add_column('experiment', sa.Column('glue_curing_temperature', sa.Float()))
    op.add_column('experiment', sa.Column('glue_curing_pressure', sa.Float()))
    op.add_column('experiment', sa.Column('fatigue_r_ratio', sa.Float()))
    op.add_column('experiment', sa.Column('fatigue_frequency', sa.Float()))
    op.alter_column('experiment', 'material_type_area_density', type_=sa.Float())

    # Add/rename columns in Test
    op.add_column('test', sa.Column('sequential_number', sa.Integer()))
    op.add_column('test', sa.Column('number_of_cycles', sa.Integer()))
    op.add_column('test', sa.Column('maximum_load', sa.Float()))
    op.add_column('test', sa.Column('subset_size', sa.Float()))
    op.add_column('test', sa.Column('step_size', sa.Float()))
    op.drop_column('test', 'specimen_number')
    op.drop_column('test', 'stress_ratio')
    op.drop_column('test', 'maximum_stress')
    op.drop_column('test', 'frequency')
    op.drop_column('test', 'displacement_controlled_loading_rate')
    op.drop_column('test', 'load_controlled_loading_rate')


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
