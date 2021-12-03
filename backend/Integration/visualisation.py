import matplotlib.pyplot as plt
import pandas
from sqlalchemy import create_engine, func
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker, scoped_session

from init_db import Test, Test_results, Experiment

Base = declarative_base()
DBSession = scoped_session(sessionmaker())
engine = None

# Initialisation of the interface with the DB
def init(dbname="postgresql://postgres:197355cC?@localhost:5432/ENAC_Exo"):
    global engine
    engine = create_engine(dbname, echo=True)
    DBSession.remove()
    DBSession.configure(bind=engine, autoflush=False, expire_on_commit=False)
    Base.metadata.drop_all(engine)
    Base.metadata.create_all(engine)

def Graphs():
    init()
    Session = sessionmaker(bind=engine)
    session = Session()

    fig = plt.figure(constrained_layout=True)
    subfigs = fig.subfigures(nrows=2, ncols=1)

    records = session.query(Test.Stress_Ratio, Experiment.id).join(Experiment).all()
    df = pandas.DataFrame(records)

    axs = subfigs[0].subplots(nrows=1, ncols=4)
    subfigs[0].suptitle('Stress Ratio')
    for x in range(1,5):
        axs[x-1].hist(df.groupby(1)[0].apply(list).get(x), bins = 15)
        axs[x-1].set_title("Exp  " + str(x))
        axs[x - 1].set_xlabel('Value')
        axs[x - 1].set_ylabel('Frequency')


    ## Visualizing the Maximum machine load distribution

    # Pulling the data by filtering per experiment
    # Iteration per experiment
    records2 = session.query(func.max(Test_results.Machine_Load), Test.id, Experiment.id)\
                .select_from(Test_results)\
                .join(Test)\
                .join(Experiment)\
                .group_by(Test.id, Experiment.id)\
                .all()

    df = pandas.DataFrame(records2)

    axs2 = subfigs[1].subplots(nrows=1, ncols=4)
    subfigs[1].suptitle('Maximum Machine Load')
    for x in range(1,5):
        axs2[x-1].hist(df.groupby(2)[0].apply(list).get(x), bins= 50, color= 'red')
        axs2[x-1].set_title("Exp  " + str(x))
        axs2[x - 1].set_xlabel('Value')
        axs2[x - 1].set_ylabel('Frequency')

    fig.savefig('figures/StressRatio_MaxMachload_distribution.png')
    plt.show()


Graphs()