import matplotlib.pyplot as plt
import pandas
from sqlalchemy import create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker, scoped_session

from init_db import Test, Test_results

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

    ## Visualizing the Stress Ratio distribution
    StressRatios = []
    SR_list = []

    # Pulling the data by filtering per experiment
    # Iteration on the experiment
    for x in range(1,5):
        # Iteration on the test list child of the experiment
        for record in session.query(Test).filter(Test.parent_id == x):
            # Adding value to a list per experiment
            StressRatios.append(record.Stress_Ratio)
        # Collection of the experiment lists
        SR_list.append(pandas.DataFrame(StressRatios))

    fig, axs = plt.subplots(2, 2)
    fig.suptitle('Stress Ratio')
    axs[0, 0].hist(SR_list[0])
    axs[0, 0].set_title("Exp 1")
    axs[1, 0].hist(SR_list[1])
    axs[1, 0].set_title("Exp 2")
    axs[0, 1].hist(SR_list[2])
    axs[0, 1].set_title("Exp 3")
    axs[1, 1].hist(SR_list[3])
    axs[1, 1].set_title("Exp 4")
    StressRatios.clear()
    plt.show()

    ## Visualizing the Maximum machine load distribution

    ML_list = []
    MachLoad = []
    MaxMachLoad = []
    # Pulling the data by filtering per experiment
    # Iteration per experiment
    for x in range(1,5):
        # Iteration on the test list child of the experiment
        for record in session.query(Test).filter(Test.parent_id == x):
            # Iteration on the test_results list child of the test
            for record_tr in session.query(Test_results).filter(Test_results.parent_id == record.id):
                # Adding value to a list per test
                MachLoad.append(record_tr.Machine_Load)
            # Taking max value in a list per experiment
            MaxMachLoad.append(max(MachLoad))
        ML_list.append(pandas.DataFrame(MaxMachLoad))
    fig, axs = plt.subplots(2, 2)
    fig.suptitle('Machine load')
    axs[0, 0].hist(ML_list[0])
    axs[0, 0].set_title("Exp 1")
    axs[1, 0].hist(ML_list[1])
    axs[1, 0].set_title("Exp 2")
    axs[0, 1].hist(ML_list[2])
    axs[0, 1].set_title("Exp 3")
    axs[1, 1].hist(ML_list[3])
    axs[1, 1].set_title("Exp 4")
    MachLoad.clear()
    plt.show()

Graphs()