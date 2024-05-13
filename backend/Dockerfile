FROM python:3.10-alpine as poetry-stage
WORKDIR /app
RUN apk add --no-cache \
    gcc \
    libffi-dev \
    musl-dev
RUN pip install poetry
COPY ./pyproject.toml ./poetry.lock* /app/
RUN poetry export --without-hashes --format=requirements.txt --output requirements.txt


FROM python:3.10-slim as fortran-stage
WORKDIR /app
RUN apt-get update && apt-get install -y \
    gcc \
    gfortran \
    && rm -rf /var/lib/apt/lists/*
RUN pip install --no-cache-dir numpy
COPY ./ccfatigue/analysis/fortran /app/fortran
RUN python -m numpy.f2py -c fortran/sendeckyj.f -m ccfatigue.analysis.fortran.sendeckyj


FROM python:3.10-slim as production-stage
EXPOSE 80
WORKDIR /app
RUN apt-get update && apt-get install -y \
    g++ \
    gcc \
    musl-dev \
    libpq-dev \
    libffi-dev \
    tree \
    && rm -rf /var/lib/apt/lists/*

COPY --from=poetry-stage /app/requirements.txt /app
RUN pip install --no-cache-dir -r requirements.txt

COPY --from=fortran-stage /app/ccfatigue/analysis/fortran/ /app/ccfatigue/analysis/fortran/

COPY ./alembic.ini /app/
COPY ./init_db.py /app/
COPY ./alembic /app/alembic
COPY ./ccfatigue /app/ccfatigue
COPY ./preprocessing /app/preprocessing

CMD [ "uvicorn", "ccfatigue.main:app", "--host", "0.0.0.0", "--port", "80" ]
