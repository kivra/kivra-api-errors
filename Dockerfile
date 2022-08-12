FROM python:3.11.0b3-slim-buster@sha256:cc20e1da721dba9d1f65d5881ad426c9031362a9e6132dd3b40d71374a4ad73f

COPY linter/requirements.txt /tmp/requirements.txt

RUN pip install -r /tmp/requirements.txt

COPY linter/lint.py .
COPY api-errors.json .

ENTRYPOINT [ "python3", "lint.py" ]
