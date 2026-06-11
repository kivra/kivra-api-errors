FROM python:3.14.6-alpine@sha256:003970a263347645cd23d4f90929ad16ba7ce7d808ee4674ffcc93cb21cc289f

COPY linter/requirements.txt /tmp/requirements.txt

RUN pip install -r /tmp/requirements.txt

COPY linter/lint.py .
COPY api-errors.json .

ENTRYPOINT [ "python3", "lint.py" ]
