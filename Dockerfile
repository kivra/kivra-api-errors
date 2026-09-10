FROM python:3.14.7-alpine@sha256:c6ead215bfd31f1e433d968853b7a769989117115b728874824e6c0a27cb96fc

COPY linter/requirements.txt /tmp/requirements.txt

RUN pip install -r /tmp/requirements.txt

COPY linter/lint.py .
COPY api-errors.json .

ENTRYPOINT [ "python3", "lint.py" ]
