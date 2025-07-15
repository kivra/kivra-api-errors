FROM python:3.11.4-slim-buster@sha256:c46b0ae5728c2247b99903098ade3176a58e274d9c7d2efeaaab3e0621a53935

COPY linter/requirements.txt /tmp/requirements.txt

RUN pip install -r /tmp/requirements.txt

COPY linter/lint.py .
COPY api-errors.json .

ENTRYPOINT [ "python3", "lint.py" ]
