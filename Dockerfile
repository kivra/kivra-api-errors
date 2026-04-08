FROM python:3.14.4-alpine@sha256:ccea73754fbcefbef8c2a2a64d902b95ffdde498b3ff8a644fe905a6efecfd41

COPY linter/requirements.txt /tmp/requirements.txt

RUN pip install -r /tmp/requirements.txt

COPY linter/lint.py .
COPY api-errors.json .

ENTRYPOINT [ "python3", "lint.py" ]
