FROM python:3.14.6-alpine@sha256:26730869004e2b9c4b9ad09cab8625e81d256d1ce97e72df5520e806b1709f92

COPY linter/requirements.txt /tmp/requirements.txt

RUN pip install -r /tmp/requirements.txt

COPY linter/lint.py .
COPY api-errors.json .

ENTRYPOINT [ "python3", "lint.py" ]
