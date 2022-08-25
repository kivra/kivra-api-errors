import json
import sys
from tokenize import String

from jsonschema import Draft7Validator


HTTP_STATUS_CODES = [
    400, 401, 402, 403, 404, 405,
    406, 407, 408, 409, 410, 411,
    412, 413, 414, 415, 416, 417,
    418, 421, 422, 423, 424, 425,
    426, 428, 429, 431, 451, 500,
    501, 502, 503, 504, 505, 506,
    507, 508, 510, 511
]


def required_codes() -> list(String):
    return map(lambda c: f"{c}00", HTTP_STATUS_CODES)


def code_pattern() -> str:
    return "(20010)|"+"|".join(map(lambda c: f"({c}" + r"\d{2})", HTTP_STATUS_CODES))

def get_schema():
    return {
        "patternProperties": {
            code_pattern(): {
                "type": "object",
                "properties": {
                    "short_message": {
                        "type": "string",
                        "pattern": r"^[A-Z][^.]*$",
                        "minLength": 2,
                        "maxLength": 45
                    },
                    "long_message": {
                        "type": "string",
                        "pattern": r"^[A-Z].*[^.]$",
                        "minLength": 2,
                        "maxLength": 200
                    }
                },
                "required": ["short_message", "long_message"],
                "additionalProperties": False
            }
        },
        "additionalProperties": False,
    }


def lint() -> None:
    errors = json.load(open('api-errors.json'))
    validator = Draft7Validator(get_schema())
    validation_errors = sorted(validator.iter_errors(errors), key=str)
    missing_codes = required_codes() - errors.keys()

    exit_status = 0
    linter_errors = []

    if len(validation_errors) != 0:
        for error in sorted(validator.iter_errors(errors), key=str):
            linter_errors.append(error.message)
        exit_status = 1

    if len(missing_codes) != 0:
        for code in missing_codes:
            linter_errors.append(f'Missing definition for required code {code}')
        exit_status = 1

    if exit_status == 1:
        error_count = len(validation_errors) + len(missing_codes)
        print(f"\nValidation failed with {error_count} error(s) 😞\n")
        for err in linter_errors:
            print(err)
    else:
        print("\nValidation succeeded! 🎉")

    sys.exit(exit_status)


if __name__ == "__main__":
    lint()
