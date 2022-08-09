import json
import sys

from jsonschema import Draft7Validator


def code_pattern() -> str:
    http_status_codes = [
        400, 401, 402, 403, 404, 405,
        406, 407, 408, 409, 410, 411,
        412, 413, 414, 415, 416, 417,
        418, 421, 422, 423, 424, 425,
        426, 428, 429, 431, 451, 500,
        501, 502, 503, 504, 505, 506,
        507, 508, 510, 511
    ]
    return "|".join(map(lambda c: f"({c}" + r"\d{2})", http_status_codes))


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

    if len(validation_errors) != 0:
        print(
            f"\nSchema validation failed with {len(validation_errors)} errors 😞\n")
        for error in sorted(validator.iter_errors(errors), key=str):
            print(error.message)
        sys.exit(1)
    else:
        print("\nSchema validation succeeded! 🎉")
        sys.exit(0)


if __name__ == "__main__":
    lint()
