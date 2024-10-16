import json
import sys
from tokenize import String

from jsonschema import Draft7Validator


class ApiError:
    def __init__(self, code: str, messages: dict[str, str]):
        self.code: str = code
        self.short_message: str = messages["short_message"]
        self.long_message: str = messages["long_message"]


class LinterError:
    def __init__(self, message: str):
        self.message: str = message

    def __repr__(self) -> str:
        return self.message
    
    def __str__(self) -> str:
        return self.message


HTTP_STATUS_CODES = [
    400, 401, 402, 403, 404, 405,
    406, 407, 408, 409, 410, 411,
    412, 413, 414, 415, 416, 417,
    418, 421, 422, 423, 424, 425,
    426, 428, 429, 431, 451, 499,
    500, 501, 502, 503, 504, 505,
    506, 507, 508, 510, 511
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


def contains(substr: tuple[str], string: str) -> bool:
    for s in substr:
        if s in string:
            return True
    return False


def title(string: str) -> str:
    # like string.title(), but allow uppercase abbreviations like "OTP"
    return ' '.join([w.title() if w.islower() else w for w in string.split()])


def validate_short_message(e: ApiError, lint_errors: list[LinterError]) -> list[LinterError]:
    excluded = ["I'm a teapot"] # allow same spelling as in official HTTP spec
    
    if not e.short_message in excluded:
        def err(message: str):
            return LinterError(f"short_message for code {e.code} {message}")

        lower = e.short_message.lower()

        no_start = ("a ", "an ", "is ", "the ", "was ", "you ")
        if lower.startswith(no_start):
            lint_errors.append(err(f"should not start with any of {no_start} (is '{e.short_message}')"))

        no_contain = [f" {w}" for w in no_start]
        if contains(no_contain, lower):
            lint_errors.append(err(f"should not contain any of {no_contain} (is '{e.short_message}')"))

        title_case = title(e.short_message)
        if not title_case == e.short_message:
            lint_errors.append(err(f"is '{e.short_message}', should be '{title_case}'"))

        max_words = 5
        word_count = e.short_message.count(" ") + 1
        if word_count > max_words:
            lint_errors.append(err(f"should not contain more than {max_words} words (contains {word_count})"))


def lint() -> None:
    errors = json.load(open("api-errors.json"))
    parsed = [ApiError(code, messages) for (code, messages) in errors.items()]
    linter_errors = []

    validator = Draft7Validator(get_schema())
    validation_errors = sorted(validator.iter_errors(errors), key=str)
    if len(validation_errors) != 0:
        for error in sorted(validator.iter_errors(errors), key=str):
            linter_errors.append(error.message)

    missing_codes = required_codes() - errors.keys()
    if len(missing_codes) != 0:
        for code in missing_codes:
            linter_errors.append(f"Missing definition for required code {code}")

    for e in parsed:
        validate_short_message(e, linter_errors)

    if len(linter_errors) != 0:
        print(f"\nValidation failed with {len(linter_errors)} error(s) 😞\n")
        for err in linter_errors:
            print(err)
        sys.exit(1)

    print("\nValidation succeeded! 🎉")
    sys.exit(0)


if __name__ == "__main__":
    lint()
