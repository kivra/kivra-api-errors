import json
import os.path

API_ERROR_JSON_FILEPATH = os.path.join(
    os.path.dirname(__file__),
    'kivra_api_errors_data/api-errors.json'
)

errors = {}

class KivraAPIError:

    @staticmethod
    def load(path: str = API_ERROR_JSON_FILEPATH) -> None:
        global errors
        with open(path) as f:
            errors = json.load(f)

    @staticmethod
    def from_code(code: str) -> dict:
        global errors
        return errors.get(code)

    @staticmethod
    def is_code(code) -> bool:
        global errors
        return code in errors
