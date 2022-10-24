import unittest
from kivra_api_errors import KivraAPIError

class TestKivraAPIErrors(unittest.TestCase):
    def test_codes(self):
        KivraAPIError.load('api-errors.json')
        self.assertTrue(KivraAPIError.is_code('40000'))
        error = KivraAPIError.from_code('40000')
        self.assertEqual(error['short_message'], 'Bad Request')
        self.assertEqual(
            error['long_message'],
            'The server cannot or will not process the request due to an apparent client error'
        )

if __name__ == '__main__':
    unittest.main()
