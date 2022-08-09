> This project is work-in-progress. Please refer to Kivra's official [API Documentation](https://developer.kivra.com/#section/Errors/Error-codes)
> for now.

# Kivra API Errors

This repository contains a complete list of Kivra API Errors along with the
corresponding error codes. It also contains `Erlang` and `Go` libraries that
simply working with Kivra API Errors in Kivra applications. 

## Kivra API Error Format

Kivra's API returns errors as a `json` payload in the following format:

```json
{
    "code": "40015",
    "short_message": "Invalid Token",
    "long_message": "Invalid access token provided"
}
```

The error `code` is a 5 digit string that identifies the type of error,
with the first 3 digits corresponding to the `http` status code of the response.
If required, applications may derive `http` status codes from error codes;
however, application logic should not depend on the presence of a specific
error `code`.

The `short_message` and `long_message` provide additional information about
errors. While each `code` is associated with a unique `short_message`, the
`long_message` may be partially or fully overwritten by Kivra applications to
provide more specific information; `long_message`s specified in [`api-errors.json`](./api-errors.json)
are indicative only and may be used as defaults by Kivra applications.

## Use Error Definitions in Applications

### Erlang

Add `kivra-api-errors` as a dependency to your project. With `rebar3`:

```erlang
{kivra_api_errors, {git, "git@github.com:kivra/kivra-api-errors.git", {tag, "..."}}}
```

Call the `load/0` function on application startup to load error definitions
from disk. Afterwards, use the `from_code/1` function to expand an error
code to a HTTP status code and error payload:

```erlang
ok                               = kivra_api_errors:load(),
{ok, {HTTPStatus, ErrorPayload}} = kivra_api_errors:from_code(<<"40001">>)

```

## Guidelines for API Errors

When adding new error codes, use the guidelines below to get started. Feel
free to suggest an update to these guidelines!

### Use the same Error Code for the same Error Class

Before introducing a new error `code`, consider whether an error can be expressed
using an existing error `code` while providing a meaningful `long_message`. For
example, to indicate that the payload validation for the field `currency` failed,
use a generic error `code` instead of creating a custom `code` for `currency`:

```json
{
    "code": "40001",
    "short_message": "Invalid Request",
    "long_message": "currency: Schema validation failed. Expected: 'SEK', Got: 'EUR'"
}
```

### Be Clear & Concise

Make sure `short_message` and `long_message` are as clear and concise as possible.
For example, the following two messages convey the same information, but
the first version is much longer without adding useful information:

    The operation on this resource is not valid for the user's current Mina Meddelanden account state

The following message contains the same information, but is more concise:

    Operation invalid for user's Mina Meddelanden account state

## Validate API Error Definitions

Run the linter to validate the error definitions in [`api-errors.json`](./api-errors.json):

    make lint
