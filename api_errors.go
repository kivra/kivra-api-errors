package apierrors

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"fmt"
	"strconv"
)

type ErrorDefinition struct {
	LongMessage  string `json:"long_message"`
	ShortMessage string `json:"short_message"`
}

type Payload struct {
	Code         string `json:"code"`
	LongMessage  string `json:"long_message"`
	ShortMessage string `json:"short_message"`
}

type ApiError struct {
	StatusCode int
	Payload    Payload
}

// Reponse header field that holds the API Error Code
const ErrorCodeHeader = "X-Error-Code"

// Fallback ApiError that is returned if no matching ApiError is found
var Fallback = "50000"

// Expands a 5-digit errorCode to an ApiError. Returns ok = false if errorCode
// is undefined or invalid.
func FromCode(errorCode string) (ApiError, bool) {
	apiError, ok := apiErrors[errorCode]
	return apiError, ok
}

// Expands a 5-digit errorCode to an ApiError. Returns Fallback if code cannot
// be found.
func FromCodeOrFallback(errorCode string) ApiError {
	apiError, ok := FromCode(errorCode)
	if ok {
		return apiError
	}
	apiError, ok = apiErrors[Fallback]
	if !ok {
		panic("fallback error code does not exist: " + errorCode)
	}
	return apiError
}

// Expands an HTTP status code to an ApiError. Returns Fallback if status
// cannot be expanded.
func FromStatusOrFallback(status int) ApiError {
	return FromCodeOrFallback(fmt.Sprintf("%d00", status))
}

// Check if a string is an error code
func IsCode(maybeCode string) bool {
	_, ok := FromCode(maybeCode)
	return ok
}

var apiErrors = make(map[string]ApiError)

// the line below (go:embed...) is a directive, not a comment!
//
//go:embed api-errors.json
var jsonErrorDefinitions []byte

// init is a special function that is guaranteed to be called during the
// initialization of the package and before any other functions are called:
// https://go.dev/doc/effective_go#init
func init() {
	var errorDefinitions map[string]ErrorDefinition
	buf := bytes.NewBuffer(jsonErrorDefinitions)
	if err := json.NewDecoder(buf).Decode(&errorDefinitions); err != nil {
		panic("failed to decode ErrorDefinitions: " + err.Error())
	}

	for code := range errorDefinitions {
		statusCode, _ := strconv.Atoi(code[0:3])
		apiErrors[code] = ApiError{
			StatusCode: statusCode,
			Payload: Payload{
				Code:         code,
				ShortMessage: errorDefinitions[code].ShortMessage,
				LongMessage:  errorDefinitions[code].LongMessage,
			},
		}
	}
}
