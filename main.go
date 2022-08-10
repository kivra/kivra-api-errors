package main

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"strconv"
)

type ErrorDefinition struct {
	Code         string `json:"code"`
	LongMessage  string `json:"long_message"`
	ShortMessage string `json:"short_message"`
}

type ErrorDefinitions = map[string]ErrorDefinition

type ApiError struct {
	StatusCode int
	Payload    ErrorDefinition
}

//the line below (go:embed...) is a directive, not a comment!
//go:embed api-errors.json
var jsonErrorDefinitions []byte

var errorDefinitions ErrorDefinitions

// Load error definitions from disk. Should only be done once at application
// startup. Might cause race condition if error definitions are accessed at
// the same time.
func Load() {
	buf := bytes.NewBuffer(jsonErrorDefinitions)
	if err := json.NewDecoder(buf).Decode(&errorDefinitions); err != nil {
		panic("failed to decode ErrorDefinitions: " + err.Error())
	}
}

// Expands a 5-digit errorCode to an ApiError. Returns ok = false if errorCode
// is undefined or invalid.
func FromCode(errorCode string) (apiError *ApiError, ok bool) {
	errorDefinition, ok := errorDefinitions[errorCode]
	if !ok {
		return apiError, false
	}

	apiError = new(ApiError)

	errorDefinition.Code = errorCode
	apiError.Payload = errorDefinition

	statusCode, _ := strconv.Atoi(errorCode[0:3])
	apiError.StatusCode = statusCode

	return apiError, true
}
