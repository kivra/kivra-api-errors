package apierrors

import (
	"bytes"
	_ "embed"
	"encoding/json"
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

var apiErrors = make(map[string]ApiError)

//the line below (go:embed...) is a directive, not a comment!
//go:embed api-errors.json
var jsonErrorDefinitions []byte

// Load error definitions from disk. Should only be done once at application
// startup. Might cause race condition if error definitions are accessed at
// the same time.
func Load() {
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

// Expands a 5-digit errorCode to an ApiError. Returns ok = false if errorCode
// is undefined or invalid.
func FromCode(errorCode string) (ApiError, bool) {
	apiError, ok := apiErrors[errorCode]
	return apiError, ok
}
