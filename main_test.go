package main

import (
	"testing"
)

func TestGetErrorOK(t *testing.T) {
	Load()
	apiError, ok := FromCode("40000")

	if !ok {
		t.Fatal("Failed to load error definition for code 40000")
	}

	if apiError.StatusCode != 400 {
		t.Fatalf("Expected Status Code '400', got '%d'", apiError.StatusCode)
	}

	if apiError.Payload.Code != "40000" {
		t.Fatalf("Expected Code '40000', got '%s'", apiError.Payload.Code)
	}

	if apiError.Payload.ShortMessage != "Bad Request" {
		t.Fatalf("Expected Short Message 'Bad Request', got '%s'", apiError.Payload.ShortMessage)
	}

	if apiError.Payload.LongMessage != "The server cannot or will not process the request due to an apparent client error" {
		t.Fatalf(
			"Expected Short Message 'The server cannot or will not process the request due to an apparent client error', got '%s'",
			apiError.Payload.LongMessage,
		)
	}
}

func TestUnknownCode(t *testing.T) {
	Load()
	_, ok := FromCode("60000")

	if ok {
		t.Fatal("Expected 'false', got 'true'")
	}
}
