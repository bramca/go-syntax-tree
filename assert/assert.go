package assert

import "testing"

func Equal[V comparable](t *testing.T, got, expected V) {
    t.Helper()

    if expected != got {
        t.Errorf(`assert.Equal(
t,
got:
%v
,
expected:
%v
)`, got, expected)
    }
}

func Error(t *testing.T, err error) {
	t.Helper()

	if err == nil {
		t.Error("Expected err not to be nil but it is")
	}
}

func NoError(t *testing.T, err error) {
	t.Helper()

	if err != nil {
		t.Errorf("Expected error to be nil but it is not. err: %v", err)
	}
}
