# Json

Standard Go "encoding/json" package re-arranged to my needs.

Last updated : 18th of May 2018

# Removed features

* TextUnmarshaler and TextMarshaler 

# New features

* fewer allocations (to be described)
* encode state is the options carrier, instead of passing around options as parameter
* optional sort map keys on serializing
* encoding of Null typed structs (NullString, NullBool, e.t.c.) - see TestNewNull in encode_test.go for the Marshalers.
* minimal reflect, extracted from reflect package (to be described)
* fast number scanner in strconv - numbers are already validated on decoding, so there is no need to validate (to be described)
* minimal strconv package included - instead of working with strings we're working with []byte (to be described)
* fixed omitempty struct bug : 

```go
    type (
		// note that all fields are omit empty
		EmptyStruct struct {
			Id   int64  `json:"id,omitempty"`
			Name string `json:"name,omitempty"`
			Bool bool   `json:"bool,omitempty"`
		}

		Test struct {
			Id       uint64      `json:"id"`
			MyStruct EmptyStruct `json:"myStruct,omitempty"` // and this one is marked as omit empty
		}
	)
	// the result should not contain "myStruct":{} - but it does
	tester := &Test{Id: 3}
	result, err := json.Marshal(tester)
```

The result is `{"id":3}` not `{"id":3,"myStruct":{}}`.