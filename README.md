# Json

Standard Go "encoding/json" package re-arranged to my needs.

Last updated : 22nd of June 2018

# Removed features

* TextUnmarshaler and TextMarshaler 

# New features

* fewer allocations - benchmarks are present
* encode state is the options carrier, instead of passing around options as parameter
* optional sort map keys on serializing
* encoding and decoding of Null typed structs (NullString, NullBool, etc.) - see TestNewNull in encode_test.go for the Marshalers and TestNewDecodeNull in decode_test.go
* minimal reflect, extracted from reflect package 
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

# Possible Known Issues

* Working with maps : when using Marshal with maps, developers should make sure that the map doesn't change. 
For allocation reasons, I have removed the code that was copying map's key and value before reading it.
