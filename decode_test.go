/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"bytes"
	"errors"
	"fmt"
	"image"
	"io"
	"io/ioutil"
	"log"
	"math"
	"math/big"
	"math/rand"
	"net"
	"net/http"
	"net/http/httptest"
	"reflect"
	"strconv"
	"strings"
	"testing"
	"time"
	"unicode/utf8"
)

type (
	T struct {
		X string
		Y int
		Z int `json:"-"`
	}

	U struct {
		Alphabet string `json:"alpha"`
	}

	V struct {
		F1 interface{}
		F2 int32
		F3 Number
		F4 *VOuter
	}

	VOuter struct {
		V V
	}

	tx struct {
		x int
	}

	u8 uint8

	// A that can unmarshal itself.

	unmarshaler struct {
		T bool
	}

	ustruct struct {
		M unmarshaler
	}

	// Test data structures for anonymous fields.

	Point struct {
		Z int
	}

	Top struct {
		Level0 int
		Embed0
		*Embed0a
		*Embed0b `json:"e,omitempty"` // treated as named
		Embed0c  `json:"-"`           // ignored
		Loop
		Embed0p // has Point with X, Y, used
		Embed0q // has Point with Z, used
		embed   // contains exported field
	}

	Embed0 struct {
		Level1a int // overridden by Embed0a's Level1a with json tag
		Level1b int // used because Embed0a's Level1b is renamed
		Level1c int // used because Embed0a's Level1c is ignored
		Level1d int // annihilated by Embed0a's Level1d
		Level1e int `json:"x"` // annihilated by Embed0a.Level1e
	}

	Embed0a struct {
		Level1a int `json:"Level1a,omitempty"`
		Level1b int `json:"LEVEL1B,omitempty"`
		Level1c int `json:"-"`
		Level1d int // annihilated by Embed0's Level1d
		Level1f int `json:"x"` // annihilated by Embed0's Level1e
	}

	Embed0b Embed0

	Embed0c Embed0

	Embed0p struct {
		image.Point
	}

	Embed0q struct {
		Point
	}

	embed struct {
		Q int
	}

	Loop struct {
		Loop1 int `json:",omitempty"`
		Loop2 int `json:",omitempty"`
		*Loop
	}

	// From reflect test:
	// The X in S6 and S7 annihilate, but they also block the X in S8.S9.
	S5 struct {
		S6
		S7
		S8
	}

	S6 struct {
		X int
	}

	S7 S6

	S8 struct {
		S9
	}

	S9 struct {
		X int
		Y int
	}

	// From reflect test:
	// The X in S11.S6 and S12.S6 annihilate, but they also block the X in S13.S8.S9.
	S10 struct {
		S11
		S12
		S13
	}

	S11 struct {
		S6
	}

	S12 struct {
		S6
	}

	S13 struct {
		S8
	}

	Ambig struct {
		// Given "hello", the first match should win.
		First  int `json:"HELLO"`
		Second int `json:"Hello"`
	}

	XYZ struct {
		X interface{}
		Y interface{}
		Z interface{}
	}

	byteWithPtrMarshalJSON byte
	byteWithMarshalJSON    byte

	intWithMarshalJSON int

	intWithPtrMarshalJSON int

	unmarshalTest struct {
		in                    string
		ptr                   interface{}
		out                   interface{}
		err                   error
		useNumber             bool
		golden                bool
		disallowUnknownFields bool
	}

	B struct {
		B bool `json:",string"`
	}
)

// ifaceNumAsFloat64/ifaceNumAsNumber are used to test unmarshaling with and
// without UseNumber
var ifaceNumAsFloat64 = map[string]interface{}{
	"k1": float64(1),
	"k2": "s",
	"k3": []interface{}{float64(1), float64(2.0), float64(3e-3)},
	"k4": map[string]interface{}{"kk1": "s", "kk2": float64(2)},
}

var ifaceNumAsNumber = map[string]interface{}{
	"k1": Number("1"),
	"k2": "s",
	"k3": []interface{}{Number("1"), Number("2.0"), Number("3e-3")},
	"k4": map[string]interface{}{"kk1": "s", "kk2": Number("2")},
}

func (u *unmarshaler) UnmarshalJSON(b []byte) error {
	*u = unmarshaler{true} // All we need to see that UnmarshalJSON is called.
	return nil
}

var (
	um0, um1 unmarshaler // target2 of unmarshaling
	ump      = &um1
	umtrue   = unmarshaler{true}
	umslice  = []unmarshaler{{true}}
	umslicep = new([]unmarshaler)
	umstruct = ustruct{unmarshaler{true}}
)

func sliceAddr(x []int) *[]int                 { return &x }
func mapAddr(x map[string]int) *map[string]int { return &x }

func (b byteWithMarshalJSON) MarshalJSON() ([]byte, error) {
	return []byte(fmt.Sprintf(`"Z%.2x"`, byte(b))), nil
}

func (b *byteWithMarshalJSON) UnmarshalJSON(data []byte) error {
	if len(data) != 5 || data[0] != '"' || data[1] != 'Z' || data[4] != '"' {
		return fmt.Errorf("bad isBasic string")
	}
	i, err := strconv.ParseInt(string(data[2:4]), 16, 8)
	if err != nil {
		return fmt.Errorf("bad hex")
	}
	*b = byteWithMarshalJSON(i)
	return nil
}

func (b *byteWithPtrMarshalJSON) MarshalJSON() ([]byte, error) {
	return byteWithMarshalJSON(*b).MarshalJSON()
}

func (b *byteWithPtrMarshalJSON) UnmarshalJSON(data []byte) error {
	return (*byteWithMarshalJSON)(b).UnmarshalJSON(data)
}

func (b intWithMarshalJSON) MarshalJSON() ([]byte, error) {
	return []byte(fmt.Sprintf(`"Z%.2x"`, int(b))), nil
}

func (b *intWithMarshalJSON) UnmarshalJSON(data []byte) error {
	if len(data) != 5 || data[0] != '"' || data[1] != 'Z' || data[4] != '"' {
		return fmt.Errorf("bad isBasic string")
	}
	i, err := strconv.ParseInt(string(data[2:4]), 16, 8)
	if err != nil {
		return fmt.Errorf("bad hex")
	}
	*b = intWithMarshalJSON(i)
	return nil
}

func (b *intWithPtrMarshalJSON) MarshalJSON() ([]byte, error) {
	return intWithMarshalJSON(*b).MarshalJSON()
}

func (b *intWithPtrMarshalJSON) UnmarshalJSON(data []byte) error {
	return (*intWithMarshalJSON)(b).UnmarshalJSON(data)
}

var unmarshalTests = []unmarshalTest{
	// basic types
	{in: `true`, ptr: new(bool), out: true},
	{in: `1`, ptr: new(int), out: 1},
	{in: `1.2`, ptr: new(float64), out: 1.2},
	{in: `-5`, ptr: new(int16), out: int16(-5)},
	{in: `2`, ptr: new(Number), out: Number("2"), useNumber: true},
	{in: `2`, ptr: new(Number), out: Number("2")},
	{in: `2`, ptr: new(interface{}), out: float64(2.0)},
	{in: `2`, ptr: new(interface{}), out: Number("2"), useNumber: true},
	{in: `"a\u1234"`, ptr: new(string), out: "a\u1234"},
	{in: `"http:\/\/"`, ptr: new(string), out: "http://"},
	{in: `"g-clef: \uD834\uDD1E"`, ptr: new(string), out: "g-clef: \U0001D11E"},
	{in: `"invalid: \uD834x\uDD1E"`, ptr: new(string), out: "invalid: \uFFFDx\uFFFD"},
	{in: "null", ptr: new(interface{}), out: nil},
	{in: `{"X": [1,2,3], "Y": 4}`, ptr: new(T), out: T{Y: 4}, err: &UnmarshalTypeError{"array", reflect.TypeOf(""), 7, "T", "X"}},
	{in: `{"x": 1}`, ptr: new(tx), out: tx{}},
	{in: `{"x": 1}`, ptr: new(tx), err: fmt.Errorf("json: unknown field \"x\""), disallowUnknownFields: true},
	{in: `{"F1":1,"F2":2,"F3":3}`, ptr: new(V), out: V{F1: float64(1), F2: int32(2), F3: Number("3")}},
	{in: `{"F1":1,"F2":2,"F3":3}`, ptr: new(V), out: V{F1: Number("1"), F2: int32(2), F3: Number("3")}, useNumber: true},
	{in: `{"k1":1,"k2":"s","k3":[1,2.0,3e-3],"k4":{"kk1":"s","kk2":2}}`, ptr: new(interface{}), out: ifaceNumAsFloat64},
	{in: `{"k1":1,"k2":"s","k3":[1,2.0,3e-3],"k4":{"kk1":"s","kk2":2}}`, ptr: new(interface{}), out: ifaceNumAsNumber, useNumber: true},

	// raw values with whitespace
	{in: "\n true ", ptr: new(bool), out: true},
	{in: "\t 1 ", ptr: new(int), out: 1},
	{in: "\r 1.2 ", ptr: new(float64), out: 1.2},
	{in: "\t -5 \n", ptr: new(int16), out: int16(-5)},
	{in: "\t \"a\\u1234\" \n", ptr: new(string), out: "a\u1234"},

	// Z has a "-" tag.
	{in: `{"Y": 1, "Z": 2}`, ptr: new(T), out: T{Y: 1}},
	{in: `{"Y": 1, "Z": 2}`, ptr: new(T), err: fmt.Errorf("json: unknown field \"Z\""), disallowUnknownFields: true},

	{in: `{"alpha": "abc", "alphabet": "xyz"}`, ptr: new(U), out: U{Alphabet: "abc"}},
	{in: `{"alpha": "abc", "alphabet": "xyz"}`, ptr: new(U), err: fmt.Errorf("json: unknown field \"alphabet\""), disallowUnknownFields: true},
	{in: `{"alpha": "abc"}`, ptr: new(U), out: U{Alphabet: "abc"}},
	{in: `{"alphabet": "xyz"}`, ptr: new(U), out: U{}},
	{in: `{"alphabet": "xyz"}`, ptr: new(U), err: fmt.Errorf("json: unknown field \"alphabet\""), disallowUnknownFields: true},

	// syntax errors
	{in: `{"X": "foo", "Y"}`, err: &SyntaxError{"invalid character '}' expected ':' after object key", 17}},
	{in: `[1, 2, 3+]`, err: &SyntaxError{"invalid character '+' expected ',' or ']' after array element", 9}},
	{in: `{"X":12x}`, err: &SyntaxError{"invalid character 'x' expected ',' or '}' after object key:value pair", 8}, useNumber: true},

	// raw value errors
	{in: "\x01 42", err: &SyntaxError{"invalid character '\x01' expected ' ' or '{' or '[' or '\"' or '-' or '0' or 't' or 'f' or 'n' or '1' looking for beginning of value", 1}},
	{in: " 42 \x01", err: &SyntaxError{"invalid character '\x01' expected ' ' or '\t' or '\n' after top-level value", 5}},
	{in: "\x01 true", err: &SyntaxError{"invalid character '\x01' expected ' ' or '{' or '[' or '\"' or '-' or '0' or 't' or 'f' or 'n' or '1' looking for beginning of value", 1}},
	{in: " false \x01", err: &SyntaxError{"invalid character '\x01' expected ' ' or '\t' or '\n' after top-level value", 8}},
	{in: "\x01 1.2", err: &SyntaxError{"invalid character '\x01' expected ' ' or '{' or '[' or '\"' or '-' or '0' or 't' or 'f' or 'n' or '1' looking for beginning of value", 1}},
	{in: " 3.4 \x01", err: &SyntaxError{"invalid character '\x01' expected ' ' or '\t' or '\n' after top-level value", 6}},
	{in: "\x01 \"string\"", err: &SyntaxError{"invalid character '\x01' expected ' ' or '{' or '[' or '\"' or '-' or '0' or 't' or 'f' or 'n' or '1' looking for beginning of value", 1}},
	{in: " \"string\" \x01", err: &SyntaxError{"invalid character '\x01' expected ' ' or '\t' or '\n' after top-level value", 11}},

	// array tests
	{in: `[1, 2, 3]`, ptr: new([3]int), out: [3]int{1, 2, 3}},
	{in: `[1, 2, 3]`, ptr: new([1]int), out: [1]int{1}},
	{in: `[1, 2, 3]`, ptr: new([5]int), out: [5]int{1, 2, 3, 0, 0}},

	// empty array to interface test
	{in: `[]`, ptr: new([]interface{}), out: []interface{}{}},
	{in: `null`, ptr: new([]interface{}), out: []interface{}(nil)},
	{in: `{"T":[]}`, ptr: new(map[string]interface{}), out: map[string]interface{}{"T": []interface{}{}}},
	{in: `{"T":null}`, ptr: new(map[string]interface{}), out: map[string]interface{}{"T": interface{}(nil)}},

	// composite tests
	{in: allValueIndent, ptr: new(All), out: allValue},
	{in: allValueCompact, ptr: new(All), out: allValue},
	{in: allValueIndent, ptr: new(*All), out: &allValue},
	{in: allValueCompact, ptr: new(*All), out: &allValue},
	{in: pallValueIndent, ptr: new(All), out: pallValue},
	{in: pallValueCompact, ptr: new(All), out: pallValue},
	{in: pallValueIndent, ptr: new(*All), out: &pallValue},
	{in: pallValueCompact, ptr: new(*All), out: &pallValue},

	// unmarshal interface test
	{in: `{"T":false}`, ptr: &um0, out: umtrue}, // use "false" so test will fail if custom unmarshaler is not called
	{in: `{"T":false}`, ptr: &ump, out: &umtrue},
	{in: `[{"T":false}]`, ptr: &umslice, out: umslice},
	{in: `[{"T":false}]`, ptr: &umslicep, out: &umslice},
	{in: `{"M":{"T":"x:y"}}`, ptr: &umstruct, out: umstruct},

	// integer-keyed map test
	{
		in:  `{"-1":"a","0":"b","1":"c"}`,
		ptr: new(map[int]string),
		out: map[int]string{-1: "a", 0: "b", 1: "c"},
	},
	{
		in:  `{"0":"a","10":"c","9":"b"}`,
		ptr: new(map[u8]string),
		out: map[u8]string{0: "a", 9: "b", 10: "c"},
	},
	{
		in:  `{"-9223372036854775808":"min","9223372036854775807":"max"}`,
		ptr: new(map[int64]string),
		out: map[int64]string{math.MinInt64: "min", math.MaxInt64: "max"},
	},
	{
		in:  `{"18446744073709551615":"max"}`,
		ptr: new(map[uint64]string),
		out: map[uint64]string{math.MaxUint64: "max"},
	},
	{
		in:  `{"0":false,"10":true}`,
		ptr: new(map[uintptr]bool),
		out: map[uintptr]bool{0: false, 10: true},
	},

	// integer-keyed map errors
	{
		in:  `{"abc":"abc"}`,
		ptr: new(map[int]string),
		err: &UnmarshalTypeError{Value: "number abc", Type: reflect.TypeOf(0), Offset: 2},
	},
	{
		in:  `{"256":"abc"}`,
		ptr: new(map[uint8]string),
		err: &UnmarshalTypeError{Value: "number 256", Type: reflect.TypeOf(uint8(0)), Offset: 2},
	},
	{
		in:  `{"128":"abc"}`,
		ptr: new(map[int8]string),
		err: &UnmarshalTypeError{Value: "number 128", Type: reflect.TypeOf(int8(0)), Offset: 2},
	},
	{
		in:  `{"-1":"abc"}`,
		ptr: new(map[uint8]string),
		err: &UnmarshalTypeError{Value: "number -1", Type: reflect.TypeOf(uint8(0)), Offset: 2},
	},

	// Overwriting of data.
	// This is different from package xml, but it's what we've always done.
	// Now documented and tested.
	{in: `[2]`, ptr: sliceAddr([]int{1}), out: []int{2}},
	{in: `{"key": 2}`, ptr: mapAddr(map[string]int{"old": 0, "key": 1}), out: map[string]int{"key": 2}},

	{
		in: `{
			"Level0": 1,
			"Level1b": 2,
			"Level1c": 3,
			"x": 4,
			"Level1a": 5,
			"LEVEL1B": 6,
			"e": {
				"Level1a": 8,
				"Level1b": 9,
				"Level1c": 10,
				"Level1d": 11,
				"x": 12
			},
			"Loop1": 13,
			"Loop2": 14,
			"X": 15,
			"Y": 16,
			"Z": 17,
			"Q": 18
		}`,
		ptr: new(Top),
		out: Top{
			Level0: 1,
			Embed0: Embed0{
				Level1b: 2,
				Level1c: 3,
			},
			Embed0a: &Embed0a{
				Level1a: 5,
				Level1b: 6,
			},
			Embed0b: &Embed0b{
				Level1a: 8,
				Level1b: 9,
				Level1c: 10,
				Level1d: 11,
				Level1e: 12,
			},
			Loop: Loop{
				Loop1: 13,
				Loop2: 14,
			},
			Embed0p: Embed0p{
				Point: image.Point{X: 15, Y: 16},
			},
			Embed0q: Embed0q{
				Point: Point{Z: 17},
			},
			embed: embed{
				Q: 18,
			},
		},
	},
	{
		in:  `{"hello": 1}`,
		ptr: new(Ambig),
		out: Ambig{First: 1},
	},

	{
		in:  `{"X": 1,"Y":2}`,
		ptr: new(S5),
		out: S5{S8: S8{S9: S9{Y: 2}}},
	},
	{
		in:  `{"X": 1,"Y":2}`,
		ptr: new(S5),
		err: fmt.Errorf("json: unknown field \"X\""),
		disallowUnknownFields: true,
	},
	{
		in:  `{"X": 1,"Y":2}`,
		ptr: new(S10),
		out: S10{S13: S13{S8: S8{S9: S9{Y: 2}}}},
	},
	{
		in:  `{"X": 1,"Y":2}`,
		ptr: new(S10),
		err: fmt.Errorf("json: unknown field \"X\""),
		disallowUnknownFields: true,
	},

	// invalid UTF-8 is coerced to valid UTF-8.
	{
		in:  "\"hello\xffworld\"",
		ptr: new(string),
		out: "hello\ufffdworld",
	},
	{
		in:  "\"hello\xc2\xc2world\"",
		ptr: new(string),
		out: "hello\ufffd\ufffdworld",
	},
	{
		in:  "\"hello\xc2\xffworld\"",
		ptr: new(string),
		out: "hello\ufffd\ufffdworld",
	},
	{
		in:  "\"hello\\ud800world\"",
		ptr: new(string),
		out: "hello\ufffdworld",
	},
	{
		in:  "\"hello\\ud800\\ud800world\"",
		ptr: new(string),
		out: "hello\ufffd\ufffdworld",
	},
	{
		in:  "\"hello\\ud800\\ud800world\"",
		ptr: new(string),
		out: "hello\ufffd\ufffdworld",
	},
	{
		in:  "\"hello\xed\xa0\x80\xed\xb0\x80world\"",
		ptr: new(string),
		out: "hello\ufffd\ufffd\ufffd\ufffd\ufffd\ufffdworld",
	},

	// issue 8305
	{
		in:  `{"2009-11-10T23:00:00Z": "hello world"}`,
		ptr: &map[Point]string{},
		err: &UnmarshalTypeError{Value: "map", Type: reflect.TypeOf(map[Point]string{}), Offset: 1},
	},
	{
		in:  `{"asdf": "hello world"}`,
		ptr: &map[unmarshaler]string{},
		err: &UnmarshalTypeError{Value: "map", Type: reflect.TypeOf(map[unmarshaler]string{}), Offset: 1},
	},

	// related to issue 13783.
	// Go 1.7 changed marshaling a slice of typed byte to use the methods on the byte type,
	// similar to marshaling a slice of typed int.
	// These tests check that, assuming the byte type also has valid decoding methods,
	// either the old base64 string encoding or the new per-element encoding can be
	// successfully unmarshaled. The custom unmarshalers were accessible in earlier
	// versions of Go, even though the custom marshaler was not.
	{
		in:  `"AQID"`,
		ptr: new([]byteWithMarshalJSON),
		out: []byteWithMarshalJSON{1, 2, 3},
	},
	{
		in:     `["Z01","Z02","Z03"]`,
		ptr:    new([]byteWithMarshalJSON),
		out:    []byteWithMarshalJSON{1, 2, 3},
		golden: true,
	},
	{
		in:  `"AQID"`,
		ptr: new([]byteWithPtrMarshalJSON),
		out: []byteWithPtrMarshalJSON{1, 2, 3},
	},
	{
		in:     `["Z01","Z02","Z03"]`,
		ptr:    new([]byteWithPtrMarshalJSON),
		out:    []byteWithPtrMarshalJSON{1, 2, 3},
		golden: true,
	},

	// ints work with the marshaler but not the base64 []byte case
	{
		in:     `["Z01","Z02","Z03"]`,
		ptr:    new([]intWithMarshalJSON),
		out:    []intWithMarshalJSON{1, 2, 3},
		golden: true,
	},
	{
		in:     `["Z01","Z02","Z03"]`,
		ptr:    new([]intWithPtrMarshalJSON),
		out:    []intWithPtrMarshalJSON{1, 2, 3},
		golden: true,
	},

	{in: `0.000001`, ptr: new(float64), out: 0.000001, golden: true},
	{in: `1e-7`, ptr: new(float64), out: 1e-7, golden: true},
	{in: `100000000000000000000`, ptr: new(float64), out: 100000000000000000000.0, golden: true},
	{in: `1e+21`, ptr: new(float64), out: 1e21, golden: true},
	{in: `-0.000001`, ptr: new(float64), out: -0.000001, golden: true},
	{in: `-1e-7`, ptr: new(float64), out: -1e-7, golden: true},
	{in: `-100000000000000000000`, ptr: new(float64), out: -100000000000000000000.0, golden: true},
	{in: `-1e+21`, ptr: new(float64), out: -1e21, golden: true},
	{in: `999999999999999900000`, ptr: new(float64), out: 999999999999999900000.0, golden: true},
	{in: `9007199254740992`, ptr: new(float64), out: 9007199254740992.0, golden: true},
	{in: `9007199254740993`, ptr: new(float64), out: 9007199254740992.0, golden: false},

	{
		in:  `{"V": {"F2": "hello"}}`,
		ptr: new(VOuter),
		err: &UnmarshalTypeError{
			Value:  "string",
			Struct: "V",
			Field:  "F2",
			Type:   reflect.TypeOf(int32(0)),
			Offset: 20,
		},
	},
	{
		in:  `{"V": {"F4": {}, "F2": "hello"}}`,
		ptr: new(VOuter),
		err: &UnmarshalTypeError{
			Value:  "string",
			Struct: "V",
			Field:  "F2",
			Type:   reflect.TypeOf(int32(0)),
			Offset: 30,
		},
	},

	// issue 15146.
	// invalid inputs in wrongStringTests below.
	{in: `{"B":"true"}`, ptr: new(B), out: B{true}, golden: true},
	{in: `{"B":"false"}`, ptr: new(B), out: B{false}, golden: true},
	{in: `{"B": "maybe"}`, ptr: new(B), err: errors.New(`json: invalid use of ,string struct tag, trying to unmarshal "maybe" into bool`)},
	{in: `{"B": "tru"}`, ptr: new(B), err: errors.New(`json: invalid use of ,string struct tag, trying to unmarshal "tru" into bool`)},
	{in: `{"B": "False"}`, ptr: new(B), err: errors.New(`json: invalid use of ,string struct tag, trying to unmarshal "False" into bool`)},
	{in: `{"B": "null"}`, ptr: new(B), out: B{false}},
	{in: `{"B": "nul"}`, ptr: new(B), err: errors.New(`json: invalid use of ,string struct tag, trying to unmarshal "nul" into bool`)},

	// additional tests for useStrict
	{
		in: `{
			"Level0": 1,
			"Level1b": 2,
			"Level1c": 3,
			"x": 4,
			"Level1a": 5,
			"LEVEL1B": 6,
			"e": {
				"Level1a": 8,
				"Level1b": 9,
				"Level1c": 10,
				"Level1d": 11,
				"x": 12
			},
			"Loop1": 13,
			"Loop2": 14,
			"X": 15,
			"Y": 16,
			"Z": 17,
			"Q": 18,
			"extra": true
		}`,
		ptr: new(Top),
		err: fmt.Errorf("json: unknown field \"extra\""),
		disallowUnknownFields: true,
	},
	{
		in: `{
			"Level0": 1,
			"Level1b": 2,
			"Level1c": 3,
			"x": 4,
			"Level1a": 5,
			"LEVEL1B": 6,
			"e": {
				"Level1a": 8,
				"Level1b": 9,
				"Level1c": 10,
				"Level1d": 11,
				"x": 12,
				"extra": null
			},
			"Loop1": 13,
			"Loop2": 14,
			"X": 15,
			"Y": 16,
			"Z": 17,
			"Q": 18
		}`,
		ptr: new(Top),
		err: fmt.Errorf("json: unknown field \"extra\""),
		disallowUnknownFields: true,
	},
}

func TestMarshal(t *testing.T) {
	b, err := MarshalWithSortMap(allValue)
	if err != nil {
		t.Fatalf("Marshal allValue: %v", err)
	}
	// t.Logf("Marshaled %s", b)
	if string(b) != allValueCompact {
		t.Errorf("Marshal allValueCompact")
		diff(t, b, []byte(allValueCompact))
		return
	}
	b, err = MarshalWithSortMap(pallValue)
	if err != nil {
		t.Fatalf("Marshal pallValue: %v", err)
	}
	// t.Logf("Marshaled %s", b)
	if string(b) != pallValueCompact {
		t.Errorf("Marshal pallValueCompact")
		diff(t, b, []byte(pallValueCompact))
		return
	}
}

var badUTF8 = []struct {
	in, out string
}{
	{"hello\xffworld", `"hello\ufffdworld"`},
	{"", `""`},
	{"\xff", `"\ufffd"`},
	{"\xff\xff", `"\ufffd\ufffd"`},
	{"a\xffb", `"a\ufffdb"`},
	{"\xe6\x97\xa5\xe6\x9c\xac\xff\xaa\x9e", `"日本\ufffd\ufffd\ufffd"`},
}

func TestMarshalBadUTF8(t *testing.T) {
	for _, tt := range badUTF8 {
		b, err := Marshal(tt.in)
		if string(b) != tt.out || err != nil {
			t.Errorf("Marshal(%q) = %#q, %v, want %#q, nil", tt.in, b, err, tt.out)
		}
	}
}

func TestMarshalNumberZeroVal(t *testing.T) {
	var n Number
	out, err := Marshal(n)
	if err != nil {
		t.Fatal(err)
	}
	outStr := string(out)
	if outStr != "0" {
		t.Fatalf("Invalid zero val for Number: %q", outStr)
	}
}

func TestMarshalEmbeds(t *testing.T) {
	top := &Top{
		Level0: 1,
		Embed0: Embed0{
			Level1b: 2,
			Level1c: 3,
		},
		Embed0a: &Embed0a{
			Level1a: 5,
			Level1b: 6,
		},
		Embed0b: &Embed0b{
			Level1a: 8,
			Level1b: 9,
			Level1c: 10,
			Level1d: 11,
			Level1e: 12,
		},
		Loop: Loop{
			Loop1: 13,
			Loop2: 14,
		},
		Embed0p: Embed0p{
			Point: image.Point{X: 15, Y: 16},
		},
		Embed0q: Embed0q{
			Point: Point{Z: 17},
		},
		embed: embed{
			Q: 18,
		},
	}
	b, err := Marshal(top)
	if err != nil {
		t.Fatal(err)
	}
	want := "{\"Level0\":1,\"Level1b\":2,\"Level1c\":3,\"Level1a\":5,\"LEVEL1B\":6,\"e\":{\"Level1a\":8,\"Level1b\":9,\"Level1c\":10,\"Level1d\":11,\"x\":12},\"Loop1\":13,\"Loop2\":14,\"X\":15,\"Y\":16,\"Z\":17,\"Q\":18}"
	if string(b) != want {
		t.Errorf("Wrong marshal result.\n got: %q\nwant: %q", b, want)
	}
}

func TestUnmarshal(t *testing.T) {
	for i, tt := range unmarshalTests {
		var scan scanner
		in := []byte(tt.in)
		if err := scan.checkValid(in); err != nil {
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("#%d: %q\n%q", i, err.Error(), tt.err)
				continue
			}
		}
		if tt.ptr == nil {
			continue
		}

		// v = new(right-type)
		v := reflect.New(reflect.TypeOf(tt.ptr).Elem())
		dec := NewDecoder(bytes.NewReader(in))
		if tt.useNumber {
			dec.UseNumber()
		}
		if tt.disallowUnknownFields {
			dec.UseStrict()
		}
		if err := dec.Decode(v.Interface()); !reflect.DeepEqual(err, tt.err) {
			t.Errorf("#%d: %v, want %v", i, err, tt.err)
			continue
		} else if err != nil {
			continue
		}
		if !reflect.DeepEqual(v.Elem().Interface(), tt.out) {
			t.Errorf("#%d: mismatch\nhave: %#+v\nwant: %#+v", i, v.Elem().Interface(), tt.out)
			data, _ := Marshal(v.Elem().Interface())
			println(string(data))
			data, _ = Marshal(tt.out)
			println(string(data))
			continue
		}

		// Check round trip also decodes correctly.
		if tt.err == nil {
			enc, err := Marshal(v.Interface())
			if err != nil {
				t.Errorf("#%d: error re-marshaling: %v", i, err)
				continue
			}
			if tt.golden && !bytes.Equal(enc, in) {
				t.Errorf("#%d: remarshal mismatch:\nhave: %s\nwant: %s", i, enc, in)
			}
			vv := reflect.New(reflect.TypeOf(tt.ptr).Elem())
			dec = NewDecoder(bytes.NewReader(enc))
			if tt.useNumber {
				dec.UseNumber()
			}
			if err := dec.Decode(vv.Interface()); err != nil {
				t.Errorf("#%d: error re-unmarshaling %#q: %v", i, enc, err)
				continue
			}
			if !reflect.DeepEqual(v.Elem().Interface(), vv.Elem().Interface()) {
				t.Errorf("#%d: mismatch\nhave: %#+v\nwant: %#+v", i, v.Elem().Interface(), vv.Elem().Interface())
				t.Errorf("     In: %q", strings.Map(noSpace, string(in)))
				t.Errorf("Marshal: %q", strings.Map(noSpace, string(enc)))
				continue
			}
		}
	}
}

func TestUnmarshalMarshal(t *testing.T) {
	initBig(true)
	var v interface{}
	if err := Unmarshal(jsonBig, &v); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	b, err := MarshalWithSortMap(v)
	if err != nil {
		t.Fatalf("Marshal: %v", err)
	}
	if !bytes.Equal(jsonBig, b) {
		t.Errorf("Marshal jsonBig")
		//diff(t, b, jsonBig)
		return
	}
}

var numberTests = []struct {
	in       string
	i        int64
	intErr   string
	f        float64
	floatErr string
}{
	{in: "-1.23e1", intErr: "IntParse: parsing `-1.23e1`: invalid syntax", f: -1.23e1},
	{in: "-12", i: -12, f: -12.0},
	{in: "1e1000", intErr: "IntParse: parsing `1e1000`: invalid syntax", floatErr: "ParseFloat: parsing `1e1000`: value out of range"},
}

// Independent of Decode, basic coverage of the accessors in Number
func TestNumberAccessors(t *testing.T) {
	for _, tt := range numberTests {
		n := Number(tt.in)
		if s := n.String(); s != tt.in {
			t.Errorf("Number(%q).String() is %q", tt.in, s)
		}
		if i, err := n.Int64(); err == nil && len(tt.intErr) == 0 && i != tt.i {
			t.Errorf("Number(%q).Int64() is %d", tt.in, i)
		} else if (err == nil && len(tt.intErr) > 0) || (err != nil && err.Error() != tt.intErr) {
			t.Errorf("Number(%q).Int64() wanted error %q but got: %v", tt.in, tt.intErr, err)
		}
		if f, err := n.Float64(); err == nil && len(tt.floatErr) == 0 && f != tt.f {
			t.Errorf("Number(%q).Float64() is %g", tt.in, f)
		} else if (err == nil && len(tt.floatErr) > 0) || (err != nil && err.Error() != tt.floatErr) {
			t.Errorf("Number(%q).Float64() wanted error %q but got: %v", tt.in, tt.floatErr, err)
		}
	}
}

func TestLargeByteSlice(t *testing.T) {
	s0 := make([]byte, 2000)
	for i := range s0 {
		s0[i] = byte(i)
	}
	b, err := Marshal(s0)
	if err != nil {
		t.Fatalf("Marshal: %v", err)
	}
	var s1 []byte
	if err := Unmarshal(b, &s1); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if !bytes.Equal(s0, s1) {
		t.Errorf("Marshal large byte slice")
		diff(t, s0, s1)
	}
}

type Xint struct {
	X int
}

func TestUnmarshalInterface(t *testing.T) {
	var xint Xint
	var i interface{} = &xint
	if err := Unmarshal([]byte(`{"X":1}`), &i); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if xint.X != 1 {
		t.Fatalf("Did not write to xint : %d", xint.X)
	}
}

func TestUnmarshalPtrPtr(t *testing.T) {
	var xint Xint
	pxint := &xint
	if err := Unmarshal([]byte(`{"X":1}`), &pxint); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if xint.X != 1 {
		t.Fatalf("Did not write to xint")
	}
}

func TestEscape(t *testing.T) {
	const input = `"foobar"<html>` + " [\u2028 \u2029]"
	const expected = `"\"foobar\"\u003chtml\u003e [\u2028 \u2029]"`
	b, err := Marshal(input)
	if err != nil {
		t.Fatalf("Marshal error: %v", err)
	}
	if s := string(b); s != expected {
		t.Errorf("Encoding of [%s]:\n got [%s]\nwant [%s]", input, s, expected)
	}
}

// WrongString is a struct that's misusing the ,string modifier.
type WrongString struct {
	Message string `json:"result,string"`
}

type wrongStringTest struct {
	in, err string
}

var wrongStringTests = []wrongStringTest{
	{`{"result":"x"}`, `json: invalid use of ,string struct tag, trying to unmarshal "x" into string`},
	{`{"result":"foo"}`, `json: invalid use of ,string struct tag, trying to unmarshal "foo" into string`},
	{`{"result":"123"}`, `json: cannot unmarshal number into Go struct field WrongString.result of type string`},
	{`{"result":123}`, `json: invalid use of ,string struct tag, trying to unmarshal unquoted value into string`},
}

// If people misuse the ,string modifier, the error message should be
// helpful, telling the user that they're doing it wrong.
func TestErrorMessageFromMisusedString(t *testing.T) {
	for n, tt := range wrongStringTests {
		r := strings.NewReader(tt.in)
		var s WrongString
		err := NewDecoder(r).Decode(&s)
		got := fmt.Sprintf("%v", err)
		if got != tt.err {
			t.Errorf("%d. got err = %q, want %q", n, got, tt.err)
		}
	}
}

func noSpace(c rune) rune {
	if whitespace&(1<<uint(c)) != 0 { //isSpace(byte(c)) { // only used for ascii
		return -1
	}
	return c
}

type All struct {
	Bool    bool
	Int     int
	Int8    int8
	Int16   int16
	Int32   int32
	Int64   int64
	Uint    uint
	Uint8   uint8
	Uint16  uint16
	Uint32  uint32
	Uint64  uint64
	Uintptr uintptr
	Float32 float32
	Float64 float64

	Foo  string `json:"bar"`
	Foo2 string `json:"bar2,dummyopt"`

	IntStr     int64   `json:",string"`
	UintptrStr uintptr `json:",string"`

	PBool    *bool
	PInt     *int
	PInt8    *int8
	PInt16   *int16
	PInt32   *int32
	PInt64   *int64
	PUint    *uint
	PUint8   *uint8
	PUint16  *uint16
	PUint32  *uint32
	PUint64  *uint64
	PUintptr *uintptr
	PFloat32 *float32
	PFloat64 *float64

	String  string
	PString *string

	Map   map[string]Small
	MapP  map[string]*Small
	PMap  *map[string]Small
	PMapP *map[string]*Small

	EmptyMap map[string]Small
	NilMap   map[string]Small

	Slice   []Small
	SliceP  []*Small
	PSlice  *[]Small
	PSliceP *[]*Small

	EmptySlice []Small
	NilSlice   []Small

	StringSlice []string
	ByteSlice   []byte

	Small   Small
	PSmall  *Small
	PPSmall **Small

	Interface  interface{}
	PInterface *interface{}

	unexported int
}

type Small struct {
	Tag string
}

var allValue = All{
	Bool:       true,
	Int:        2,
	Int8:       3,
	Int16:      4,
	Int32:      5,
	Int64:      6,
	Uint:       7,
	Uint8:      8,
	Uint16:     9,
	Uint32:     10,
	Uint64:     11,
	Uintptr:    12,
	Float32:    14.1,
	Float64:    15.1,
	Foo:        "foo",
	Foo2:       "foo2",
	IntStr:     42,
	UintptrStr: 44,
	String:     "16",
	Map: map[string]Small{
		"17": {Tag: "tag17"},
		"18": {Tag: "tag18"},
	},
	MapP: map[string]*Small{
		"19": {Tag: "tag19"},
		"20": nil,
	},
	EmptyMap:    map[string]Small{},
	Slice:       []Small{{Tag: "tag20"}, {Tag: "tag21"}},
	SliceP:      []*Small{{Tag: "tag22"}, nil, {Tag: "tag23"}},
	EmptySlice:  []Small{},
	StringSlice: []string{"str24", "str25", "str26"},
	ByteSlice:   []byte{27, 28, 29},
	Small:       Small{Tag: "tag30"},
	PSmall:      &Small{Tag: "tag31"},
	Interface:   5.2,
}

var pallValue = All{
	PBool:      &allValue.Bool,
	PInt:       &allValue.Int,
	PInt8:      &allValue.Int8,
	PInt16:     &allValue.Int16,
	PInt32:     &allValue.Int32,
	PInt64:     &allValue.Int64,
	PUint:      &allValue.Uint,
	PUint8:     &allValue.Uint8,
	PUint16:    &allValue.Uint16,
	PUint32:    &allValue.Uint32,
	PUint64:    &allValue.Uint64,
	PUintptr:   &allValue.Uintptr,
	PFloat32:   &allValue.Float32,
	PFloat64:   &allValue.Float64,
	PString:    &allValue.String,
	PMap:       &allValue.Map,
	PMapP:      &allValue.MapP,
	PSlice:     &allValue.Slice,
	PSliceP:    &allValue.SliceP,
	PPSmall:    &allValue.PSmall,
	PInterface: &allValue.Interface,
}

var allValueIndent = `{
	"Bool": true,
	"Int": 2,
	"Int8": 3,
	"Int16": 4,
	"Int32": 5,
	"Int64": 6,
	"Uint": 7,
	"Uint8": 8,
	"Uint16": 9,
	"Uint32": 10,
	"Uint64": 11,
	"Uintptr": 12,
	"Float32": 14.1,
	"Float64": 15.1,
	"bar": "foo",
	"bar2": "foo2",
	"IntStr": "42",
	"UintptrStr": "44",
	"PBool": null,
	"PInt": null,
	"PInt8": null,
	"PInt16": null,
	"PInt32": null,
	"PInt64": null,
	"PUint": null,
	"PUint8": null,
	"PUint16": null,
	"PUint32": null,
	"PUint64": null,
	"PUintptr": null,
	"PFloat32": null,
	"PFloat64": null,
	"String": "16",
	"PString": null,
	"Map": {
		"17": {
			"Tag": "tag17"
		},
		"18": {
			"Tag": "tag18"
		}
	},
	"MapP": {
		"19": {
			"Tag": "tag19"
		},
		"20": null
	},
	"PMap": null,
	"PMapP": null,
	"EmptyMap": {},
	"NilMap": null,
	"Slice": [
		{
			"Tag": "tag20"
		},
		{
			"Tag": "tag21"
		}
	],
	"SliceP": [
		{
			"Tag": "tag22"
		},
		null,
		{
			"Tag": "tag23"
		}
	],
	"PSlice": null,
	"PSliceP": null,
	"EmptySlice": [],
	"NilSlice": null,
	"StringSlice": [
		"str24",
		"str25",
		"str26"
	],
	"ByteSlice": "Gxwd",
	"Small": {
		"Tag": "tag30"
	},
	"PSmall": {
		"Tag": "tag31"
	},
	"PPSmall": null,
	"Interface": 5.2,
	"PInterface": null
}`

var allValueCompact = strings.Map(noSpace, allValueIndent)

var pallValueIndent = `{
	"Bool": false,
	"Int": 0,
	"Int8": 0,
	"Int16": 0,
	"Int32": 0,
	"Int64": 0,
	"Uint": 0,
	"Uint8": 0,
	"Uint16": 0,
	"Uint32": 0,
	"Uint64": 0,
	"Uintptr": 0,
	"Float32": 0,
	"Float64": 0,
	"bar": "",
	"bar2": "",
        "IntStr": "0",
	"UintptrStr": "0",
	"PBool": true,
	"PInt": 2,
	"PInt8": 3,
	"PInt16": 4,
	"PInt32": 5,
	"PInt64": 6,
	"PUint": 7,
	"PUint8": 8,
	"PUint16": 9,
	"PUint32": 10,
	"PUint64": 11,
	"PUintptr": 12,
	"PFloat32": 14.1,
	"PFloat64": 15.1,
	"String": "",
	"PString": "16",
	"Map": null,
	"MapP": null,
	"PMap": {
		"17": {
			"Tag": "tag17"
		},
		"18": {
			"Tag": "tag18"
		}
	},
	"PMapP": {
		"19": {
			"Tag": "tag19"
		},
		"20": null
	},
	"EmptyMap": null,
	"NilMap": null,
	"Slice": null,
	"SliceP": null,
	"PSlice": [
		{
			"Tag": "tag20"
		},
		{
			"Tag": "tag21"
		}
	],
	"PSliceP": [
		{
			"Tag": "tag22"
		},
		null,
		{
			"Tag": "tag23"
		}
	],
	"EmptySlice": null,
	"NilSlice": null,
	"StringSlice": null,
	"ByteSlice": null,
	"Small": {
		"Tag": ""
	},
	"PSmall": null,
	"PPSmall": {
		"Tag": "tag31"
	},
	"Interface": null,
	"PInterface": 5.2
}`

var pallValueCompact = strings.Map(noSpace, pallValueIndent)

func TestRefUnmarshal(t *testing.T) {
	type S struct {
		// Ref is defined in encode_test.go.
		R0 Ref
		R1 *Ref
	}
	want := S{
		R0: 12,
		R1: new(Ref),
	}
	*want.R1 = 12

	var got S
	if err := Unmarshal([]byte(`{"R0":"ref","R1":"ref"}`), &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("got %+v, want %+v", got, want)
	}
}

// Test that the empty string doesn't panic decoding when ,string is specified
// Issue 3450
func TestEmptyString(t *testing.T) {
	type T2 struct {
		Number1 int `json:",string"`
		Number2 int `json:",string"`
	}
	data := `{"Number1":"1", "Number2":""}`
	dec := NewDecoder(strings.NewReader(data))
	var t2 T2
	err := dec.Decode(&t2)
	if err == nil {
		t.Fatal("Decode: did not return error")
	}
	if t2.Number1 != 1 {
		t.Fatal("Decode: did not set Number1")
	}
}

// Test that a null for ,string is not replaced with the previous isBasic string (issue 7046).
// It should also not be an error (issue 2540, issue 8587).
func TestNullString(t *testing.T) {
	type T struct {
		A int  `json:",string"`
		B int  `json:",string"`
		C *int `json:",string"`
	}
	data := []byte(`{"A": "1", "B": null, "C": null}`)
	var s T
	s.B = 1
	s.C = new(int)
	*s.C = 2
	err := Unmarshal(data, &s)
	if err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if s.B != 1 || s.C != nil {
		t.Fatalf("after Unmarshal, s.B=%d, s.C=%p, want 1, nil", s.B, s.C)
	}
}

func intp(x int) *int {
	p := new(int)
	*p = x
	return p
}

func intpp(x *int) **int {
	pp := new(*int)
	*pp = x
	return pp
}

var interfaceSetTests = []struct {
	pre  interface{}
	json string
	post interface{}
}{
	{"foo", `"bar"`, "bar"},
	{"foo", `2`, 2.0},
	{"foo", `true`, true},
	{"foo", `null`, nil},

	{nil, `null`, nil},
	{new(int), `null`, nil},
	{(*int)(nil), `null`, nil},
	{new(*int), `null`, new(*int)},
	{(**int)(nil), `null`, nil},
	{intp(1), `null`, nil},
	{intpp(nil), `null`, intpp(nil)},
	{intpp(intp(1)), `null`, intpp(nil)},
}

func TestInterfaceSet(t *testing.T) {
	for _, tt := range interfaceSetTests {
		b := struct{ X interface{} }{tt.pre}
		blob := `{"X":` + tt.json + `}`
		if err := Unmarshal([]byte(blob), &b); err != nil {
			t.Errorf("Unmarshal %#q: %v", blob, err)
			continue
		}
		if !reflect.DeepEqual(b.X, tt.post) {
			t.Errorf("Unmarshal %#q into %#v: X=%#v, want %#v", blob, tt.pre, b.X, tt.post)
		}
	}
}

type NullTest struct {
	Bool      bool
	Int       int
	Int8      int8
	Int16     int16
	Int32     int32
	Int64     int64
	Uint      uint
	Uint8     uint8
	Uint16    uint16
	Uint32    uint32
	Uint64    uint64
	Float32   float32
	Float64   float64
	String    string
	PBool     *bool
	Map       map[string]string
	Slice     []string
	Interface interface{}

	PRaw    *RawMessage
	PTime   *time.Time
	PBigInt *big.Int
	PBuffer *bytes.Buffer // has methods, just not relevant ones
	PStruct *struct{}

	Raw    RawMessage
	Time   time.Time
	BigInt big.Int
	Buffer bytes.Buffer
	Struct struct{}
}

// JSON null values should be ignored for primitives and string values instead of resulting in an error.
// Issue 2540
func TestUnmarshalNulls(t *testing.T) {
	// Unmarshal docs:
	// The JSON null value unmarshals into an interface, map, pointer, or slice
	// by setting that Go value to nil. Because null is often used in JSON to mean
	// ``not present,'' unmarshaling a JSON null into any other Go type has no effect
	// on the value and produces no error.

	jsonData := []byte(`{
				"Bool"    : null,
				"Int"     : null,
				"Int8"    : null,
				"Int16"   : null,
				"Int32"   : null,
				"Int64"   : null,
				"Uint"    : null,
				"Uint8"   : null,
				"Uint16"  : null,
				"Uint32"  : null,
				"Uint64"  : null,
				"Float32" : null,
				"Float64" : null,
				"String"  : null,
				"PBool": null,
				"Map": null,
				"Slice": null,
				"Interface": null,
				"PRaw": null,
				"PTime": null,
				"PBigInt": null,
				"PText": null,
				"PBuffer": null,
				"PStruct": null,
				"Raw": null,
				"Time": null,
				"BigInt": null,
				"Text": null,
				"Buffer": null,
				"Struct": null
			}`)
	nulls := NullTest{
		Bool:      true,
		Int:       2,
		Int8:      3,
		Int16:     4,
		Int32:     5,
		Int64:     6,
		Uint:      7,
		Uint8:     8,
		Uint16:    9,
		Uint32:    10,
		Uint64:    11,
		Float32:   12.1,
		Float64:   13.1,
		String:    "14",
		PBool:     new(bool),
		Map:       map[string]string{},
		Slice:     []string{},
		Interface: new(MustNotUnmarshalJSON),
		PRaw:      new(RawMessage),
		PTime:     new(time.Time),
		PBigInt:   new(big.Int),
		PStruct:   new(struct{}),
		PBuffer:   new(bytes.Buffer),
		Raw:       RawMessage("123"),
		Time:      time.Unix(123456789, 0),
		BigInt:    *big.NewInt(123),
	}

	before := nulls.Time.String()

	err := Unmarshal(jsonData, &nulls)
	if err != nil {
		t.Errorf("Unmarshal of null values failed: %v", err)
	}
	if !nulls.Bool || nulls.Int != 2 || nulls.Int8 != 3 || nulls.Int16 != 4 || nulls.Int32 != 5 || nulls.Int64 != 6 ||
		nulls.Uint != 7 || nulls.Uint8 != 8 || nulls.Uint16 != 9 || nulls.Uint32 != 10 || nulls.Uint64 != 11 ||
		nulls.Float32 != 12.1 || nulls.Float64 != 13.1 || nulls.String != "14" {
		t.Errorf("Unmarshal of null values affected primitives")
	}

	if nulls.PBool != nil {
		t.Errorf("Unmarshal of null did not clear nulls.PBool")
	}
	if nulls.Map != nil {
		t.Errorf("Unmarshal of null did not clear nulls.Map")
	}
	if nulls.Slice != nil {
		t.Errorf("Unmarshal of null did not clear nulls.Slice")
	}
	if nulls.Interface != nil {
		t.Errorf("Unmarshal of null did not clear nulls.Interface")
	}
	if nulls.PRaw != nil {
		t.Errorf("Unmarshal of null did not clear nulls.PRaw")
	}
	if nulls.PTime != nil {
		t.Errorf("Unmarshal of null did not clear nulls.PTime")
	}
	if nulls.PBigInt != nil {
		t.Errorf("Unmarshal of null did not clear nulls.PBigInt")
	}

	if nulls.PBuffer != nil {
		t.Errorf("Unmarshal of null did not clear nulls.PBuffer")
	}
	if nulls.PStruct != nil {
		t.Errorf("Unmarshal of null did not clear nulls.PStruct")
	}

	if string(nulls.Raw) != "null" {
		t.Errorf("Unmarshal of RawMessage null did not record null: %v", string(nulls.Raw))
	}
	if nulls.Time.String() != before {
		t.Errorf("Unmarshal of time.Time null set time to %v", nulls.Time.String())
	}
	if nulls.BigInt.String() != "123" {
		t.Errorf("Unmarshal of big.Int null set int to %v", nulls.BigInt.String())
	}
}

type MustNotUnmarshalJSON struct{}

func (x MustNotUnmarshalJSON) UnmarshalJSON(data []byte) error {
	return errors.New("MustNotUnmarshalJSON was used")
}

func TestStringKind(t *testing.T) {
	type stringKind string

	var m1, m2 map[stringKind]int
	m1 = map[stringKind]int{
		"foo": 42,
	}

	data, err := Marshal(m1)
	if err != nil {
		t.Errorf("Unexpected error marshaling: %v", err)
	}

	err = Unmarshal(data, &m2)
	if err != nil {
		t.Errorf("Unexpected error unmarshaling: %v", err)
	}

	if !reflect.DeepEqual(m1, m2) {
		t.Error("Items should be equal after encoding and then decoding")
	}
}

// Custom types with []byte as underlying type could not be marshaled
// and then unmarshaled.
// Issue 8962.
func TestByteKind(t *testing.T) {
	type byteKind []byte

	a := byteKind("hello")

	data, err := Marshal(a)
	if err != nil {
		t.Error(err)
	}
	var b byteKind
	err = Unmarshal(data, &b)
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(a, b) {
		t.Errorf("expected %v == %v", a, b)
	}
}

// The fix for issue 8962 introduced a regression.
// Issue 12921.
func TestSliceOfCustomByte(t *testing.T) {
	type Uint8 uint8

	a := []Uint8("hello")

	data, err := Marshal(a)
	if err != nil {
		t.Fatal(err)
	}
	var b []Uint8
	err = Unmarshal(data, &b)
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(a, b) {
		t.Fatalf("expected %v == %v", a, b)
	}
}

var decodeTypeErrorTests = []struct {
	dest interface{}
	src  string
}{
	{new(string), `{"user": "name"}`}, // issue 4628.
	{new(error), `{}`},                // issue 4222
	{new(error), `[]`},
	{new(error), `""`},
	{new(error), `123`},
	{new(error), `true`},
}

func TestUnmarshalTypeError(t *testing.T) {
	for _, item := range decodeTypeErrorTests {
		err := Unmarshal([]byte(item.src), item.dest)
		if _, ok := err.(*UnmarshalTypeError); !ok {
			t.Errorf("expected type error for Unmarshal(%q, type %T): got %T",
				item.src, item.dest, err)
		}
	}
}

var unmarshalSyntaxTests = []string{
	"tru",
	"fals",
	"nul",
	"123e",
	`"hello`,
	`[1,2,3`,
	`{"key":1`,
	`{"key":1,`,
}

func TestUnmarshalSyntax(t *testing.T) {
	var x interface{}
	for _, src := range unmarshalSyntaxTests {
		err := Unmarshal([]byte(src), &x)
		if _, ok := err.(*SyntaxError); !ok {
			t.Errorf("expected syntax error for Unmarshal(%q): got %T", src, err)
		}
	}
}

// Test handling of unexported fields that should be ignored.
// Issue 4660
type unexportedFields struct {
	Name string
	m    map[string]interface{} `json:"-"`
	m2   map[string]interface{} `json:"abcd"`
}

func TestUnmarshalUnexported(t *testing.T) {
	input := `{"Name": "Bob", "m": {"x": 123}, "m2": {"y": 456}, "abcd": {"z": 789}}`
	want := &unexportedFields{Name: "Bob"}

	out := &unexportedFields{}
	err := Unmarshal([]byte(input), out)
	if err != nil {
		t.Errorf("got error %v, expected nil", err)
	}
	if !reflect.DeepEqual(out, want) {
		t.Errorf("got %q, want %q", out, want)
	}
}

// Time3339 is a time.Time which encodes to and from JSON
// as an RFC 3339 time in UTC.
type Time3339 time.Time

func (t *Time3339) UnmarshalJSON(b []byte) error {
	if len(b) < 2 || b[0] != '"' || b[len(b)-1] != '"' {
		return fmt.Errorf("types: failed to unmarshal non-string value %q as an RFC 3339 time", b)
	}
	tm, err := time.Parse(time.RFC3339, string(b[1:len(b)-1]))
	if err != nil {
		return err
	}
	*t = Time3339(tm)
	return nil
}

func TestUnmarshalJSONLiteralError(t *testing.T) {
	var t3 Time3339
	err := Unmarshal([]byte(`"0000-00-00T00:00:00Z"`), &t3)
	if err == nil {
		t.Fatalf("expected error; got time %v", time.Time(t3))
	}
	if !strings.Contains(err.Error(), "range") {
		t.Errorf("got err = %v; want out of range error", err)
	}
}

// Test that extra object elements in an array do not result in a
// "data changing underfoot" error.
// Issue 3717
func TestSkipArrayObjects(t *testing.T) {
	json := `[{}]`
	var dest [0]interface{}

	err := Unmarshal([]byte(json), &dest)
	if err != nil {
		t.Errorf("got error %q, want nil", err)
	}
}

// Test semantics of pre-filled struct fields and pre-filled map fields.
// Issue 4900.
func TestPrefilled(t *testing.T) {
	ptrToMap := func(m map[string]interface{}) *map[string]interface{} { return &m }

	// Values here change, cannot reuse table across runs.
	var prefillTests = []struct {
		in  string
		ptr interface{}
		out interface{}
	}{
		{
			in:  `{"X": 1, "Y": 2}`,
			ptr: &XYZ{X: float32(3), Y: int16(4), Z: 1.5},
			out: &XYZ{X: float64(1), Y: float64(2), Z: 1.5},
		},
		{
			in:  `{"X": 1, "Y": 2}`,
			ptr: ptrToMap(map[string]interface{}{"X": float32(3), "Y": int16(4), "Z": 1.5}),
			out: ptrToMap(map[string]interface{}{"X": float64(1), "Y": float64(2), "Z": 1.5}),
		},
	}

	for _, tt := range prefillTests {
		ptrstr := fmt.Sprintf("%v", tt.ptr)
		err := Unmarshal([]byte(tt.in), tt.ptr) // tt.ptr edited here
		if err != nil {
			t.Errorf("Unmarshal: %v", err)
		}
		if !reflect.DeepEqual(tt.ptr, tt.out) {
			t.Errorf("Unmarshal(%#q, %s): have %v, want %v", tt.in, ptrstr, tt.ptr, tt.out)
		}
	}
}

var invalidUnmarshalTests = []struct {
	v    interface{}
	want string
}{
	{nil, "json: Unmarshal(nil)"},
	{struct{}{}, "json: Unmarshal(non-pointer struct {})"},
	{(*int)(nil), "json: Unmarshal(nil *int)"},
}

func TestInvalidUnmarshal(t *testing.T) {
	buf := []byte(`{"a":"1"}`)
	for _, tt := range invalidUnmarshalTests {
		err := Unmarshal(buf, tt.v)
		if err == nil {
			t.Errorf("Unmarshal expecting error, got nil")
			continue
		}
		if got := err.Error(); got != tt.want {
			t.Errorf("Unmarshal = %q; want %q", got, tt.want)
		}
	}
}

var invalidUnmarshalTextTests = []struct {
	v    interface{}
	want string
}{
	{nil, "json: Unmarshal(nil)"},
	{struct{}{}, "json: Unmarshal(non-pointer struct {})"},
	{(*int)(nil), "json: Unmarshal(nil *int)"},
	{new(net.IP), "json: cannot unmarshal number into Go value of type net.IP"},
}

func TestInvalidUnmarshalText(t *testing.T) {
	buf := []byte(`123`)
	for _, tt := range invalidUnmarshalTextTests {
		err := Unmarshal(buf, tt.v)
		if err == nil {
			t.Errorf("Unmarshal expecting error, got nil")
			continue
		}
		if got := err.Error(); got != tt.want {
			t.Errorf("Unmarshal = %q; want %q", got, tt.want)
		}
	}
}

// Test that string option is ignored for invalid types.
// Issue 9812.
func TestInvalidStringOption(t *testing.T) {
	num := 0
	item := struct {
		T time.Time         `json:",string"`
		M map[string]string `json:",string"`
		S []string          `json:",string"`
		A [1]string         `json:",string"`
		I interface{}       `json:",string"`
		P *int              `json:",string"`
	}{M: make(map[string]string), S: make([]string, 0), I: num, P: &num}

	data, err := Marshal(item)
	if err != nil {
		t.Fatalf("Marshal: %v", err)
	}

	err = Unmarshal(data, &item)
	if err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
}

// Test unmarshal behavior with regards to embedded pointers to unexported structs.
// If unallocated, this returns an error because unmarshal cannot set the field.
// Issue 21357.
func TestUnmarshalEmbeddedPointerUnexported(t *testing.T) {
	type (
		embed1 struct{ Q int }
		embed2 struct{ Q int }
		embed3 struct {
			Q int64 `json:",string"`
		}
		S1 struct {
			*embed1
			R int
		}
		S2 struct {
			*embed1
			Q int
		}
		S3 struct {
			embed1
			R int
		}
		S4 struct {
			*embed1
			embed2
		}
		S5 struct {
			*embed3
			R int
		}
	)

	tests := []struct {
		in  string
		ptr interface{}
		out interface{}
		err error
	}{{
		// Error since we cannot set S1.embed1, but still able to set S1.R.
		in:  `{"R":2,"Q":1}`,
		ptr: new(S1),
		out: &S1{R: 2},
		err: fmt.Errorf("json: cannot set embedded pointer to unexported struct: json.embed1"),
	}, {
		// The top level Q field takes precedence.
		in:  `{"Q":1}`,
		ptr: new(S2),
		out: &S2{Q: 1},
	}, {
		// No issue with non-pointer variant.
		in:  `{"R":2,"Q":1}`,
		ptr: new(S3),
		out: &S3{embed1: embed1{Q: 1}, R: 2},
	}, {
		// No error since both embedded structs have field R, which annihilate each other.
		// Thus, no attempt is made at setting S4.embed1.
		in:  `{"R":2}`,
		ptr: new(S4),
		out: new(S4),
	}, {
		// Error since we cannot set S5.embed1, but still able to set S5.R.
		in:  `{"R":2,"Q":1}`,
		ptr: new(S5),
		out: &S5{R: 2},
		err: fmt.Errorf("json: cannot set embedded pointer to unexported struct: json.embed3"),
	}}

	for i, tt := range tests {
		err := Unmarshal([]byte(tt.in), tt.ptr)
		if !reflect.DeepEqual(err, tt.err) {
			t.Errorf("#%d: %v, want %v", i, err, tt.err)
		}
		if !reflect.DeepEqual(tt.ptr, tt.out) {
			t.Errorf("#%d: mismatch\ngot:  %#+v\nwant: %#+v", i, tt.ptr, tt.out)
		}
	}
}

var foldTests = []struct {
	fn   func(s, t []byte) bool
	s, t string
	want bool
}{
	{equalFoldRight, "", "", true},
	{equalFoldRight, "a", "a", true},
	{equalFoldRight, "", "a", false},
	{equalFoldRight, "a", "", false},
	{equalFoldRight, "a", "A", true},
	{equalFoldRight, "AB", "ab", true},
	{equalFoldRight, "AB", "ac", false},
	{equalFoldRight, "sbkKc", "ſbKKc", true},
	{equalFoldRight, "SbKkc", "ſbKKc", true},
	{equalFoldRight, "SbKkc", "ſbKK", false},
	{equalFoldRight, "e", "é", false},
	{equalFoldRight, "s", "S", true},

	{simpleLetterEqualFold, "", "", true},
	{simpleLetterEqualFold, "abc", "abc", true},
	{simpleLetterEqualFold, "abc", "ABC", true},
	{simpleLetterEqualFold, "abc", "ABCD", false},
	{simpleLetterEqualFold, "abc", "xxx", false},

	{asciiEqualFold, "a_B", "A_b", true},
	{asciiEqualFold, "aa@", "aa`", false}, // verify 0x40 and 0x60 aren't case-equivalent
}

func TestFold(t *testing.T) {
	for i, tt := range foldTests {
		if got := tt.fn([]byte(tt.s), []byte(tt.t)); got != tt.want {
			t.Errorf("%d. %q, %q = %v; want %v", i, tt.s, tt.t, got, tt.want)
		}
		truth := strings.EqualFold(tt.s, tt.t)
		if truth != tt.want {
			t.Errorf("strings.EqualFold doesn't agree with case %d", i)
		}
	}
}

func TestFoldAgainstUnicode(t *testing.T) {
	const bufSize = 5
	buf1 := make([]byte, 0, bufSize)
	buf2 := make([]byte, 0, bufSize)
	var runes []rune
	for i := 0x20; i <= 0x7f; i++ {
		runes = append(runes, rune(i))
	}
	runes = append(runes, kelvin, smallLongEss)

	funcs := []struct {
		name   string
		fold   func(s, t []byte) bool
		letter bool // must be ASCII letter
		simple bool // must be simple ASCII letter (not 'S' or 'K')
	}{
		{
			name: "equalFoldRight",
			fold: equalFoldRight,
		},
		{
			name:   "asciiEqualFold",
			fold:   asciiEqualFold,
			simple: true,
		},
		{
			name:   "simpleLetterEqualFold",
			fold:   simpleLetterEqualFold,
			simple: true,
			letter: true,
		},
	}

	for _, ff := range funcs {
		for _, r := range runes {
			if r >= utf8.RuneSelf {
				continue
			}
			if ff.letter && !isASCIILetter(byte(r)) {
				continue
			}
			if ff.simple && (r == 's' || r == 'S' || r == 'k' || r == 'K') {
				continue
			}
			for _, r2 := range runes {
				buf1 := append(buf1[:0], 'x')
				buf2 := append(buf2[:0], 'x')
				buf1 = buf1[:1+utf8.EncodeRune(buf1[1:bufSize], r)]
				buf2 = buf2[:1+utf8.EncodeRune(buf2[1:bufSize], r2)]
				buf1 = append(buf1, 'x')
				buf2 = append(buf2, 'x')
				want := bytes.EqualFold(buf1, buf2)
				if got := ff.fold(buf1, buf2); got != want {
					t.Errorf("%s(%q, %q) = %v; want %v", ff.name, buf1, buf2, got, want)
				}
			}
		}
	}
}

func isASCIILetter(b byte) bool {
	return ('A' <= b && b <= 'Z') || ('a' <= b && b <= 'z')
}

// Test values for the stream test.
// One of each JSON kind.
var streamTest = []interface{}{
	0.1,
	"hello",
	nil,
	true,
	false,
	[]interface{}{"a", "b", "c"},
	map[string]interface{}{"K": "Kelvin", "ß": "long s"},
	3.14, // another value to make sure something can follow map
}

var streamEncoded = `0.1
"hello"
null
true
false
["a","b","c"]
{"ß":"long s","K":"Kelvin"}
3.14
`

func TestEncoder(t *testing.T) {
	for i := 0; i <= len(streamTest); i++ {
		var buf bytes.Buffer
		enc := NewSortedMapsEncoder(&buf)
		// Check that enc.SetIndent("", "") turns off indentation.
		enc.SetIndent(">", ".")
		enc.SetIndent("", "")
		for j, v := range streamTest[0:i] {
			if err := enc.Encode(v); err != nil {
				t.Fatalf("encode #%d: %v", j, err)
			}
		}
		if have, want := buf.String(), nlines(streamEncoded, i); have != want {
			t.Errorf("encoding %d items: mismatch", i)
			diff(t, []byte(have), []byte(want))
			break
		}
	}
}

var streamEncodedIndent = `0.1
"hello"
null
true
false
[
>."a",
>."b",
>."c"
>]
{
>."ß": "long s",
>."K": "Kelvin"
>}
3.14
`

func TestEncoderIndent(t *testing.T) {
	var buf bytes.Buffer
	enc := NewSortedMapsEncoder(&buf)
	enc.SetIndent(">", ".")
	for _, v := range streamTest {
		enc.Encode(v)
	}
	if have, want := buf.String(), streamEncodedIndent; have != want {
		t.Error("indented encoding mismatch")
		diff(t, []byte(have), []byte(want))
	}
}

func TestEncoderSetEscapeHTML(t *testing.T) {
	var c C
	for _, tt := range []struct {
		name       string
		v          interface{}
		wantEscape string
		want       string
	}{
		{"c", c, `"\u003c\u0026\u003e"`, `"<&>"`},
		{`"<&>"`, "<&>", `"\u003c\u0026\u003e"`, `"<&>"`},
	} {
		var buf bytes.Buffer
		enc := NewEncoder(&buf)
		if err := enc.Encode(tt.v); err != nil {
			t.Fatalf("Encode(%s): %s", tt.name, err)
		}
		if got := strings.TrimSpace(buf.String()); got != tt.wantEscape {
			t.Errorf("Encode(%s) = %#q, want %#q", tt.name, got, tt.wantEscape)
		}
		buf.Reset()
		enc.SetEscapeHTML(false)
		if err := enc.Encode(tt.v); err != nil {
			t.Fatalf("SetEscapeHTML(false) Encode(%s): %s", tt.name, err)
		}
		if got := strings.TrimSpace(buf.String()); got != tt.want {
			t.Errorf("SetEscapeHTML(false) Encode(%s) = %#q, want %#q",
				tt.name, got, tt.want)
		}
	}
}

func TestDecoder(t *testing.T) {
	for i := 0; i <= len(streamTest); i++ {
		// Use stream without newlines as input,
		// just to stress the decoder even more.
		// Our test input does not include back-to-back numbers.
		// Otherwise stripping the newlines would
		// merge two adjacent JSON values.
		var buf bytes.Buffer
		for _, c := range nlines(streamEncoded, i) {
			if c != '\n' {
				buf.WriteRune(c)
			}
		}
		out := make([]interface{}, i)
		dec := NewDecoder(&buf)
		for j := range out {
			if err := dec.Decode(&out[j]); err != nil {
				t.Fatalf("decode #%d/%d: %v", j, i, err)
			}
		}
		if !reflect.DeepEqual(out, streamTest[0:i]) {
			t.Errorf("decoding %d items: mismatch", i)
			for j := range out {
				if !reflect.DeepEqual(out[j], streamTest[j]) {
					t.Errorf("#%d: have %v want %v", j, out[j], streamTest[j])
				}
			}
			break
		}
	}
}

func TestDecoderBuffered(t *testing.T) {
	r := strings.NewReader(`{"Name": "Gopher"} extra `)
	var m struct {
		Name string
	}
	d := NewDecoder(r)
	err := d.Decode(&m)
	if err != nil {
		t.Fatal(err)
	}
	if m.Name != "Gopher" {
		t.Errorf("Name = %q; want Gopher", m.Name)
	}
	rest, err := ioutil.ReadAll(d.Buffered())
	if err != nil {
		t.Fatal(err)
	}
	if g, w := string(rest), " extra "; g != w {
		t.Errorf("Remaining = %q; want %q", g, w)
	}
}

func nlines(s string, n int) string {
	if n <= 0 {
		return ""
	}
	for i, c := range s {
		if c == '\n' {
			if n--; n == 0 {
				return s[0 : i+1]
			}
		}
	}
	return s
}

func TestRawMessage(t *testing.T) {
	var data struct {
		X  float64
		Id *RawMessage
		Y  float32
	}
	const raw = `["\u0056",null]`
	const msg = `{"X":0.1,"Id":["\u0056",null],"Y":0.2}`
	err := Unmarshal([]byte(msg), &data)
	if err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if string([]byte(*data.Id)) != raw {
		t.Fatalf("Raw mismatch: have %#q want %#q", []byte(*data.Id), raw)
	}
	b, err := Marshal(&data)
	if err != nil {
		t.Fatalf("Marshal: %v", err)
	}
	if string(b) != msg {
		t.Fatalf("Marshal: have %#q want %#q", b, msg)
	}
}

func TestNullRawMessage(t *testing.T) {
	var data struct {
		X  float64
		Id *RawMessage
		Y  float32
	}
	data.Id = new(RawMessage)
	const msg = `{"X":0.1,"Id":null,"Y":0.2}`
	err := Unmarshal([]byte(msg), &data)
	if err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if data.Id != nil {
		t.Fatalf("Raw mismatch: have non-nil, want nil")
	}
	b, err := Marshal(&data)
	if err != nil {
		t.Fatalf("Marshal: %v", err)
	}
	if string(b) != msg {
		t.Fatalf("Marshal: have %#q want %#q", b, msg)
	}
}

var blockingTests = []string{
	`{"x": 1}`,
	`[1, 2, 3]`,
}

func TestBlocking(t *testing.T) {
	for _, enc := range blockingTests {
		r, w := net.Pipe()
		go w.Write([]byte(enc))
		var val interface{}

		// If Decode reads beyond what w.Write writes above,
		// it will block, and the test will deadlock.
		if err := NewDecoder(r).Decode(&val); err != nil {
			t.Errorf("decoding %s: %v", enc, err)
		}
		r.Close()
		w.Close()
	}
}

type tokenStreamCase struct {
	json      string
	expTokens []interface{}
}

type decodeThis struct {
	v interface{}
}

var tokenStreamCases = []tokenStreamCase{
	// streaming token cases
	{json: `10`, expTokens: []interface{}{float64(10)}},
	{json: ` [10] `, expTokens: []interface{}{
		'[', float64(10), ']'}},
	{json: ` [false,10,"b"] `, expTokens: []interface{}{
		'[', false, float64(10), "b", ']'}},
	{json: `{ "a": 1 }`, expTokens: []interface{}{
		'{', "a", float64(1), '}'}},
	{json: `{"a": 1, "b":"3"}`, expTokens: []interface{}{
		'{', "a", float64(1), "b", "3", '}'}},
	{json: ` [{"a": 1},{"a": 2}] `, expTokens: []interface{}{
		'[',
		'{', "a", float64(1), '}',
		'{', "a", float64(2), '}',
		']'}},
	{json: `{"obj": {"a": 1}}`, expTokens: []interface{}{
		'{', "obj", '{', "a", float64(1), '}',
		'}'}},
	{json: `{"obj": [{"a": 1}]}`, expTokens: []interface{}{
		'{', "obj", '[',
		'{', "a", float64(1), '}',
		']', '}'}},

	// streaming tokens with intermittent Decode()
	{json: `{ "a": 1 }`, expTokens: []interface{}{
		'{', "a",
		decodeThis{float64(1)},
		'}'}},
	{json: ` [ { "a" : 1 } ] `, expTokens: []interface{}{
		'[',
		decodeThis{map[string]interface{}{"a": float64(1)}},
		']'}},
	{json: ` [{"a": 1},{"a": 2}] `, expTokens: []interface{}{
		'[',
		decodeThis{map[string]interface{}{"a": float64(1)}},
		decodeThis{map[string]interface{}{"a": float64(2)}},
		']'}},
	{json: `{ "obj" : [ { "a" : 1 } ] }`, expTokens: []interface{}{
		'{', "obj", '[',
		decodeThis{map[string]interface{}{"a": float64(1)}},
		']', '}'}},

	{json: `{"obj": {"a": 1}}`, expTokens: []interface{}{
		'{', "obj",
		decodeThis{map[string]interface{}{"a": float64(1)}},
		'}'}},
	{json: `{"obj": [{"a": 1}]}`, expTokens: []interface{}{
		'{', "obj",
		decodeThis{[]interface{}{
			map[string]interface{}{"a": float64(1)},
		}},
		'}'}},
	{json: ` [{"a": 1} {"a": 2}] `, expTokens: []interface{}{
		'[',
		decodeThis{map[string]interface{}{"a": float64(1)}},
		decodeThis{&SyntaxError{"expected comma after array element", 11}},
	}},
	{json: `{ "` + strings.Repeat("a", 513) + `" 1 }`, expTokens: []interface{}{
		'{', strings.Repeat("a", 513),
		decodeThis{&SyntaxError{"expected colon after object key", 518}},
	}},
	{json: `{ "\a" }`, expTokens: []interface{}{
		'{',
		&SyntaxError{"invalid character 'a' in string escape code", 3},
	}},
	{json: ` \a`, expTokens: []interface{}{
		&SyntaxError{"invalid character '\\' expected ' ' or '{' or '[' or '\"' or '-' or '0' or 't' or 'f' or 'n' or '1' looking for beginning of value", 1},
	}},
}

func TestDecodeInStream(t *testing.T) {

	for ci, tcase := range tokenStreamCases {

		dec := NewDecoder(strings.NewReader(tcase.json))
		for i, expectedToken := range tcase.expTokens {

			var token interface{}
			var err error

			if dt, ok := expectedToken.(decodeThis); ok {
				expectedToken = dt.v
				err = dec.Decode(&token)
			} else {
				token, err = dec.Token()
			}
			if experr, ok := expectedToken.(error); ok {
				if err == nil || !reflect.DeepEqual(err, experr) {
					t.Errorf("case %v: Expected error %#v in %q\nbut was %#v", ci, experr, tcase.json, err)
				}
				break
			} else if err == io.EOF {
				t.Errorf("case %v: Unexpected EOF in %q", ci, tcase.json)
				break
			} else if err != nil {
				t.Errorf("case %v: Unexpected error '%#v' in %q", ci, err, tcase.json)
				break
			}
			byteToken, _ := token.([]byte)
			expByte, _ := expectedToken.([]byte)
			if !bytes.Equal(expByte, byteToken) {
				t.Errorf(`byte case %v: %q @ %v expected %T(%v) was %T(%v)`, ci, tcase.json, i, expByte, expectedToken, byteToken, token)
			}

		}
	}

}

// Test from golang.org/issue/11893
func TestHTTPDecoding(t *testing.T) {
	const raw = `{ "foo": "bar" }`

	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(raw))
	}))
	defer ts.Close()
	res, err := http.Get(ts.URL)
	if err != nil {
		log.Fatalf("GET failed: %v", err)
	}
	defer res.Body.Close()

	foo := struct {
		Foo string
	}{}

	d := NewDecoder(res.Body)
	err = d.Decode(&foo)
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	if foo.Foo != "bar" {
		t.Errorf("decoded %q; want \"bar\"", foo.Foo)
	}

	// make sure we get the EOF the second time
	err = d.Decode(&foo)
	if err != io.EOF {
		t.Errorf("err = %v; want io.EOF", err)
	}
}

var validTests = []struct {
	data string
	ok   bool
}{
	{`foo`, false},
	{`}{`, false},
	{`{]`, false},
	{`{}`, true},
	{`{"foo":"bar"}`, true},
	{`{"foo":"bar","bar":{"baz":["qux"]}}`, true},
}

func TestValid(t *testing.T) {
	for _, tt := range validTests {
		if ok := Valid([]byte(tt.data)); ok != tt.ok {
			t.Errorf("Valid(%#q) = %v, want %v", tt.data, ok, tt.ok)
		}
	}
}

// Tests of simple examples.

type example struct {
	compact string
	indent  string
}

var examples = []example{
	{`1`, `1`},
	{`{}`, `{}`},
	{`[]`, `[]`},
	{`{"":2}`, "{\n\t\"\": 2\n}"},
	{`[3]`, "[\n\t3\n]"},
	{`[1,2,3]`, "[\n\t1,\n\t2,\n\t3\n]"},
	{`{"x":1}`, "{\n\t\"x\": 1\n}"},
	{ex1, ex1i},
}

var ex1 = `[true,false,null,"x",1,1.5,0,-5e+2]`

var ex1i = `[
	true,
	false,
	null,
	"x",
	1,
	1.5,
	0,
	-5e+2
]`

func TestCompact(t *testing.T) {
	var buf bytes.Buffer
	for _, tt := range examples {
		buf.Reset()
		if err := Compact(&buf, []byte(tt.compact)); err != nil {
			t.Errorf("Compact(%#q): %v", tt.compact, err)
		} else if s := buf.String(); s != tt.compact {
			t.Errorf("Compact(%#q) = %#q, want original", tt.compact, s)
		}

		buf.Reset()
		if err := Compact(&buf, []byte(tt.indent)); err != nil {
			t.Errorf("Compact(%#q): %v", tt.indent, err)
			continue
		} else if s := buf.String(); s != tt.compact {
			t.Errorf("Compact(%#q) = %#q, want %#q", tt.indent, s, tt.compact)
		}
	}
}

func TestCompactSeparators(t *testing.T) {
	// U+2028 and U+2029 should be escaped inside strings.
	// They should not appear outside strings.
	tests := []struct {
		in, compact string
	}{
		{"{\"\u2028\": 1}", `{"\u2028":1}`},
		{"{\"\u2029\" :2}", `{"\u2029":2}`},
	}
	for _, tt := range tests {
		var buf bytes.Buffer
		if err := Compact(&buf, []byte(tt.in)); err != nil {
			t.Errorf("Compact(%q): %v", tt.in, err)
		} else if s := buf.String(); s != tt.compact {
			t.Errorf("Compact(%q) = %q, want %q", tt.in, s, tt.compact)
		}
	}
}

func TestIndent(t *testing.T) {
	var buf bytes.Buffer
	for _, tt := range examples {
		buf.Reset()
		if err := Indent(&buf, []byte(tt.indent), "", "\t"); err != nil {
			t.Errorf("Indent(%#q): %v", tt.indent, err)
		} else if s := buf.String(); s != tt.indent {
			t.Errorf("Indent(%#q) = %#q, want original", tt.indent, s)
		}

		buf.Reset()
		if err := Indent(&buf, []byte(tt.compact), "", "\t"); err != nil {
			t.Errorf("Indent(%#q): %v", tt.compact, err)
			continue
		} else if s := buf.String(); s != tt.indent {
			t.Errorf("Indent(%#q) = %#q, want %#q", tt.compact, s, tt.indent)
		}
	}
}

// Tests of a large random structure.

func TestCompactBig(t *testing.T) {
	initBig(false)
	var buf bytes.Buffer
	if err := Compact(&buf, jsonBig); err != nil {
		t.Fatalf("Compact: %v", err)
	}
	b := buf.Bytes()
	if !bytes.Equal(b, jsonBig) {
		t.Error("Compact(jsonBig) != jsonBig")
		diff(t, b, jsonBig)
		return
	}
}

func TestIndentBig(t *testing.T) {
	t.Parallel()
	initBig(false)
	var buf bytes.Buffer
	if err := Indent(&buf, jsonBig, "", "\t"); err != nil {
		t.Fatalf("Indent1: %v", err)
	}
	b := buf.Bytes()
	if len(b) == len(jsonBig) {
		// jsonBig is compact (no unnecessary spaces);
		// indenting should make it bigger
		t.Fatalf("Indent(jsonBig) did not get bigger")
	}

	// should be idempotent
	var buf1 bytes.Buffer
	if err := Indent(&buf1, b, "", "\t"); err != nil {
		t.Fatalf("Indent2: %v", err)
	}
	b1 := buf1.Bytes()
	if !bytes.Equal(b1, b) {
		t.Error("Indent(Indent(jsonBig)) != Indent(jsonBig)")
		diff(t, b1, b)
		return
	}

	// should get back to original
	buf1.Reset()
	if err := Compact(&buf1, b); err != nil {
		t.Fatalf("Compact: %v", err)
	}
	b1 = buf1.Bytes()
	if !bytes.Equal(b1, jsonBig) {
		t.Error("Compact(Indent(jsonBig)) != jsonBig")
		diff(t, b1, jsonBig)
		return
	}
}

type indentErrorTest struct {
	in  string
	err error
}

var indentErrorTests = []indentErrorTest{
	{`{"X": "foo", "Y"}`, &SyntaxError{"invalid character '}' expected ':' after object key", 17}},
	{`{"X": "foo" "Y": "bar"}`, &SyntaxError{"invalid character '\"' expected ',' or '}' after object key:value pair", 13}},
}

func TestIndentErrors(t *testing.T) {
	for i, tt := range indentErrorTests {
		slice := make([]uint8, 0)
		buf := bytes.NewBuffer(slice)
		if err := Indent(buf, []uint8(tt.in), "", ""); err != nil {
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("#%d: Indent: %#v", i, err)
				continue
			}
		}
	}
}

func TestNextValueBig(t *testing.T) {
	initBig(false)
	var scan scanner
	item, rest, err := scan.nextValue(jsonBig)
	if err != nil {
		t.Fatalf("nextValue: %s", err)
	}
	if len(item) != len(jsonBig) || &item[0] != &jsonBig[0] {
		t.Errorf("invalid item: %d %d", len(item), len(jsonBig))
	}
	if len(rest) != 0 {
		t.Errorf("invalid rest: %d", len(rest))
	}

	item, rest, err = scan.nextValue(append(jsonBig, "HELLO WORLD"...))
	if err != nil {
		t.Fatalf("nextValue extra: %s", err)
	}
	if len(item) != len(jsonBig) {
		t.Errorf("invalid item: %d %d", len(item), len(jsonBig))
	}
	if string(rest) != "HELLO WORLD" {
		t.Errorf("invalid rest: %d", len(rest))
	}
}

var benchScan scanner

func diff(t *testing.T, a, b []byte) {
	for i := 0; ; i++ {
		if i >= len(a) || i >= len(b) || a[i] != b[i] {
			j := i - 10
			if j < 0 {
				j = 0
			}
			t.Errorf("diverge at %d: «%s» vs «%s»\nFull dump:\n%s\n%s", i, trim(a[j:]), trim(b[j:]), a, b)
			return
		}
	}
}

func trim(b []byte) []byte {
	if len(b) > 20 {
		return b[0:20]
	}
	return b
}

// Generate a random JSON object.

var jsonBig []byte

func initBig(withMapSort bool) {
	n := 10000
	if testing.Short() {
		n = 100
	}
	var err error
	var b []byte
	if withMapSort {
		b, err = MarshalWithSortMap(genValue(n))
		if err != nil {
			panic(err)
		}
	} else {
		b, err = Marshal(genValue(n))
		if err != nil {
			panic(err)
		}
	}
	jsonBig = b
}

func genValue(n int) interface{} {
	if n > 1 {
		switch rand.Intn(2) {
		case 0:
			return genArray(n)
		case 1:
			return genMap(n)
		}
	}
	switch rand.Intn(3) {
	case 0:
		return rand.Intn(2) == 0
	case 1:
		return rand.NormFloat64()
	case 2:
		return genString(30)
	}
	panic("unreachable")
}

func genString(stddev float64) string {
	n := int(math.Abs(rand.NormFloat64()*stddev + stddev/2))
	c := make([]rune, n)
	for i := range c {
		f := math.Abs(rand.NormFloat64()*64 + 32)
		if f > 0x10ffff {
			f = 0x10ffff
		}
		c[i] = rune(f)
	}
	return string(c)
}

func genArray(n int) []interface{} {
	f := int(math.Abs(rand.NormFloat64()) * math.Min(10, float64(n/2)))
	if f > n {
		f = n
	}
	if f < 1 {
		f = 1
	}
	x := make([]interface{}, f)
	for i := range x {
		x[i] = genValue(((i+1)*n)/f - (i*n)/f)
	}
	return x
}

func genMap(n int) map[string]interface{} {
	f := int(math.Abs(rand.NormFloat64()) * math.Min(10, float64(n/2)))
	if f > n {
		f = n
	}
	if n > 0 && f == 0 {
		f = 1
	}
	x := make(map[string]interface{})
	for i := 0; i < f; i++ {
		x[genString(10)] = genValue(((i+1)*n)/f - (i*n)/f)
	}
	return x
}

func TestNegativeUint(t *testing.T) {
	type T struct {
		A uint  `json:",string"`
		B uint  `json:",string"`
		C *uint `json:",string"`
	}
	data := []byte(`{"A": "-1", "B": null, "C": null}`)
	var s T
	s.B = 1
	s.C = new(uint)
	*s.C = 2
	err := Unmarshal(data, &s)
	if err == nil && err.Error() != "json: cannot unmarshal negative??? unsigned integer -1 into Go struct field T.A of type uint" {
		t.Fatalf("Unmarshal should error with negative??? unsigned integer")
	}
}
