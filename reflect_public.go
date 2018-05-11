/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import "bytes"

// ReflectOn returns a new Value initialized to the concrete value stored in the interface i. ReflectOn(nil) returns the zero Value.
func ReflectOn(i interface{}) Value {
	if i == nil {
		return Value{}
	}
	// unpackEface converts the empty interface i to a Value.
	e := toIface(ptr(&i))
	// NOTE: don't read e.word until we know whether it is really a pointer or not.
	if e.Type == nil {
		return Value{}
	}
	f := Flag(e.Type.Kind())
	if e.Type.isDirectIface() {
		f |= pointerFlag // set the pointer flag
	}
	return Value{Type: e.Type, Ptr: e.word, Flag: f}
}

// TypeOf used to be exactly this function.
// TypeOf returns the reflection Type that represents the dynamic type of i.
// If i is a nil interface value, TypeOf returns nil.
func TypeOf(i interface{}) *RType {
	result := (*toIface(ptr(&i))).Type
	if result == nil {
		return nil
	}
	return result
}

// Get returns the value associated with key in the tag string.
// If there is no such key in the tag, Get returns the empty string.
// If the tag does not have the conventional format, the value
// returned by Get is unspecified. To determine whether a tag is
// explicitly set to the empty string, use Lookup.
func GetTagNamed(tag []byte, key []byte) []byte {
	v, _ := TagLookup(tag, key)
	return v
}

// Lookup returns the value associated with key in the tag string.
// If the key is present in the tag the value (which may be empty)
// is returned. Otherwise the returned value will be the empty string.
// The ok return value reports whether the value was explicitly set in
// the tag string. If the tag does not have the conventional format,
// the value returned by Lookup is unspecified.
func TagLookup(tag []byte, key []byte) ([]byte, bool) {
	// When modifying this code, also update the validateStructTag code in cmd/vet/structtag.go.

	for len(tag) > 0 {
		// Skip leading space.
		i := 0
		for i < len(tag) && tag[i] == ' ' {
			i++
		}
		tag = tag[i:]
		if len(tag) == 0 {
			break
		}

		// Scan to colon. A space, a quote or a control character is a syntax error.
		// Strictly speaking, control chars include the range [0x7f, 0x9f], not just
		// [0x00, 0x1f], but in practice, we ignore the multi-byte control characters
		// as it is simpler to inspect the tag's bytes than the tag's runes.
		i = 0
		for i < len(tag) && tag[i] > ' ' && tag[i] != ':' && tag[i] != '"' && tag[i] != 0x7f {
			i++
		}
		if i == 0 || i+1 >= len(tag) || tag[i] != ':' || tag[i+1] != '"' {
			break
		}
		name := tag[:i]
		tag = tag[i+1:]

		// Scan quoted string to find value.
		i = 1
		for i < len(tag) && tag[i] != '"' {
			if tag[i] == '\\' {
				i++
			}
			i++
		}
		if i >= len(tag) {
			break
		}
		qvalue := tag[:i+1]
		tag = tag[i+1:]

		if bytes.Equal(key, name) {
			value, err := Unquote(qvalue)
			if err != nil {
				break
			}
			return value, true
		}
	}
	return []byte{}, false
}
