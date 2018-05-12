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

// ==============
// Setters
// ==============
func New(typ *RType) Value {
	if typ == nil {
		panic("reflect: New(nil)")
	}
	ptr := unsafeNew(typ)
	return Value{Type: typ.PtrTo(), Ptr: ptr, Flag: Flag(Ptr)}
}

func Zero(typ *RType) Value {
	if typ == nil {
		panic("reflect: Zero(nil)")
	}
	if typ.isDirectIface() {
		return Value{Type: typ, Ptr: unsafeNew(typ), Flag: Flag(typ.Kind()) | pointerFlag}
	}
	return Value{Type: typ, Ptr: nil, Flag: Flag(typ.Kind())}
}

// MakeMap creates a new map with the specified type.
func MakeMap(typ *RType) Value {
	return MakeMapWithSize(typ, 0)
}

// MakeMapWithSize creates a new map with the specified type
// and initial space for approximately n elements.
func MakeMapWithSize(typ *RType, n int) Value {
	if typ.Kind() != Map {

		panic("reflect.MakeMapWithSize of non-map type")

	}
	m := makemap(typ, n)
	return Value{typ, m, Flag(Map)}
}

// Copy copies the contents of src into dst until either
// dst has been filled or src has been exhausted.
// It returns the number of elements copied.
// Dst and src each must have kind Slice or Array, and
// dst and src must have the same element type.
//
// As a special case, src can have kind String if the element type of dst is kind Uint8.
func Copy(dest, src Value) (int, bool) {
	dKind := dest.Kind()
	if dKind != Array && dKind != Slice {
		panic("reflect.Copy: destination not array or slice")

	}
	if dKind == Array {
		if !dest.IsValid() || !dest.CanSet() {

			panic("reflect.Copy: destination must be assignable")

		}
	}
	if !dest.IsValid() || !dest.isExported() {

		panic("reflect.Copy: destination must be exported")

	}
	if !src.IsValid() || !src.isExported() {

		panic("reflect.Copy: source must be exported")

	}

	destKind := dest.Type.Kind()

	sKind := src.Kind()
	var stringCopy bool
	if sKind != Array && sKind != Slice {
		hasUTF8 := false
		if destKind == Array {
			hasUTF8 = dest.Type.ConvToArray().ElemType.Kind() == Uint8
		} else if destKind == Slice {
			hasUTF8 = dest.Type.ConvToSlice().ElemType.Kind() == Uint8
		}
		stringCopy = sKind == String && hasUTF8
		if !stringCopy {

			panic("reflect.CopySlice: source not array, slice or string")

		}
	}

	var de *RType
	if destKind == Array {
		de = dest.Type.ConvToArray().ElemType
	} else if destKind == Slice {
		de = dest.Type.ConvToSlice().ElemType
	}

	if !stringCopy {
		var se *RType
		if src.Type.Kind() == Array {
			se = src.Type.ConvToArray().ElemType
		} else if src.Type.Kind() == Slice {
			se = src.Type.ConvToSlice().ElemType
		}
		if de != se {

			panic("Unmatched types ") // + TypeToString(de) + " != " + TypeToString(se))

		}
	}

	var ds, ss sliceHeader
	if dKind == Array {
		ds.Data = dest.Ptr
		ds.Len = dest.Len()
		ds.Cap = ds.Len
	} else {
		ds = *(*sliceHeader)(dest.Ptr)
	}
	if sKind == Array {
		ss.Data = src.Ptr
		ss.Len = src.Len()
		ss.Cap = ss.Len
	} else if sKind == Slice {
		ss = *(*sliceHeader)(src.Ptr)
	} else {
		sh := *(*stringHeader)(src.Ptr)
		ss.Data = sh.Data
		ss.Len = sh.Len
		ss.Cap = sh.Len
	}

	return typedslicecopy(de, ds, ss), true
}

func NewSlice(ofType *RType) Value {
	if ofType == nil {

		panic("reflect: New(nil)")

	}
	newPtr := unsafeNew(ofType)
	return Value{Type: ofType.PtrTo(), Ptr: newPtr, Flag: Flag(Ptr)}
}

// MakeSlice creates a new zero-initialized slice value for the specified slice type, length, and capacity.
func MakeSlice(ofType *RType, len, cap int) Value {
	if ofType.Kind() != Slice {

		panic("reflect.MakeSlice of non-slice type")

	}
	if len < 0 {

		panic("reflect.MakeSlice: negative len")

	}
	if cap < 0 {

		panic("reflect.MakeSlice: negative cap")
	}
	if len > cap {

		panic("reflect.MakeSlice: len > cap")

	}
	s := sliceHeader{unsafeNewArray(ofType.ConvToSlice().ElemType, cap), len, cap}
	return Value{Type: ofType, Ptr: ptr(&s), Flag: pointerFlag | Flag(Slice)}
}
