/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

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

// ==============
// Setters
// ==============
func New(typ *RType) Value {
	return Value{Type: typ.PtrTo(), Ptr: unsafeNew(typ), Flag: Flag(Ptr)}
}

func Zero(typ *RType) Value {
	if typ.isDirectIface() {
		return Value{Type: typ, Ptr: unsafeNew(typ), Flag: Flag(typ.Kind()) | pointerFlag}
	}
	return Value{Type: typ, Ptr: nil, Flag: Flag(typ.Kind())}
}

func StringKind(k Kind) string {
	if int(k) < len(kindNames) {
		return kindNames[k]
	}
	return "kind" + string(FormatInt(int64(k)))
}
