/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"bytes"
	"runtime"
)

func structFieldOffset(f *structField) uintptr  { return f.offsetEmbed >> 1 }
func declareReflectName(n name) int32           { return addReflectOff(ptr(n.bytes)) } // It returns a new nameOff that can be used to refer to the pointer.
func add(p ptr, x uintptr) ptr                  { return ptr(uintptr(p) + x) }         // add returns p+x.
func arrayAt(p ptr, i int, eltSize uintptr) ptr { return add(p, uintptr(i)*eltSize) }
func loadConvPtr(p ptr, x ptr)                  { *(*ptr)(p) = x }
func noOfIfaceMethods(t *RType) int             { return len((*ifaceType)(ptr(t)).methods) }

func isEmptyValue(v Value) bool {
	switch v.Kind() {
	case Map:
		return maplen(v.pointer()) == 0
	case Array:
		return (*arrayType)(ptr(v.Type)).Len == 0
	case Slice:
		return (*sliceHeader)(v.Ptr).Len == 0
	case String:
		return (*stringHeader)(v.Ptr).Len == 0
	case Bool:
		return !*(*bool)(v.Ptr)
	case Int:
		return *(*int)(v.Ptr) == 0
	case Int8:
		return *(*int8)(v.Ptr) == 0
	case Int16:
		return *(*int16)(v.Ptr) == 0
	case Int32:
		return *(*int32)(v.Ptr) == 0
	case Int64:
		return *(*int64)(v.Ptr) == 0
	case Uint:
		return *(*uint)(v.Ptr) == 0
	case Uint8:
		return *(*uint8)(v.Ptr) == 0
	case Uint16:
		return *(*uint16)(v.Ptr) == 0
	case Uint32:
		return *(*uint32)(v.Ptr) == 0
	case Uint64:
		return *(*uint64)(v.Ptr) == 0
	case UintPtr:
		return *(*uintptr)(v.Ptr) == 0
	case Float32:
		return *(*float32)(v.Ptr) == 0
	case Float64:
		return *(*float64)(v.Ptr) == 0
	case Interface:
		return *(*ptr)(v.Ptr) == nil
	case Ptr:
		return v.IsNil()
	}
	return false
}

func lenExportedMethods(t *RType) int {
	all, ok := methods(t)
	if !ok {
		return 0
	}
	count := 0
	for _, method := range all {
		methodName := t.nameOffset(method.nameOffset)
		if methodName.isExported() {
			count++
		}
	}
	return count
}

func methods(t *RType) ([]method, bool) {
	if !t.hasInfoFlag() {
		return nil, false
	}

	var ut *uncommonType
	switch t.Kind() {
	case Struct:
		ut = &(*uncommonStruct)(ptr(t)).u
	case Ptr:
		ut = &(*uncommonPtr)(ptr(t)).u
	case Slice:
		ut = &(*uncommonSlice)(ptr(t)).u
	case Array:
		ut = &(*uncommonArray)(ptr(t)).u
	case Interface:
		ut = &(*uncommonInterface)(ptr(t)).u
	case Func:
		panic("Func")
	default:
		ut = &(*uncommonConcrete)(ptr(t)).u
	}

	if ut.mCount == 0 {
		return nil, false
	}

	return (*[1 << 16]method)(ptr(uintptr(ptr(ut)) + uintptr(ut.mOffset)))[:ut.mCount:ut.mCount], true
}

func emptyPtrProto() ptrType {
	var iptr interface{} = (*ptr)(nil)
	prototype := *(**ptrType)(ptr(&iptr))
	return *prototype
}

func byteSliceFromParams(params ...interface{}) []byte {
	result := make([]byte, 0)
	for _, param := range params {
		switch v := param.(type) {
		case string:
			result = append(result, []byte(v)...)
		case byte:
			result = append(result, v)
		case []byte:
			result = append(result, v...)
		default:
			panic("reflect.x.error : bad usage of the name builder.")
		}
	}
	return result
}

// fnv1 incorporates the list of bytes into the hash x using the FNV-1 hash function.
func fnv1(x uint32, list ...byte) uint32 {
	for _, b := range list {
		x = x*16777619 ^ uint32(b)
	}
	return x
}

// typesByString returns the subslice of typelinks() whose elements have
// the given string representation.
// It may be empty (no known types with that string) or may have
// multiple elements (multiple types with that string).
func typesByString(target []byte) []*RType {
	sections, offset := typeLinks()
	var results []*RType
	var currentType *RType
	var search []byte
	for offsI, offs := range offset {
		section := sections[offsI]

		// We are looking for the first index i where the string becomes >= target.
		// This is a copy of sort.Search, with f(h) replaced by (*Type[h].String() >= target).
		i, j := 0, len(offs)
		for i < j {
			h := i + (j-i)/2 // avoid overflow when computing h
			// i ≤ h < j
			currentType = (*RType)(ptr(uintptr(section) + uintptr(offs[h])))
			search = currentType.nameOffsetStr().name()
			if currentType.hasExtraStar() {
				search = search[1:]
			}
			// Compare(a, b []byte) int
			// The result will be 0 if a==b, -1 if a < b, and +1 if a > b.
			// if search >= target {
			if bytes.Compare(search, target) >= 0 {
				j = h // preserves f(j) == true
			} else {
				i = h + 1 // preserves f(i-1) == false
			}
		}
		// i == j, f(i-1) == false, and f(j) (= f(i)) == true  =>  answer is i.

		// Having found the first, linear scan forward to find the last.
		// We could do a second binary search, but the caller is going
		// to do a linear scan anyway.
		for k := i; k < len(offs); k++ {
			currentType = (*RType)(ptr(uintptr(section) + uintptr(offs[k])))
			search = currentType.nameOffsetStr().name()
			if currentType.hasExtraStar() {
				search = search[1:]
			}
			// if search != target {
			if !bytes.Equal(search, target) {
				break
			}
			results = append(results, currentType)
		}
	}
	return results
}

// ==============
// Setters
// ==============
func internalNew(t *RType) Value {
	return Value{Type: t, Ptr: unsafeNew(t), Flag: Flag(t.Kind())&exportFlag | pointerFlag | addressableFlag | Flag(t.Kind())}
}

// makeInt returns a Value of type t equal to bits (possibly truncated),
// where t is a signed or unsigned int type.
func makeInt(v uint64, t *RType) Value {
	ptr := unsafeNew(t)
	switch t.size {
	case 1:
		*(*uint8)(ptr) = uint8(v)
	case 2:
		*(*uint16)(ptr) = uint16(v)
	case 4:
		*(*uint32)(ptr) = uint32(v)
	case 8:
		*(*uint64)(ptr) = v
	}
	return Value{Type: t, Ptr: ptr, Flag: pointerFlag | Flag(t.Kind())}
}

// makeFloat returns a Value of type t equal to v (possibly truncated to float32),
// where t is a float32 or float64 type.
func makeFloat(v float64, t *RType) Value {
	ptr := unsafeNew(t)
	switch t.size {
	case 4:
		*(*float32)(ptr) = float32(v)
	case 8:
		*(*float64)(ptr) = v
	}
	return Value{Type: t, Ptr: ptr, Flag: pointerFlag | Flag(t.Kind())}
}

// makeComplex returns a Value of type t equal to v (possibly truncated to complex64),
// where t is a complex64 or complex128 type.
func makeComplex(v complex128, t *RType) Value {
	ptr := unsafeNew(t)
	switch t.size {
	case 8:
		*(*complex64)(ptr) = complex64(v)
	case 16:
		*(*complex128)(ptr) = v
	}
	return Value{Type: t, Ptr: ptr, Flag: pointerFlag | Flag(t.Kind())}
}

func makeString(s string, t *RType) Value {
	ret := internalNew(t)
	*(*string)(ret.Ptr) = s
	ret.Flag = ret.Flag &^ addressableFlag
	return ret
}

func makeBytes(byt []byte, t *RType) Value {
	ret := internalNew(t)
	*(*[]byte)(ret.Ptr) = byt
	ret.Flag = ret.Flag &^ addressableFlag
	return ret
}

func makeRunes(run []rune, t *RType) Value {
	ret := internalNew(t)
	*(*[]rune)(ret.Ptr) = run
	ret.Flag = ret.Flag &^ addressableFlag
	return ret
}

// convert operation: direct copy
func cvtDirect(v Value, typ *RType) Value {
	f := v.Flag
	ptr := v.Ptr
	if v.CanAddr() {
		// indirect, mutable word - make a copy
		c := unsafeNew(typ)
		typedmemmove(typ, c, ptr)
		ptr = c
		f &^= addressableFlag
	}
	return Value{Type: typ, Ptr: ptr, Flag: f}
}

func assertE2I(v Value, dst *RType, target ptr) {
	// TODO : @badu - Type links to methods
	//to be read "if NumMethod(dst) == 0{"
	if (dst.Kind() == Interface && noOfIfaceMethods(dst) == 0) || (dst.Kind() != Interface && lenExportedMethods(dst) == 0) {
		// the case of "interface{}"
		*(*interface{})(target) = v.valueInterface()
	} else {
		ifaceE2I(dst, v.valueInterface(), target)
	}
}

// convert operation: concrete -> interface
func cvtT2I(v Value, typ *RType) Value {
	target := unsafeNew(typ)
	assertE2I(v, typ, target)
	return Value{Type: typ, Ptr: target, Flag: pointerFlag | Flag(Interface)}
}

// convert operation: interface -> interface
func cvtI2I(v Value, typ *RType) Value {
	if v.IsNil() {
		ret := Zero(typ)
		//ret.Flag |= v.ro()
		return ret
	}
	switch v.Kind() {
	case Ptr:
		return cvtT2I(valueDeref(v), typ)
	case Interface:
		return cvtT2I(valueIface(v), typ)
	default:
		return cvtT2I(v, typ)
	}
}

func valueConvert(v Value, typ *RType) Value {
	destKind := typ.Kind()
	srcKind := v.Type.Kind()

	switch srcKind {
	case Int, Int8, Int16, Int32, Int64:
		switch destKind {
		case Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, UintPtr:
			return makeInt(uint64(v.Int()), typ) // convert operation: intXX -> [u]intXX
		case Float32, Float64:
			return makeFloat(float64(v.Int()), typ) // convert operation: intXX -> floatXX
		case String:
			return makeString(string(v.Int()), typ) // convert operation: intXX -> string
		}
	case Uint, Uint8, Uint16, Uint32, Uint64, UintPtr:
		switch destKind {
		case Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, UintPtr:
			return makeInt(v.Uint(), typ) // convert operation: uintXX -> [u]intXX
		case Float32, Float64:
			return makeFloat(float64(v.Uint()), typ) // convert operation: uintXX -> floatXX
		case String:
			return makeString(string(v.Uint()), typ) // convert operation: uintXX -> string
		}
	case Float32, Float64:
		switch destKind {
		case Int, Int8, Int16, Int32, Int64:
			return makeInt(uint64(int64(v.Float())), typ) // convert operation: floatXX -> intXX
		case Uint, Uint8, Uint16, Uint32, Uint64, UintPtr:
			return makeInt(uint64(v.Float()), typ) // convert operation: floatXX -> uintXX
		case Float32, Float64:
			return makeFloat(v.Float(), typ) // convert operation: floatXX -> floatXX
		}
	case Complex64, Complex128:
		switch destKind {
		case Complex64, Complex128:
			return makeComplex(v.Complex(), typ) // convert operation: complexXX -> complexXX
		}
	case String:
		sliceElem := (*sliceType)(ptr(typ)).ElemType
		if destKind == Slice && sliceElem.pkgPathLen() == 0 {
			switch sliceElem.Kind() {
			case Uint8:
				return makeBytes([]byte(*(*string)(v.Ptr)), typ) // convert operation: string -> []byte
			case Int32:
				return makeRunes([]rune(*(*string)(v.Ptr)), typ) // convert operation: string -> []rune
			}
		}
	case Slice:
		sliceElem := (*sliceType)(ptr(v.Type)).ElemType
		if destKind == String && sliceElem.pkgPathLen() == 0 {
			switch sliceElem.Kind() {
			case Uint8:
				return makeString(string(*(*[]byte)(v.Ptr)), typ) // convert operation: []byte -> string
			case Int32:
				return makeString(string(*(*[]rune)(v.Ptr)), typ) // // convert operation: []rune -> string
			}
		}
	}

	// dst and src have same underlying type.
	if v.Type.haveIdenticalUnderlyingType(typ, false) {
		return cvtDirect(v, typ)
	}
	derefType := (*ptrType)(ptr(v.Type)).Type
	destDerefType := (*ptrType)(ptr(typ)).Type
	// dst and src are unnamed pointer types with same underlying base type.
	if destKind == Ptr && !typ.hasName() &&
		srcKind == Ptr && !v.Type.hasName() &&
		derefType.haveIdenticalUnderlyingType(destDerefType, false) {
		return cvtDirect(v, typ)
	}

	if v.Type.implements(typ) {
		if srcKind == Interface {
			return cvtI2I(v, typ)
		}
		return cvtT2I(v, typ)
	}

	panic("reflect.Value.Convert: value of type ") // + TypeToString(v.Type) + " cannot be converted to type " + TypeToString(t))
}

// Note : checking v.Kind() == Interface is caller responsibility
func valueIface(v Value) Value {
	var eface interface{}
	if noOfIfaceMethods(v.Type) == 0 {
		// the case of "interface{}"
		eface = *(*interface{})(v.Ptr)
	} else {
		eface = *(*interface{ M() })(v.Ptr)
	}
	// unpackEface converts the empty interface 'eface' to a Value.
	e := (*ifaceRtype)(ptr(&eface))
	f := Flag(e.Type.Kind())
	if e.Type.isDirectIface() {
		f |= pointerFlag
	}
	return Value{Type: e.Type, Ptr: e.word, Flag: f}
}

func valueDeref(v Value) Value {
	ptrToV := v.Ptr
	if v.isPointer() {
		ptrToV = *(*ptr)(ptrToV)
	}
	// The returned value's address is v's value.
	if ptrToV == nil {
		return Value{}
	}
	// if we got here, there is not a dereference, nor the pointer is nil - studying the type's pointer
	typ := (*ptrType)(ptr(v.Type)).Type
	return Value{Type: typ, Ptr: ptrToV, Flag: v.Flag&exportFlag | pointerFlag | addressableFlag | Flag(typ.Kind())}
}

func valueAssignTo(v *Value, dst *RType, target ptr) {
	switch {
	default:
		// Failed.
		panic("reflect.Value.assignTo: value of type ") // + TypeToString(v.Type) + " is not assignable to type " + TypeToString(dst))

	case v.Type.directlyAssignable(dst):
		// Overwrite type so that they match. Same memory layout, so no harm done.
		fl := v.Flag & (addressableFlag | pointerFlag)
		fl |= Flag(dst.Kind())
		v.Type = dst
		v.Flag = fl

	case v.Type.implements(dst):
		if target == nil {
			target = unsafeNew(dst)
		}
		if v.Kind() == Interface && v.IsNil() {
			// A nil ReadWriter passed to nil Reader is OK, but using ifaceE2I below will panic.
			// Avoid the panic by returning a nil dst (e.g., Reader) explicitly.
			v.Type = dst
			v.Ptr = nil
			v.Flag = Flag(Interface)
			return
		}
		assertE2I(*v, dst, target)
		v.Type = dst
		v.Ptr = target
		v.Flag = pointerFlag | Flag(Interface)
	}
}

// ==============
// Others (unused)
// ==============

func methodName() string {
	pc, _, line, _ := runtime.Caller(3)
	f := runtime.FuncForPC(pc)
	if f == nil {
		return "unknown method"
	}
	return f.Name() + " line : " + string(FormatInt(int64(line)))
}
