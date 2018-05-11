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

func toIface(t ptr) *ifaceRtype                 { return (*ifaceRtype)(t) }
func convPtr(p ptr) ptr                         { return *(*ptr)(p) }
func isEmbedded(f *structField) bool            { return f.offsetEmbed&1 != 0 }
func structFieldOffset(f *structField) uintptr  { return f.offsetEmbed >> 1 }
func declareReflectName(n name) nameOff         { return addReflectOff(ptr(n.bytes)) } // It returns a new nameOff that can be used to refer to the pointer.
func loadConvIface(p ptr, x interface{})        { *(*interface{})(p) = x }
func convIfaceMeth(p ptr) interface{}           { return *(*interface{ M() })(p) }
func convIface(p ptr) interface{}               { return *(*interface{})(p) }
func add(p ptr, x uintptr) ptr                  { return ptr(uintptr(p) + x) } // add returns p+x.
func arrayAt(p ptr, i int, eltSize uintptr) ptr { return add(p, uintptr(i)*eltSize) }

func lenExportedMethods(t *RType) int {
	all, ok := methods(t)
	if !ok {
		return 0
	}
	count := 0
	for _, method := range all {
		methodName := t.nameOffset(method.nameOffset)
		if methodName.isExported() {
			if method.typeOffset == 0 {
				panic("reflect.x.error : method type is zero. Apply fix.")
			}
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
	case Func:
		ut = &(*uncommonFunc)(ptr(t)).u
	case Slice:
		ut = &(*uncommonSlice)(ptr(t)).u
	case Array:
		ut = &(*uncommonArray)(ptr(t)).u
	case Interface:
		ut = &(*uncommonInterface)(ptr(t)).u
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

func appendBitVector(vec *bitVector, bit uint8) {
	if vec.num%8 == 0 {
		vec.data = append(vec.data, 0)
	}
	vec.data[vec.num/8] |= bit << (vec.num % 8)
	vec.num++
}

func methodName() string {
	pc, _, line, _ := runtime.Caller(2)
	f := runtime.FuncForPC(pc)
	if f == nil {
		return "unknown method"
	}
	return f.Name() + " line : " + string(FormatInt(int64(line)))
}