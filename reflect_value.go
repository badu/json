/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"bytes"
	"math"
)

func (t *RType) hasPointers() bool                { return t.kind&kindNoPointers == 0 }
func (t *RType) isDirectIface() bool              { return t.kind&kindDirectIface == 0 } // isDirectIface reports whether t is stored indirectly in an interface value.
func (t *RType) Kind() Kind                       { return Kind(t.kind & kindMask) }
func (t *RType) hasExtraStar() bool               { return t.extraTypeFlag&hasExtraStarFlag != 0 }
func (t *RType) nameOffset(offset nameOff) name   { return name{(*byte)(resolveNameOff(ptr(t), offset))} }
func (t *RType) nameOffsetStr() name              { return name{(*byte)(resolveNameOff(ptr(t), t.str))} }
func (t *RType) typeOffset(offset typeOff) *RType { return (*RType)(resolveTypeOff(ptr(t), offset)) }
func (t *RType) hasInfoFlag() bool                { return t.extraTypeFlag&hasExtraInfoFlag != 0 }
func (t *RType) NoOfIfaceMethods() int            { return len((*ifaceType)(ptr(t)).methods) }
func (t *RType) ifaceMethods() []ifaceMethod      { return (*ifaceType)(ptr(t)).methods }
func (t *RType) isAnon() bool                     { return t.extraTypeFlag&hasNameFlag == 0 }
func (t *RType) hasName() bool                    { return !t.isAnon() && t.nameOffsetStr().nameLen() > 0 }

func (t *RType) pkg() (int32, bool) {
	if !t.hasInfoFlag() {
		return 0, false
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
	return ut.pkgPath, true
}

func (t *RType) PtrTo() *RType {
	if t.ptrToThis != 0 {
		return t.typeOffset(t.ptrToThis)
	}

	// Look in known types.
	typeName := byteSliceFromParams(star, t.nomen())
	for _, existingType := range typesByString(typeName) {
		// Attention : cannot use .Deref() here because below we need to return the pointer to the *Type
		pointerType := (*ptrType)(ptr(existingType))
		if pointerType.Type != t {
			continue
		}
		return &pointerType.RType
	}

	// Create a new ptrType starting with the description of an *ptr.
	proto := emptyPtrProto()
	proto.str = declareReflectName(newName(typeName))
	proto.ptrToThis = 0

	// For the type structures linked into the binary, the
	// compiler provides a good hash of the string.
	// Create a good hash for the new string by using
	// the FNV-1 hash's mixing function to combine the
	// old hash and the new "*".
	proto.hash = fnv1(t.hash, '*')
	proto.Type = t
	return &proto.RType
}

func (t *RType) nomen() []byte {
	s := t.nameOffsetStr().name()
	if t.hasExtraStar() {
		return s[1:]
	}
	return s
}

func (t *RType) pkgPathLen() int {
	if t.isAnon() {
		return 0
	}
	pk, ok := t.pkg()
	if !ok {
		return 0
	}
	return t.nameOffset(pk).nameLen()
}

func (t *RType) Name() string {
	if t.isAnon() {
		return ""
	}
	s := t.nameOffsetStr().name()
	i := len(s) - 1
	for i >= 0 {
		if s[i] == '.' {
			break
		}
		i--
	}
	// if we have extra star, and it's the full name, then set it to avoid first char (which is the star)
	if t.hasExtraStar() && i == -1 {
		i = 0
	}
	return string(s[i+1:])
}

func (t *RType) byteName() []byte {
	if t.isAnon() {
		return nil
	}
	s := t.nameOffsetStr().name()
	i := len(s) - 1
	for i >= 0 {
		if s[i] == '.' {
			break
		}
		i--
	}
	// if we have extra star, and it's the full name, then set it to avoid first char (which is the star)
	if t.hasExtraStar() && i == -1 {
		i = 0
	}
	return s[i+1:]
}

func (t *RType) directlyAssignable(dest *RType) bool {
	// x's type V is identical to T?
	if dest == t {
		return true
	}

	// Otherwise at least one of T and V must be unnamed
	// and they must have the same kind.
	if dest.hasName() && t.hasName() || dest.Kind() != t.Kind() {
		return false
	}
	// x's type T and V must  have identical underlying types.
	return t.haveIdenticalUnderlyingType(dest, true)
}

func (t *RType) haveIdenticalType(dest *RType, cmpTags bool) bool {
	if cmpTags {
		return dest == t
	}
	if dest.Name() != t.Name() || dest.Kind() != t.Kind() {
		return false
	}
	return t.haveIdenticalUnderlyingType(dest, false)
}

func (t *RType) haveIdenticalUnderlyingType(dest *RType, cmpTags bool) bool {
	if dest == t {
		return true
	}

	kind := dest.Kind()
	if kind != t.Kind() {
		return false
	}

	// Non-composite types of equal kind have same underlying type (the predefined instance of the type).
	if Bool <= kind && kind <= Complex128 || kind == String || kind == UnsafePointer {
		return true
	}

	// Composite types.
	switch kind {
	case Array:
		destArray := (*arrayType)(ptr(dest)) // convert to array
		srcArray := (*arrayType)(ptr(t))     // convert to array
		return destArray.Len == srcArray.Len && t.haveIdenticalType(destArray.ElemType, cmpTags)
	case Interface:
		destMethods := dest.ifaceMethods()
		srcMethods := t.ifaceMethods()
		// the case of "interface{}"
		if len(destMethods) == 0 && len(srcMethods) == 0 {
			return true
		}
		// Might have the same methods but still need a run time conversion.
		return false
	case Map:
		typedMap := (*mapType)(ptr(t))
		destTypedMap := (*mapType)(ptr(dest))
		return typedMap.KeyType.haveIdenticalType(destTypedMap.KeyType, cmpTags) && typedMap.ElemType.haveIdenticalType(destTypedMap.ElemType, cmpTags)
	case Slice:
		return (*sliceType)(ptr(t)).ElemType.haveIdenticalType((*sliceType)(ptr(dest)).ElemType, cmpTags)
	case Ptr:
		derefType := (*ptrType)(ptr(t)).Type
		destDerefType := (*ptrType)(ptr(dest)).Type
		return derefType.haveIdenticalType(destDerefType, cmpTags)
	case Struct:
		destStruct := (*structType)(ptr(dest))
		srcStruct := (*structType)(ptr(t))
		if len(destStruct.fields) != len(srcStruct.fields) {
			return false
		}
		if !bytes.Equal(destStruct.pkgPath.name(), srcStruct.pkgPath.name()) {
			return false
		}
		for i := range destStruct.fields {
			destField := &destStruct.fields[i]
			srcField := &srcStruct.fields[i]
			if !bytes.Equal(destField.name.name(), srcField.name.name()) {
				return false
			}
			if !srcField.Type.haveIdenticalType(destField.Type, cmpTags) {
				return false
			}
			if cmpTags && !bytes.Equal(destField.name.tag(), srcField.name.tag()) {
				return false
			}
			if destField.offsetEmbed != srcField.offsetEmbed {
				return false
			}
		}
		return true
	default:
		return false
	}
}

func (t *RType) implements(dest *RType) bool {
	if dest == nil {
		return false
	}
	if dest.Kind() != Interface {
		return false
	}
	destIntf := (*ifaceType)(ptr(dest))
	// the case of "interface{}"
	if len(destIntf.methods) == 0 {
		return true
	}

	// The same algorithm applies in both cases, but the method tables for an interface type and a concrete type are different, so the code is duplicated.
	// In both cases the algorithm is a linear scan over the two lists - T's methods and V's methods - simultaneously.
	// Since method tables are stored in a unique sorted order (alphabetical, with no duplicate method names), the scan through V's methods must hit a match for each of T's methods along the way, or else V does not implement T.
	// This lets us run the scan in overall linear time instead of the quadratic time  a naive search would require.
	// See also ../runtime/iface.go.
	if t.Kind() == Interface {
		srcIntf := (*ifaceType)(ptr(t))
		i := 0
		for j := 0; j < len(srcIntf.methods); j++ {
			destMethod := &destIntf.methods[i]
			destMethodName := destIntf.nameOffset(destMethod.nameOffset)
			srcMethod := &srcIntf.methods[j]
			srcMethodName := t.nameOffset(srcMethod.nameOffset)
			if bytes.Equal(srcMethodName.name(), destMethodName.name()) &&
				t.typeOffset(srcMethod.typeOffset) == destIntf.typeOffset(destMethod.typeOffset) {
				if !destMethodName.isExported() {
					destPkgPath := destMethodName.pkgPath()
					if len(destPkgPath) == 0 {
						destPkgPath = destIntf.pkgPath.name()
					}
					srcPkgPath := srcMethodName.pkgPath()
					if len(srcPkgPath) == 0 {
						srcPkgPath = srcIntf.pkgPath.name()
					}
					if !bytes.Equal(destPkgPath, srcPkgPath) {
						continue
					}
				}
				if i++; i >= len(destIntf.methods) {
					return true
				}
			}
		}
		return false
	}

	vmethods, ok := methods(t)
	if !ok {
		return false
	}
	origPkgPath := make([]byte, 0)
	pkg, ok := t.pkg()
	if ok {
		origPkgPath = t.nameOffset(pkg).name()
	}
	i := 0
	for j := 0; j < len(vmethods); j++ {
		destMethod := &destIntf.methods[i]
		destMethodName := destIntf.nameOffset(destMethod.nameOffset)
		srcMethod := vmethods[j]
		srcMethodName := t.nameOffset(srcMethod.nameOffset)
		if bytes.Equal(srcMethodName.name(), destMethodName.name()) &&
			t.typeOffset(srcMethod.typeOffset) == destIntf.typeOffset(destMethod.typeOffset) {
			if !destMethodName.isExported() {
				destPkgPath := destMethodName.pkgPath()
				if len(destPkgPath) == 0 {
					destPkgPath = destIntf.pkgPath.name()
				}
				srcPkgPath := srcMethodName.pkgPath()
				if len(srcPkgPath) == 0 {
					srcPkgPath = origPkgPath
				}
				if !bytes.Equal(destPkgPath, srcPkgPath) {
					continue
				}
			}
			if i++; i >= len(destIntf.methods) {
				return true
			}
		}
	}
	return false
}

func (v Value) CanAddr() bool       { return v.Flag&addressableFlag != 0 }
func (v Value) hasMethodFlag() bool { return v.Flag&methodFlag != 0 }
func (v Value) isPointer() bool     { return v.Flag&pointerFlag != 0 }
func (v Value) Kind() Kind          { return Kind(v.Flag & kindMaskFlag) }
func (v Value) IsValid() bool       { return v.Flag != 0 }
func (v Value) isExported() bool    { return v.Flag&exportFlag == 0 }
func (v Value) IsNil() bool {
	switch v.Kind() {
	case Map, Ptr:
		ptrToV := v.Ptr
		if v.isPointer() {
			ptrToV = *(*ptr)(ptrToV)
		}
		return ptrToV == nil
	case Interface, Slice:
		// Both interface and slice are nil if first word is 0.
		// Both are always bigger than a word; assume pointerFlag.
		return *(*ptr)(v.Ptr) == nil
	default:
		return true
	}
}

func (v Value) pointer() ptr {
	if v.Type.size != PtrSize || !v.Type.hasPointers() {
		return nil
	}
	if v.isPointer() {
		return *(*ptr)(v.Ptr)
	}
	return v.Ptr
}

func (v Value) ro() Flag {
	if !v.isExported() {
		return stickyROFlag
	}
	return 0
}

func (v Value) valueInterface() interface{} {
	if v.hasMethodFlag() {
		panic("hasMethodFlag.")
		// TODO : Value must be func kind
		//return v.makeMethodValue().packEface()
	}

	if v.Kind() == Interface {
		// Special case: return the element inside the interface.
		// Empty interface has one layout, all interfaces with methods have a second layout.
		if v.Type.NoOfIfaceMethods() == 0 {
			// the case of "interface{}"
			return *(*interface{})(v.Ptr)
		}
		return *(*interface{ M() })(v.Ptr)
	}

	return v.packEface()
}

func (v Value) packEface() interface{} {
	var i interface{}
	e := (*ifaceRtype)(ptr(&i))
	// First, fill in the data portion of the interface.
	switch {
	case v.Type.isDirectIface():
		if !v.isPointer() {
			panic("reflect.x.error : packEface this is not a pointer")
		}
		// Value is indirect, and so is the interface we're making.
		ptr := v.Ptr
		if v.CanAddr() {
			c := unsafeNew(v.Type)
			typedmemmove(v.Type, c, ptr)
			ptr = c
		}
		e.word = ptr
	case v.isPointer():
		// Value is indirect, but interface is direct. We need to load the data at v.ptr into the interface data word.
		e.word = *(*ptr)(v.Ptr)
	default:
		// Value is direct, and so is the interface.
		e.word = v.Ptr
	}
	// Now, fill in the type portion. We're very careful here not to have any operation between the e.word and e.Type assignments that would let the garbage collector observe the partially-built interface value.
	e.Type = v.Type
	return i
}

func (v Value) getField(i int) Value {
	field := &(*structType)(ptr(v.Type)).fields[i]

	// Inherit permission bits from v, but clear embedROFlag.
	fl := v.Flag&(stickyROFlag|pointerFlag|addressableFlag) | Flag(field.Type.Kind())
	// Using an unexported field forces exportFlag.
	if !field.name.isExported() {
		// is embedded ?
		if field.offsetEmbed&1 != 0 {
			fl |= embedROFlag
		} else {
			fl |= stickyROFlag
		}
	}
	// Either pointerFlag is set and v.ptr points at struct, or pointerFlag is not set and v.ptr is the actual struct data.
	// In the former case, we want v.ptr + offset.
	// In the latter case, we must have field.offset = 0, so v.ptr + field.offset is still the correct address.
	return Value{Type: field.Type, Ptr: add(v.Ptr, structFieldOffset(field)), Flag: fl}
}

// ==============
// Setters
// ==============
func (v Value) CanSet() bool { return v.Flag&(addressableFlag|exportFlag) == addressableFlag }

func (v Value) Complex() complex128 {
	switch v.Kind() {
	case Complex64:
		return complex128(*(*complex64)(v.Ptr))
	default:
		return *(*complex128)(v.Ptr)
	}
}

func (v Value) Float() float64 {
	switch v.Kind() {
	case Float32:
		return float64(*(*float32)(v.Ptr))
	default:
		return *(*float64)(v.Ptr)
	}
}

func (v Value) Int() int64 {
	switch v.Kind() {
	case Int8:
		return int64(*(*int8)(v.Ptr))
	case Int16:
		return int64(*(*int16)(v.Ptr))
	case Int32:
		return int64(*(*int32)(v.Ptr))
	case Int64:
		return *(*int64)(v.Ptr)
	default:
		return int64(*(*int)(v.Ptr))
	}
}

func (v Value) Uint() uint64 {
	switch v.Kind() {
	case Uint8:
		return uint64(*(*uint8)(v.Ptr))
	case Uint16:
		return uint64(*(*uint16)(v.Ptr))
	case Uint32:
		return uint64(*(*uint32)(v.Ptr))
	case Uint64:
		return *(*uint64)(v.Ptr)
	case UintPtr:
		return uint64(*(*uintptr)(v.Ptr))
	default:
		return uint64(*(*uint)(v.Ptr))
	}
}

func (v Value) Set(toX Value) {
	var target ptr
	if v.Kind() == Interface {
		target = v.Ptr
	}
	valueAssignTo(&toX, v.Type, target)
	if toX.isPointer() {
		typedmemmove(v.Type, v.Ptr, toX.Ptr)
	} else {
		loadConvPtr(v.Ptr, toX.Ptr)
	}
}

func (v Value) SetZero(typ *RType) {
	var target ptr
	if v.Kind() == Interface {
		target = v.Ptr
	}

	if typ.isDirectIface() {
		toX := Value{Type: typ, Ptr: unsafeNew(typ), Flag: Flag(typ.Kind()) | pointerFlag}

		valueAssignTo(&toX, v.Type, target)
		if toX.isPointer() {
			typedmemmove(v.Type, v.Ptr, toX.Ptr)
		} else {
			loadConvPtr(v.Ptr, toX.Ptr)
		}
		return
	}
	toX := Value{Type: typ, Ptr: nil, Flag: Flag(typ.Kind())}

	valueAssignTo(&toX, v.Type, target)
	if toX.isPointer() {
		typedmemmove(v.Type, v.Ptr, toX.Ptr)
	} else {
		loadConvPtr(v.Ptr, toX.Ptr)
	}

}

func (v Value) setMapIndex(mapType *mapType, key, value Value) {
	valueAssignTo(&key, mapType.KeyType, nil)
	var keyPtr ptr
	if key.isPointer() {
		keyPtr = key.Ptr
	} else {
		keyPtr = ptr(&key.Ptr)
	}

	valueAssignTo(&value, mapType.ElemType, nil)
	var elemPtr ptr
	if value.isPointer() {
		elemPtr = value.Ptr
	} else {
		elemPtr = ptr(&value.Ptr)
	}
	mapassign(v.Type, v.pointer(), keyPtr, elemPtr)
}

func (v Value) OverflowUint(x uint64) bool {
	k := v.Kind()
	switch k {
	case Uint, UintPtr, Uint8, Uint16, Uint32, Uint64:
		bitSize := v.Type.size * 8
		trunc := (x << (64 - bitSize)) >> (64 - bitSize)
		return x != trunc
	}
	panic("Overflow Uint")
	//return x != (x<<(64-v.Type.size))>>(64-v.Type.size)
}

func (v Value) OverflowInt(x int64) bool {
	k := v.Kind()
	switch k {
	case Int, Int8, Int16, Int32, Int64:
		bitSize := v.Type.size * 8
		trunc := (x << (64 - bitSize)) >> (64 - bitSize)
		return x != trunc
	}
	panic("Overflow Int")
	//return x != (x<<(64-v.Type.size))>>(64-v.Type.size)
}

func (v Value) OverflowFloat(x float64) bool {
	k := v.Kind()
	switch k {
	case Float32:
		if x < 0 {
			x = -x
		}
		return math.MaxFloat32 < x && x <= math.MaxFloat64
	case Float64:
		return false
	}
	panic("Overflow float")
}

func (v Value) NumMethod() int {
	// we're sure that it is a struct : check is performed in ToStruct()
	if v.Type.Kind() == Interface {
		return v.Type.NoOfIfaceMethods()
	}

	return lenExportedMethods(v.Type)
}

func (t *RType) NumMethod() int {
	if t.Kind() == Interface {
		return t.NoOfIfaceMethods()
	}

	return lenExportedMethods(t)
}

func (t *RType) String() string {
	return string(t.nomen())
}
