/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import "bytes"

func (t *RType) hasPointers() bool                { return t.kind&kindNoPointers == 0 }
func (t *RType) isDirectIface() bool              { return t.kind&kindDirectIface == 0 } // isDirectIface reports whether t is stored indirectly in an interface value.
func (t *RType) Kind() Kind                       { return Kind(t.kind & kindMask) }
func (t *RType) convToPtr() *ptrType              { return (*ptrType)(ptr(t)) }
func (t *RType) hasExtraStar() bool               { return t.extraTypeFlag&hasExtraStarFlag != 0 }
func (t *RType) nameOffset(offset nameOff) name   { return name{(*byte)(resolveNameOff(ptr(t), offset))} }
func (t *RType) nameOffsetStr() name              { return name{(*byte)(resolveNameOff(ptr(t), t.str))} }
func (t *RType) convToStruct() *structType        { return (*structType)(ptr(t)) }
func (t *RType) convToFn() *funcType              { return (*funcType)(ptr(t)) }
func (t *RType) typeOffset(offset typeOff) *RType { return (*RType)(resolveTypeOff(ptr(t), offset)) }
func (t *RType) convToIface() *ifaceType          { return (*ifaceType)(ptr(t)) }
func (t *RType) ConvToMap() *mapType              { return (*mapType)(ptr(t)) }
func (t *RType) ConvToSlice() *sliceType          { return (*sliceType)(ptr(t)) }
func (t *RType) ConvToArray() *arrayType          { return (*arrayType)(ptr(t)) }
func (t *RType) Deref() *RType                    { return (*ptrType)(ptr(t)).Type }
func (t *RType) hasInfoFlag() bool                { return t.extraTypeFlag&hasExtraInfoFlag != 0 }
func (t *RType) NoOfIfaceMethods() int            { return len(t.ifaceMethods()) }
func (t *RType) ifaceMethods() []ifaceMethod      { return t.convToIface().methods }
func (t *RType) isAnon() bool                     { return t.extraTypeFlag&hasNameFlag == 0 }
func (t *RType) hasName() bool                    { return !t.isAnon() && len(t.nameOffsetStr().name()) > 0 }

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
		pointerType := existingType.convToPtr()
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
func (t *RType) Fields(inspect InspectTypeFn) {
	if t.Kind() != Struct {
		panic("Not struct")
		return
	}
	structType := t.convToStruct()
	for i := range structType.fields {
		field := &structType.fields[i]
		var PkgPath []byte
		if !field.name.isExported() {
			PkgPath = structType.pkgPath.name()
		}
		var Tag []byte
		if tag := field.name.tag(); len(tag) > 0 {
			Tag = tag
		}
		inspect(field.Type, field.name.name(), Tag, PkgPath, isEmbedded(field), field.name.isExported(), structFieldOffset(field), i)
	}
}

func (t *RType) PkgPath() string {
	if t.isAnon() {
		return ""
	}
	pk, ok := t.pkg()
	if !ok {
		return ""
	}
	return string(t.nameOffset(pk).name())
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
		destArray := dest.ConvToArray()
		return destArray.Len == t.ConvToArray().Len && t.haveIdenticalType(destArray.ElemType, cmpTags)
	case Func:
		panic("haveIdenticalUnderlyingType: Comparing Func")
		return true
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
		return t.ConvToMap().KeyType.haveIdenticalType(dest.ConvToMap().KeyType, cmpTags) && t.ConvToMap().ElemType.haveIdenticalType(dest.ConvToMap().ElemType, cmpTags)
	case Slice:
		return t.ConvToSlice().ElemType.haveIdenticalType(dest.ConvToSlice().ElemType, cmpTags)
	case Ptr:
		return t.Deref().haveIdenticalType(dest.Deref(), cmpTags)
	case Struct:
		destStruct := dest.convToStruct()
		srcStruct := t.convToStruct()
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

func (t *RType) addTypeBits(vec *bitVector, offset uintptr) {
	switch t.Kind() {
	case Chan, Func, Map, Ptr, Slice, String, UnsafePointer:
		// 1 pointer at start of representation
		for vec.num < uint32(offset/uintptr(PtrSize)) {
			appendBitVector(vec, 0)
		}
		appendBitVector(vec, 1)
	case Interface:
		// 2 pointers
		for vec.num < uint32(offset/uintptr(PtrSize)) {
			appendBitVector(vec, 0)
		}
		appendBitVector(vec, 1)
		appendBitVector(vec, 1)
	case Array:
		// repeat inner type
		tArray := t.ConvToArray()
		for i := 0; i < int(tArray.Len); i++ {
			if tArray.ElemType.hasPointers() {
				tArray.ElemType.addTypeBits(vec, offset+uintptr(i)*tArray.ElemType.size)
			}
		}
	case Struct:
		// apply fields
		structType := t.convToStruct()
		for i := range structType.fields {
			field := &structType.fields[i]
			if field.Type.hasPointers() {
				field.Type.addTypeBits(vec, offset+structFieldOffset(field))
			}
		}
	}
}
func (t *RType) Implements(u *RType) bool {
	if u == nil {
		return false
	}
	if u.Kind() != Interface {
		return false
	}
	return t.implements(u)
}
func (t *RType) implements(dest *RType) bool {
	if dest.Kind() != Interface {
		return false
	}
	destIntf := dest.convToIface()
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
		srcIntf := t.convToIface()
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
	case Chan, Func, Map, Ptr:
		if v.hasMethodFlag() {
			panic("Has method flag while checking isNil.")
			return false
		}
		ptrToV := v.Ptr
		if v.isPointer() {
			ptrToV = convPtr(ptrToV)
		}
		return ptrToV == nil
	case Interface, Slice:
		// Both interface and slice are nil if first word is 0.
		// Both are always bigger than a word; assume pointerFlag.
		return convPtr(v.Ptr) == nil
	default:
		return true
	}
}

func (v Value) pointer() ptr {
	if v.Type.size != PtrSize || !v.Type.hasPointers() {
		return nil
	}
	if v.isPointer() {
		return convPtr(v.Ptr)
	}
	return v.Ptr
}

func (v Value) Deref() Value {
	switch v.Kind() {
	case Ptr:
		ptrToV := v.Ptr
		if v.isPointer() {
			ptrToV = convPtr(ptrToV)
		}
		// The returned value's address is v's value.
		if ptrToV == nil {
			return Value{}
		}
		// if we got here, there is not a dereference, nor the pointer is nil - studying the type's pointer
		typ := v.Type.Deref()
		fl := v.Flag&exportFlag | pointerFlag | addressableFlag | Flag(typ.Kind())
		return Value{Type: typ, Ptr: ptrToV, Flag: fl}
	default:
		return v
	}
}

func (v Value) Interface() interface{} {
	if !v.IsValid() {
		return nil
	}
	if !v.isExported() {
	}
	return v.valueInterface()
}
func (v Value) Addr() Value {
	if !v.CanAddr() {
		panic("reflect.Value.Addr: called on a NON addressable value")
	}
	return Value{Type: v.Type.PtrTo(), Ptr: v.Ptr, Flag: v.ro() | Flag(Ptr)}
}
func (v Value) ro() Flag {
	if !v.isExported() {
		return stickyROFlag
	}
	return 0
}

func (v Value) Len() int {
	switch v.Kind() {
	case Array:
		return int(v.Type.ConvToArray().Len)
	case Slice:
		return (*sliceHeader)(v.Ptr).Len // Slice is bigger than a word; assume pointerFlag.
	case String:
		return (*stringHeader)(v.Ptr).Len // String is bigger than a word; assume pointerFlag.
	default:
		panic("reflect.SliceValue.Len : unknown kind `") // + StringKind(v.Kind()) + "`. How did you got here?")

		return 0 // The length of "unknown"
	}

}

// Len returns v's length.
func (v Value) MapLen() int {
	return maplen(v.pointer())
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
			return convIface(v.Ptr)
		}
		return convIfaceMeth(v.Ptr)
	}

	return v.packEface()
}

func (v Value) packEface() interface{} {
	var i interface{}
	e := toIface(ptr(&i))
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
		e.word = convPtr(v.Ptr)
	default:
		// Value is direct, and so is the interface.
		e.word = v.Ptr
	}
	// Now, fill in the type portion. We're very careful here not to have any operation between the e.word and e.Type assignments that would let the garbage collector observe the partially-built interface value.
	e.Type = v.Type
	return i
}

func (v Value) assignTo(dst *RType, target ptr) Value {
	if v.hasMethodFlag() {
		//v = v.makeMethodValue()
		panic("Value.assignTo : This is a method.")
	}

	switch {
	default:
		// TODO : shouldn't we fail first?
		// Failed.
		panic("reflect.Value.assignTo: value of type ") // + TypeToString(v.Type) + " is not assignable to type " + TypeToString(dst))
	case v.Type.directlyAssignable(dst):
		// Overwrite type so that they match. Same memory layout, so no harm done.
		fl := v.Flag&(addressableFlag|pointerFlag) | v.ro()
		fl |= Flag(dst.Kind())
		return Value{Type: dst, Ptr: v.Ptr, Flag: fl}

	case v.Type.implements(dst):
		if target == nil {
			target = unsafeNew(dst)
		}
		if v.Kind() == Interface && v.IsNil() {
			// A nil ReadWriter passed to nil Reader is OK, but using ifaceE2I below will panic.
			// Avoid the panic by returning a nil dst (e.g., Reader) explicitly.
			return Value{Type: dst, Ptr: nil, Flag: Flag(Interface)}
		}
		v.assertE2I(dst, target)
		return Value{Type: dst, Ptr: target, Flag: pointerFlag | Flag(Interface)}
	}
}

func (v Value) assertE2I(dst *RType, target ptr) {
	// TODO : @badu - Type links to methods
	//to be read "if NumMethod(dst) == 0{"
	if (dst.Kind() == Interface && dst.NoOfIfaceMethods() == 0) || (dst.Kind() != Interface && lenExportedMethods(dst) == 0) {
		// the case of "interface{}"
		loadConvIface(target, v.valueInterface())
	} else {
		ifaceE2I(dst, v.valueInterface(), target)
	}
}

func (v Value) Bytes() []byte {
	if v.Kind() != Slice {
		return nil
	}
	if v.Type.ConvToSlice().ElemType.Kind() != Uint8 {
		return nil
	}
	// Slice is always bigger than a word; assume pointerFlag.
	return *(*[]byte)(v.Ptr)
}

func (v Value) Index(i int) Value {
	switch v.Kind() {
	case Array:
		tt := v.Type.ConvToArray()
		if uint(i) >= uint(tt.Len) {
			// TODO : panic
			return Value{}
		}
		typ := tt.ElemType
		offset := uintptr(i) * typ.size
		// Either pointerFlag is set and v.ptr points at array, or pointerFlag is not set and v.ptr is the actual array data.
		// In the former case, we want v.ptr + offset.
		// In the latter case, we must be doing Index(0), so offset = 0, so v.ptr + offset is still the correct address.
		val := add(v.Ptr, offset)
		fl := v.Flag&(pointerFlag|addressableFlag) | v.ro() | Flag(typ.Kind()) // bits same as overall array
		return Value{Type: typ, Ptr: val, Flag: fl}

	case Slice:
		// Element flag same as Deref of ptr.
		// Addressable, indirect, possibly read-only.
		s := (*sliceHeader)(v.Ptr)
		if uint(i) >= uint(s.Len) {
			// TODO : panic
			return Value{}
		}
		typ := v.Type.ConvToSlice().ElemType
		val := arrayAt(s.Data, i, typ.size)
		fl := addressableFlag | pointerFlag | v.ro() | Flag(typ.Kind())
		return Value{Type: typ, Ptr: val, Flag: fl}

	case String:
		s := (*stringHeader)(v.Ptr)
		if uint(i) >= uint(s.Len) {
			// TODO : panic
			return Value{}
		}
		p := arrayAt(s.Data, i, 1)
		fl := v.ro() | Flag(Uint8) | pointerFlag
		return Value{Type: uint8Type, Ptr: p, Flag: fl}

	default:
		// kind checks are performed in public ToSlice(), so this should NEVER happen
		// TODO : panic
		return Value{}
	}
}

// MapIndex returns the value associated with key in the map v.
// It returns the zero Value if key is not found in the map or if v represents a nil map.
// As in Go, the key's value must be assignable to the map's key type.
func (v Value) MapIndex(key Value) Value {
	mapType := v.Type.ConvToMap()
	// Do not require key to be exported, so that DeepEqual and other programs can use all the keys returned by MapKeys as arguments to MapIndex. If either the map or the key is unexported, though, the result will be considered unexported.
	// This is consistent with the behavior for structs, which allow read but not write of unexported fields.
	key = key.assignTo(mapType.KeyType, nil)

	var keyPtr ptr
	if key.isPointer() {
		keyPtr = key.Ptr
	} else {
		keyPtr = ptr(&key.Ptr)
	}

	elemPtr := mapaccess(v.Type, v.pointer(), keyPtr)
	if elemPtr == nil {
		// we could return nil, but deep equal will panic
		// TODO : panic
		return Value{}
	}

	mapElemType := mapType.ElemType
	fl := v.ro() | key.ro()
	fl |= Flag(mapElemType.Kind())
	if !mapElemType.isDirectIface() {
		return Value{mapElemType, convPtr(elemPtr), fl}
	}

	// Copy result so future changes to the map won't change the underlying value.
	mapElemValue := unsafeNew(mapElemType)
	typedmemmove(mapElemType, mapElemValue, elemPtr)
	return Value{mapElemType, mapElemValue, fl | pointerFlag}
}

// MapKeys returns a slice containing all the keys present in the map, in unspecified order.
// It returns an empty slice if v represents a nil map.
func (v Value) MapKeys() []Value {
	mapType := v.Type.ConvToMap()
	keyType := mapType.KeyType

	fl := v.ro() | Flag(keyType.Kind())

	mapPtr := v.pointer()
	mapLen := int(0)
	if mapPtr != nil {
		mapLen = maplen(mapPtr)
	}

	it := mapiterinit(v.Type, mapPtr)
	result := make([]Value, mapLen)
	var i int
	for i = 0; i < len(result); i++ {
		key := mapiterkey(it)
		if key == nil {
			// Someone deleted an entry from the map since we called maplen above. It's a data race, but nothing we can do about it.
			break
		}
		if keyType.isDirectIface() {
			// Copy result so future changes to the map won't change the underlying value.
			keyValue := unsafeNew(keyType)
			typedmemmove(keyType, keyValue, key)
			result[i] = Value{keyType, keyValue, fl | pointerFlag}
		} else {
			result[i] = Value{keyType, convPtr(key), fl}
		}
		mapiternext(it)
	}
	return result[:i]
}

func (v Value) Iface() Value {
	switch v.Kind() {
	case Interface:
		var eface interface{}
		if v.Type.NoOfIfaceMethods() == 0 {
			// the case of "interface{}"
			eface = convIface(v.Ptr)
		} else {
			eface = convIfaceMeth(v.Ptr)
		}
		// unpackEface converts the empty interface 'eface' to a Value.
		e := toIface(ptr(&eface))
		// NOTE: don't read e.word until we know whether it is really a pointer or not.
		t := e.Type
		if t == nil {
			panic("Invalid IFACE")
			// it's invalid
			return Value{}
		}
		f := Flag(t.Kind())
		if t.isDirectIface() {
			f |= pointerFlag
		}
		x := Value{Type: t, Ptr: e.word, Flag: f}
		if x.IsValid() {
			x.Flag |= v.ro()
		}
		return x
	default:
		panic("Not IFACE.")
		return v
	}
}
func (v Value) isEmptyValue() bool {
	switch v.Kind() {
	case Map:
		return maplen(v.pointer()) == 0
	case Array:
		return v.Type.ConvToArray().Len == 0
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
		return convPtr(v.Ptr) == nil
	case Ptr:
		return v.IsNil()
	}
	return false
}

func (v Value) Field(i int) Value {
	// we're sure that it is a struct : check is performed in ToStruct()
	structType := v.Type.convToStruct()
	if uint(i) >= uint(len(structType.fields)) {
		panic("reflect.Value.Field: Field index out of range")
	}

	field := &structType.fields[i]
	typ := field.Type

	// Inherit permission bits from v, but clear embedROFlag.
	fl := v.Flag&(stickyROFlag|pointerFlag|addressableFlag) | Flag(typ.Kind())
	// Using an unexported field forces exportFlag.
	if !field.name.isExported() {
		if isEmbedded(field) {
			fl |= embedROFlag
		} else {
			fl |= stickyROFlag
		}
	}
	// Either pointerFlag is set and v.ptr points at struct, or pointerFlag is not set and v.ptr is the actual struct data.
	// In the former case, we want v.ptr + offset.
	// In the latter case, we must have field.offset = 0, so v.ptr + field.offset is still the correct address.
	ptr := add(v.Ptr, structFieldOffset(field))
	return Value{Type: typ, Ptr: ptr, Flag: fl}
}
