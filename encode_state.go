/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"bytes"
	"encoding/base64"
	"errors"
	"math"
	"runtime"
	"sort"
	"sync"
	"unicode/utf8"
)

// NOTE: keep in sync with below.
func writeString(e *encodeState, name string) {
	e.WriteByte(quote)
	start := 0
	for i := 0; i < len(name); {
		if run := name[i]; run < utf8.RuneSelf {
			if htmlSafeSet[run] || (!e.opts.escapeHTML && safeSet[run]) {
				i++
				continue
			}
			if start < i {
				e.WriteString(name[start:i])
			}
			switch run {
			case backSlash, quote:
				e.WriteByte(backSlash)
				e.WriteByte(run)
			case newLine:
				e.WriteByte(backSlash)
				e.WriteByte(nChr)
			case retChar:
				e.WriteByte(backSlash)
				e.WriteByte(rChr)
			case tab:
				e.WriteByte(backSlash)
				e.WriteByte(tChr)
			default:
				// This encodes bytes < 0x20 except for \t, \n and \r.
				// If escapeHTML is set, it also escapes <, >, and &
				// because they can lead to security holes when
				// user-controlled strings are rendered into JSON
				// and served to some browsers.
				e.WriteByte(backSlash)
				e.WriteByte(uChr)
				e.WriteByte(zero)
				e.WriteByte(zero)
				e.WriteByte(hex[run>>4])
				e.WriteByte(hex[run&0xF])
			}
			i++
			start = i
			continue
		}
		c, size := utf8.DecodeRuneInString(name[i:])
		if c == utf8.RuneError && size == 1 {
			if start < i {
				e.WriteString(name[start:i])
			}
			e.WriteByte(backSlash)
			e.WriteByte(uChr)
			e.WriteByte(fChr)
			e.WriteByte(fChr)
			e.WriteByte(fChr)
			e.WriteByte(dChr)
			i += size
			start = i
			continue
		}
		// U+2028 is LINE SEPARATOR.
		// U+2029 is PARAGRAPH SEPARATOR.
		// They are both technically valid characters in JSON strings, but don't work in JSONP, which has to be evaluated as JavaScript, and can lead to security holes there. It is valid JSON to escape them, so we do so unconditionally.
		// See http://timelessrepo.com/json-isnt-a-javascript-subset for discussion.
		if c == '\u2028' || c == '\u2029' {
			if start < i {
				e.WriteString(name[start:i])
			}
			e.WriteByte(backSlash)
			e.WriteByte(uChr)
			e.WriteByte(two)
			e.WriteByte(zero)
			e.WriteByte(two)
			e.WriteByte(hex[c&0xF])
			i += size
			start = i
			continue
		}
		i += size
	}

	if start < len(name) {
		e.WriteString(name[start:])
	}

	e.WriteByte(quote)
}

func writeBytes(e *encodeState, s []byte) {
	e.WriteByte(quote)

	start := 0
	for i := 0; i < len(s); {
		// normal rune
		if s[i] < utf8.RuneSelf {
			if htmlSafeSet[s[i]] || (!e.opts.escapeHTML && safeSet[s[i]]) {
				i++
				continue
			}
			// unsafe rune detected
			if start < i {
				// writing what we have so far...
				e.Write(s[start:i])
			}
			switch s[i] {
			case backSlash, quote:
				e.WriteByte(backSlash)
				e.WriteByte(s[i])
			case newLine:
				e.WriteByte(backSlash)
				e.WriteByte(nChr)
			case retChar:
				e.WriteByte(backSlash)
				e.WriteByte(rChr)
			case tab:
				e.WriteByte(backSlash)
				e.WriteByte(tChr)
			default:
				// This encodes bytes < 0x20 except for \t, \n and \r.
				// If escapeHTML is set, it also escapes <, >, and &
				// because they can lead to security holes when
				// user-controlled strings are rendered into JSON
				// and served to some browsers.
				e.WriteByte(backSlash)
				e.WriteByte(uChr)
				e.WriteByte(zero)
				e.WriteByte(zero)
				e.WriteByte(hex[s[i]>>4])
				e.WriteByte(hex[s[i]&0xF])
			}
			// set new start and continue looking for runes
			i++
			start = i
			continue
		}

		// special rune
		c, size := utf8.DecodeRune(s[i:])
		if c == utf8.RuneError && size == 1 {
			if start < i {
				e.Write(s[start:i])
			}
			e.WriteByte(backSlash)
			e.WriteByte(uChr)
			e.WriteByte(fChr)
			e.WriteByte(fChr)
			e.WriteByte(fChr)
			e.WriteByte(dChr)
			i += size
			start = i
			continue
		}
		// U+2028 is LINE SEPARATOR.
		// U+2029 is PARAGRAPH SEPARATOR.
		// They are both technically valid characters in JSON strings,
		// but don't work in JSONP, which has to be evaluated as JavaScript,
		// and can lead to security holes there. It is valid JSON to
		// escape them, so we do so unconditionally.
		// See http://timelessrepo.com/json-isnt-a-javascript-subset for discussion.
		if c == '\u2028' || c == '\u2029' {
			if start < i {
				e.Write(s[start:i])
			}
			e.WriteByte(backSlash)
			e.WriteByte(uChr)
			e.WriteByte(two)
			e.WriteByte(zero)
			e.WriteByte(two)
			e.WriteByte(hex[c&0xF])
			i += size
			start = i
			continue
		}
		i += size
	}

	if start < len(s) {
		e.Write(s[start:])
	}

	e.WriteByte(quote)
}

func marshal(e *encodeState, v interface{}) (err error) {
	defer func() {
		if r := recover(); r != nil {
			if _, ok := r.(runtime.Error); ok {
				panic(r)
			}
			if s, ok := r.(string); ok {
				panic(s)
			}
			err = r.(error)
		}
	}()
	value := ReflectOn(v)
	if !value.IsValid() {
		invalidValueEncoder(e, value)
		return
	}
	typeEncoder(value.Type)(e, value)
	return nil
}

func encodeError(_ *encodeState, err error) {
	panic(err)
}

func marshalerEncoder(e *encodeState, v Value) {
	if v.Kind() == Ptr && v.IsNil() {
		e.Write(nullLiteral)
		return
	}
	if !v.IsValid() {
		encodeError(e, errors.New("Invalid value while calling Marshal"))
		return
	}
	m, ok := v.valueInterface().(Marshaler)
	if !ok {
		// TODO : seems this silent error spoils the tests. IMO it should be an error
		e.Write(nullLiteral)
		return
	}
	b, err := m.MarshalJSON()
	if err == nil {
		if e.opts.willCheckEmptyStruct {
			// omit []byte("null") returns - since the field is nullable, we are omitting it entirely
			if bytes.Equal(nullLiteral, b) {
				return
			}
			// we store the result to be written in place later
			e.opts.nullableReturn = b
			return
		}
		// copy JSON into buffer, checking validity.
		err = compact(&e.Buffer, b, e.opts.escapeHTML)
	}
	if err != nil {
		encodeError(e, &MarshalerError{v.Type, err})
	}
}

func addrMarshalerEncoder(e *encodeState, v Value) {
	va := Value{Type: v.Type.PtrTo(), Ptr: v.Ptr, Flag: Flag(Ptr)}
	if va.IsNil() {
		e.Write(nullLiteral)
		return
	}
	if !va.IsValid() {
		encodeError(e, errors.New("Invalid value while calling [addr] Marshal"))
		return
	}
	m, ok := va.valueInterface().(Marshaler)
	if !ok {
		// TODO : seems this silent error spoils the tests. IMO it should be an error
		e.Write(nullLiteral)
		return
	}
	b, err := m.MarshalJSON()
	if err == nil {
		if e.opts.willCheckEmptyStruct {
			// omit []byte("null") returns - since the field is nullable, we are omitting it entirely
			if bytes.Equal(nullLiteral, b) {
				return
			}
			// we store the result to be written in place later
			e.opts.nullableReturn = b
			return
		}
		// copy JSON into buffer, checking validity.
		err = compact(&e.Buffer, b, true)
	}
	if err != nil {
		encodeError(e, &MarshalerError{v.Type, err})
	}
}

func boolEncoder(e *encodeState, v Value) {
	if *(*bool)(v.Ptr) {
		if e.opts.quoted {
			e.WriteByte(quote)
			e.Write(trueLiteral)
			e.WriteByte(quote)
		} else {
			e.Write(trueLiteral)
		}
	} else {
		if e.opts.quoted {
			e.WriteByte(quote)
			e.Write(falseLiteral)
			e.WriteByte(quote)
		} else {
			e.Write(falseLiteral)
		}
	}
}

func intEncoder(e *encodeState, v Value) {
	if e.opts.quoted {
		e.WriteByte(quote)
		e.Write(FormatInt(v.Int()))
		e.WriteByte(quote)
	} else {
		e.Write(FormatInt(v.Int()))
	}
}

func uintEncoder(e *encodeState, v Value) {
	if e.opts.quoted {
		e.WriteByte(quote)
		e.Write(FormatUint(v.Uint()))
		e.WriteByte(quote)
	} else {
		e.Write(FormatUint(v.Uint()))
	}
}

func floatHelper(e *encodeState, floatValue float64, bits int) []byte {
	if math.IsInf(floatValue, 0) || math.IsNaN(floatValue) {
		encodeError(e, &UnsupportedValueError{FormatFloat(floatValue, bits)})
	}

	// Convert as if by ES6 number to string conversion.
	// This matches most other JSON generators.
	// See golang.org/issue/6384 and golang.org/issue/14135.
	// Like fmt %g, but the exponent cutoffs are different and exponents themselves are not padded to two digits.
	abs := math.Abs(floatValue)
	fmt := fChr
	// Note: Must use float32 comparisons for underlying float32 value to get precise cutoffs right.
	if abs != 0 {
		if bits == 32 {
			if float32(abs) < 1e-6 || float32(abs) >= 1e21 {
				fmt = eChr
			}
		} else {
			if abs < 1e-6 || abs >= 1e21 {
				fmt = eChr
			}
		}

	}

	b := makeFloatBytes(floatValue, fmt, bits)
	if fmt == eChr {
		// clean up e-09 to e-9
		n := len(b)
		if n >= 4 && b[n-4] == eChr && b[n-3] == minus && b[n-2] == zero {
			b[n-2] = b[n-1]
			b = b[:n-1]
		}
	}
	return b
}

func float32Encoder(e *encodeState, v Value) {
	value := float64(*(*float32)(v.Ptr))
	b := floatHelper(e, value, 32)
	if e.opts.quoted {
		e.WriteByte(quote)
		e.Write(b)
		e.WriteByte(quote)
	} else {
		e.Write(b)
	}
}

func float64Encoder(e *encodeState, v Value) {
	value := *(*float64)(v.Ptr)
	b := floatHelper(e, value, 64)
	if e.opts.quoted {
		e.WriteByte(quote)
		e.Write(b)
		e.WriteByte(quote)
	} else {
		e.Write(b)
	}
}

func stringEncoder(e *encodeState, v Value) {
	theStr := *(*string)(v.Ptr)
	if v.Type == typeOfNo {
		// In Go1.5 the empty string encodes to "0", while this is not a valid number literal we keep compatibility so check validity after this.
		if len(theStr) == 0 {
			theStr = "0" // Number's zero-val
		}
		if !IsValidNumber(theStr) {
			encodeError(e, errors.New("json: invalid number literal `"+theStr+"`"))
		}
		e.WriteString(theStr)
		return
	}

	if e.opts.quoted {
		sb, err := Marshal(theStr)
		if err != nil {
			encodeError(e, err)
		}
		writeBytes(e, sb)
	} else {
		writeString(e, theStr)
	}
}

func encodeByteSlice(e *encodeState, v Value) {
	if v.IsNil() {
		// write "null"
		e.Write(nullLiteral)
		return
	}

	value := *(*[]byte)(v.Ptr)
	if len(value) < 1024 {
		// for small buffers, using Encode directly is much faster.
		dst := make([]byte, base64.StdEncoding.EncodedLen(len(value)))
		base64.StdEncoding.Encode(dst, value)
		e.WriteByte(quote)
		e.Write(dst)
		e.WriteByte(quote)
	} else {
		e.WriteByte(quote)
		// for large buffers, avoid unnecessary extra temporary buffer space.
		enc := base64.NewEncoder(base64.StdEncoding, e)
		enc.Write(value)
		enc.Close()
		e.WriteByte(quote)
	}
}

func interfaceEncoder(e *encodeState, v Value) {
	if v.IsNil() {
		e.Write(nullLiteral)
		return
	}
	value := valueIface(v)
	if !value.IsValid() {
		invalidValueEncoder(e, value)
		return
	}
	typeEncoder(value.Type)(e, value)
}

func invalidValueEncoder(e *encodeState, _ Value) {
	e.Write(nullLiteral)
}

func unsupportedTypeEncoder(e *encodeState, v Value) {
	encodeError(e, &UnsupportedTypeError{v.Type})
}

func (ae *allEncoder) encodeCond(e *encodeState, v Value) {
	if v.CanAddr() {
		addrMarshalerEncoder(e, v)
	} else {
		ae.encs[0](e, v)
	}
}

func (ae *allEncoder) encodePtr(e *encodeState, v Value) {
	if v.IsNil() {
		e.Write(nullLiteral)
		return
	}
	ae.encs[0](e, valueDeref(v))
}

func (ae *allEncoder) encodeArray(e *encodeState, v Value) {
	arrLen := 0
	isSlice := false

	var slcHeader *sliceHeader
	var fl Flag
	var arrElemType *RType
	switch v.Kind() {
	case Array:
		arrType := (*arrayType)(ptr(v.Type)) // convert to array
		arrElemType = arrType.ElemType
		arrLen = int(arrType.Len)
		fl = v.Flag&(pointerFlag|addressableFlag) | Flag(arrType.Kind())
	case Slice:
		// slices can be empty
		if v.IsNil() {
			e.Write(nullLiteral)
			return
		}
		isSlice = true
		arrElemType = (*sliceType)(ptr(v.Type)).ElemType
		slcHeader = (*sliceHeader)(v.Ptr)
		arrLen = slcHeader.Len
		fl = addressableFlag | pointerFlag | Flag(arrElemType.Kind())
	}
	// Mark Array Start
	e.WriteByte(squareOpen)

	// doesn't seem to help much, but we're reusing it anyway
	elemVal := Value{Type: arrElemType, Flag: fl}

	for i := 0; i < arrLen; i++ {
		if i > 0 {
			// Next Array Element
			e.WriteByte(comma)
		}
		if isSlice {
			elemVal.Ptr = arrayAt(slcHeader.Data, i, arrElemType.size)
		} else {
			elemVal.Ptr = add(v.Ptr, uintptr(i)*arrElemType.size)
		}

		ae.encs[0](e, elemVal)
	}
	// Mark Array End
	e.WriteByte(squareClose)
}

func (ae *allEncoder) encodeMap(e *encodeState, v Value) {
	if v.IsNil() {
		// Write "null"
		e.Write(nullLiteral)
		return
	}
	// getting map length
	mapPtr := v.pointer()
	mapLen := 0
	if mapPtr != nil {
		mapLen = maplen(mapPtr)
	}
	// fail fast for no allocation
	if mapLen == 0 {
		e.WriteByte(curlOpen)
		e.WriteByte(curlClose)
		return
	}
	// prepare map keys
	typedMap := (*mapType)(ptr(v.Type))
	// iterating over map keys
	it := mapiterinit(v.Type, mapPtr)

	hasPointerToKeys := false
	mapKeys := make([]KeyValuePair, mapLen)
	for i := 0; i < mapLen; i++ {
		key := mapiterkey(it)
		if key == nil {
			// Someone deleted an entry from the map since we called maplen above. It's a data race, but nothing we can do about it.
			break
		}

		//var isPointer bool
		var keyPointer ptr

		if typedMap.KeyType.isDirectIface() {
			hasPointerToKeys = true
			//isPointer = true
			// TODO : developer should pay attention on NOT changing the map while we're Marshaling. If you enconter troubles, comment the next line and uncomment the two lines below it
			keyPointer = key
			// Below code was used to copy result so future changes to the map won't change the underlying value.
			// keyPointer = unsafeNew(typedMap.KeyType)
			// typedmemmove(typedMap.KeyType, keyPointer, key)
		} else {
			// by default we're taking the pointer (which means it's not a pointer to pointer)
			keyPointer = *(*ptr)(key)
		}

		// taking only the required info, ignoring the rest
		switch typedMap.KeyType.Kind() {
		case String:
			//TODO : test race condition - instead of copying key, we just reference to it, to reduce allocations
			header := (*stringHeader)(ptr(keyPointer))
			var b []byte
			byteHeader := (*sliceHeader)(ptr(&b))
			byteHeader.Data = header.Data
			byteHeader.Len = header.Len
			byteHeader.Cap = header.Len
			mapKeys[i] = KeyValuePair{
				keyName: b, //[]byte(*(*string)(keyPointer)),
				//isPointer: isPointer,
				ptr: keyPointer,
			}
		case Int:
			mapKeys[i] = KeyValuePair{
				keyName: FormatInt(int64(*(*int)(keyPointer))),
				//isPointer: isPointer,
				ptr: keyPointer,
			}
		case Int8:
			mapKeys[i] = KeyValuePair{
				keyName: FormatInt(int64(*(*int8)(keyPointer))),
				//isPointer: isPointer,
				ptr: keyPointer,
			}
		case Int16:
			mapKeys[i] = KeyValuePair{
				keyName: FormatInt(int64(*(*int16)(keyPointer))),
				//isPointer: isPointer,
				ptr: keyPointer,
			}
		case Int32:
			mapKeys[i] = KeyValuePair{
				keyName: FormatInt(int64(*(*int32)(keyPointer))),
				//isPointer: isPointer,
				ptr: keyPointer,
			}
		case Int64:
			mapKeys[i] = KeyValuePair{
				keyName: FormatInt(*(*int64)(keyPointer)),
				//isPointer: isPointer,
				ptr: keyPointer,
			}
		case Uint:
			mapKeys[i] = KeyValuePair{
				keyName: FormatUint(uint64(*(*uint)(keyPointer))),
				//isPointer: isPointer,
				ptr: keyPointer,
			}
		case Uint8:
			mapKeys[i] = KeyValuePair{
				keyName: FormatUint(uint64(*(*uint8)(keyPointer))),
				//isPointer: isPointer,
				ptr: keyPointer,
			}
		case Uint16:
			mapKeys[i] = KeyValuePair{
				keyName: FormatUint(uint64(*(*uint16)(keyPointer))),
				//isPointer: isPointer,
				ptr: keyPointer,
			}
		case Uint32:
			mapKeys[i] = KeyValuePair{
				keyName: FormatUint(uint64(*(*uint32)(keyPointer))),
				//isPointer: isPointer,
				ptr: keyPointer,
			}
		case Uint64:
			mapKeys[i] = KeyValuePair{
				keyName: FormatUint(*(*uint64)(keyPointer)),
				//isPointer: isPointer,
				ptr: keyPointer,
			}
		case UintPtr:
			mapKeys[i] = KeyValuePair{
				keyName: FormatUint(uint64(*(*uintptr)(keyPointer))),
				//isPointer: isPointer,
				ptr: keyPointer,
			}
		default:
			panic("Bad key kind!")
		}

		mapiternext(it)
	}

	// new feature : optional sorting for map keys (default false)
	if e.opts.willSortMapKeys {
		sort.Slice(mapKeys, func(i, j int) bool {
			//The result will be -1 if result[i].keyName < result[j].keyName, and +1 if result[i].keyName > result[j].keyName.
			return bytes.Compare(mapKeys[i].keyName, mapKeys[j].keyName) == -1
		})
	}

	// Mark Map Start
	e.WriteByte(curlOpen)

	// doesn't seem to help much, but we're reusing it anyway
	elemVal := Value{Type: typedMap.ElemType, Flag: Flag(typedMap.ElemType.Kind())}

	if typedMap.ElemType.isDirectIface() {
		elemVal.Flag = elemVal.Flag | pointerFlag
	}

	// default, unsorted map keys
	for i, key := range mapKeys {
		if i > 0 {
			e.WriteByte(comma)
		}
		writeBytes(e, key.keyName)
		e.WriteByte(colon)

		var keyPtr ptr
		//if key.isPointer {
		if hasPointerToKeys {
			keyPtr = key.ptr
		} else {
			keyPtr = ptr(&key.ptr)
		}

		elemPtr := mapaccess(v.Type, mapPtr, keyPtr)
		if typedMap.ElemType.isDirectIface() {
			// TODO : developer should pay attention on NOT changing the map while we're Marshaling. If you enconter troubles, comment the next line and uncomment the three lines below it
			elemVal.Ptr = elemPtr
			// Below code was used to copy result so future changes to the map won't change the underlying value.
			//mapElemValue := unsafeNew(typedMap.ElemType)
			//typedmemmove(typedMap.ElemType, mapElemValue, elemPtr)
			//elemVal.Ptr = mapElemValue
		} else {
			elemVal.Ptr = *(*ptr)(elemPtr)
		}

		ae.encs[0](e, elemVal)
	}

	// Mark Map End
	e.WriteByte(curlClose)
}

func (ae *allEncoder) encodeStruct(e *encodeState, v Value) {
	fields := getCachedFields(v.Type)

	if e.opts.willCheckEmptyStruct {
		allFieldsEmpty := true
		for _, curField := range fields {
			// restoring to original value type, since it gets altered below
			fieldValue := v

			for _, idx := range curField.indexes {
				if fieldValue.Kind() == Ptr {
					if fieldValue.IsNil() {
						continue
					}
					fieldValue = valueDeref(fieldValue)
				}
				fieldValue = fieldValue.getField(idx)
			}

			if !fieldValue.IsValid() {
				continue
			}

			if !isEmptyValue(fieldValue) {
				allFieldsEmpty = false
			}
		}

		// all fields are empty : omit the struct entirely
		if allFieldsEmpty {
			return
		}
	}

	// Mark Struct Start
	e.WriteByte(curlOpen)

	first := true
	for i, curField := range fields {
		// restoring to original value type, since it gets altered below
		fieldValue := v

		for _, idx := range curField.indexes {
			if fieldValue.Kind() == Ptr {
				if fieldValue.IsNil() {
					continue
				}
				fieldValue = valueDeref(fieldValue)
			}
			fieldValue = fieldValue.getField(idx)
		}

		// omitted invalid
		if !fieldValue.IsValid() {
			continue
		}

		// empty value and flagged as omitted
		if isEmptyValue(fieldValue) && curField.isOmitted {
			continue
		}

		// checking if it's a struct which has an omitempty option, but not nullable
		if !curField.isNullable && curField.isOmitted && fieldValue.Kind() == Struct {
			//println("checking (once) if is all native : " + string(curField.name))
			// checking if all the struct fields are marked as native
			allNative := true
			for _, strctField := range getCachedFields(fieldValue.Type) {
				if !strctField.isNative {
					allNative = false
					break
				}
			}
			// will omit empty, but not nullable (OPTIONAL Marshaler)
			if allNative {
				//println(string(curField.name) + " marked all native.")
				curField.isNullable = true // thus we won't enter this loop again
			}
		}

		if curField.isNullable {
			// current field is needed for checks of flags
			e.opts.willCheckEmptyStruct = true
			// special case : we're calling 1. unmarshal encoder or 2. struct encoder
			ae.encs[i](e, fieldValue)
			// the encoder responded
			if len(e.opts.nullableReturn) > 0 {
				//println("response : `" + string(e.opts.nullableReturn) + "` on " + string(curField.name))
				// we cannot move the below `comma` code above, because some fields are empty and we might double the commas
				if first {
					first = false
				} else {
					e.WriteByte(comma)
				}
				// we have a response from nullable struct or omit empty simple struct
				writeBytes(e, curField.name)
				e.WriteByte(colon)
				e.Write(e.opts.nullableReturn)
			}
			// reset the last return
			e.opts.nullableReturn = emptyByte
			// reset the current field
			e.opts.willCheckEmptyStruct = false
		} else {

			if first {
				first = false
			} else {
				e.WriteByte(comma)
			}

			writeBytes(e, curField.name)
			e.WriteByte(colon)
			e.opts.quoted = curField.isStringer

			ae.encs[i](e, fieldValue)
		}
	}

	//Mark Struct End
	e.WriteByte(curlClose)
}

// newTypeEncoder constructs an encoderFunc for a type.
// The returned encoder only checks CanAddr when allowAddr is true.
func newTypeEncoder(t *RType, allowAddr bool) encoderFunc {
	if t.implements(marshalerType) {
		return marshalerEncoder
	}
	if t.Kind() != Ptr && allowAddr {
		if t.PtrTo().implements(marshalerType) {
			ae := &allEncoder{encs: encFns{newTypeEncoder(t, false)}}
			return ae.encodeCond
		}
	}

	switch t.Kind() {
	case Bool:
		return boolEncoder
	case Int, Int8, Int16, Int32, Int64:
		return intEncoder
	case Uint, Uint8, Uint16, Uint32, Uint64, UintPtr:
		return uintEncoder
	case Float32:
		return float32Encoder
	case Float64:
		return float64Encoder
	case String:
		return stringEncoder
	case Interface:
		return interfaceEncoder
	case Struct:
		fieldsInfo := getCachedFields(t)
		se := &allEncoder{
			encs: make([]encoderFunc, len(fieldsInfo)),
		}
		for i, curField := range fieldsInfo {
			et := t
			for _, index := range curField.indexes {
				if et.Kind() == Ptr {
					// Deref
					et = (*ptrType)(ptr(et)).Type
				}
				st := (*structType)(ptr(et))
				field := &st.fields[index]
				et = field.Type
			}
			se.encs[i] = typeEncoder(et)
		}
		return se.encodeStruct
	case Map:
		mapInfo := (*mapType)(ptr(t))
		switch mapInfo.KeyType.Kind() {
		case String,
			Int, Int8, Int16, Int32, Int64,
			Uint, Uint8, Uint16, Uint32, Uint64, UintPtr:
		default:
			return unsupportedTypeEncoder

		}
		ae := &allEncoder{encs: encFns{typeEncoder((*mapType)(ptr(t)).ElemType)}}
		return ae.encodeMap
	case Slice:
		// read the slice element
		elemType := (*sliceType)(ptr(t)).ElemType
		// Byte slices get special treatment; arrays don't.
		if elemType.Kind() == Uint8 {
			ptrToDeref := elemType.PtrTo()
			// check if []int8 has it's own marshal implementation
			if !ptrToDeref.implements(marshalerType) {
				return encodeByteSlice
			}
		}
		ae := &allEncoder{encs: encFns{typeEncoder((*sliceType)(ptr(t)).ElemType)}}
		return ae.encodeArray
	case Array:
		ae := &allEncoder{encs: encFns{typeEncoder((*arrayType)(ptr(t)).ElemType)}}
		return ae.encodeArray
	case Ptr:
		// TODO :  deref all pointers to get rid of the struct and deref values faster
		ae := &allEncoder{encs: encFns{typeEncoder((*ptrType)(ptr(t)).Type)}}
		return ae.encodePtr
	default:
		return unsupportedTypeEncoder
	}
}

func typeEncoder(t *RType) encoderFunc {
	if fi, ok := universe.Load(t); ok {
		return fi.(encoderFunc)
	}

	// To deal with recursive types, populate the map with an
	// indirect func before we build it. This type waits on the
	// real func (f) to be ready and then calls it. This indirect
	// func is only used for recursive types.
	var (
		wg sync.WaitGroup
		f  encoderFunc
	)
	wg.Add(1)
	fi, loaded := universe.LoadOrStore(t, encoderFunc(func(e *encodeState, v Value) {
		wg.Wait()
		f(e, v)
	}))
	if loaded {
		return fi.(encoderFunc)
	}

	// Compute the real encoder and replace the indirect func with it.
	f = newTypeEncoder(t, true)
	wg.Done()
	universe.Store(t, f)
	return f
}

// getMarshalFields returns a list of fields that JSON should recognize for the given type.
// The algorithm is breadth-first search over the set of structs to include - the top struct and then any reachable anonymous structs.
func getMarshalFields(t *RType) marshalFields {
	// Embedded fields to explore at the current level and the next.
	var fields []visitField
	next := []visitField{{Type: t}}

	// Count of queued names for current level and the next.
	count := map[*RType]int{}
	nextCount := map[*RType]int{}

	// Types already visited at an earlier level.
	visited := map[*RType]bool{}
	empty := map[*RType]int{}

	// Fields found. Is better (?) than just declaring the var and append
	cs := (*structType)(ptr(t))
	result := make(marshalFields, 0, len(cs.fields))

	for len(next) > 0 {
		fields, next = next, fields[:0]
		count, nextCount = nextCount, empty

		for _, curField := range fields {
			if visited[curField.Type] {
				continue
			}
			visited[curField.Type] = true
			// Scan curField.Type for fields to include.
			curStruct := (*structType)(ptr(curField.Type))
			for index := range curStruct.fields {
				field := &curStruct.fields[index]
				fieldType := field.Type

				embedded := field.offsetEmbed&1 != 0
				isNotExported := false
				if !field.name.isExported() {
					isNotExported = curStruct.pkgPath.nameLen() > 0
				}
				if embedded {
					if fieldType.Kind() == Ptr {
						// Follow pointer.
						fieldType = (*ptrType)(ptr(fieldType)).Type
					}
					if isNotExported && fieldType.Kind() != Struct {
						// Ignore embedded fields of unexported non-struct types.
						continue
					}
					// Do not ignore embedded fields of unexported struct types since they may have exported fields.
				} else if isNotExported {
					// Ignore unexported non-embedded fields.
					continue
				}

				// start processing tags
				tag := tagLookup(field.name.tag(), jsonTagName)
				// ignored
				if idx := bytes.IndexByte(tag, minus); idx != -1 {
					continue
				}

				tagged := true
				jsonName, opts := parseTag(tag)
				if !isValidTag(jsonName) {
					tagged = false
					jsonName = field.name.name()
				}

				indexes := make([]int, len(curField.indexes)+1)
				copy(indexes, curField.indexes)
				indexes[len(curField.indexes)] = index

				fieldType = field.Type

				if !fieldType.hasName() && fieldType.Kind() == Ptr {
					// Follow pointer.
					fieldType = (*ptrType)(ptr(fieldType)).Type
				}

				fieldKind := fieldType.Kind()

				// Record found field and index sequence.
				if tagged || !embedded || fieldKind != Struct {

					// Only strings, floats, integers, and booleans.
					isNative := false
					switch fieldKind {
					case Bool, Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, UintPtr, Float32, Float64, String:
						isNative = true
					}

					f := MarshalField{
						name:        jsonName,
						hasValidTag: tagged,
						indexes:     indexes,
						equalFold:   foldFunc(jsonName),
						isStringer:  tagContains(opts, stringTagOption) && isNative,
						isNative:    isNative,
						isOmitted:   tagContains(opts, omitTagOption),
					}

					if fieldKind == Struct && f.isOmitted && bytes.HasPrefix(fieldType.byteName(), capitalNullLiteral) {
						f.isNullable = true
					}

					result = append(result, f)

					if count[curField.Type] > 1 {
						// If there were multiple instances, add a second, so that the annihilation code will see a duplicate.
						// It only cares about the distinction between 1 or 2, so don't bother generating any more copies.
						result = append(result, result[len(result)-1])
					}
					continue
				}

				// Record new anonymous struct to explore in next round.
				nextCount[fieldType]++
				if nextCount[fieldType] == 1 {
					f := visitField{indexes: indexes, Type: fieldType}
					next = append(next, f)
				}
			}
		}
	}

	sort.Slice(result, func(i, j int) bool {
		x := result
		// sort field by name, breaking ties with depth, then breaking ties with "name came from json tag", then breaking ties with index sequence.
		if !bytes.Equal(x[i].name, x[j].name) {
			//The result will be -1 if x[i].name < x[j].name, and +1 if x[i].name > x[j].name.
			return bytes.Compare(x[i].name, x[j].name) == -1
		}
		if len(x[i].indexes) != len(x[j].indexes) {
			return len(x[i].indexes) < len(x[j].indexes)
		}
		if x[i].hasValidTag != x[j].hasValidTag {
			return x[i].hasValidTag
		}
		return x.Less(i, j)
	})

	// Delete all fields that are hidden by the Go rules for embedded fields, except that fields with JSON tags are promoted.

	// The fields are sorted in primary order of name, secondary order of field index length. Loop over names; for each name, delete hidden fields by choosing the one dominant field that survives.
	out := result[:0]
	for advance, i := 0, 0; i < len(result); i += advance {
		// One iteration per name. Find the sequence of fields with the name of this first field.
		fi := result[i]
		for advance = 1; i+advance < len(result); advance++ {
			fj := result[i+advance]
			if !bytes.Equal(fj.name, fi.name) {
				break
			}
		}
		if advance == 1 { // Only one field with this name
			out = append(out, fi)
			continue
		}
		dominant, ok := dominantMarshalField(result[i : i+advance])
		if ok {
			out = append(out, dominant)
		}
	}

	result = out
	sort.Sort(result)

	return result
}

func getCachedFields(typ *RType) marshalFields {
	cachedFields, _ := fieldsCache.value.Load().(map[*RType]marshalFields)
	fieldsInfo := cachedFields[typ]
	if fieldsInfo == nil {
		// Compute fields without lock.
		// Might duplicate effort but won't hold other computations back.
		fieldsInfo = getMarshalFields(typ)
		if fieldsInfo != nil {
			fieldsCache.mu.Lock()
			cachedFields, _ = fieldsCache.value.Load().(map[*RType]marshalFields)

			newFieldsMap := make(map[*RType]marshalFields, len(cachedFields)+1)

			for typeKey, fieldsValues := range cachedFields {
				newFieldsMap[typeKey] = fieldsValues
			}

			newFieldsMap[typ] = fieldsInfo
			fieldsCache.value.Store(newFieldsMap)
			fieldsCache.mu.Unlock()
		}
	}
	return fieldsInfo
}
