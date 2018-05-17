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
	"time"
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
				e.WriteBytes(backSlash, run)
			case newLine:
				e.WriteBytes(backSlash, nChr)
			case retChar:
				e.WriteBytes(backSlash, rChr)
			case tab:
				e.WriteBytes(backSlash, tChr)
			default:
				// This encodes bytes < 0x20 except for \t, \n and \r.
				// If escapeHTML is set, it also escapes <, >, and &
				// because they can lead to security holes when
				// user-controlled strings are rendered into JSON
				// and served to some browsers.
				e.WriteBytes(backSlash, uChr, zero, zero, hex[run>>4], hex[run&0xF])
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
			e.WriteBytes(backSlash, uChr, fChr, fChr, fChr, dChr)
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
			e.WriteBytes(backSlash, uChr, two, zero, two, hex[c&0xF])
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
				e.WriteBytes(backSlash, s[i])
			case newLine:
				e.WriteBytes(backSlash, nChr)
			case retChar:
				e.WriteBytes(backSlash, rChr)
			case tab:
				e.WriteBytes(backSlash, tChr)
			default:
				// This encodes bytes < 0x20 except for \t, \n and \r.
				// If escapeHTML is set, it also escapes <, >, and &
				// because they can lead to security holes when
				// user-controlled strings are rendered into JSON
				// and served to some browsers.
				e.WriteBytes(backSlash, uChr, zero, zero, hex[s[i]>>4], hex[s[i]&0xF])
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
			e.WriteBytes(backSlash, uChr, fChr, fChr, fChr, dChr)
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
			e.WriteBytes(backSlash, uChr, two, zero, two, hex[c&0xF])
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
		// TODO : signal error
		e.Write(nullLiteral)
		return
	}
	b, err := m.MarshalJSON()
	if err == nil {
		// copy JSON into buffer, checking validity.
		err = compact(&e.Buffer, b, e.opts.escapeHTML)
	}
	if err != nil {
		encodeError(e, &MarshalerError{v.Type, err})
	}
}

func addrMarshalerEncoder(e *encodeState, v Value) {
	va := Value{Type: v.Type.PtrTo(), Ptr: v.Ptr, Flag: v.ro() | Flag(Ptr)}
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
		// TODO : signal error
		e.Write(nullLiteral)
		return
	}
	b, err := m.MarshalJSON()
	if err == nil {
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
			e.WriteQuoted(trueLiteral)
		} else {
			e.Write(trueLiteral)
		}
	} else {
		if e.opts.quoted {
			e.WriteQuoted(falseLiteral)
		} else {
			e.Write(falseLiteral)
		}
	}
}

func intEncoder(e *encodeState, v Value) {
	if e.opts.quoted {
		e.WriteQuoted(FormatInt(v.Int()))
	} else {
		e.Write(FormatInt(v.Int()))
	}
}

func uintEncoder(e *encodeState, v Value) {
	if e.opts.quoted {
		e.WriteQuoted(FormatUint(v.Uint()))
	} else {
		e.Write(FormatUint(v.Uint()))
	}
}

func float32Encoder(e *encodeState, v Value) {
	value := float64(*(*float32)(v.Ptr))
	if math.IsInf(value, 0) || math.IsNaN(value) {
		encodeError(e, &UnsupportedValueError{FormatFloat(value, 32)})
	}

	// Convert as if by ES6 number to string conversion.
	// This matches most other JSON generators.
	// See golang.org/issue/6384 and golang.org/issue/14135.
	// Like fmt %g, but the exponent cutoffs are different and exponents themselves are not padded to two digits.
	abs := math.Abs(value)
	fmt := fChr
	// Note: Must use float32 comparisons for underlying float32 value to get precise cutoffs right.
	if abs != 0 {
		if float32(abs) < 1e-6 || float32(abs) >= 1e21 {
			fmt = eChr
		}

	}

	b := makeFloatBytes(value, fmt, 32)
	if fmt == eChr {
		// clean up e-09 to e-9
		n := len(b)
		if n >= 4 && b[n-4] == eChr && b[n-3] == minus && b[n-2] == zero {
			b[n-2] = b[n-1]
			b = b[:n-1]
		}
	}

	if e.opts.quoted {
		e.WriteQuoted(b)
	} else {
		e.Write(b)
	}
}

func float64Encoder(e *encodeState, v Value) {
	value := *(*float64)(v.Ptr)
	if math.IsInf(value, 0) || math.IsNaN(value) {
		encodeError(e, &UnsupportedValueError{FormatFloat(value, 64)})
	}

	// Convert as if by ES6 number to string conversion.
	// This matches most other JSON generators.
	// See golang.org/issue/6384 and golang.org/issue/14135.
	// Like fmt %g, but the exponent cutoffs are different and exponents themselves are not padded to two digits.
	abs := math.Abs(value)
	fmt := fChr
	if abs != 0 {
		if abs < 1e-6 || abs >= 1e21 {
			fmt = eChr
		}
	}

	b := makeFloatBytes(value, fmt, 64)
	if fmt == eChr {
		// clean up e-09 to e-9
		n := len(b)
		if n >= 4 && b[n-4] == eChr && b[n-3] == minus && b[n-2] == zero {
			b[n-2] = b[n-1]
			b = b[:n-1]
		}
	}

	if e.opts.quoted {
		e.WriteQuoted(b)
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
		e.WriteQuoted(dst)
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
		fl = v.Flag&(pointerFlag|addressableFlag) | v.ro() | Flag(arrType.Kind())
	case Slice:
		if v.IsNil() {
			e.Write(nullLiteral)
			return
		}
		isSlice = true
		arrElemType = (*sliceType)(ptr(v.Type)).ElemType
		slcHeader = (*sliceHeader)(v.Ptr)
		arrLen = slcHeader.Len
		fl = addressableFlag | pointerFlag | v.ro() | Flag(arrElemType.Kind())
	}
	// Mark Array Start
	e.WriteByte(squareOpen)

	// doesn't seem to help much, but we're reusing it anyway
	daVal := Value{Type: arrElemType, Flag: fl}
	for i := 0; i < arrLen; i++ {
		if i > 0 {
			// Next Array Element
			e.WriteByte(comma)
		}
		if isSlice {
			daVal.Ptr = arrayAt(slcHeader.Data, i, arrElemType.size)
		} else {
			daVal.Ptr = add(v.Ptr, uintptr(i)*arrElemType.size)
		}

		ae.encs[0](e, daVal)
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

	mapElemType := (*mapType)(ptr(v.Type)).ElemType
	// prepare easy access
	fl := v.ro() | Flag(mapElemType.Kind())
	vPointer := v.pointer()

	// prepare map keys
	typedMap := (*mapType)(ptr(v.Type))
	keyType := typedMap.KeyType

	mapkeyfl := v.ro() | Flag(keyType.Kind())

	mapPtr := v.pointer()
	mapLen := int(0)
	if mapPtr != nil {
		mapLen = maplen(mapPtr)
	}

	it := mapiterinit(v.Type, mapPtr)
	mapKeys := make([]Value, mapLen)
	for i := 0; i < len(mapKeys); i++ {
		key := mapiterkey(it)
		if key == nil {
			// Someone deleted an entry from the map since we called maplen above. It's a data race, but nothing we can do about it.
			break
		}
		if keyType.isDirectIface() {
			// Copy result so future changes to the map won't change the underlying value.
			keyValue := unsafeNew(keyType)
			typedmemmove(keyType, keyValue, key)
			mapKeys[i] = Value{keyType, keyValue, mapkeyfl | pointerFlag}
		} else {
			mapKeys[i] = Value{keyType, *(*ptr)(key), mapkeyfl}
		}
		mapiternext(it)
	}

	// new feature : optional sorting for map keys (default false)
	if e.opts.willSortMapKeys {
		// Mark Map Start
		e.WriteByte(curlOpen)

		// TODO : maybe use preparation above in the same operation (instead of iterating twice)
		result := make([]KeyValuePair, len(mapKeys))

		for idx, key := range mapKeys {
			result[idx].value = key
			if err := resolveKey(&result[idx]); err != nil {
				//error(&MarshalerError{key.Type(), err})
				panic("Error : " + err.Error() + " on " + key.Type.Name())
			}
		}
		sort.Slice(result, func(i, j int) bool {
			//The result will be -1 if result[i].keyName < result[j].keyName, and +1 if result[i].keyName > result[j].keyName.
			return bytes.Compare(result[i].keyName, result[j].keyName) == -1
		})
		// doesn't seem to help much, but we're reusing it anyway
		daVal := Value{Type: mapElemType}
		for j, key := range result {
			if j > 0 {
				e.WriteByte(comma)
			}
			writeBytes(e, key.keyName)
			e.WriteByte(colon)

			var keyPtr ptr
			if key.value.isPointer() {
				keyPtr = key.value.Ptr
			} else {
				keyPtr = ptr(&key.value.Ptr)
			}
			elemPtr := mapaccess(v.Type, vPointer, keyPtr)
			if mapElemType.isDirectIface() {
				// Copy result so future changes to the map won't change the underlying value.
				mapElemValue := unsafeNew(mapElemType)
				typedmemmove(mapElemType, mapElemValue, elemPtr)
				daVal.Ptr = mapElemValue
				daVal.Flag = fl | pointerFlag | key.value.ro()
			} else {
				daVal.Ptr = *(*ptr)(elemPtr)
				daVal.Flag = fl | key.value.ro()
			}

			ae.encs[0](e, daVal)
		}

		// Mark Map End
		e.WriteByte(curlClose)
		return
	}

	// Mark Map Start
	e.WriteByte(curlOpen)
	// checking and setting key kind
	keyKind := Invalid

	if len(mapKeys) > 0 {
		key := mapKeys[0]
		keyKind = key.Kind()
		switch keyKind {
		case Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, UintPtr, String:
		default:
			panic("Bad key kind!")
		}
	}
	// doesn't seem to help much, but we're reusing it anyway
	daVal := Value{Type: mapElemType}
	// default, unsorted map keys
	for i, key := range mapKeys {
		var keyName []byte

		switch keyKind {
		case Int, Int8, Int16, Int32, Int64:
			keyName = FormatInt(key.Int())
		case Uint, Uint8, Uint16, Uint32, Uint64, UintPtr:
			keyName = FormatUint(key.Uint())
		case String:
			keyName = []byte(*(*string)(key.Ptr))
		}
		if i > 0 {
			e.WriteByte(comma)
		}
		writeBytes(e, keyName)
		e.WriteByte(colon)

		var keyPtr ptr
		if key.isPointer() {
			keyPtr = key.Ptr
		} else {
			keyPtr = ptr(&key.Ptr)
		}
		elemPtr := mapaccess(v.Type, vPointer, keyPtr)
		if mapElemType.isDirectIface() {
			// Copy result so future changes to the map won't change the underlying value.
			mapElemValue := unsafeNew(mapElemType)
			typedmemmove(mapElemType, mapElemValue, elemPtr)
			daVal.Ptr = mapElemValue
			daVal.Flag = fl | pointerFlag | key.ro()
		} else {
			daVal.Ptr = *(*ptr)(elemPtr)
			daVal.Flag = fl | key.ro()
		}

		ae.encs[0](e, daVal)
	}

	// Mark Map End
	e.WriteByte(curlClose)
}

func (ae *allEncoder) encodeStruct(e *encodeState, v Value) {

	// Mark Struct Start
	e.WriteByte(curlOpen)

	first := true
	fieldsInfo := getCachedFields(v.Type)
	for i, curField := range fieldsInfo {
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
		if isEmptyValue(fieldValue) && curField.willOmit {
			continue
		}

		// Serialize Nulls Condition : 1. is a struct which has a name that starts with "Null" and must have "omitempty"
		if curField.isNullSuspect {
			subFields := getCachedFields(fieldValue.Type)

			// 2. has exactly two fields : one boolean (called Valid) and one of a basic type (called as the basic type - e.g. "String")
			if len(subFields) == 2 {
				var validField, carrierField Value
				for _, sf := range subFields {
					if bytes.Equal(sf.name, validLiteral) {
						validField = fieldValue
						for _, idx2 := range sf.indexes {
							if validField.Kind() == Ptr {
								if validField.IsNil() {
									validField = Value{}
									break
								}
							}
							validField = validField.getField(idx2)
						}
					} else {
						carrierField = fieldValue
						for _, idx2 := range sf.indexes {
							if carrierField.Kind() == Ptr {
								if carrierField.IsNil() {
									carrierField = Value{}
									break
								}
							}
							carrierField = carrierField.getField(idx2)
						}

					}
				}

				if validField.IsValid() && carrierField.IsValid() {
					isTrue := *(*bool)(validField.Ptr)

					switch carrierField.Kind() {
					case Bool, Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, UintPtr, Float32, Float64, String:
						if isTrue {
							if !first {
								e.WriteByte(comma)
							}
							writeBytes(e, curField.name)
							e.WriteByte(colon)
							e.opts.quoted = curField.isBasic
							switch carrierField.Kind() {
							case Bool:
								boolEncoder(e, carrierField)
							case Int, Int8, Int16, Int32, Int64:
								intEncoder(e, carrierField)
							case Uint, Uint8, Uint16, Uint32, Uint64, UintPtr:
								uintEncoder(e, carrierField)
							case Float32:
								float32Encoder(e, carrierField)
							case Float64:
								float64Encoder(e, carrierField)
							case String:
								stringEncoder(e, carrierField)
							}
						}
						continue
					default:
						if !isTrue {
							// if it's time, we're avoiding writing "null"
							_, isTime := carrierField.valueInterface().(time.Time)
							if isTime {
								continue
							}
						}
						// even if it's time, we're allowing Marshaler implementations (just to be able to format it)
					}
				}
			}
		}

		if first {
			first = false
		} else {
			e.WriteByte(comma)
		}

		writeBytes(e, curField.name)
		e.WriteByte(colon)
		e.opts.quoted = curField.isBasic

		ae.encs[i](e, fieldValue)
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
			return newCondAddrEncoder(newTypeEncoder(t, false)).encodeCond
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
		return newStructEncoder(t).encodeStruct
	case Map:
		mapInfo := (*mapType)(ptr(t))
		switch mapInfo.KeyType.Kind() {
		case String,
			Int, Int8, Int16, Int32, Int64,
			Uint, Uint8, Uint16, Uint32, Uint64, UintPtr:
		default:
			return unsupportedTypeEncoder

		}
		return newMapEncoder(t).encodeMap
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
		return newArrayEncoder(t).encodeArray
	case Array:
		return newArrayEncoder(t).encodeArray
	case Ptr:
		// deref all pointers
		/**
				derefType := t

				derefed := 0
				for derefType.Kind() == Ptr {
					derefType = (*ptrType)(ptr(derefType)).Type
					derefed++
				}
				if derefed != 1 {
					println("Deref " + StringKind(derefType.Kind()) + " " + string(FormatInt(int64(derefed))))
				}
		**/
		return newPtrEncoder(t).encodePtr
	default:
		return unsupportedTypeEncoder
	}
}

func newCondAddrEncoder(elseEnc encoderFunc) *allEncoder {
	return &allEncoder{encs: encFns{elseEnc}}
}

func newPtrEncoder(t *RType) *allEncoder {
	return &allEncoder{encs: encFns{typeEncoder((*ptrType)(ptr(t)).Type)}}
}

func newStructEncoder(t *RType) *allEncoder {
	fieldsInfo := getCachedFields(t)
	se := allEncoder{
		encs: make([]encoderFunc, len(fieldsInfo)),
	}

	for i, curField := range fieldsInfo {
		et := t
		for _, index := range curField.indexes {
			if et.Kind() == Ptr {
				et = (*ptrType)(ptr(et)).Type
			}
			st := (*structType)(ptr(et))
			field := &st.fields[index]
			et = field.Type
		}
		se.encs[i] = typeEncoder(et)
	}
	return &se
}

func newArrayEncoder(t *RType) *allEncoder {
	switch t.Kind() {
	case Array:
		return &allEncoder{encs: encFns{typeEncoder((*arrayType)(ptr(t)).ElemType)}}
	case Slice:
		return &allEncoder{encs: encFns{typeEncoder((*sliceType)(ptr(t)).ElemType)}}
	default:
		panic("Not Array, nor Slice")
	}
}

func newMapEncoder(t *RType) *allEncoder {
	return &allEncoder{encs: encFns{typeEncoder((*mapType)(ptr(t)).ElemType)}}
}

func typeEncoder(t *RType) encoderFunc {
	if fi, ok := encoderCache.Load(t); ok {
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
	fi, loaded := encoderCache.LoadOrStore(t, encoderFunc(func(e *encodeState, v Value) {
		wg.Wait()
		f(e, v)
	}))
	if loaded {
		return fi.(encoderFunc)
	}

	// Compute the real encoder and replace the indirect func with it.
	f = newTypeEncoder(t, true)
	wg.Done()
	encoderCache.Store(t, f)
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

	// Fields found.
	var result marshalFields

	for len(next) > 0 {
		fields, next = next, fields[:0]
		count, nextCount = nextCount, map[*RType]int{}

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
					// Only strings, floats, integers, and booleans implies isBasic.
					isBasic := false
					if tagContains(opts, stringTagOption) {
						switch fieldKind {
						case Bool, Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, UintPtr, Float32, Float64, String:
							isBasic = true
						}
					}
					willBeOmitted := tagContains(opts, omitTagOption)
					f := MarshalField{
						name:      jsonName,
						tag:       tagged,
						indexes:   indexes,
						equalFold: foldFunc(jsonName),
						isBasic:   isBasic,
						willOmit:  willBeOmitted,
					}

					if bytes.HasPrefix(fieldType.byteName(), capitalNullLiteral) && fieldKind == Struct && willBeOmitted {
						f.isNullSuspect = true
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
		if x[i].tag != x[j].tag {
			return x[i].tag
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
