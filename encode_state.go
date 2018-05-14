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
	"time"
	"unicode/utf8"
)

func (e *encodeState) stringBytes(s []byte) {
	e.WriteByte(quote)
	start := 0
	for i := 0; i < len(s); {
		if s[i] < utf8.RuneSelf {
			if htmlSafeSet[s[i]] || (!e.opts.escapeHTML && safeSet[s[i]]) {
				i++
				continue
			}
			if start < i {
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
				e.WriteString(`\u00`)
				e.WriteByte(hex[s[i]>>4])
				e.WriteByte(hex[s[i]&0xF])
			}
			i++
			start = i
			continue
		}
		c, size := utf8.DecodeRune(s[i:])
		if c == utf8.RuneError && size == 1 {
			if start < i {
				e.Write(s[start:i])
			}
			e.WriteString(`\ufffd`)
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
			e.WriteString(`\u202`)
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

func (e *encodeState) marshal(v interface{}) (err error) {
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
	walk(e, ReflectOn(v))
	return nil
}

func (e *encodeState) error(err error) {
	panic(err)
}

func marshalerEncoder(e *encodeState, v Value) {
	if v.Kind() == Ptr && v.IsNil() {
		e.Write(nullLiteral)
		return
	}
	m, ok := v.Interface().(Marshaler)
	if !ok {
		e.Write(nullLiteral)
		return
	}
	b, err := m.MarshalJSON()
	if err == nil {
		// copy JSON into buffer, checking validity.
		err = compact(&e.Buffer, b, e.opts.escapeHTML)
	}
	if err != nil {
		e.error(&MarshalerError{v.Type, err})
	}
}

func addrMarshalerEncoder(e *encodeState, v Value) {
	va := v.Addr()
	if va.IsNil() {
		e.Write(nullLiteral)
		return
	}
	m := va.Interface().(Marshaler)
	b, err := m.MarshalJSON()
	if err == nil {
		// copy JSON into buffer, checking validity.
		err = compact(&e.Buffer, b, true)
	}
	if err != nil {
		e.error(&MarshalerError{v.Type, err})
	}
}

func write(e *encodeState, v Value) {
	switch v.Kind() {
	case Bool:
		if e.opts.quoted {
			e.WriteByte(quote)
		}
		if *(*bool)(v.Ptr) {
			e.Write(trueLiteral)
		} else {
			e.Write(falseLiteral)
		}
		if e.opts.quoted {
			e.WriteByte(quote)
		}

	case Int:
		if e.opts.quoted {
			e.WriteByte(quote)
		}
		e.Write(FormatInt(int64(*(*int)(v.Ptr))))
		if e.opts.quoted {
			e.WriteByte(quote)
		}
	case Int8:
		if e.opts.quoted {
			e.WriteByte(quote)
		}
		e.Write(FormatInt(int64(*(*int8)(v.Ptr))))
		if e.opts.quoted {
			e.WriteByte(quote)
		}
	case Int16:
		if e.opts.quoted {
			e.WriteByte(quote)
		}
		e.Write(FormatInt(int64(*(*int16)(v.Ptr))))
		if e.opts.quoted {
			e.WriteByte(quote)
		}
	case Int32:
		if e.opts.quoted {
			e.WriteByte(quote)
		}
		e.Write(FormatInt(int64(*(*int32)(v.Ptr))))
		if e.opts.quoted {
			e.WriteByte(quote)
		}
	case Int64:
		if e.opts.quoted {
			e.WriteByte(quote)
		}
		e.Write(FormatInt(*(*int64)(v.Ptr)))
		if e.opts.quoted {
			e.WriteByte(quote)
		}
	case Uint:
		if e.opts.quoted {
			e.WriteByte(quote)
		}
		e.Write(FormatUint(uint64(*(*uint)(v.Ptr))))
		if e.opts.quoted {
			e.WriteByte(quote)
		}
	case Uint8:
		if e.opts.quoted {
			e.WriteByte(quote)
		}
		e.Write(FormatUint(uint64(*(*uint8)(v.Ptr))))
		if e.opts.quoted {
			e.WriteByte(quote)
		}
	case Uint16:
		if e.opts.quoted {
			e.WriteByte(quote)
		}
		e.Write(FormatUint(uint64(*(*uint16)(v.Ptr))))
		if e.opts.quoted {
			e.WriteByte(quote)
		}
	case Uint32:
		if e.opts.quoted {
			e.WriteByte(quote)
		}
		e.Write(FormatUint(uint64(*(*uint32)(v.Ptr))))
		if e.opts.quoted {
			e.WriteByte(quote)
		}
	case Uint64:
		if e.opts.quoted {
			e.WriteByte(quote)
		}
		e.Write(FormatUint(*(*uint64)(v.Ptr)))
		if e.opts.quoted {
			e.WriteByte(quote)
		}
	case UintPtr:
		if e.opts.quoted {
			e.WriteByte(quote)
		}
		e.Write(FormatUint(uint64(*(*uintptr)(v.Ptr))))
		if e.opts.quoted {
			e.WriteByte(quote)
		}
	case Float32:
		value := float64(*(*float32)(v.Ptr))
		if math.IsInf(value, 0) || math.IsNaN(value) {
			e.error(&UnsupportedValueError{FormatFloat(value, 32)})
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
		var b []byte
		b = AppendFloat(b, value, fmt, 32)
		if fmt == eChr {
			// clean up e-09 to e-9
			n := len(b)
			if n >= 4 && b[n-4] == eChr && b[n-3] == minus && b[n-2] == zero {
				b[n-2] = b[n-1]
				b = b[:n-1]
			}
		}

		if e.opts.quoted {
			e.WriteByte(quote)
		}
		e.Write(b)
		if e.opts.quoted {
			e.WriteByte(quote)
		}
	case Float64:
		value := *(*float64)(v.Ptr)
		if math.IsInf(value, 0) || math.IsNaN(value) {
			e.error(&UnsupportedValueError{FormatFloat(value, 64)})
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
		var b []byte
		b = AppendFloat(b, value, fmt, 64)
		if fmt == eChr {
			// clean up e-09 to e-9
			n := len(b)
			if n >= 4 && b[n-4] == eChr && b[n-3] == minus && b[n-2] == zero {
				b[n-2] = b[n-1]
				b = b[:n-1]
			}
		}

		if e.opts.quoted {
			e.WriteByte(quote)
		}
		e.Write(b)
		if e.opts.quoted {
			e.WriteByte(quote)
		}
	case String:
		theStr := *(*string)(v.Ptr)
		if v.Type == typeOfNo {
			// In Go1.5 the empty string encodes to "0", while this is not a valid number literal we keep compatibility so check validity after this.
			if len(theStr) == 0 {
				theStr = "0" // Number's zero-val
			}
			if !IsValidNumber(theStr) {
				e.error(errors.New("json: invalid number literal `" + theStr + "`"))
			}
			e.WriteString(theStr)
		} else {
			if e.opts.quoted {
				sb, err := Marshal(theStr)
				if err != nil {
					e.error(err)
				}
				e.stringBytes(sb)
			} else {
				bStr := []byte(theStr)
				e.stringBytes(bStr)
			}
		}

	}
}

func walk(e *encodeState, v Value) {
	if !v.IsValid() {
		// write "null"
		e.Write(nullLiteral)
		return
	}

	// check Marshaler implementation
	if v.Type.implements(marshalerType) {
		marshalerEncoder(e, v)
		return
	}

	if v.Kind() != Ptr && v.Type.PtrTo().implements(marshalerType) && v.CanAddr() {
		addrMarshalerEncoder(e, v)
		return
	}

	// no Marshaler
	switch v.Kind() {
	case Bool, Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, UintPtr, Float32, Float64, String:
		write(e, v)
	case Interface:
		if v.IsNil() {
			// write "null"
			e.Write(nullLiteral)
			return
		}
		walk(e, v.Iface())
	case Ptr:
		if v.IsNil() {
			// write "null"
			e.Write(nullLiteral)
			return
		}
		walk(e, v.Deref())
	case Map:
		mapEncoder(e, v)
	case Array:
		arrayEncoder(e, v)
	case Slice:
		sliceEncoder(e, v)
	case Struct:
		structEncoder(e, v)
	default:
		e.error(&UnsupportedTypeError{v.Type})
	}
}

func sliceEncoder(e *encodeState, v Value) {
	// read the slice element
	deref := v.Type.ConvToSlice().ElemType
	// Byte slices get special treatment; arrays don't.
	if deref.Kind() == Uint8 {
		p := deref.PtrTo()
		// check if []int8 has it's own marshal implementation
		if !p.implements(marshalerType) {
			if v.IsNil() {
				// write "null"
				e.Write(nullLiteral)
				return
			}

			e.WriteByte(quote)
			value := v.Bytes()
			if len(value) < 1024 {
				// for small buffers, using Encode directly is much faster.
				dst := make([]byte, base64.StdEncoding.EncodedLen(len(value)))
				base64.StdEncoding.Encode(dst, value)
				e.Write(dst)
			} else {
				// for large buffers, avoid unnecessary extra temporary buffer space.
				enc := base64.NewEncoder(base64.StdEncoding, e)
				enc.Write(value)
				enc.Close()
			}
			e.WriteByte(quote)

			return
		}
	}

	if v.IsNil() {
		// write "null"
		e.Write(nullLiteral)
		return
	}

	// process "array"
	arrayEncoder(e, v)
}

func arrayEncoder(e *encodeState, v Value) {
	// Mark Array Start
	e.WriteByte(squareOpen)
	arrLen := 0
	isSlice := false
	var slcHeader *sliceHeader
	var elemType *RType
	var fl Flag
	switch v.Kind() {
	case Array:
		arrType := (*arrayType)(ptr(v.Type))
		arrLen = int(arrType.Len)
		elemType = arrType.ElemType
		fl = v.Flag&(pointerFlag|addressableFlag) | v.ro() | Flag(elemType.Kind())
	case Slice:
		isSlice = true
		slcHeader = (*sliceHeader)(v.Ptr)
		arrLen = slcHeader.Len
		elemType = (*sliceType)(ptr(v.Type)).ElemType
		fl = addressableFlag | pointerFlag | v.ro() | Flag(elemType.Kind())
	}

	for i := 0; i < arrLen; i++ {
		if i > 0 {
			// Next Array Element
			e.WriteByte(comma)
		}
		if isSlice {
			walk(e, Value{Type: elemType, Ptr: arrayAt(slcHeader.Data, i, elemType.size), Flag: fl})
		} else {
			walk(e, Value{Type: elemType, Ptr: add(v.Ptr, uintptr(i)*elemType.size), Flag: fl})
		}
	}
	// Mark Array End
	e.WriteByte(squareClose)
}

func getCachedFields(typ *RType) marshalFields {
	cachedFields, _ := fieldsCache.value.Load().(map[*RType]marshalFields)
	fieldsInfo := cachedFields[typ]
	if fieldsInfo == nil {
		// Compute fields without lock.
		// Might duplicate effort but won't hold other computations back.
		fieldsInfo = typ.getMarshalFields()
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

func structEncoder(e *encodeState, v Value) {

	fieldsInfo := getCachedFields(v.Type)

	// Mark Struct Start
	e.WriteByte(curlOpen)

	first := true
	for _, curField := range fieldsInfo {
		// restoring to original value type, since it gets altered below
		fieldValue := v

		for _, idx := range curField.indexes {
			if fieldValue.Kind() == Ptr {
				if fieldValue.IsNil() {
					continue
				}
				fieldValue = fieldValue.Deref()
			}
			fieldValue = fieldValue.getField(idx)
		}

		// omitted invalid
		if !fieldValue.IsValid() {
			continue
		}

		// empty value and flagged as omitted
		if fieldValue.isEmptyValue() && curField.willOmit {
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
							e.stringBytes(curField.name)
							e.WriteByte(colon)
							e.opts.quoted = curField.isBasic
							write(e, carrierField)
						}
						continue
					default:
						if !isTrue {
							// if it's time, we're avoiding writing "null"
							_, isTime := carrierField.Interface().(time.Time)
							if isTime {
								continue
							}
						}
						// even if it's time, we're allowing Marshaler implementations (just to be able to format it)
					}
				}
			}
		}

		if !first {
			e.WriteByte(comma)
		}

		e.stringBytes(curField.name)
		e.WriteByte(colon)
		e.opts.quoted = curField.isBasic

		switch fieldValue.Kind() {
		case Bool, Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, UintPtr, Float32, Float64, String:
			// check Marshaler implementation
			if fieldValue.Type.implements(marshalerType) {
				marshalerEncoder(e, fieldValue)
			} else if fieldValue.Kind() != Ptr && fieldValue.Type.PtrTo().implements(marshalerType) && fieldValue.CanAddr() {
				addrMarshalerEncoder(e, fieldValue)
			} else {
				write(e, fieldValue)
			}
		default:
			walk(e, fieldValue)
		}

		if first {
			first = false
		}

	}

	//Mark Struct End
	e.WriteByte(curlClose)
}

func mapEncoder(e *encodeState, v Value) {
	if v.IsNil() {
		// Write "null"
		e.Write(nullLiteral)
		return
	}

	// prepare easy access
	mapElemType := v.Type.ConvToMap().ElemType
	fl := v.ro() | Flag(mapElemType.Kind())
	vPointer := v.pointer()

	// new feature : optional sorting for map keys (default false)
	if e.opts.willSortMapKeys {
		// Mark Map Start
		e.WriteByte(curlOpen)

		result := make([]KeyValuePair, len(v.MapKeys()))

		for idx, key := range v.MapKeys() {
			result[idx].value = key
			if err := result[idx].resolve(); err != nil {
				//error(&MarshalerError{key.Type(), err})
				panic("Error : " + err.Error() + " on " + key.Type.Name())
			}
		}
		sort.Slice(result, func(i, j int) bool {
			//The result will be -1 if result[i].keyName < result[j].keyName, and +1 if result[i].keyName > result[j].keyName.
			if bytes.Compare(result[i].keyName, result[j].keyName) == -1 {
				return true
			}
			return false
		})

		for i, key := range result {
			if i > 0 {
				e.WriteByte(comma)
			}
			e.stringBytes(key.keyName)
			e.WriteByte(colon)

			var keyPtr ptr
			if key.value.isPointer() {
				keyPtr = key.value.Ptr
			} else {
				keyPtr = ptr(&key.value.Ptr)
			}
			elemPtr := mapaccess(v.Type, vPointer, keyPtr)
			if elemPtr != nil {
				if mapElemType.isDirectIface() {
					// Copy result so future changes to the map won't change the underlying value.
					mapElemValue := unsafeNew(mapElemType)
					typedmemmove(mapElemType, mapElemValue, elemPtr)
					walk(e, Value{mapElemType, mapElemValue, fl | pointerFlag | key.value.ro()})
				} else {
					walk(e, Value{mapElemType, convPtr(elemPtr), fl | key.value.ro()})
				}
			} else {
				panic("elemPtr == nil")
			}

		}

		// Mark Map End
		e.WriteByte(curlClose)
		return
	}

	// Mark Map Start
	e.WriteByte(curlOpen)
	// checking and setting key kind
	keyKind := Invalid
	for _, key := range v.MapKeys() {
		keyKind = key.Kind()
		switch keyKind {
		case Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, UintPtr, String:
		default:
			panic("Bad key kind!")
		}
		break
	}
	// default, unsorted map keys
	for i, key := range v.MapKeys() {
		var keyName []byte

		switch keyKind {
		case Int:
			keyName = FormatInt(int64(*(*int)(key.Ptr)))
		case Int8:
			keyName = FormatInt(int64(*(*int8)(key.Ptr)))
		case Int16:
			keyName = FormatInt(int64(*(*int16)(key.Ptr)))
		case Int32:
			keyName = FormatInt(int64(*(*int32)(key.Ptr)))
		case Int64:
			keyName = FormatInt(*(*int64)(key.Ptr))
		case Uint:
			keyName = FormatUint(uint64(*(*uint)(key.Ptr)))
		case Uint8:
			keyName = FormatUint(uint64(*(*uint8)(key.Ptr)))
		case Uint16:
			keyName = FormatUint(uint64(*(*uint16)(key.Ptr)))
		case Uint32:
			keyName = FormatUint(uint64(*(*uint32)(key.Ptr)))
		case Uint64:
			keyName = FormatUint(*(*uint64)(key.Ptr))
		case UintPtr:
			keyName = FormatUint(uint64(*(*uintptr)(key.Ptr)))
		case String:
			keyName = []byte(*(*string)(key.Ptr))
		}
		if i > 0 {
			e.WriteByte(comma)
		}
		e.stringBytes(keyName)
		e.WriteByte(colon)

		var keyPtr ptr
		if key.isPointer() {
			keyPtr = key.Ptr
		} else {
			keyPtr = ptr(&key.Ptr)
		}
		elemPtr := mapaccess(v.Type, vPointer, keyPtr)
		if elemPtr != nil {
			if mapElemType.isDirectIface() {
				// Copy result so future changes to the map won't change the underlying value.
				mapElemValue := unsafeNew(mapElemType)
				typedmemmove(mapElemType, mapElemValue, elemPtr)
				walk(e, Value{Type: mapElemType, Ptr: mapElemValue, Flag: fl | pointerFlag | key.ro()})
			} else {
				walk(e, Value{Type: mapElemType, Ptr: convPtr(elemPtr), Flag: fl | key.ro()})
			}
		} else {
			panic("elemPtr == nil")
		}
	}

	// Mark Map End
	e.WriteByte(curlClose)
}

// getMarshalFields returns a list of fields that JSON should recognize for the given type.
// The algorithm is breadth-first search over the set of structs to include - the top struct and then any reachable anonymous structs.
func (t *RType) getMarshalFields() marshalFields {
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
				fieldName := make([]byte, field.name.nameLen())
				copy(fieldName, field.name.name())
				fieldTag := make([]byte, field.name.tagLen())
				copy(fieldTag, field.name.tag())

				embedded := field.offsetEmbed&1 != 0
				isNotExported := false
				if !field.name.isExported() {
					isNotExported = curStruct.pkgPath.nameLen() > 0
				}
				if embedded {
					if fieldType.Kind() == Ptr {
						// Follow pointer.
						fieldType = fieldType.Deref()
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
				tag := tagLookup(fieldTag, jsonTagName)
				// ignored
				if idx := bytes.IndexByte(tag, minus); idx != -1 {
					continue
				}

				tagged := true
				jsonName, opts := parseTag(tag)
				if !isValidTag(jsonName) {
					tagged = false
					jsonName = fieldName
				}

				indexes := make([]int, len(curField.indexes)+1)
				copy(indexes, curField.indexes)
				indexes[len(curField.indexes)] = index

				fieldType = field.Type

				if len(fieldType.Name()) == 0 && fieldType.Kind() == Ptr {
					// Follow pointer.
					fieldType = fieldType.Deref()
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
			if bytes.Compare(x[i].name, x[j].name) == -1 {
				return true
			}
			return false
		}
		if len(x[i].indexes) != len(x[j].indexes) {
			return len(x[i].indexes) < len(x[j].indexes)
		}
		if x[i].tag != x[j].tag {
			return x[i].tag
		}
		return marshalFields(x).Less(i, j)
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
		dominant, ok := result[i : i+advance].dominantMarshalField()
		if ok {
			out = append(out, dominant)
		}
	}

	result = out
	sort.Sort(marshalFields(result))

	return result
}
