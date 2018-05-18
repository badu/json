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
	"runtime"
)

// error aborts the decoding by panicking with err.
func decodeError(d *decodeState, err error) {
	panic(addErrorContext(d, err))
}

// saveError saves the first err it is called with, for reporting at the end of the unmarshal.
func saveError(d *decodeState, err error) {
	if d.opts.savedError == nil {
		d.opts.savedError = addErrorContext(d, err)
	}
}

// addErrorContext returns a new error enhanced with information from d.errorContext
func addErrorContext(d *decodeState, err error) error {
	if len(d.opts.errorContext.Field) > 0 {
		switch err := err.(type) {
		case *UnmarshalTypeError:
			err.Struct = d.opts.errorContext.Type.Name()
			err.Field = string(d.opts.errorContext.Field)
			return err
		}
	}
	return err
}

func initState(d *decodeState, data []byte) *decodeState {
	d.data = data
	d.offset = 0
	d.opts.savedError = nil
	return d
}

// next cuts off and returns the next full JSON value in d.data[d.off:].
// The next value is known to be an object or array, not a literal.
func readNext(d *decodeState) []byte {
	c := d.data[d.offset]
	item, rest, err := nextValue(&d.nextScan, d.data[d.offset:])
	if err != nil {
		decodeError(d, err)
	}
	d.offset = len(d.data) - len(rest)

	// Our scanner has seen the opening brace/bracket and thinks we're still in the middle of the object.
	// invent a closing brace/bracket to get it out.
	if c == curlOpen {
		d.scan.step(&d.scan, curlClose)
	} else {
		d.scan.step(&d.scan, squareClose)
	}

	return item
}

// scanWhile processes bytes in d.data[d.off:] until it receives a scan code not equal to op.
// It updates d.off and returns the new scan code.
func scanWhile(d *decodeState, op int) int {
	var newOp int
	for {
		if d.offset >= len(d.data) {
			newOp = scanEof(&d.scan)
			d.offset = len(d.data) + 1 // mark processed EOF with len+1
		} else {
			c := d.data[d.offset]
			d.offset++
			newOp = d.scan.step(&d.scan, c)
		}
		if newOp != op {
			break
		}
	}
	return newOp
}

func doRead(d *decodeState) {
	_, rest, err := nextValue(&d.nextScan, d.data[d.offset:])
	if err != nil {
		decodeError(d, err)
	}
	d.offset = len(d.data) - len(rest)
	// d.scan thinks we're still at the beginning of the item.
	// Feed in an empty string - the shortest, simplest value - so that it knows we got to the end of the value.
	if d.scan.redo {
		// rewind.
		d.scan.redo = false
		d.scan.step = stateBeginValue
	}
	d.scan.step(&d.scan, quote)
	d.scan.step(&d.scan, quote)
	n := len(d.scan.parseState)
	if n > 0 && d.scan.parseState[n-1] == parseObjectKey {
		// d.scan thinks we just read an object key; finish the object
		d.scan.step(&d.scan, colon)
		d.scan.step(&d.scan, quote)
		d.scan.step(&d.scan, quote)
		d.scan.step(&d.scan, curlClose)
	}
}

func unmarshal(d *decodeState, v interface{}) (err error) {
	defer func() {
		if r := recover(); r != nil {
			if _, ok := r.(runtime.Error); ok {
				panic(r)
			}
			err = r.(error)
		}
	}()

	value := ReflectOn(v)
	if value.Kind() != Ptr || value.IsNil() {
		return &InvalidUnmarshalError{TypeOf(v)}
	}

	scanReset(&d.scan)

	// We decode value not value.Deref because the Unmarshaler interface test must be applied at the top level of the value.
	process(d, value)
	return d.opts.savedError
}

func callUnmarshaller(d *decodeState, value Value, item []byte) {
	if !value.IsValid() {
		decodeError(d, errors.New("Invalid value while calling Unmarshal"))
		return
	}
	unmarshaler, _ := value.valueInterface().(Unmarshaler)
	err := unmarshaler.UnmarshalJSON(item)
	if err != nil {
		decodeError(d, err)
	}
}

// value decodes a JSON value from d.data[d.off:] into the value.
// it updates d.off to point past the decoded value.
func process(d *decodeState, v Value) {
	if !v.IsValid() {
		doRead(d)
		return
	}

	//TODO : decoder knows what it begins - it should return to walker this "state" (array, object, etc)
	switch op := scanWhile(d, scanSkipSpace); op {
	default:
		decodeError(d, errPhase)
	case scanBeginArray:
		isUnmarshaler := false
		v, isUnmarshaler = indirect(v, false)
		if isUnmarshaler {
			d.offset--
			callUnmarshaller(d, v, readNext(d))
			return
		}

		doArray(d, v)

	case scanBeginObject:
		isUnmarshaler := false
		v, isUnmarshaler = indirect(v, false)
		if isUnmarshaler {
			d.offset--
			callUnmarshaller(d, v, readNext(d))
			return
		}

		// object consumes an object from d.data[d.off-1:], decoding into the value v.
		// the first byte (curlOpen) of the object has been read already.
		// Decoding into nil interface? Switch to non-reflect code.

		if v.Kind() == Interface && v.NumMethod() == 0 {
			v.Set(ReflectOn(objectInterface(d)))
			return
		}

		switch v.Kind() {
		default:
			saveError(d, &UnmarshalTypeError{Value: "object", Type: v.Type, Offset: int64(d.offset)})
			d.offset--
			readNext(d) // skip over { } in input

		case Map:
			doMap(d, v)

		case Struct:
			doStruct(d, v)
		}

	case scanBeginLiteral:

		// literal consumes a literal from d.data[d.off-1:], decoding into the value v.
		// The first byte of the literal has been read already (that's how the caller knows it's a literal).

		// All bytes inside literal return scanContinue op code.
		start := d.offset - 1
		op := scanWhile(d, scanContinue)

		// Scan read one byte too far; back up.
		d.offset--
		scanUndo(&d.scan, op)

		item := d.data[start:d.offset]

		literalStore(d, item, v)
	}
}

// literalStore decodes a literal stored in item into v.
// fromQuoted indicates whether this literal came from unwrapping a string from the ",string" struct tag option. this is used only to produce more helpful error messages.
func literalStore(d *decodeState, item []byte, v Value) {
	if len(item) == 0 {
		// Empty string given
		saveError(d, errors.New("json: invalid use of ,string struct tag, trying to unmarshal "+string(item)+" into "+v.Type.String()))
		return
	}

	isUnmarshaler := false

	// Check for unmarshaler.
	if item[0] == nChr { // null
		v, isUnmarshaler = indirect(v, true)
	} else {
		v, isUnmarshaler = indirect(v, false)
	}

	if isUnmarshaler {
		callUnmarshaller(d, v, item)
		return
	}

	switch c := item[0]; c {
	case nChr: // null
		// The main parser checks that only true and false can reach here,
		// but if this was a isStringer string input, it could be anything.
		if !bytes.Equal(item, nullLiteral) {
			saveError(d, errors.New("json: invalid use of ,string struct tag, trying to unmarshal "+string(item)+" into "+v.Type.String()))
			return
		}
		//	setting null.
		switch v.Kind() {
		case Interface, Ptr, Map, Slice:
			v.SetZero(v.Type)
			// otherwise, ignore null for primitives/string
		}

	case tChr, fChr: // true, false
		boolValue := item[0] == tChr
		// The main parser checks that only true and false can reach here,
		// but if this was a isStringer string input, it could be anything.
		if !bytes.Equal(item, trueLiteral) && !bytes.Equal(item, falseLiteral) {
			saveError(d, errors.New("json: invalid use of ,string struct tag, trying to unmarshal "+string(item)+" into "+v.Type.String()))
			return
		}

		switch v.Kind() {
		default:
			saveError(d, &UnmarshalTypeError{Value: "bool", Type: v.Type, Offset: int64(d.offset)})
			return
		case Bool:
			*(*bool)(v.Ptr) = boolValue
		case Interface:
			if v.NumMethod() == 0 {
				v.Set(ReflectOn(boolValue))
			} else {
				saveError(d, &UnmarshalTypeError{Value: "bool", Type: v.Type, Offset: int64(d.offset)})
				return
			}
		}

	case quote: // string
		switch v.Kind() {
		default:
			saveError(d, &UnmarshalTypeError{Value: "string", Type: v.Type, Offset: int64(d.offset)})
			return
		case Slice:
			if (*sliceType)(ptr(v.Type)).ElemType.Kind() != Uint8 {
				saveError(d, &UnmarshalTypeError{Value: "string", Type: v.Type, Offset: int64(d.offset)})
				return
			}
			b := make([]byte, base64.StdEncoding.DecodedLen(len(item)))
			s, ok := unquoteBytes(item)
			if !ok {
				decodeError(d, errors.New("json: invalid use of ,string struct tag, trying to unmarshal "+string(item)+" into "+v.Type.String()))
				return
			}
			n, err := base64.StdEncoding.Decode(b, s)
			if err != nil {
				saveError(d, &UnmarshalTypeError{Value: "string", Type: v.Type, Offset: int64(d.offset)})
				return
			}
			*(*[]byte)(v.Ptr) = b[:n]
		case String:
			s, ok := unquoteBytes(item)
			if !ok {
				decodeError(d, errors.New("json: invalid use of ,string struct tag, trying to unmarshal "+string(item)+" into "+v.Type.String()))
				return
			}
			*(*string)(v.Ptr) = *(*string)(ptr(&s)) // string := s []byte -> string
		case Interface:
			if v.NumMethod() == 0 {
				s, ok := unquoteBytes(item)
				if !ok {
					decodeError(d, errors.New("json: invalid use of ,string struct tag, trying to unmarshal "+string(item)+" into "+v.Type.String()))
					return
				}
				v.Set(ReflectOn(string(s)))
			} else {
				saveError(d, &UnmarshalTypeError{Value: "string", Type: v.Type, Offset: int64(d.offset)})
				return
			}
		}

	default: // number
		if c != minus && (c < zero || c > nine) {
			decodeError(d, errors.New("json: invalid use of ,string struct tag, trying to unmarshal "+string(item)+" into "+v.Type.String()))
			return
		}

		switch v.Kind() {
		default:
			if v.Kind() == String && v.Type == typeOfNo {
				//println("Yes, typed number, but string")
				if v.CanSet() {
					*(*string)(v.Ptr) = string(item)
				}
				//TODO: check condition below (unused?)
				if !IsValidNumber(string(item)) {
					saveError(d, &UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
					return
				}
			} else {
				saveError(d, &UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
				return
			}
		case Interface:
			n, err := convertNumber(item, d.opts.useNumber)
			if err != nil {
				saveError(d, &UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
				return
			}
			if v.NumMethod() != 0 {
				saveError(d, &UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
				return
			}
			v.Set(ReflectOn(n))
		case Int, Int8, Int16, Int32, Int64:
			result, isNegative := FastNumber(item)
			if isNegative {
				result = -result
			}
			if v.OverflowInt(int64(result)) {
				saveError(d, &UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
				return
			}
			// TODO : GLOBAL CanSet should be first condition.
			if v.CanSet() {
				switch v.Kind() {
				case Int8:
					*(*int8)(v.Ptr) = int8(result)
				case Int16:
					*(*int16)(v.Ptr) = int16(result)
				case Int32:
					*(*int32)(v.Ptr) = int32(result)
				case Int64:
					*(*int64)(v.Ptr) = int64(result)
				default:
					*(*int)(v.Ptr) = int(result)
				}
			}

		case Uint, Uint8, Uint16, Uint32, Uint64, UintPtr:
			result, isNegative := FastNumber(item)
			if isNegative {
				// TODO : make issue on golang github
				saveError(d, &UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
				return
			}
			if v.OverflowUint(result) {
				saveError(d, &UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
				return
			}

			if v.CanSet() {
				switch v.Kind() {
				case Uint8:
					*(*uint8)(v.Ptr) = uint8(result)
				case Uint16:
					*(*uint16)(v.Ptr) = uint16(result)
				case Uint32:
					*(*uint32)(v.Ptr) = uint32(result)
				case Uint64:
					*(*uint64)(v.Ptr) = result
				case UintPtr:
					*(*uintptr)(v.Ptr) = uintptr(result)
				default:
					*(*uint)(v.Ptr) = uint(result)
				}

			}

		case Float32:
			n, err := Atof32(item)
			if err != nil || v.OverflowFloat(float64(n)) {
				saveError(d, &UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
				return
			}

			if v.CanSet() {
				*(*float32)(v.Ptr) = n
			}

		case Float64:
			n, err := Atof64(item)
			if err != nil || v.OverflowFloat(n) {
				saveError(d, &UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
				return
			}

			if v.CanSet() {
				*(*float64)(v.Ptr) = n
			}
		}
	}
}

// array consumes an array from d.data[d.off-1:], decoding into the value v. the first byte of the array ('[') has been read already.
func doArray(d *decodeState, v Value) {
	if v.Kind() == Interface && v.NumMethod() == 0 {
		v.Set(ReflectOn(arrayInterface(d)))
		return
	}

	var slcHeader *sliceHeader
	var arrType *arrayType
	var elemType *RType
	theLen := 0
	isSlice := false
	// Check type of target.
	switch v.Kind() {
	case Array: // valid
		arrType = (*arrayType)(ptr(v.Type))
		theLen = int(arrType.Len)
		elemType = arrType.ElemType
	case Slice: // valid
		isSlice = true
		elemType = (*sliceType)(ptr(v.Type)).ElemType
		slcHeader = (*sliceHeader)(v.Ptr)
		theLen = slcHeader.Len
	default:
		saveError(d, &UnmarshalTypeError{Value: "array", Type: v.Type, Offset: int64(d.offset)})
		d.offset--
		readNext(d)
		return
	}

	var newSlice Value // for reusing
	// fill it out
	i := 0
	for {
		// Look ahead for ] - can only happen on first iteration.
		op := scanWhile(d, scanSkipSpace)
		if op == scanEndArray {
			break
		}

		// Back up so d.value can have the byte we just read.
		d.offset--
		scanUndo(&d.scan, op)

		// Get element of array, growing if necessary.
		if isSlice {
			// Grow slice if necessary
			if i >= slcHeader.Cap {
				newCap := slcHeader.Cap + slcHeader.Cap/2
				if newCap < 4 {
					newCap = 4 // by a fair roll of dice ?
				}

				// short version for MakeSlice(typ,len,cap)
				s := sliceHeader{unsafeNewArray(elemType, newCap), slcHeader.Len, newCap}
				newSlice = Value{Type: v.Type, Ptr: ptr(&s), Flag: pointerFlag | Flag(Slice)}

				// short version of Copy(destSlice, srcSlice)
				ds := *(*sliceHeader)(newSlice.Ptr)
				typedslicecopy(elemType, ds, *slcHeader)

				v.Set(newSlice)
			}

			if i >= slcHeader.Len {
				slcHeader.Len = i + 1 // short version of  v.SetLen(i + 1)
				theLen = slcHeader.Len
			}
		}

		if i < theLen {
			// Decode into element.
			if isSlice {
				process(d, Value{
					Type: elemType,
					Ptr:  arrayAt(slcHeader.Data, i, elemType.size),
					Flag: addressableFlag | pointerFlag | Flag(elemType.Kind()),
				})
			} else {
				process(d, Value{
					Type: elemType,
					Ptr:  add(v.Ptr, uintptr(i)*elemType.size),
					Flag: v.Flag&(pointerFlag|addressableFlag) | Flag(elemType.Kind()), // bits same as overall array,
				})
			}
		} else {
			// Ran out of fixed array: skip.
			process(d, Value{})
		}

		i++

		// Next token must be , or ].
		op = scanWhile(d, scanSkipSpace)
		if op == scanEndArray {
			break
		}
		if op != scanArrayValue {
			decodeError(d, errPhase)
		}
	}

	if i < theLen {
		switch v.Kind() {
		case Array:
			// Array. Zero the rest.
			for ; i < theLen; i++ {
				Value{
					Type: elemType,
					Ptr:  add(v.Ptr, uintptr(i)*elemType.size),
					Flag: v.Flag&(pointerFlag|addressableFlag) | Flag(elemType.Kind()), // bits same as overall array,
				}.SetZero(elemType)
			}
		default:
			(*sliceHeader)(v.Ptr).Len = i // short version of v.SetLen(i)
		}
	}

	if i == 0 && v.Kind() == Slice {
		// short version for MakeSlice(typ,len,cap)
		s := sliceHeader{unsafeNewArray(elemType, 0), 0, 0}
		newSlice := Value{Type: v.Type, Ptr: ptr(&s), Flag: pointerFlag | Flag(Slice)}

		v.Set(newSlice)
	}
}

func doMap(d *decodeState, v Value) {
	// Map key must either have string kind, have an integer kind
	// Check type of target: `struct` or `map[T1]T2` where `T1` is string, an integer type
	typedMap := (*mapType)(ptr(v.Type))
	valueTypeKey := typedMap.KeyType
	switch valueTypeKey.Kind() {
	case String,
		Int, Int8, Int16, Int32, Int64,
		Uint, Uint8, Uint16, Uint32, Uint64, UintPtr:
	default:
		saveError(d, &UnmarshalTypeError{Value: "map", Type: v.Type, Offset: int64(d.offset)})
		d.offset--
		readNext(d) // skip over { } in input
		return
	}

	if v.IsNil() {
		m := makemap(v.Type, 0)
		mapVal := Value{v.Type, m, Flag(Map)}
		v.Set(mapVal)
	}

	mapElem := internalNew(typedMap.ElemType)

	// fill it out
	for {
		// Read opening " of string key or closing }.
		op := scanWhile(d, scanSkipSpace)
		if op == scanEndObject {
			// closing } - can only happen on first iteration.
			break
		}
		if op != scanBeginLiteral {
			decodeError(d, errPhase)
		}

		// Read key.
		start := d.offset - 1
		op = scanWhile(d, scanContinue)
		item := d.data[start : d.offset-1]
		key, ok := unquoteBytes(item)
		if !ok {
			decodeError(d, errPhase)
		}

		// Read : before value.
		if op == scanSkipSpace {
			op = scanWhile(d, scanSkipSpace)
		}

		if op != scanObjectKey {
			decodeError(d, errPhase)
		}

		if mapElem.IsValid() {
			mapElem.SetZero(typedMap.ElemType)
		}
		// process it
		process(d, mapElem)

		// Write value back to map; if using struct, corespValue points into struct already.
		switch valueTypeKey.Kind() {
		case String:
			keyValue := valueConvert(ReflectOn(key), valueTypeKey)
			v.setMapIndex(typedMap, keyValue, mapElem)
		case Int, Int8, Int16, Int32, Int64:
			num, err := IntParse(key)
			if err != nil || Zero(valueTypeKey).OverflowInt(num) {
				saveError(d, &UnmarshalTypeError{Value: "number " + string(key), Type: valueTypeKey, Offset: int64(start + 1)})
				return
			}
			keyValue := valueConvert(ReflectOn(num), valueTypeKey)
			v.setMapIndex(typedMap, keyValue, mapElem)
		case Uint, Uint8, Uint16, Uint32, Uint64, UintPtr:
			num, err := UintParse(key)
			if err != nil || Zero(valueTypeKey).OverflowUint(num) {
				saveError(d, &UnmarshalTypeError{Value: "number " + string(key), Type: valueTypeKey, Offset: int64(start + 1)})
				return
			}
			keyValue := valueConvert(ReflectOn(num), valueTypeKey)
			v.setMapIndex(typedMap, keyValue, mapElem)
		}

		// Next token must be , or }.
		op = scanWhile(d, scanSkipSpace)
		if op == scanEndObject {
			break
		}
		if op != scanObjectValue {
			decodeError(d, errPhase)
		}
	}
}

func getFieldNamed(value Value, fieldName []byte) *MarshalField {
	cachedFields, _ := fieldsCache.value.Load().(map[*RType]marshalFields)
	fields := cachedFields[value.Type]
	if fields == nil {
		// Compute fields without lock.
		// Might duplicate effort but won't hold other computations back.
		fields = getMarshalFields(value.Type)
		if fields == nil {
			//fields = unmarshalFields{}
			return nil
		}

		fieldsCache.mu.Lock()
		cachedFields, _ = fieldsCache.value.Load().(map[*RType]marshalFields)
		newFieldsMap := make(map[*RType]marshalFields, len(cachedFields)+1)
		for k, v := range cachedFields {
			newFieldsMap[k] = v
		}
		newFieldsMap[value.Type] = fields
		fieldsCache.value.Store(newFieldsMap)
		fieldsCache.mu.Unlock()
	}

	var result *MarshalField
	for i := range fields {
		curField := &fields[i]
		if bytes.Equal(curField.name, fieldName) {
			return curField
		}
		if result == nil && curField.equalFold(curField.name, fieldName) {
			result = curField
		}
	}
	return result
}

func doStruct(d *decodeState, v Value) {

	// fill it out
	for {
		// Read opening " of string fieldName or closing }.
		op := scanWhile(d, scanSkipSpace)
		if op == scanEndObject {
			// closing } - can only happen on first iteration.
			break
		}
		if op != scanBeginLiteral {
			decodeError(d, errPhase)
		}

		// Read fieldName.
		start := d.offset - 1
		op = scanWhile(d, scanContinue)
		item := d.data[start : d.offset-1]
		fieldName, ok := unquoteBytes(item)
		if !ok {
			decodeError(d, errPhase)
		}
		// Read : before value.
		if op == scanSkipSpace {
			op = scanWhile(d, scanSkipSpace)
		}

		if op != scanObjectKey {
			decodeError(d, errPhase)
		}

		// extract field
		curField := getFieldNamed(v, fieldName)
		// Figure out field corresponding to fieldName.
		var corespValue Value
		isBasic := false // whether the value is wrapped in a string to be decoded first
		if curField != nil {
			corespValue = v
			isBasic = curField.isStringer
			for _, idx := range curField.indexes {
				if corespValue.Kind() == Ptr {
					if corespValue.IsNil() {
						// If a struct embeds a pointer to an unexported type, it is not possible to set a newly allocated value since the field is unexported.
						// See https://golang.org/issue/21357
						derefType := (*ptrType)(ptr(corespValue.Type)).Type
						if !corespValue.CanSet() {
							saveError(d, errors.New("json: cannot set embedded pointer to unexported struct: "+derefType.String()))
							// Invalidate corespValue to ensure d.value(corespValue) skips over the JSON value without assigning it to corespValue.
							corespValue = Value{}
							isBasic = false
							break
						}
						corespValue.Set(New(derefType))
					}
					corespValue = valueDeref(corespValue)
				}

				corespValue = corespValue.getField(idx)
			}
			d.opts.errorContext.Field = curField.name
			d.opts.errorContext.Type = v.Type

		} else if d.opts.useStrict {
			saveError(d, errors.New("json: unknown field "+string(fieldName)))
		}

		if isBasic {
			// valueQuoted is like value but decodes a isStringer string literal or literal null into an interface value.
			// If it finds anything other than a isStringer string literal or null, valueQuoted returns unquotedValue{}.
			switch op := scanWhile(d, scanSkipSpace); op {
			default:
				saveError(d, errors.New("json: should never happen, because we're expecting begin literal, but received operation "+string(FormatInt(int64(op)))))
			case scanBeginLiteral:
				// All bytes inside literal return scanContinue op code.
				start := d.offset - 1
				op := scanWhile(d, scanContinue)

				// Scan read one byte too far; back up.
				d.offset--
				scanUndo(&d.scan, op)
				item := d.data[start:d.offset]

				switch c := item[0]; c {
				case nChr: // null
					// setting null to corespValue
					switch corespValue.Kind() {
					case Interface, Ptr, Map, Slice:
						corespValue.SetZero(corespValue.Type)
						// otherwise, ignore null for primitives/string
					}
				case quote: // string
					s, ok := unquoteBytes(item)
					if !ok {
						decodeError(d, errPhase)
					}
					literalStore(d, s, corespValue)

				default: // number
					saveError(d, errors.New("json: invalid use of ,string struct tag, trying to unmarshal unquoted value into "+corespValue.Type.String()))
				}
			}
		} else {
			process(d, corespValue)
		}

		// Next token must be , or }.
		op = scanWhile(d, scanSkipSpace)
		if op == scanEndObject {
			break
		}
		if op != scanObjectValue {
			decodeError(d, errPhase)
		}

		d.opts.errorContext.Field = emptyByte
	}
}

// valueInterface is like value but returns interface{}
func valueInterface(d *decodeState) interface{} {
	switch scanWhile(d, scanSkipSpace) {
	case scanBeginArray:
		return arrayInterface(d)
	case scanBeginObject:
		return objectInterface(d)
	case scanBeginLiteral:
		return literalInterface(d)
	default:
		decodeError(d, errPhase)
		panic("Unreachable")
	}
}

// arrayInterface is like array but returns []interface{}.
func arrayInterface(d *decodeState) []interface{} {
	var v = make([]interface{}, 0)
	for {
		// Look ahead for ] - can only happen on first iteration.
		op := scanWhile(d, scanSkipSpace)
		if op == scanEndArray {
			break
		}

		// Back up so d.value can have the byte we just read.
		d.offset--
		scanUndo(&d.scan, op)

		v = append(v, valueInterface(d))

		// Next token must be , or ].
		op = scanWhile(d, scanSkipSpace)
		if op == scanEndArray {
			break
		}
		if op != scanArrayValue {
			decodeError(d, errPhase)
		}
	}
	return v
}

// objectInterface is like object but returns map[string]interface{}.
func objectInterface(d *decodeState) map[string]interface{} {
	m := make(map[string]interface{})
	for {
		// Read opening " of string key or closing }.
		op := scanWhile(d, scanSkipSpace)
		if op == scanEndObject {
			// closing } - can only happen on first iteration.
			break
		}
		if op != scanBeginLiteral {
			decodeError(d, errPhase)
		}

		// Read string key.
		start := d.offset - 1
		op = scanWhile(d, scanContinue)
		item := d.data[start : d.offset-1]
		key, ok := unquoteBytes(item)
		if !ok {
			decodeError(d, errPhase)
		}

		// Read : before value.
		if op == scanSkipSpace {
			op = scanWhile(d, scanSkipSpace)
		}
		if op != scanObjectKey {
			decodeError(d, errPhase)
		}

		// Read value. *(*string)(ptr(&key)) means transforming key []byte -> string
		m[*(*string)(ptr(&key))] = valueInterface(d)

		// Next token must be , or }.
		op = scanWhile(d, scanSkipSpace)
		if op == scanEndObject {
			break
		}
		if op != scanObjectValue {
			decodeError(d, errPhase)
		}
	}
	return m
}

// literalInterface is like literal but returns an interface value.
func literalInterface(d *decodeState) interface{} {
	// All bytes inside literal return scanContinue op code.
	start := d.offset - 1
	op := scanWhile(d, scanContinue)

	// Scan read one byte too far; back up.
	d.offset--
	scanUndo(&d.scan, op)
	item := d.data[start:d.offset]

	switch c := item[0]; c {
	case nChr: // null
		return nil

	case tChr, fChr: // true, false
		return c == tChr

	case quote: // string
		s, ok := unquoteBytes(item)
		if !ok {
			decodeError(d, errPhase)
		}
		return *(*string)(ptr(&s)) // return s []byte -> string

	default: // number
		if c != minus && (c < zero || c > nine) {
			decodeError(d, errPhase)
		}

		// convertNumber converts the number literal s to a float64 or a Number depending on the setting of d.useNumber.
		if d.opts.useNumber {
			return Number(string(item))
		}

		floa, err := Atof64(item)
		if err != nil {
			saveError(d, &UnmarshalTypeError{Value: "number " + string(item), Type: TypeOf(0.0), Offset: int64(d.offset)})
		}
		return floa
	}
}

// indirect walks down v allocating pointers as needed, until it gets to a non-pointer.
// if it encounters an Unmarshaler, indirect stops and returns that.
// if decodingNull is true, indirect stops at the last pointer so it can be set to nil.
func indirect(v Value, decodingNull bool) (Value, bool) {
	// If v is a named type and is addressable, start with its address, so that if the type has pointer methods, we find them.
	if v.Kind() != Ptr && v.Type.hasName() && v.CanAddr() {
		v = Value{Type: v.Type.PtrTo(), Ptr: v.Ptr, Flag: Flag(Ptr)}
	}
	for {
		// Load value from interface, but only if the result will be
		// usefully addressable.
		if v.Kind() == Interface && !v.IsNil() {
			e := valueIface(v)
			if e.Kind() == Ptr && !e.IsNil() && (!decodingNull || valueDeref(e).Kind() == Ptr) {
				v = e
				continue
			}
		}

		if v.Kind() != Ptr {
			break
		}

		// now v.Kind() == Ptr
		if valueDeref(v).Kind() != Ptr && decodingNull && v.CanSet() {
			break
		}

		if v.IsNil() {
			v.Set(New((*ptrType)(ptr(v.Type)).Type))
		}

		if v.Type.NumMethod() > 0 {
			if v.Type.implements(unmarshalerType) {
				return v, true
			}
		}

		v = valueDeref(v)
	}
	return v, false
}
