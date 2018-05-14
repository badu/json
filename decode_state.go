/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"bytes"
	"encoding/base64"
	"fmt"
	"github.com/adrg/errors"
	"runtime"
)

// error aborts the decoding by panicking with err.
func (d *decodeState) error(err error) {
	panic(d.addErrorContext(err))
}

// saveError saves the first err it is called with, for reporting at the end of the unmarshal.
func (d *decodeState) saveError(err error) {
	if d.savedError == nil {
		d.savedError = d.addErrorContext(err)
	}
}

// addErrorContext returns a new error enhanced with information from d.errorContext
func (d *decodeState) addErrorContext(err error) error {
	if len(d.errorContext.Struct) > 0 || len(d.errorContext.Field) > 0 {
		switch err := err.(type) {
		case *UnmarshalTypeError:
			err.Struct = d.errorContext.Struct
			err.Field = d.errorContext.Field
			return err
		}
	}
	return err
}

func (d *decodeState) init(data []byte) *decodeState {
	d.data = data
	d.offset = 0
	d.savedError = nil
	d.errorContext.Struct = ""
	d.errorContext.Field = ""
	return d
}

// next cuts off and returns the next full JSON value in d.data[d.off:].
// The next value is known to be an object or array, not a literal.
func (d *decodeState) next() []byte {
	c := d.data[d.offset]
	item, rest, err := d.nextScan.nextValue(d.data[d.offset:])
	if err != nil {
		d.error(err)
	}
	d.offset = len(d.data) - len(rest)

	// Our scanner has seen the opening brace/bracket and thinks we're still in the middle of the object.
	// invent a closing brace/bracket to get it out.
	if c == curlOpen {
		d.scan.step(curlClose)
	} else {
		d.scan.step(squareClose)
	}

	return item
}

// scanWhile processes bytes in d.data[d.off:] until it receives a scan code not equal to op.
// It updates d.off and returns the new scan code.
func (d *decodeState) scanWhile(op int) int {
	var newOp int
	for {
		if d.offset >= len(d.data) {
			newOp = d.scan.eof()
			d.offset = len(d.data) + 1 // mark processed EOF with len+1
		} else {
			c := d.data[d.offset]
			d.offset++
			newOp = d.scan.step(c)
		}
		if newOp != op {
			break
		}
	}
	return newOp
}

func (d *decodeState) doRead() {
	_, rest, err := d.nextScan.nextValue(d.data[d.offset:])
	if err != nil {
		d.error(err)
	}
	d.offset = len(d.data) - len(rest)
	// d.scan thinks we're still at the beginning of the item.
	// Feed in an empty string - the shortest, simplest value - so that it knows we got to the end of the value.
	if d.scan.redo {
		// rewind.
		d.scan.redo = false
		d.scan.step = d.scan.stateBeginValue
	}
	d.scan.step(quote)
	d.scan.step(quote)
	n := len(d.scan.parseState)
	if n > 0 && d.scan.parseState[n-1] == parseObjectKey {
		// d.scan thinks we just read an object key; finish the object
		d.scan.step(colon)
		d.scan.step(quote)
		d.scan.step(quote)
		d.scan.step(curlClose)
	}
}

func (d *decodeState) unmarshal(v interface{}) (err error) {
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

	d.scan.reset()

	// We decode value not value.Deref because the Unmarshaler interface test must be applied at the top level of the value.
	d.process(value)
	return d.savedError
}

func (d *decodeState) callUnmarshaller(value Value, item []byte) {
	if !value.IsValid() {
		d.error(errors.New("Invalid value while calling Unmarshal"))
		return
	}
	unmarshaler, _ := value.valueInterface().(Unmarshaler)
	err := unmarshaler.UnmarshalJSON(item)
	if err != nil {
		d.error(err)
	}
}

// value decodes a JSON value from d.data[d.off:] into the value.
// it updates d.off to point past the decoded value.
func (d *decodeState) process(v Value) {
	if !v.IsValid() {
		d.doRead()
		return
	}

	//TODO : decoder knows what it begins - it should return to walker this "state" (array, object, etc)
	switch op := d.scanWhile(scanSkipSpace); op {
	default:
		d.error(errPhase)
	case scanBeginArray:
		isUnmarshaler := false
		v, isUnmarshaler = indirect(v, false)
		if isUnmarshaler {
			d.offset--
			d.callUnmarshaller(v, d.next())
			return
		}

		d.array(v)

	case scanBeginObject:
		isUnmarshaler := false
		v, isUnmarshaler = indirect(v, false)
		if isUnmarshaler {
			d.offset--
			d.callUnmarshaller(v, d.next())
			return
		}

		// object consumes an object from d.data[d.off-1:], decoding into the value v.
		// the first byte (curlOpen) of the object has been read already.
		// Decoding into nil interface? Switch to non-reflect code.

		if v.Kind() == Interface && v.NumMethod() == 0 {
			v.Set(ReflectOn(d.objectInterface()))
			return
		}

		switch v.Kind() {
		default:
			d.saveError(&UnmarshalTypeError{Value: "object", Type: v.Type, Offset: int64(d.offset)})
			d.offset--
			d.next() // skip over { } in input

		case Map:
			d.doMap(v)

		case Struct:
			d.doStruct(v)
		}

	case scanBeginLiteral:

		// literal consumes a literal from d.data[d.off-1:], decoding into the value v.
		// The first byte of the literal has been read already (that's how the caller knows it's a literal).

		// All bytes inside literal return scanContinue op code.
		start := d.offset - 1
		op := d.scanWhile(scanContinue)

		// Scan read one byte too far; back up.
		d.offset--
		d.scan.undo(op)

		item := d.data[start:d.offset]

		d.literalStore(item, v)
	}
}

// literalStore decodes a literal stored in item into v.
// fromQuoted indicates whether this literal came from unwrapping a string from the ",string" struct tag option. this is used only to produce more helpful error messages.
func (d *decodeState) literalStore(item []byte, v Value) {
	if len(item) == 0 {
		// Empty string given
		d.saveError(fmt.Errorf("json: invalid use of ,string struct tag, trying to unmarshal %q into %v", item, v.Type))
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
		d.callUnmarshaller(v, item)
		return
	}

	switch c := item[0]; c {
	case nChr: // null
		// The main parser checks that only true and false can reach here,
		// but if this was a isBasic string input, it could be anything.
		if !bytes.Equal(item, nullLiteral) {
			d.saveError(fmt.Errorf("json: invalid use of ,string struct tag, trying to unmarshal %q into %v", item, v.Type))
			return
		}
		//	setting null.
		switch v.Kind() {
		case Interface, Ptr, Map, Slice:
			v.Set(Zero(v.Type))
			// otherwise, ignore null for primitives/string
		}

	case tChr, fChr: // true, false
		boolValue := item[0] == tChr
		// The main parser checks that only true and false can reach here,
		// but if this was a isBasic string input, it could be anything.
		if !bytes.Equal(item, trueLiteral) && !bytes.Equal(item, falseLiteral) {
			d.saveError(fmt.Errorf("json: invalid use of ,string struct tag, trying to unmarshal %q into %v", item, v.Type))
			return
		}

		switch v.Kind() {
		default:
			d.saveError(&UnmarshalTypeError{Value: "bool", Type: v.Type, Offset: int64(d.offset)})
			return
		case Bool:
			*(*bool)(v.Ptr) = boolValue
		case Interface:
			if v.NumMethod() == 0 {
				v.Set(ReflectOn(boolValue))
			} else {
				d.saveError(&UnmarshalTypeError{Value: "bool", Type: v.Type, Offset: int64(d.offset)})
				return
			}
		}

	case quote: // string
		s, ok := unquoteBytes(item)
		if !ok {
			d.error(fmt.Errorf("json: invalid use of ,string struct tag, trying to unmarshal %q into %v", item, v.Type))
			return
		}

		switch v.Kind() {
		default:
			d.saveError(&UnmarshalTypeError{Value: "string", Type: v.Type, Offset: int64(d.offset)})
			return
		case Slice:
			stringValue := string(s)
			if v.Type.ConvToSlice().ElemType.Kind() != Uint8 {
				d.saveError(&UnmarshalTypeError{Value: "string", Type: v.Type, Offset: int64(d.offset)})
				return
			}
			b := make([]byte, base64.StdEncoding.DecodedLen(len(stringValue)))
			n, err := base64.StdEncoding.Decode(b, s)
			if err != nil {
				d.saveError(&UnmarshalTypeError{Value: "string", Type: v.Type, Offset: int64(d.offset)})
				return
			}
			*(*[]byte)(v.Ptr) = b[:n]
		case String:
			*(*string)(v.Ptr) = string(s)
		case Interface:
			if v.NumMethod() == 0 {
				stringValue := string(s)
				v.Set(ReflectOn(stringValue))
			} else {
				d.saveError(&UnmarshalTypeError{Value: "string", Type: v.Type, Offset: int64(d.offset)})
				return
			}
		}

	default: // number
		if c != minus && (c < zero || c > nine) {
			d.error(fmt.Errorf("json: invalid use of ,string struct tag, trying to unmarshal %q into %v", item, v.Type))
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
					d.saveError(&UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
					return
				}
			} else {
				d.saveError(&UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
				return
			}
		case Interface:
			n, err := convertNumber(item, d.useNumber)
			if err != nil {
				d.saveError(&UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
				return
			}
			if v.NumMethod() != 0 {
				d.saveError(&UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
				return
			}
			v.Set(ReflectOn(n))
		case Int, Int8, Int16, Int32, Int64:
			result, isNegative := FastNumber(item)
			if isNegative {
				result = -result
			}
			if v.OverflowInt(int64(result)) {
				d.saveError(&UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
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
				d.saveError(&UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
				return
			}
			if v.OverflowUint(result) {
				d.saveError(&UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
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
					*(*uint64)(v.Ptr) = uint64(result)
				case UintPtr:
					*(*uintptr)(v.Ptr) = uintptr(result)
				default:
					*(*uint)(v.Ptr) = uint(result)
				}

			}

		case Float32:
			n, err := Atof32(item)
			if err != nil || v.OverflowFloat(float64(n)) {
				d.saveError(&UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
				return
			}

			if v.CanSet() {
				*(*float32)(v.Ptr) = float32(n)
			}

		case Float64:
			n, err := Atof64(item)
			if err != nil || v.OverflowFloat(n) {
				d.saveError(&UnmarshalTypeError{Value: "number", Type: v.Type, Offset: int64(d.offset)})
				return
			}

			if v.CanSet() {
				*(*float64)(v.Ptr) = n
			}
		}
	}
}

// array consumes an array from d.data[d.off-1:], decoding into the value v. the first byte of the array ('[') has been read already.
func (d *decodeState) array(v Value) {
	if v.Kind() == Interface && v.NumMethod() == 0 {
		v.Set(ReflectOn(d.arrayInterface()))
		return
	}

	var slcHeader *sliceHeader
	var elemType *RType
	theLen := 0
	// Check type of target.
	switch v.Kind() {
	case Array: // valid
		theLen = int(v.Type.ConvToArray().Len)
		elemType = v.Type.ConvToArray().ElemType
	case Slice: // valid
		elemType = v.Type.ConvToSlice().ElemType
		slcHeader = (*sliceHeader)(v.Ptr)
		theLen = slcHeader.Len
	default:
		d.saveError(&UnmarshalTypeError{Value: "array", Type: v.Type, Offset: int64(d.offset)})
		d.offset--
		d.next()
		return
	}

	// fill it out
	i := 0
	for {
		// Look ahead for ] - can only happen on first iteration.
		op := d.scanWhile(scanSkipSpace)
		if op == scanEndArray {
			break
		}

		// Back up so d.value can have the byte we just read.
		d.offset--
		d.scan.undo(op)

		// Get element of array, growing if necessary.
		switch v.Kind() {
		case Slice:
			// Grow slice if necessary
			if i >= slcHeader.Cap {
				newCap := slcHeader.Cap + slcHeader.Cap/2
				if newCap < 4 {
					newCap = 4 // by a fair roll of dice ?
				}

				// short version for MakeSlice(typ,len,cap)
				s := sliceHeader{unsafeNewArray(elemType, newCap), slcHeader.Len, newCap}
				newSlice := Value{Type: v.Type, Ptr: ptr(&s), Flag: pointerFlag | Flag(Slice)}

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
			d.process(v.Index(i))
		} else {
			// Ran out of fixed array: skip.
			d.process(Value{})
		}

		i++

		// Next token must be , or ].
		op = d.scanWhile(scanSkipSpace)
		if op == scanEndArray {
			break
		}
		if op != scanArrayValue {
			d.error(errPhase)
		}
	}

	if i < theLen {
		switch v.Kind() {
		case Array:
			// Array. Zero the rest.
			zero := Zero(elemType)
			for ; i < theLen; i++ {
				v.Index(i).Set(zero)
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

func (d *decodeState) doMap(v Value) {
	// Map key must either have string kind, have an integer kind
	// Check type of target: `struct` or `map[T1]T2` where `T1` is string, an integer type
	typedMap := v.Type.ConvToMap()
	valueTypeKey := typedMap.KeyType
	switch valueTypeKey.Kind() {
	case String,
		Int, Int8, Int16, Int32, Int64,
		Uint, Uint8, Uint16, Uint32, Uint64, UintPtr:
	default:
		d.saveError(&UnmarshalTypeError{Value: "map", Type: v.Type, Offset: int64(d.offset)})
		d.offset--
		d.next() // skip over { } in input
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
		op := d.scanWhile(scanSkipSpace)
		if op == scanEndObject {
			// closing } - can only happen on first iteration.
			break
		}
		if op != scanBeginLiteral {
			d.error(errPhase)
		}

		// Read key.
		start := d.offset - 1
		op = d.scanWhile(scanContinue)
		item := d.data[start : d.offset-1]
		key, ok := unquoteBytes(item)
		if !ok {
			d.error(errPhase)
		}

		// Read : before value.
		if op == scanSkipSpace {
			op = d.scanWhile(scanSkipSpace)
		}

		if op != scanObjectKey {
			d.error(errPhase)
		}

		if mapElem.IsValid() {
			mapElem.Set(Zero(typedMap.ElemType))
		}
		// process it
		d.process(mapElem)

		// Write value back to map; if using struct, corespValue points into struct already.
		switch valueTypeKey.Kind() {
		case String:
			keyValue := ReflectOn(key).Convert(valueTypeKey)
			v.setMapIndex(typedMap, keyValue, mapElem)
		case Int, Int8, Int16, Int32, Int64:
			num, err := IntParse(key)
			if err != nil || Zero(valueTypeKey).OverflowInt(num) {
				d.saveError(&UnmarshalTypeError{Value: "number " + string(key), Type: valueTypeKey, Offset: int64(start + 1)})
				return
			}
			keyValue := ReflectOn(num).Convert(valueTypeKey)
			v.setMapIndex(typedMap, keyValue, mapElem)
		case Uint, Uint8, Uint16, Uint32, Uint64, UintPtr:
			num, err := UintParse(key)
			if err != nil || Zero(valueTypeKey).OverflowUint(num) {
				d.saveError(&UnmarshalTypeError{Value: "number " + string(key), Type: valueTypeKey, Offset: int64(start + 1)})
				return
			}
			keyValue := ReflectOn(num).Convert(valueTypeKey)
			v.setMapIndex(typedMap, keyValue, mapElem)
		}

		// Next token must be , or }.
		op = d.scanWhile(scanSkipSpace)
		if op == scanEndObject {
			break
		}
		if op != scanObjectValue {
			d.error(errPhase)
		}
	}
}

func (d *decodeState) getFieldNamed(value Value, fieldName []byte) *MarshalField {
	cachedFields, _ := fieldsCache.value.Load().(map[*RType]marshalFields)
	fields := cachedFields[value.Type]
	if fields == nil {
		// Compute fields without lock.
		// Might duplicate effort but won't hold other computations back.
		fields = value.Type.getMarshalFields()
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

func (d *decodeState) doStruct(v Value) {

	// fill it out
	for {
		// Read opening " of string fieldName or closing }.
		op := d.scanWhile(scanSkipSpace)
		if op == scanEndObject {
			// closing } - can only happen on first iteration.
			break
		}
		if op != scanBeginLiteral {
			d.error(errPhase)
		}

		// Read fieldName.
		start := d.offset - 1
		op = d.scanWhile(scanContinue)
		item := d.data[start : d.offset-1]
		fieldName, ok := unquoteBytes(item)
		if !ok {
			d.error(errPhase)
		}
		// Read : before value.
		if op == scanSkipSpace {
			op = d.scanWhile(scanSkipSpace)
		}

		if op != scanObjectKey {
			d.error(errPhase)
		}

		// extract field
		curField := d.getFieldNamed(v, fieldName)
		// Figure out field corresponding to fieldName.
		var corespValue Value
		isBasic := false // whether the value is wrapped in a string to be decoded first
		if curField != nil {
			corespValue = v
			isBasic = curField.isBasic
			for _, idx := range curField.indexes {
				if corespValue.Kind() == Ptr {
					if corespValue.IsNil() {
						// If a struct embeds a pointer to an unexported type, it is not possible to set a newly allocated value since the field is unexported.
						// See https://golang.org/issue/21357
						if !corespValue.CanSet() {
							d.saveError(fmt.Errorf("json: cannot set embedded pointer to unexported struct: %v", corespValue.Type.Deref()))
							// Invalidate corespValue to ensure d.value(corespValue) skips over the JSON value without assigning it to corespValue.
							corespValue = Value{}
							isBasic = false
							break
						}
						corespValue.Set(New(corespValue.Type.Deref()))
					}
					corespValue = corespValue.Deref()
				}

				corespValue = corespValue.getField(idx)
			}
			d.errorContext.Field = string(curField.name)
			d.errorContext.Struct = v.Type.Name()

		} else if d.useStrict {
			d.saveError(fmt.Errorf("json: unknown field %q", fieldName))
		}

		if isBasic {
			// valueQuoted is like value but decodes a isBasic string literal or literal null into an interface value.
			// If it finds anything other than a isBasic string literal or null, valueQuoted returns unquotedValue{}.
			switch op := d.scanWhile(scanSkipSpace); op {
			default:
				d.saveError(fmt.Errorf("json: should never happen, because we're expecting begin literal, but received operation %d", op))
			case scanBeginLiteral:
				switch va := d.literalInterface().(type) {
				case nil:
					// setting null to corespValue
					switch corespValue.Kind() {
					case Interface, Ptr, Map, Slice:
						corespValue.Set(Zero(corespValue.Type))
						// otherwise, ignore null for primitives/string
					}
				case string:
					d.literalStore([]byte(va), corespValue)
				default:
					d.saveError(fmt.Errorf("json: invalid use of ,string struct tag, trying to unmarshal unquoted value into %v", corespValue.Type))
				}
			}
		} else {
			d.process(corespValue)
		}

		// Next token must be , or }.
		op = d.scanWhile(scanSkipSpace)
		if op == scanEndObject {
			break
		}
		if op != scanObjectValue {
			d.error(errPhase)
		}

		d.errorContext.Struct = ""
		d.errorContext.Field = ""
	}
}

// valueInterface is like value but returns interface{}
func (d *decodeState) valueInterface() interface{} {
	switch d.scanWhile(scanSkipSpace) {
	case scanBeginArray:
		return d.arrayInterface()
	case scanBeginObject:
		return d.objectInterface()
	case scanBeginLiteral:
		return d.literalInterface()
	default:
		d.error(errPhase)
		panic("Unreachable")
	}
}

// arrayInterface is like array but returns []interface{}.
func (d *decodeState) arrayInterface() []interface{} {
	var v = make([]interface{}, 0)
	for {
		// Look ahead for ] - can only happen on first iteration.
		op := d.scanWhile(scanSkipSpace)
		if op == scanEndArray {
			break
		}

		// Back up so d.value can have the byte we just read.
		d.offset--
		d.scan.undo(op)

		v = append(v, d.valueInterface())

		// Next token must be , or ].
		op = d.scanWhile(scanSkipSpace)
		if op == scanEndArray {
			break
		}
		if op != scanArrayValue {
			d.error(errPhase)
		}
	}
	return v
}

// objectInterface is like object but returns map[string]interface{}.
func (d *decodeState) objectInterface() map[string]interface{} {
	m := make(map[string]interface{})
	for {
		// Read opening " of string key or closing }.
		op := d.scanWhile(scanSkipSpace)
		if op == scanEndObject {
			// closing } - can only happen on first iteration.
			break
		}
		if op != scanBeginLiteral {
			d.error(errPhase)
		}

		// Read string key.
		start := d.offset - 1
		op = d.scanWhile(scanContinue)
		item := d.data[start : d.offset-1]
		key, ok := unquote(item)
		if !ok {
			d.error(errPhase)
		}

		// Read : before value.
		if op == scanSkipSpace {
			op = d.scanWhile(scanSkipSpace)
		}
		if op != scanObjectKey {
			d.error(errPhase)
		}

		// Read value.
		m[key] = d.valueInterface()

		// Next token must be , or }.
		op = d.scanWhile(scanSkipSpace)
		if op == scanEndObject {
			break
		}
		if op != scanObjectValue {
			d.error(errPhase)
		}
	}
	return m
}

// literalInterface is like literal but returns an interface value.
func (d *decodeState) literalInterface() interface{} {
	// All bytes inside literal return scanContinue op code.
	start := d.offset - 1
	op := d.scanWhile(scanContinue)

	// Scan read one byte too far; back up.
	d.offset--
	d.scan.undo(op)
	item := d.data[start:d.offset]

	switch c := item[0]; c {
	case nChr: // null
		return nil

	case tChr, fChr: // true, false
		return c == tChr

	case quote: // string
		s, ok := unquote(item)
		if !ok {
			d.error(errPhase)
		}
		return s

	default: // number
		if c != minus && (c < zero || c > nine) {
			d.error(errPhase)
		}

		// convertNumber converts the number literal s to a float64 or a Number depending on the setting of d.useNumber.
		if d.useNumber {
			return Number(string(item))
		}

		floa, err := Atof64(item)
		if err != nil {
			d.saveError(&UnmarshalTypeError{Value: "number " + string(item), Type: TypeOf(0.0), Offset: int64(d.offset)})
		}
		return floa
	}
}
