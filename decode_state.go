/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"bytes"
	"fmt"
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

	walker := &SetWalker{}
	if !walker.init(v) {
		return &InvalidUnmarshalError{TypeOf(v)}
	}

	d.scan.reset()

	// We decode value not value.Deref because the Unmarshaler interface test must be applied at the top level of the value.
	d.process(walker)
	return d.savedError
}

func (d *decodeState) callUnmarshaller(walker *SetWalker, item []byte) {
	unmarshaler, _ := walker.Value.Interface().(Unmarshaler)
	err := unmarshaler.UnmarshalJSON(item)
	if err != nil {
		d.error(err)
	}
}

// value decodes a JSON value from d.data[d.off:] into the value.
// it updates d.off to point past the decoded value.
func (d *decodeState) process(walker *SetWalker) {
	if !walker.Value.IsValid() {
		d.doRead()
		return
	}

	//TODO : decoder knows what it begins - it should return to walker this "state" (array, object, etc)
	switch op := d.scanWhile(scanSkipSpace); op {
	default:
		d.error(errPhase)
	case scanBeginArray:
		isUnmarshaler := walker.indirect(unmarshalerType, false)
		if isUnmarshaler {
			d.offset--
			d.callUnmarshaller(walker, d.next())
			return
		}

		d.array(walker)

	case scanBeginObject:
		isUnmarshaler := walker.indirect(unmarshalerType, false)
		if isUnmarshaler {
			d.offset--
			d.callUnmarshaller(walker, d.next())
			return
		}

		// object consumes an object from d.data[d.off-1:], decoding into the value v.
		// the first byte (curlOpen) of the object has been read already.
		// Decoding into nil interface? Switch to non-reflect code.
		if walker.isIfaceWNoMeths() {
			walker.Value.Set(ReflectOn(d.objectInterface()))
			return
		}

		switch walker.Value.Kind() {
		default:
			d.saveError(&UnmarshalTypeError{Value: "object", Type: walker.Value.Type, Offset: int64(d.offset)})
			d.offset--
			d.next() // skip over { } in input

		case Map:
			d.doMap(walker)

		case Struct:
			d.doStruct(walker)
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

		d.literalStore(item, walker)
	}
}

// literalStore decodes a literal stored in item into v.
// fromQuoted indicates whether this literal came from unwrapping a string from the ",string" struct tag option. this is used only to produce more helpful error messages.
func (d *decodeState) literalStore(item []byte, walker *SetWalker) {
	if len(item) == 0 {
		// Empty string given
		d.saveError(fmt.Errorf("json: invalid use of ,string struct tag, trying to unmarshal %q into %v", item, walker.Value.Type))
		return
	}

	isUnmarshaler := false

	// Check for unmarshaler.
	if item[0] == nChr { // null
		isUnmarshaler = walker.indirect(unmarshalerType, true)
	} else {
		isUnmarshaler = walker.indirect(unmarshalerType, false)
	}

	if isUnmarshaler {
		d.callUnmarshaller(walker, item)
		return
	}

	switch c := item[0]; c {
	case nChr: // null
		// The main parser checks that only true and false can reach here,
		// but if this was a isBasic string input, it could be anything.
		if !bytes.Equal(item, nullLiteral) {
			d.saveError(fmt.Errorf("json: invalid use of ,string struct tag, trying to unmarshal %q into %v", item, walker.Value.Type))
			break
		}
		walker.setNull()
	case tChr, fChr: // true, false
		boolValue := item[0] == tChr
		// The main parser checks that only true and false can reach here,
		// but if this was a isBasic string input, it could be anything.
		if !bytes.Equal(item, trueLiteral) && !bytes.Equal(item, falseLiteral) {
			d.saveError(fmt.Errorf("json: invalid use of ,string struct tag, trying to unmarshal %q into %v", item, walker.Value.Type))
			break
		}
		err := walker.setBool(boolValue)
		if err != nil {
			d.saveError(&UnmarshalTypeError{Value: "bool", Type: walker.Value.Type, Offset: int64(d.offset)})
		}

	case quote: // string
		s, ok := unquoteBytes(item)
		if !ok {
			d.error(fmt.Errorf("json: invalid use of ,string struct tag, trying to unmarshal %q into %v", item, walker.Value.Type))
		}
		err := walker.setString(s)
		if err != nil {
			d.saveError(&UnmarshalTypeError{Value: "string", Type: walker.Value.Type, Offset: int64(d.offset)})
		}

	default: // number
		if c != minus && (c < zero || c > nine) {
			d.error(fmt.Errorf("json: invalid use of ,string struct tag, trying to unmarshal %q into %v", item, walker.Value.Type))
		}
		err := walker.setNumber(item, d.useNumber)
		if err != nil {
			d.saveError(&UnmarshalTypeError{Value: "number", Type: walker.Value.Type, Offset: int64(d.offset)})
		}
	}
}

// array consumes an array from d.data[d.off-1:], decoding into the value v. the first byte of the array ('[') has been read already.
func (d *decodeState) array(walker *SetWalker) {
	if walker.isIfaceWNoMeths() {
		walker.Value.Set(ReflectOn(d.arrayInterface()))
		return
	}
	// Check type of target.
	switch walker.Value.Kind() {
	case Array: // valid
	case Slice: // valid
	default:
		d.saveError(&UnmarshalTypeError{Value: "array", Type: walker.Value.Type, Offset: int64(d.offset)})
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

		if walker.growSlice(i) {
			d.process(newWalker(Value{}))
		} else {
			d.process(newWalker(walker.Value.Index(i)))
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

	walker.finishArray(i)
}

func (d *decodeState) doMap(walker *SetWalker) {
	ok := walker.startMap()
	if !ok {
		d.saveError(&UnmarshalTypeError{Value: "map", Type: walker.Value.Type, Offset: int64(d.offset)})
		d.offset--
		d.next() // skip over { } in input
		return
	}
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

		// process it
		d.process(walker.getMapElem())

		// load it
		err := walker.loadMapIndex(key)
		if err != nil {
			// probably number error, since "Unexpected key type" should NEVER occur
			d.saveError(&UnmarshalTypeError{Value: "number " + string(key), Type: walker.Value.Type.ConvToMap().KeyType, Offset: int64(start + 1)})
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
	var result *MarshalField

	cachedFields, _ := unmarshalerFieldCache.value.Load().(map[*RType]*marshalFields)
	fields := cachedFields[value.Type]
	if fields == nil {
		// Compute fields without lock.
		// Might duplicate effort but won't hold other computations back.
		fields = value.Type.getMarshalFields()
		if fields == nil {
			//fields = unmarshalFields{}
			return result
		}

		unmarshalerFieldCache.mu.Lock()
		cachedFields, _ = unmarshalerFieldCache.value.Load().(map[*RType]*marshalFields)
		newFieldsMap := make(map[*RType]*marshalFields, len(cachedFields)+1)
		for k, v := range cachedFields {
			newFieldsMap[k] = v
		}
		newFieldsMap[value.Type] = fields
		unmarshalerFieldCache.value.Store(newFieldsMap)
		unmarshalerFieldCache.mu.Unlock()
	}

	for i := range *fields {
		curField := (*fields)[i]
		if bytes.Equal(curField.name, fieldName) {
			result = &curField
			break
		}
		// TODO : equal fold should be searched in a sync.Map by the fieldName -> map[ []byte ] func(srcKey, destKey []byte) bool
		if result == nil && curField.equalFold(curField.name, fieldName) {
			result = &curField
		}
	}
	return result
}
func (d *decodeState) doStruct(walker *SetWalker) {

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
		curField := d.getFieldNamed(walker.Value, fieldName)
		// Figure out field corresponding to fieldName.
		var corespValue Value
		isBasic := false // whether the value is wrapped in a string to be decoded first
		if curField != nil {
			corespValue = walker.Value
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

				corespValue = corespValue.Field(idx)
			}
			d.errorContext.Field = string(curField.name)
			d.errorContext.Struct = walker.Value.Type.Name()

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
				switch v := d.literalInterface().(type) {
				case nil:
					newWalker(corespValue).setNull()
				case string:
					d.literalStore([]byte(v), newWalker(corespValue))
				default:
					d.saveError(fmt.Errorf("json: invalid use of ,string struct tag, trying to unmarshal unquoted value into %v", corespValue.Type))
				}
			}
		} else {
			d.process(newWalker(corespValue))
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
		panic("unreachable")
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
