/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"encoding/base64"
	"fmt"
	"reflect"
)

type (
	SetWalker struct {
		reflect.Value
		mapElem reflect.Value
	}

	qualFn          func(srcKey, destKey []byte) bool
	unmarshalFields []UnmarshalField // unmarshalFields sorts field by index sequence.

	// A field represents a single field found in a struct.
	UnmarshalField struct {
		name      string
		nameBytes []byte // []byte(name)
		indexes   []int
		Type      reflect.Type
		// TODO : extract equalFold from here and make a sync.Map (@see getFieldNamed of decode_state)
		equalFold qualFn // bytes.EqualFold or equivalent
		tag       bool
		isBasic   bool
		//willOmit  bool
	}
)

func newWalker(v reflect.Value) *SetWalker {
	return &SetWalker{Value: v}
}

func (w *SetWalker) init(v interface{}) bool {
	w.Value = reflect.ValueOf(v)
	if w.Value.Kind() != reflect.Ptr || w.Value.IsNil() {
		return false
	}
	return true
}

// indirect walks down v allocating pointers as needed, until it gets to a non-pointer.
// if it encounters an Unmarshaler, indirect stops and returns that.
// if decodingNull is true, indirect stops at the last pointer so it can be set to nil.
func (w *SetWalker) indirect(implements reflect.Type, decodingNull bool) bool {
	// If v is a named type and is addressable,
	// start with its address, so that if the type has pointer methods,
	// we find them.
	if w.Value.Kind() != reflect.Ptr && w.Value.Type().Name() != "" && w.Value.CanAddr() {
		w.Value = w.Value.Addr()
	}
	for {
		// Load value from interface, but only if the result will be
		// usefully addressable.
		if w.Value.Kind() == reflect.Interface && !w.Value.IsNil() {
			e := w.Value.Elem()
			if e.Kind() == reflect.Ptr && !e.IsNil() && (!decodingNull || e.Elem().Kind() == reflect.Ptr) {
				w.Value = e
				continue
			}
		}

		if w.Value.Kind() != reflect.Ptr {
			break
		}

		if w.Value.Elem().Kind() != reflect.Ptr && decodingNull && w.Value.CanSet() {
			break
		}

		if w.Value.IsNil() {
			w.Value.Set(reflect.New(w.Value.Type().Elem()))
		}

		if w.Value.Type().NumMethod() > 0 {
			if w.Value.Type().Implements(implements) {
				return true
			}
		}

		w.Value = w.Value.Elem()
	}
	return false
}

func (w *SetWalker) setNull() {
	switch w.Value.Kind() {
	case reflect.Interface, reflect.Ptr, reflect.Map, reflect.Slice:
		w.Value.Set(reflect.Zero(w.Value.Type()))
		// otherwise, ignore null for primitives/string
	}
}

func (w *SetWalker) setBool(boolValue bool) error {
	switch w.Value.Kind() {
	default:
		return fmt.Errorf("invalid bool")
	case reflect.Bool:
		w.Value.SetBool(boolValue)
	case reflect.Interface:
		if w.Value.NumMethod() == 0 {
			w.Value.Set(reflect.ValueOf(boolValue))
		} else {
			return fmt.Errorf("invalid bool")
		}
	}
	return nil
}

func (w *SetWalker) isIfaceWNoMeths() bool {
	if w.Value.Kind() == reflect.Interface && w.Value.NumMethod() == 0 {
		return true
	}
	return false
}

func (w *SetWalker) setString(s []byte) error {
	switch w.Value.Kind() {
	default:
		return fmt.Errorf("invalid string")
	case reflect.Slice:
		stringValue := string(s)
		if w.Value.Type().Elem().Kind() != reflect.Uint8 {
			return fmt.Errorf("invalid string")
		}
		b := make([]byte, base64.StdEncoding.DecodedLen(len(stringValue)))
		n, err := base64.StdEncoding.Decode(b, s)
		if err != nil {
			return err
		}
		w.Value.SetBytes(b[:n])
	case reflect.String:
		stringValue := string(s)
		w.Value.SetString(stringValue)
	case reflect.Interface:
		if w.Value.NumMethod() == 0 {
			stringValue := string(s)
			w.Value.Set(reflect.ValueOf(stringValue))
		} else {
			return fmt.Errorf("invalid string")
		}
	}
	return nil
}

func convertNumber(from []byte, useNumber bool) (interface{}, error) {
	if useNumber {
		return Number(string(from)), nil
	}
	f, err := Atof64(from)
	if err != nil {
		return nil, fmt.Errorf("invalid number") //&UnmarshalTypeError{Value: "number " + string(from), Type: reflect.TypeOf(0.0), Offset: int64(d.offset)}
	}
	return f, nil
}

func (w *SetWalker) setNumber(item []byte, useNumber bool) error {
	switch w.Value.Kind() {
	default:
		if w.Value.Kind() == reflect.String && w.Value.Type() == numberType {
			s := string(item)
			w.Value.SetString(s)
			if !IsValidNumber(s) {
				//d.error(fmt.Errorf("json: invalid number literal, trying to unmarshal %q into Number", item))
				return fmt.Errorf("invalid number")
			}
			break
		}
		return fmt.Errorf("invalid number")
	case reflect.Interface:
		n, err := convertNumber(item, useNumber)
		if err != nil {
			//d.saveError(err)
			//break
			return fmt.Errorf("invalid number : %v", err)
		}
		if w.Value.NumMethod() != 0 {
			return fmt.Errorf("invalid number")
		}
		w.Value.Set(reflect.ValueOf(n))
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		result, isNegative := FastNumber(item)
		if isNegative {
			result = -result
		}
		if w.Value.OverflowInt(int64(result)) {
			//d.saveError(&UnmarshalTypeError{Value: "integer " + string(item), Type: value.Type(), Offset: int64(d.offset)})
			return fmt.Errorf("invalid number")
		}
		w.Value.SetInt(int64(result))
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		result, isNegative := FastNumber(item)
		if isNegative {
			// TODO : make issue on golang github
			//d.saveError(&UnmarshalTypeError{Value: "negative??? unsigned integer " + string(item), Type: value.Type(), Offset: int64(d.offset)})
			return fmt.Errorf("invalid number")
		}
		if w.Value.OverflowUint(result) {
			//d.saveError(&UnmarshalTypeError{Value: "unsigned integer " + string(item), Type: value.Type(), Offset: int64(d.offset)})
			return fmt.Errorf("invalid number")
		}
		w.Value.SetUint(result)
	case reflect.Float32:
		n, err := Atof32(item)
		if err != nil || w.Value.OverflowFloat(float64(n)) {
			//d.saveError(&UnmarshalTypeError{Value: "float " + s, Type: value.Type(), Offset: int64(d.offset)})
			return fmt.Errorf("invalid number")
		}
		w.Value.SetFloat(float64(n))
	case reflect.Float64:
		n, err := Atof64(item)
		if err != nil || w.Value.OverflowFloat(n) {
			//d.saveError(&UnmarshalTypeError{Value: "float " + s, Type: value.Type(), Offset: int64(d.offset)})
			return fmt.Errorf("invalid number")
		}
		w.Value.SetFloat(n)
	}
	return nil
}

func (w *SetWalker) startMap() bool {
	// Map key must either have string kind, have an integer kind
	// Check type of target: `struct` or `map[T1]T2` where `T1` is string, an integer type
	switch w.Value.Type().Key().Kind() {
	case reflect.String,
		reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
	default:
		return false
	}

	if w.Value.IsNil() {
		w.Value.Set(reflect.MakeMap(w.Value.Type()))
	}

	w.mapElem = reflect.New(w.Value.Type().Elem()).Elem()

	return true
}

// Figure out field corresponding to key.
func (w *SetWalker) getMapElem() *SetWalker {
	if w.mapElem.IsValid() {
		w.mapElem.Set(reflect.Zero(w.Value.Type().Elem()))
	}
	return newWalker(w.mapElem)
}

func (w *SetWalker) loadMapIndex(key []byte) error {
	valueTypeKey := w.Value.Type().Key()
	// Write value back to map; if using struct, corespValue points into struct already.
	switch valueTypeKey.Kind() {
	case reflect.String:
		keyValue := reflect.ValueOf(key).Convert(valueTypeKey)
		w.Value.SetMapIndex(keyValue, w.mapElem)
		return nil
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		num, err := IntParse(key)
		if err != nil || reflect.Zero(valueTypeKey).OverflowInt(num) {
			//d.saveError(&UnmarshalTypeError{Value: "number " + s, Type: valueTypeKey, Offset: int64(start + 1)})
			return fmt.Errorf("bad map key")
		}
		keyValue := reflect.ValueOf(num).Convert(valueTypeKey)
		w.Value.SetMapIndex(keyValue, w.mapElem)
		return nil
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		num, err := UintParse(key)
		if err != nil || reflect.Zero(valueTypeKey).OverflowUint(num) {
			//d.saveError(&UnmarshalTypeError{Value: "number " + s, Type: valueTypeKey, Offset: int64(start + 1)})
			return fmt.Errorf("bad map key")
		}
		keyValue := reflect.ValueOf(num).Convert(valueTypeKey)
		w.Value.SetMapIndex(keyValue, w.mapElem)
		return nil
	}
	//panic("json: Unexpected key type") // should never occur
	return fmt.Errorf("Should never occur")
}

func (w *SetWalker) growSlice(i int) bool {
	// Get element of array, growing if necessary.
	if w.Value.Kind() == reflect.Slice {
		// Grow slice if necessary
		if i >= w.Value.Cap() {
			newCap := w.Value.Cap() + w.Value.Cap()/2
			if newCap < 4 {
				newCap = 4
			}
			newSlice := reflect.MakeSlice(w.Value.Type(), w.Value.Len(), newCap)
			reflect.Copy(newSlice, w.Value)
			w.Value.Set(newSlice)
		}
		if i >= w.Value.Len() {
			w.Value.SetLen(i + 1)
		}
	}
	if i < w.Value.Len() {
		// Decode into element.
		return false
	}

	// Ran out of fixed array: skip.
	return true
}

func (w *SetWalker) finishArray(i int) {
	if i < w.Value.Len() {
		switch w.Value.Kind() {
		case reflect.Array:
			// Array. Zero the rest.
			zero := reflect.Zero(w.Value.Type().Elem())
			for ; i < w.Value.Len(); i++ {
				w.Value.Index(i).Set(zero)
			}
		default:
			w.Value.SetLen(i)
		}
	}

	if i == 0 && w.Value.Kind() == reflect.Slice {
		w.Value.Set(reflect.MakeSlice(w.Value.Type(), 0, 0))
	}
}
