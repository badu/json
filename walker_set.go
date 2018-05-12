/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"encoding/base64"
	"fmt"
)

func newWalker(v Value) *SetWalker {
	return &SetWalker{Value: v}
}

func (w *SetWalker) init(v interface{}) bool {
	w.Value = ReflectOn(v)
	if w.Value.Kind() != Ptr || w.Value.IsNil() {
		return false
	}
	return true
}

// indirect walks down v allocating pointers as needed, until it gets to a non-pointer.
// if it encounters an Unmarshaler, indirect stops and returns that.
// if decodingNull is true, indirect stops at the last pointer so it can be set to nil.
func (w *SetWalker) indirect(implements *RType, decodingNull bool) bool {
	// If v is a named type and is addressable, start with its address, so that if the type has pointer methods, we find them.
	if w.Value.Kind() != Ptr && w.Value.Type.Name() != "" && w.Value.CanAddr() {
		w.Value = w.Value.Addr()
	}
	for {
		// Load value from interface, but only if the result will be
		// usefully addressable.
		if w.Value.Kind() == Interface && !w.Value.IsNil() {
			e := w.Value.Iface() // .Elem()
			if e.Kind() == Ptr && !e.IsNil() {
				if !decodingNull || e.Deref().Kind() == Ptr {
					w.Value = e
					continue
				}
			}
		}

		if w.Value.Kind() != Ptr {
			break
		}

		// now w.Value.Kind() == Ptr
		if w.Value.Deref().Kind() != Ptr && decodingNull && w.Value.CanSet() {
			break
		}

		if w.Value.IsNil() {
			w.Value.Set(New(w.Value.Type.Deref()))
		}

		if w.Value.Type.NumMethod() > 0 {
			if w.Value.Type.Implements(implements) {
				return true
			}
		}

		w.Value = w.Value.Deref()
	}
	return false
}

func (w *SetWalker) setNull() {
	switch w.Value.Kind() {
	case Interface, Ptr, Map, Slice:
		w.Value.Set(Zero(w.Value.Type))
		// otherwise, ignore null for primitives/string
	}
}

func (w *SetWalker) setBool(boolValue bool) error {
	switch w.Value.Kind() {
	default:
		return fmt.Errorf("invalid bool")
	case Bool:
		*(*bool)(w.Value.Ptr) = boolValue
	case Interface:
		if w.Value.NumMethod() == 0 {
			w.Value.Set(ReflectOn(boolValue))
		} else {
			return fmt.Errorf("invalid bool")
		}
	}
	return nil
}

func (w *SetWalker) isIfaceWNoMeths() bool {
	if w.Value.Kind() == Interface && w.Value.NumMethod() == 0 {
		return true
	}
	return false
}

func (w *SetWalker) setString(s []byte) error {
	switch w.Value.Kind() {
	default:
		return fmt.Errorf("invalid string")
	case Slice:
		stringValue := string(s)
		if w.Value.Type.ConvToSlice().ElemType.Kind() != Uint8 {
			return fmt.Errorf("invalid string")
		}
		b := make([]byte, base64.StdEncoding.DecodedLen(len(stringValue)))
		n, err := base64.StdEncoding.Decode(b, s)
		if err != nil {
			return err
		}
		*(*[]byte)(w.Value.Ptr) = b[:n]
	case String:
		*(*string)(w.Value.Ptr) = string(s)
	case Interface:
		if w.Value.NumMethod() == 0 {
			stringValue := string(s)
			w.Value.Set(ReflectOn(stringValue))
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
		return nil, fmt.Errorf("invalid number") //&UnmarshalTypeError{Value: "number " + string(from), Type: TypeOf(0.0), Offset: int64(d.offset)}
	}
	return f, nil
}

func (w *SetWalker) setNumber(item []byte, useNumber bool) error {
	switch w.Value.Kind() {
	default:
		if w.Value.Kind() == String && w.Value.Type == typeOfNo {
			println("Yes, typed number, but string")
			if w.Value.CanSet() {
				*(*string)(w.Value.Ptr) = string(item)
			}
			//TODO: check condition below (unused?)
			if !IsValidNumber(string(item)) {
				//d.error(fmt.Errorf("json: invalid number literal, trying to unmarshal %q into Number", item))
				return fmt.Errorf("invalid number")
			}
			break
		}
		return fmt.Errorf("invalid number")
	case Interface:
		n, err := convertNumber(item, useNumber)
		if err != nil {
			//d.saveError(err)
			//break
			return fmt.Errorf("invalid number : %v", err)
		}
		if w.Value.NumMethod() != 0 {
			return fmt.Errorf("invalid number")
		}
		w.Value.Set(ReflectOn(n))
	case Int, Int8, Int16, Int32, Int64:
		result, isNegative := FastNumber(item)
		if isNegative {
			result = -result
		}
		if w.Value.OverflowInt(int64(result)) {
			println("Overflows")
			//d.saveError(&UnmarshalTypeError{Value: "integer " + string(item), Type: value.Type(), Offset: int64(d.offset)})
			return fmt.Errorf("overflows number")
		}
		// TODO : GLOBLA CanSet should be first condition.
		if w.Value.CanSet() {
			switch w.Value.Kind() {
			case Int8:
				*(*int8)(w.Value.Ptr) = int8(result)
			case Int16:
				*(*int16)(w.Value.Ptr) = int16(result)
			case Int32:
				*(*int32)(w.Value.Ptr) = int32(result)
			case Int64:
				*(*int64)(w.Value.Ptr) = int64(result)
			default:
				*(*int)(w.Value.Ptr) = int(result)
			}
		}

	case Uint, Uint8, Uint16, Uint32, Uint64, UintPtr:
		result, isNegative := FastNumber(item)
		if isNegative {
			// TODO : make issue on golang github
			//d.saveError(&UnmarshalTypeError{Value: "negative??? unsigned integer " + string(item), Type: value.Type(), Offset: int64(d.offset)})
			return fmt.Errorf("invalid number")
		}
		if w.Value.OverflowUint(result) {
			//d.saveError(&UnmarshalTypeError{Value: "unsigned integer " + string(item), Type: value.Type(), Offset: int64(d.offset)})
			return fmt.Errorf("overflowsv number")
		}

		if w.Value.CanSet() {
			switch w.Value.Kind() {
			case Uint8:
				*(*uint8)(w.Value.Ptr) = uint8(result)
			case Uint16:
				*(*uint16)(w.Value.Ptr) = uint16(result)
			case Uint32:
				*(*uint32)(w.Value.Ptr) = uint32(result)
			case Uint64:
				*(*uint64)(w.Value.Ptr) = uint64(result)
			case UintPtr:
				*(*uintptr)(w.Value.Ptr) = uintptr(result)
			default:
				*(*uint)(w.Value.Ptr) = uint(result)
			}

		}

	case Float32:
		n, err := Atof32(item)
		if err != nil || w.Value.OverflowFloat(float64(n)) {
			//d.saveError(&UnmarshalTypeError{Value: "float " + s, Type: value.Type(), Offset: int64(d.offset)})
			return fmt.Errorf("overflows number")
		}

		if w.Value.CanSet() {
			*(*float32)(w.Value.Ptr) = float32(n)
		}

	case Float64:
		n, err := Atof64(item)
		if err != nil || w.Value.OverflowFloat(n) {
			//d.saveError(&UnmarshalTypeError{Value: "float " + s, Type: value.Type(), Offset: int64(d.offset)})
			return fmt.Errorf("overflows number")
		}

		if w.Value.CanSet() {
			*(*float64)(w.Value.Ptr) = n
		}
	}
	return nil
}

func (w *SetWalker) startMap() bool {
	// Map key must either have string kind, have an integer kind
	// Check type of target: `struct` or `map[T1]T2` where `T1` is string, an integer type

	mapType := w.Value.Type.ConvToMap()
	switch mapType.KeyType.Kind() {
	case String,
		Int, Int8, Int16, Int32, Int64,
		Uint, Uint8, Uint16, Uint32, Uint64, UintPtr:
	default:
		return false
	}

	if w.Value.IsNil() {
		w.Value.Set(MakeMap(w.Value.Type))
	}

	w.mapElem = New(mapType.ElemType).Deref()

	return true
}

// Figure out field corresponding to key.
func (w *SetWalker) getMapElem() *SetWalker {
	if w.mapElem.IsValid() {
		w.mapElem.Set(Zero(w.Value.Type.ConvToMap().ElemType))
	}
	return newWalker(w.mapElem)
}

func (w *SetWalker) loadMapIndex(key []byte) error {
	valueTypeKey := w.Value.Type.ConvToMap().KeyType
	// Write value back to map; if using struct, corespValue points into struct already.
	switch valueTypeKey.Kind() {
	case String:
		keyValue := ReflectOn(key).Convert(valueTypeKey)
		w.Value.SetMapIndex(keyValue, w.mapElem)
		return nil
	case Int, Int8, Int16, Int32, Int64:
		num, err := IntParse(key)
		if err != nil || Zero(valueTypeKey).OverflowInt(num) {
			//d.saveError(&UnmarshalTypeError{Value: "number " + s, Type: valueTypeKey, Offset: int64(start + 1)})
			return fmt.Errorf("bad map key")
		}
		keyValue := ReflectOn(num).Convert(valueTypeKey)
		w.Value.SetMapIndex(keyValue, w.mapElem)
		return nil
	case Uint, Uint8, Uint16, Uint32, Uint64, UintPtr:
		num, err := UintParse(key)
		if err != nil || Zero(valueTypeKey).OverflowUint(num) {
			//d.saveError(&UnmarshalTypeError{Value: "number " + s, Type: valueTypeKey, Offset: int64(start + 1)})
			return fmt.Errorf("bad map key")
		}
		keyValue := ReflectOn(num).Convert(valueTypeKey)
		w.Value.SetMapIndex(keyValue, w.mapElem)
		return nil
	}
	//panic("json: Unexpected key type") // should never occur
	return fmt.Errorf("Should never occur")
}

func (w *SetWalker) growSlice(i int) bool {
	// Get element of array, growing if necessary.
	if w.Value.Kind() == Slice {
		// Grow slice if necessary
		if i >= w.Value.Cap() {
			newCap := w.Value.Cap() + w.Value.Cap()/2
			if newCap < 4 {
				newCap = 4
			}
			newSlice := MakeSlice(w.Value.Type, w.Value.Len(), newCap)
			Copy(newSlice, w.Value)
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
		case Array:
			// Array. Zero the rest.
			zero := Zero(w.Value.Type.ConvToArray().ElemType) //.Elem())
			for ; i < w.Value.Len(); i++ {
				w.Value.Index(i).Set(zero)
			}
		default:
			w.Value.SetLen(i)
		}
	}

	if i == 0 && w.Value.Kind() == Slice {
		w.Value.Set(MakeSlice(w.Value.Type, 0, 0))
	}
}
