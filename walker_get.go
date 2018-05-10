/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"reflect"
	"strings"
	"time"
)

func (w *KeyValuePair) resolve() error {
	switch w.value.Kind() {
	case reflect.String:
		w.keyName = w.value.String()
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		w.keyName = FormatInt(w.value.Int())
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		w.keyName = FormatUint(w.value.Uint())
	default:
		panic("unexpected map key type")
	}
	return nil
}

func marshalWalk(v interface{}, walker *encodeState) {
	reflectValue(reflect.ValueOf(v), walker)
}

func reflectValue(v reflect.Value, walker *encodeState) {
	if !v.IsValid() {
		walker.InvalidValue()
		return
	}
	walk(v, walker)
}

func walk(v reflect.Value, walker *encodeState) {
	if !walker.InspectValue(v) {
		return
	}
	basic(v, walker)
}

func basic(v reflect.Value, walker *encodeState) {
	switch v.Kind() {
	case reflect.Bool:
		walker.Bool(v.Bool())
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		walker.Int(v.Int())
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		walker.Uint(v.Uint())
	case reflect.Float32:
		walker.Float(v.Float(), 32)
	case reflect.Float64:
		walker.Float(v.Float(), 64)
	case reflect.String:
		walker.TypedString(v.String(), v.Type())
	case reflect.Interface:
		if v.IsNil() {
			walker.NullValue()
			return
		}
		reflectValue(v.Elem(), walker)
	case reflect.Ptr:
		if v.IsNil() {
			walker.NullValue()
			return
		}
		walk(v.Elem(), walker)
	case reflect.Map:
		mapEncoder(v, walker)
	case reflect.Array:
		arrayEncoder(v, walker)
	case reflect.Slice:
		sliceEncoder(v, walker)
	case reflect.Struct:
		structEncoder(v, walker)
	default:
		walker.UnsupportedTypeEncoder(v.Type())
	}
}

func sliceEncoder(v reflect.Value, walker *encodeState) {
	// read the slice element
	deref := v.Type().Elem()
	// Byte slices get special treatment; arrays don't.
	if deref.Kind() == reflect.Uint8 {
		p := reflect.PtrTo(deref)
		// check if []int8 has it's own marshal implementation
		if walker.InspectType(p) {
			if v.IsNil() {
				walker.NullValue()
				return
			}
			walker.ByteSlice(v.Bytes())
			return
		}
	}
	// it's an "array"
	if v.IsNil() {
		walker.NullValue()
		return
	}
	//
	arrayEncoder(v, walker)
}

func arrayEncoder(v reflect.Value, walker *encodeState) {
	walker.ArrayStart()
	n := v.Len()
	for i := 0; i < n; i++ {
		if i > 0 {
			walker.ArrayElem()
		}

		walk(v.Index(i), walker)
	}
	walker.ArrayEnd()
}

func structEncoder(v reflect.Value, walker *encodeState) {

	fieldsInfo := walker.StructStart(v)

	first := true
	for _, f := range fieldsInfo {
		// restoring to original value type, since it gets altered below
		valueType := v.Type()

		fieldValue := v

		for _, idx := range f.indexes {
			if valueType.Kind() == reflect.Ptr {
				valueType = valueType.Elem()
				if fieldValue.IsNil() {
					// TODO : maybe avoid this ?
					fieldValue = reflect.Value{}
					break
				}
				fieldValue = fieldValue.Elem()
			}
			valueType = valueType.Field(idx).Type
			fieldValue = fieldValue.Field(idx)
		}

		// omitted invalid
		if !fieldValue.IsValid() {
			continue
		}

		if isEmptyValue(fieldValue) {
			// TODO : omit should be decided by walker
			if f.willOmit {
				continue
			}
		}
		// Serialize Nulls Condition : 1. is a struct which has a name that starts with "Null" and must have "omitempty"
		if strings.HasPrefix(f.Type.Name(), "Null") && fieldValue.Kind() == reflect.Struct && f.willOmit {
			subFields := walker.ReadFields(fieldValue)
			// 2. has exactly two fields : one boolean (called Valid) and one of a basic type (called as the basic type - e.g. "String")
			if len(subFields) == 2 {
				foundValid := false
				foundBasic := false

				var validField, carrierField reflect.Value

				for _, sf := range subFields {
					if sf.name == "Valid" {
						foundValid = true
						subFieldType := fieldValue.Type()
						validField = fieldValue
						for _, idx2 := range sf.indexes {
							if subFieldType.Kind() == reflect.Ptr {
								subFieldType = subFieldType.Elem()
								if validField.IsNil() {
									validField = reflect.Value{}
									break
								}
							}
							subFieldType = subFieldType.Field(idx2).Type
							validField = validField.Field(idx2)
						}
					} else {

						foundBasic = true
						subFieldType := fieldValue.Type()
						carrierField = fieldValue
						for _, idx2 := range sf.indexes {
							if subFieldType.Kind() == reflect.Ptr {
								subFieldType = subFieldType.Elem()
								if carrierField.IsNil() {
									carrierField = reflect.Value{}
									break
								}
							}
							subFieldType = subFieldType.Field(idx2).Type
							carrierField = carrierField.Field(idx2)
						}

					}
				}

				if foundBasic && foundValid {
					if validField.Bool() {
						switch carrierField.Kind() {
						case reflect.Bool:
							walker.StructField(f, first)
							walker.Bool(carrierField.Bool())
							continue
						case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
							walker.StructField(f, first)
							walker.Int(carrierField.Int())
							continue
						case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
							walker.StructField(f, first)
							walker.Uint(carrierField.Uint())
							continue
						case reflect.Float32:
							walker.StructField(f, first)
							walker.Float(carrierField.Float(), 32)
							continue
						case reflect.Float64:
							walker.StructField(f, first)
							walker.Float(carrierField.Float(), 64)
							continue
						case reflect.String:
							walker.StructField(f, first)
							walker.TypedString(carrierField.String(), carrierField.Type())
							continue
						default:
							// even if it's time, we're allowing Marshaler implementations (just to be able to format it)
						}
					} else {
						switch carrierField.Kind() {
						case reflect.Bool:
							continue
						case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
							continue
						case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
							continue
						case reflect.Float32:
							continue
						case reflect.Float64:
							continue
						case reflect.String:
							continue
						default:
							// if it's time, we're avoiding writing "null"
							_, isTime := carrierField.Interface().(time.Time)
							if isTime {
								continue
							}
						}
					}

				}
			}
		}

		walker.StructField(f, first)
		walk(fieldValue, walker)
		if first {
			first = false
		}

	}

	walker.StructEnd()

}

func isEmptyValue(value reflect.Value) bool {
	switch value.Kind() {
	case reflect.Array, reflect.Map, reflect.Slice, reflect.String:
		return value.Len() == 0
	case reflect.Bool:
		return !value.Bool()
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return value.Int() == 0
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		return value.Uint() == 0
	case reflect.Float32, reflect.Float64:
		return value.Float() == 0
	case reflect.Interface, reflect.Ptr:
		return value.IsNil()
	}
	return false
}

func mapEncoder(v reflect.Value, walker *encodeState) {
	if v.IsNil() {
		walker.NullValue()
		return
	}
	sortedKeys, hasSort := walker.MapStart(v.MapKeys())
	if hasSort {
		for i, key := range sortedKeys {
			walker.MapKey(key.keyName, i == 0)
			walk(v.MapIndex(key.value), walker)
		}
	} else {
		for i, key := range v.MapKeys() {
			keyName := ""
			// TODO : shouldn't this check be performed before we start
			switch key.Kind() {
			case reflect.String:
				keyName = key.String()
			case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
				keyName = FormatInt(key.Int())
			case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
				keyName = FormatUint(key.Uint())
			default:
				panic("Bad key kind!")
			}
			walker.MapKey(keyName, i == 0)
			walk(v.MapIndex(key), walker)
		}
	}
	walker.MapEnd()
}
