/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"reflect"
)

type KeyValuePair struct {
	value   reflect.Value
	keyName string
}

type Walker interface {
	NullValue()
	InvalidValue()
	Bool(value bool)
	Int(value int64)
	Uint(value uint64)
	ByteSlice(value []byte)
	TypedString(value string, Type reflect.Type)
	Float(value float64, sixtyFourBit bool)
	UnsupportedTypeEncoder(Type reflect.Type)

	InspectValue(value reflect.Value) bool
	InspectType(typ reflect.Type) bool

	ArrayStart()
	ArrayElem()
	ArrayEnd()

	StructStart(value reflect.Value) []field
	StructField(whichField field)
	NextStructField()
	StructEnd()
	// returns a slice of stored key value pairs and a bool signaling we're sorting keys
	MapStart(keys []reflect.Value) ([]KeyValuePair, bool)
	MapKey(key string)
	NextMapEntry()
	MapEnd()
}

func marshalWalk(v interface{}, walker Walker) {
	reflectValue(reflect.ValueOf(v), walker)
}

func reflectValue(v reflect.Value, walker Walker) {
	if !v.IsValid() {
		walker.InvalidValue()
		return
	}
	walk(v, walker)
}

func walk(v reflect.Value, walker Walker) {
	if !walker.InspectValue(v) {
		return
	}
	basic(v, walker)
}

func basic(v reflect.Value, walker Walker) {
	switch v.Kind() {
	case reflect.Bool:
		walker.Bool(v.Bool())
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		walker.Int(v.Int())
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		walker.Uint(v.Uint())
	case reflect.Float32:
		walker.Float(v.Float(), false)
	case reflect.Float64:
		walker.Float(v.Float(), true)
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

func sliceEncoder(v reflect.Value, walker Walker) {
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

func arrayEncoder(v reflect.Value, walker Walker) {
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

func structEncoder(v reflect.Value, walker Walker) {
	fields := walker.StructStart(v)

	first := true

	for _, field := range fields {
		// restoring to original value type, since it gets altered below
		valueType := v.Type()

		fieldValue := v
		/**
		if len(field.indexes) > 1 {
			fmt.Fprintf(os.Stderr, "%#v indexes\n", field.indexes)
		}
		**/
		for _, idx := range field.indexes {
			if valueType.Kind() == reflect.Ptr {
				valueType = valueType.Elem()
				if fieldValue.IsNil() {
					fieldValue = reflect.Value{}
					break
				}
				fieldValue = fieldValue.Elem()
			}
			valueType = valueType.Field(idx).Type
			fieldValue = fieldValue.Field(idx)
		}

		if !fieldValue.IsValid() || field.willOmit && isEmptyValue(fieldValue) {
			continue
		}

		if first {
			first = false
		} else {
			walker.NextStructField()
		}

		walker.StructField(field)

		walk(fieldValue, walker)
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

func mapEncoder(v reflect.Value, walker Walker) {
	if v.IsNil() {
		walker.NullValue()
		return
	}
	sortedKeys, hasSort := walker.MapStart(v.MapKeys())
	if hasSort {
		for i, key := range sortedKeys {
			if i > 0 {
				walker.NextMapEntry()
			}
			walker.MapKey(key.keyName)
			walk(v.MapIndex(key.value), walker)

		}
	} else {
		for i, key := range v.MapKeys() {
			if i > 0 {
				walker.NextMapEntry()
			}

			keyName := ""
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

			walker.MapKey(keyName)
			walk(v.MapIndex(key), walker)

		}
	}
	walker.MapEnd()
}
