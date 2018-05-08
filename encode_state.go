/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"encoding/base64"
	"fmt"
	"math"
	"reflect"
	"runtime"
	"sort"
	"unicode/utf8"
)

// NOTE: keep in sync with stringBytes below.
func (e *encodeState) string(name string, escapeHTML bool) {
	e.WriteByte(quote)
	start := 0
	for i := 0; i < len(name); {
		if run := name[i]; run < utf8.RuneSelf {
			if htmlSafeSet[run] || (!escapeHTML && safeSet[run]) {
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
				e.WriteByte('n')
			case retChar:
				e.WriteByte(backSlash)
				e.WriteByte('r')
			case tab:
				e.WriteByte(backSlash)
				e.WriteByte('t')
			default:
				// This encodes bytes < 0x20 except for \t, \n and \r.
				// If escapeHTML is set, it also escapes <, >, and &
				// because they can lead to security holes when
				// user-controlled strings are rendered into JSON
				// and served to some browsers.
				e.WriteString(`\u00`)
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
			e.WriteString(`\ufffd`)
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
			e.WriteString(`\u202`)
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

// NOTE: keep in sync with string above.
func (e *encodeState) stringBytes(s []byte, escapeHTML bool) {
	e.WriteByte(quote)
	start := 0
	for i := 0; i < len(s); {
		if b := s[i]; b < utf8.RuneSelf {
			if htmlSafeSet[b] || (!escapeHTML && safeSet[b]) {
				i++
				continue
			}
			if start < i {
				e.Write(s[start:i])
			}
			switch b {
			case backSlash, quote:
				e.WriteByte(backSlash)
				e.WriteByte(b)
			case newLine:
				e.WriteByte(backSlash)
				e.WriteByte('n')
			case retChar:
				e.WriteByte(backSlash)
				e.WriteByte('r')
			case tab:
				e.WriteByte(backSlash)
				e.WriteByte('t')
			default:
				// This encodes bytes < 0x20 except for \t, \n and \r.
				// If escapeHTML is set, it also escapes <, >, and &
				// because they can lead to security holes when
				// user-controlled strings are rendered into JSON
				// and served to some browsers.
				e.WriteString(`\u00`)
				e.WriteByte(hex[b>>4])
				e.WriteByte(hex[b&0xF])
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
	marshalWalk(v, e)
	return nil
}

func (e *encodeState) error(err error) {
	panic(err)
}

func (e *encodeState) marshalerEncoder(v reflect.Value) {
	if v.Kind() == reflect.Ptr && v.IsNil() {
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
		e.error(&MarshalerError{v.Type(), err})
	}
}

func (e *encodeState) addrMarshalerEncoder(v reflect.Value) {
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
		e.error(&MarshalerError{v.Type(), err})
	}
}

func (e *encodeState) InvalidValue() {
	e.Write(nullLiteral)
}

func (e *encodeState) NullValue() {
	e.Write(nullLiteral)
}

func (e *encodeState) Bool(value bool) {
	if e.opts.quoted {
		e.WriteByte(quote)
	}
	if value {
		e.Write(trueLiteral)
	} else {
		e.Write(falseLiteral)
	}
	if e.opts.quoted {
		e.WriteByte(quote)
	}
}

func (e *encodeState) Int(value int64) {
	b := AppendInt(e.scratch[:0], value)
	if e.opts.quoted {
		e.WriteByte(quote)
	}
	e.Write(b)
	if e.opts.quoted {
		e.WriteByte(quote)
	}
}

func (e *encodeState) Uint(value uint64) {
	b := AppendUint(e.scratch[:0], value)
	if e.opts.quoted {
		e.WriteByte(quote)
	}
	e.Write(b)
	if e.opts.quoted {
		e.WriteByte(quote)
	}
}

func (e *encodeState) TypedString(value string, Type reflect.Type) {
	if Type == numberType {
		numStr := value
		// In Go1.5 the empty string encodes to "0", while this is not a valid number literal we keep compatibility so check validity after this.
		if len(numStr) == 0 {
			numStr = "0" // Number's zero-val
		}
		if !IsValidNumber(numStr) {
			e.error(fmt.Errorf("json: invalid number literal %q", numStr))
		}
		e.WriteString(numStr)
		return
	}
	if e.opts.quoted {
		sb, err := Marshal(value)
		if err != nil {
			e.error(err)
		}
		e.string(string(sb), e.opts.escapeHTML)
	} else {
		e.string(value, e.opts.escapeHTML)
	}
}

func (e *encodeState) Float(value float64, isSixtyFour bool) {
	if !isSixtyFour {
		if math.IsInf(value, 0) || math.IsNaN(value) {
			e.error(&UnsupportedValueError{FormatFloat(value, 32)})
		}

		// Convert as if by ES6 number to string conversion.
		// This matches most other JSON generators.
		// See golang.org/issue/6384 and golang.org/issue/14135.
		// Like fmt %g, but the exponent cutoffs are different
		// and exponents themselves are not padded to two digits.
		b := e.scratch[:0]
		abs := math.Abs(value)
		fmt := byte('f')
		// Note: Must use float32 comparisons for underlying float32 value to get precise cutoffs right.
		if abs != 0 {
			if float32(abs) < 1e-6 || float32(abs) >= 1e21 {
				fmt = 'e'
			}
		}
		b = AppendFloat(b, value, fmt, 32)
		if fmt == 'e' {
			// clean up e-09 to e-9
			n := len(b)
			if n >= 4 && b[n-4] == 'e' && b[n-3] == minus && b[n-2] == zero {
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
	} else {
		if math.IsInf(value, 0) || math.IsNaN(value) {
			e.error(&UnsupportedValueError{FormatFloat(value, 64)})
		}

		// Convert as if by ES6 number to string conversion.
		// This matches most other JSON generators.
		// See golang.org/issue/6384 and golang.org/issue/14135.
		// Like fmt %g, but the exponent cutoffs are different
		// and exponents themselves are not padded to two digits.
		b := e.scratch[:0]
		abs := math.Abs(value)
		fmt := byte('f')
		// Note: Must use float32 comparisons for underlying float32 value to get precise cutoffs right.
		if abs != 0 {
			if abs < 1e-6 || abs >= 1e21 {
				fmt = 'e'
			}
		}
		b = AppendFloat(b, value, fmt, 64)
		if fmt == 'e' {
			// clean up e-09 to e-9
			n := len(b)
			if n >= 4 && b[n-4] == 'e' && b[n-3] == minus && b[n-2] == zero {
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
	}
}

func (e *encodeState) UnsupportedTypeEncoder(Type reflect.Type) {
	e.error(&UnsupportedTypeError{Type})
}

func (e *encodeState) InspectType(typ reflect.Type) bool {
	if typ.Implements(marshalerType) {
		return false
	}
	return true
}

func (e *encodeState) InspectValue(value reflect.Value) bool {
	if value.Type().Implements(marshalerType) {
		e.marshalerEncoder(value)
		return false
	}
	if value.Type().Kind() != reflect.Ptr {
		if reflect.PtrTo(value.Type()).Implements(marshalerType) {
			if value.CanAddr() {
				e.addrMarshalerEncoder(value)
				return false
			}

		}
	}
	return true
}

func (e *encodeState) ByteSlice(value []byte) {
	e.WriteByte(quote)
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
}

func (e *encodeState) ArrayElem() {
	e.WriteByte(comma)
}

func (e *encodeState) ArrayStart() {
	e.WriteByte(squareOpen)
}

func (e *encodeState) ArrayEnd() {
	e.WriteByte(squareClose)
}

// marshalerFieldCache is like typeFields but uses a cache to avoid repeated work.
func (e *encodeState) StructStart(value reflect.Value) []MarshalField {
	e.WriteByte(curlOpen)

	cachedFields, _ := marshalerFieldCache.value.Load().(map[reflect.Type][]MarshalField)
	fields := cachedFields[value.Type()]
	if fields != nil {
		return fields
	}

	// Compute fields without lock.
	// Might duplicate effort but won't hold other computations back.
	fields = marshalFields(value)
	if fields == nil {
		return []MarshalField{}
	}

	marshalerFieldCache.mu.Lock()
	cachedFields, _ = marshalerFieldCache.value.Load().(map[reflect.Type][]MarshalField)

	newFieldsMap := make(map[reflect.Type][]MarshalField, len(cachedFields)+1)

	for typeKey, fieldsValues := range cachedFields {
		newFieldsMap[typeKey] = fieldsValues
	}

	newFieldsMap[value.Type()] = fields
	marshalerFieldCache.value.Store(newFieldsMap)
	marshalerFieldCache.mu.Unlock()

	return fields
}

func (e *encodeState) StructEnd() {
	e.WriteByte(curlClose)
}

func (e *encodeState) StructField(currentField MarshalField, isFirst bool) {
	if !isFirst {
		e.WriteByte(comma)
	}
	e.string(currentField.name, e.opts.escapeHTML)
	e.WriteByte(colon)
	e.opts.quoted = currentField.isBasic
}

func (e *encodeState) MapStart(keys []reflect.Value) ([]KeyValuePair, bool) {
	e.WriteByte(curlOpen)

	// new feature : optional sorting for map keys (default false)
	if e.opts.willSortMapKeys {
		//TODO : make `template` with the code sequnce below
		result := make([]KeyValuePair, len(keys))

		for idx, key := range keys {
			result[idx].value = key
			if err := result[idx].resolve(); err != nil {
				//error(&MarshalerError{key.Type(), err})
				panic("Error : " + err.Error() + " on " + key.Type().String())
			}
		}
		sort.Slice(result, func(i, j int) bool {
			return result[i].keyName < result[j].keyName
		})

		// end `template`
		return result, true
	}

	return []KeyValuePair{}, false
}

func (e *encodeState) MapEnd() {
	e.WriteByte(curlClose)
}

func (e *encodeState) NextMapEntry() {
	e.WriteByte(comma)
}

func (e *encodeState) MapKey(key string) {
	e.string(key, e.opts.escapeHTML)
	e.WriteByte(colon)
}
