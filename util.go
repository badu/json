/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"bytes"
	"fmt"
	"unicode"
	"unicode/utf16"
	"unicode/utf8"
)

// IsValidNumber reports whether s is a valid JSON number literal.
func IsValidNumber(target string) bool {
	// This function implements the JSON numbers grammar.
	// See https://tools.ietf.org/html/rfc7159#section-6 and http://json.org/number.gif

	if len(target) == 0 {
		return false
	}

	// Optional - (minus)
	if target[0] == minus {
		target = target[1:]
		if len(target) == 0 {
			return false
		}
	}

	// Digits
	switch {
	default:
		return false

	case target[0] == zero:
		target = target[1:]

	case one <= target[0] && target[0] <= nine:
		target = target[1:]
		for len(target) > 0 && zero <= target[0] && target[0] <= nine {
			target = target[1:]
		}
	}

	// . followed by 1 or more digits.
	if len(target) >= 2 && target[0] == period && zero <= target[1] && target[1] <= nine {
		target = target[2:]
		for len(target) > 0 && zero <= target[0] && target[0] <= nine {
			target = target[1:]
		}
	}

	// e or E followed by an optional - or + and 1 or more digits.
	if len(target) >= 2 && (target[0] == eChr || target[0] == bigEChr) {
		target = target[1:]
		if target[0] == plus || target[0] == minus {
			target = target[1:]
			if len(target) == 0 {
				return false
			}
		}
		for len(target) > 0 && zero <= target[0] && target[0] <= nine {
			target = target[1:]
		}
	}

	// Make sure we are at the end.
	return len(target) == 0
}

// decodeUxxxx decodes \uXXXX from the beginning of s, returning the hex value, or it returns -1.
func decodeUxxxx(target []byte) rune {
	if len(target) < 6 || target[0] != backSlash || target[1] != uChr {
		return -1
	}
	var char rune
	for _, c := range target[2:6] {
		switch {
		case zero <= c && c <= nine:
			c = c - zero
		case aChr <= c && c <= fChr:
			c = c - aChr + 10
		case bigAChr <= c && c <= bigFChr:
			c = c - bigAChr + 10
		default:
			return -1
		}
		char = char*16 + rune(c)
	}
	return char
}

// unquote converts a isBasic JSON string literal s into an actual string t.
// The rules are different than for Go, so cannot use strconv.Unquote.
func unquote(s []byte) (string, bool) {
	s, ok := unquoteBytes(s)
	return string(s), ok
}

// TODO : maybe this could be "inserted" in Request, so we move faster ?
func unquoteBytes(target []byte) ([]byte, bool) {
	// if target is too small or we don't have it surrounded with quotes
	if len(target) < 2 || target[0] != quote || target[len(target)-1] != quote {
		return nil, false
	}
	// we take the unquoted part of the target
	target = target[1 : len(target)-1]
	// Check for unusual characters. If there are none, then no unquoting is needed, so return a slice of the original bytes.
	usualCharsLen := 0
	for usualCharsLen < len(target) {
		c := target[usualCharsLen]
		if c == backSlash || c == quote || c < space {
			break
		}
		if c < utf8.RuneSelf {
			usualCharsLen++
			continue
		}
		rr, size := utf8.DecodeRune(target[usualCharsLen:])
		if rr == utf8.RuneError && size == 1 {
			break
		}
		usualCharsLen += size
	}
	// no unusual chars found
	if usualCharsLen == len(target) {
		return target, true
	}
	// TODO : find a better len calculus. Maybe we can also have a pool of bytes?
	result := make([]byte, len(target)+2*utf8.UTFMax)
	// copy the "normal" chars found
	copyLen := copy(result, target[0:usualCharsLen])

	for usualCharsLen < len(target) {
		// Out of room? Can only happen if s is full of malformed UTF-8 and we're replacing each byte with RuneError.
		if copyLen >= len(result)-2*utf8.UTFMax {
			nb := make([]byte, (len(result)+utf8.UTFMax)*2)
			copy(nb, result[0:copyLen])
			result = nb
		}
		// processing those unusual chars
		switch c := target[usualCharsLen]; {
		case c == backSlash:
			usualCharsLen++
			if usualCharsLen >= len(target) {
				return nil, false
			}
			switch target[usualCharsLen] {
			default:
				return nil, false
			case quote, backSlash, slash, sQuote:
				result[copyLen] = target[usualCharsLen]
				usualCharsLen++
				copyLen++
			case bChr:
				result[copyLen] = '\b'
				usualCharsLen++
				copyLen++
			case fChr:
				result[copyLen] = '\f'
				usualCharsLen++
				copyLen++
			case nChr:
				result[copyLen] = newLine
				usualCharsLen++
				copyLen++
			case rChr:
				result[copyLen] = retChar
				usualCharsLen++
				copyLen++
			case tChr:
				result[copyLen] = tab
				usualCharsLen++
				copyLen++
			case uChr:
				usualCharsLen--
				transf := decodeUxxxx(target[usualCharsLen:])
				if transf < 0 {
					return nil, false
				}
				usualCharsLen += 6
				if utf16.IsSurrogate(transf) {
					rr1 := decodeUxxxx(target[usualCharsLen:])
					if dec := utf16.DecodeRune(transf, rr1); dec != unicode.ReplacementChar {
						// A valid pair; consume.
						usualCharsLen += 6
						copyLen += utf8.EncodeRune(result[copyLen:], dec)
						break
					}
					// Invalid surrogate; fall back to replacement rune.
					transf = unicode.ReplacementChar
				}
				copyLen += utf8.EncodeRune(result[copyLen:], transf)
			}

			// Quote, control characters are invalid.
		case c == quote, c < space:
			return nil, false

			// ASCII
		case c < utf8.RuneSelf:
			result[copyLen] = c
			usualCharsLen++
			copyLen++

			// Coerce to well-formed UTF-8.
		default:
			transf, size := utf8.DecodeRune(target[usualCharsLen:])
			usualCharsLen += size
			copyLen += utf8.EncodeRune(result[copyLen:], transf)
		}
	}
	return result[0:copyLen], true
}

func newEncodeState(opts encOpts) *encodeState {
	if v := encodeStatePool.Get(); v != nil {
		e := v.(*encodeState)
		e.Buffer.Reset()
		e.opts = opts
		return e
	}
	return &encodeState{opts: opts}
}

// foldFunc returns one of four different case folding equivalence functions, from most general (and slow) to fastest:
//
// 1) bytes.EqualFold, if the key s contains any non-ASCII UTF-8
// 2) equalFoldRight, if s contains special folding ASCII ('k', 'K', 's', 'S')
// 3) asciiEqualFold, no special, but includes non-letters (including _)
// 4) simpleLetterEqualFold, no specials, no non-letters.
//
// The letters S and K are special because they map to 3 runes, not just 2:
//  * S maps to s and to U+017F 'ſ' Latin small letter long s
//  * k maps to K and to U+212A 'K' Kelvin sign
// See https://play.golang.org/p/tTxjOc0OGo
//
// The returned function is specialized for matching against s and should only be given s. It's not curried for performance reasons.
func foldFunc(key []byte) func(srcKey, destKey []byte) bool {
	nonLetter := false
	special := false // special letter
	for _, run := range key {
		if run >= utf8.RuneSelf {
			return bytes.EqualFold
		}
		upper := run & caseMask
		if upper < bigAChr || upper > bigZChr {
			nonLetter = true
		} else if upper == bigKChr || upper == bigSChr {
			// See above for why these letters are special.
			special = true
		}
	}
	if special {
		return equalFoldRight
	}
	if nonLetter {
		return asciiEqualFold
	}
	return simpleLetterEqualFold
}

// equalFoldRight is a specialization of bytes.EqualFold when s is known to be all ASCII (including punctuation), but contains an 's', 'S', 'k', or 'K', requiring a Unicode fold on the bytes in t.
func equalFoldRight(srcKey, destKey []byte) bool {
	for _, srcRun := range srcKey {
		if len(destKey) == 0 {
			return false
		}
		destRun := destKey[0]
		if destRun < utf8.RuneSelf {
			if srcRun != destRun {
				sbUpper := srcRun & caseMask
				if bigAChr <= sbUpper && sbUpper <= bigZChr {
					if sbUpper != destRun&caseMask {
						return false
					}
				} else {
					return false
				}
			}
			destKey = destKey[1:]
			continue
		}
		// srcRun is ASCII and t is not. t must be either kelvin sign or long s; srcRun must be s, S, k, or K.
		tr, size := utf8.DecodeRune(destKey)
		switch srcRun {
		case sChr, bigSChr:
			if tr != smallLongEss {
				return false
			}
		case kChr, bigKChr:
			if tr != kelvin {
				return false
			}
		default:
			return false
		}
		destKey = destKey[size:]

	}
	if len(destKey) > 0 {
		return false
	}
	return true
}

// asciiEqualFold is a specialization of bytes.EqualFold for use when s is all ASCII (but may contain non-letters) and contains no special-folding letters.
func asciiEqualFold(srcKey, destKey []byte) bool {
	if len(srcKey) != len(destKey) {
		return false
	}
	for idx, srcRun := range srcKey {
		tb := destKey[idx]
		if srcRun == tb {
			continue
		}
		if (aChr <= srcRun && srcRun <= zChr) || (bigAChr <= srcRun && srcRun <= bigZChr) {
			if srcRun&caseMask != tb&caseMask {
				return false
			}
		} else {
			return false
		}
	}
	return true
}

// simpleLetterEqualFold is a specialization of bytes.EqualFold for use when s is all ASCII letters (no underscores, etc) and also doesn't contain 'k', 'K', 's', or 'S'.
func simpleLetterEqualFold(srcKey, destKey []byte) bool {
	if len(srcKey) != len(destKey) {
		return false
	}
	for idx, srcRun := range srcKey {
		if srcRun&caseMask != destKey[idx]&caseMask {
			return false
		}
	}
	return true
}

func compact(dst *Buffer, src []byte, escape bool) error {
	origLen := dst.Len()
	var scan scanner
	scan.reset()
	start := 0
	for i, c := range src {
		if escape && (c == '<' || c == '>' || c == '&') {
			if start < i {
				dst.Write(src[start:i])
			}
			dst.WriteString(`\u00`)
			dst.WriteByte(hex[c>>4])
			dst.WriteByte(hex[c&0xF])
			start = i + 1
		}
		// Convert U+2028 and U+2029 (E2 80 A8 and E2 80 A9).
		if c == 0xE2 && i+2 < len(src) && src[i+1] == 0x80 && src[i+2]&^1 == 0xA8 {
			if start < i {
				dst.Write(src[start:i])
			}
			dst.WriteString(`\u202`)
			dst.WriteByte(hex[src[i+2]&0xF])
			start = i + 3
		}
		v := scan.step(c)
		if v >= scanSkipSpace {
			if v == scanError {
				break
			}
			if start < i {
				dst.Write(src[start:i])
			}
			start = i + 1
		}
	}
	if scan.eof() == scanError {
		dst.Truncate(origLen)
		return scan.err
	}
	if start < len(src) {
		dst.Write(src[start:])
	}
	return nil
}

func newline(dst *Buffer, prefix, indent string, depth int) {
	dst.WriteByte(newLine)
	dst.WriteString(prefix)
	for i := 0; i < depth; i++ {
		dst.WriteString(indent)
	}
}

func nonSpace(b []byte) bool {
	for _, c := range b {
		//if !isSpace(c) {
		if whitespace&(1<<uint(c)) == 0 {
			return true
		}
	}
	return false
}

// quoteChar formats c as a isBasic character literal
func quoteChar(c byte) string {
	// special cases - different from isBasic strings
	if c == sQuote {
		return `'\''`
	}
	if c == quote {
		return `'"'`
	}

	// use isBasic string with different quotation marks

	return "'" + string(c) + "'"
}

// indirect walks down v allocating pointers as needed, until it gets to a non-pointer.
// if it encounters an Unmarshaler, indirect stops and returns that.
// if decodingNull is true, indirect stops at the last pointer so it can be set to nil.
func indirect(v Value, decodingNull bool) (Value, bool) {
	// If v is a named type and is addressable, start with its address, so that if the type has pointer methods, we find them.
	if v.Kind() != Ptr && v.Type.Name() != "" && v.CanAddr() {
		v = Value{Type: v.Type.PtrTo(), Ptr: v.Ptr, Flag: v.ro() | Flag(Ptr)}
	}
	for {
		// Load value from interface, but only if the result will be
		// usefully addressable.
		if v.Kind() == Interface && !v.IsNil() {
			e := v.Iface() // .Elem()
			if e.Kind() == Ptr && !e.IsNil() && (!decodingNull || e.Deref().Kind() == Ptr) {
				v = e
				continue
			}
		}

		if v.Kind() != Ptr {
			break
		}

		// now v.Kind() == Ptr
		if v.Deref().Kind() != Ptr && decodingNull && v.CanSet() {
			break
		}

		if v.IsNil() {
			v.Set(New(v.Type.Deref()))
		}

		if v.Type.NumMethod() > 0 {
			if v.Type.implements(unmarshalerType) {
				return v, true
			}
		}

		v = v.Deref()
	}
	return v, false
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
