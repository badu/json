/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"bytes"
	"unicode"
	"unicode/utf8"
)

func newName(newName []byte) name {
	if len(newName) > 1<<16-1 {
		panic("reflect : name too long: " + string(newName))
	}
	l := 1 + 2 + len(newName)
	b := make([]byte, l)
	var bits byte
	b[0] = bits
	b[1] = uint8(len(newName) >> 8)
	b[2] = uint8(len(newName))
	copy(b[3:], newName)
	return name{bytes: &b[0]}
}

func (n name) data(offset int) *byte {
	return (*byte)(ptr(uintptr(ptr(n.bytes)) + uintptr(offset)))
}

func (n name) isExported() bool {
	return (*n.bytes)&(1) != 0
}

func (n name) nameLen() int {
	if n.bytes == nil {
		return 0
	}
	return int(uint16(*n.data(1))<<8 | uint16(*n.data(2)))
}

func (n name) tagLen() int {
	if *n.data(0)&(2) == 0 {
		return 0
	}
	offset := 3 + n.nameLen()
	return int(uint16(*n.data(offset))<<8 | uint16(*n.data(offset + 1)))
}

func (n name) name() []byte {
	if n.bytes == nil {
		return nil
	}
	info := (*[4]byte)(ptr(n.bytes))
	nameLen := n.nameLen()
	result := make([]byte, nameLen)
	header := (*stringHeader)(ptr(&result))
	header.Data = ptr(&info[3])
	header.Len = nameLen
	return result
}

func (n name) tag() []byte {
	tagLen := n.tagLen()
	if tagLen == 0 {
		return nil
	}
	nameLen := n.nameLen()
	result := make([]byte, tagLen)
	header := (*stringHeader)(ptr(&result))
	header.Data = ptr(n.data(5 + nameLen))
	header.Len = tagLen
	return result
}

func (n name) pkgPath() []byte {
	if n.bytes == nil || *n.data(0)&(1<<2) == 0 {
		return nil
	}
	offset := 3 + n.nameLen()
	if tagLen := n.tagLen(); tagLen > 0 {
		offset += 2 + tagLen
	}
	var nameOffset int32
	// Note that this field may not be aligned in memory, so we cannot use a direct int32 assignment here.
	copy((*[4]byte)(ptr(&nameOffset))[:], (*[4]byte)(ptr(n.data(offset)))[:])
	pkgPathName := name{(*byte)(resolveTypeOff(ptr(n.bytes), nameOffset))}
	return pkgPathName.name()
}

// tagContains reports whether a comma-separated list of options
// contains a particular substr flag. substr must be surrounded by a
// string boundary or commas.
func tagContains(tag, optionName []byte) bool {
	if len(tag) == 0 {
		return false
	}

	tmp := tag
	for len(tmp) > 0 {
		var next []byte
		i := bytes.IndexByte(tmp, comma)
		if i >= 0 {
			tmp, next = tmp[:i], tmp[i+1:]
		}
		if bytes.Equal(tmp, optionName) {
			return true
		}
		tmp = next
	}
	return false
}

// parseTag splits a struct field's json tag into its name and comma-separated options.
func parseTag(tag []byte) ([]byte, []byte) {
	if idx := bytes.IndexByte(tag, comma); idx != -1 {
		return tag[:idx], tag[idx+1:]
	}
	return tag, nil
}

func isValidTag(tag []byte) bool {
	if len(tag) == 0 {
		return false
	}
	tmp := tag
	for len(tmp) > 0 {
		r, size := utf8.DecodeRune(tmp)
		switch {
		//TODO : can do better than this
		case bytes.ContainsRune(allowedRunesInTag, r):
			//case strings.ContainsRune("!#$%&()*+-./:<=>?@[]^_{|}~ ", c):
			// Backslash and quote chars are reserved, but otherwise any punctuation chars are allowed in a tag name.
		default:
			if !unicode.IsLetter(r) && !unicode.IsDigit(r) {
				return false
			}
		}
		tmp = tmp[size:]
	}
	return true
}

func tagLookup(tag, key []byte) []byte {
	tmp := tag
	for len(tmp) > 0 {
		// Skip leading space.
		i := 0
		for i < len(tmp) && tmp[i] == ' ' {
			i++
		}
		tmp = tmp[i:]
		if len(tmp) == 0 {
			break
		}

		i = 0
		for i < len(tmp) && tmp[i] > ' ' && tmp[i] != ':' && tmp[i] != '"' && tmp[i] != 0x7f {
			i++
		}
		if i == 0 || i+1 >= len(tmp) || tmp[i] != ':' || tmp[i+1] != '"' {
			break
		}
		name := tmp[:i]
		tmp = tmp[i+1:]

		// Scan quoted string to find value.
		i = 1
		for i < len(tmp) && tmp[i] != '"' {
			if tmp[i] == '\\' {
				i++
			}
			i++
		}
		if i >= len(tmp) {
			break
		}
		qvalue := tmp[:i+1]
		tmp = tmp[i+1:]

		if bytes.Equal(key, name) {
			value, err := Unquote(qvalue)
			if err != nil {
				break
			}
			return value
		}
	}
	return nil
}
