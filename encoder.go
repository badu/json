/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import "bytes"

// Encode writes the JSON encoding of v to the stream, followed by a newline character.
// See the documentation for Marshal for details about the conversion of Go values to JSON.
func (enc *Encoder) Encode(v interface{}) error {
	if enc.err != nil {
		return enc.err
	}
	state := newEncodeState()
	state.opts = encOpts{escapeHTML: enc.escapeHTML, willSortMapKeys: enc.sortMapKeys}
	err := state.marshal(v)
	if err != nil {
		return err
	}

	// Terminate each value with a newline.
	// This makes the output look a little nicer when debugging, and some kind of space is required if the encoded value was a number, so that the reader knows there aren't more digits coming.
	state.WriteByte(newLine)

	b := state.Bytes()
	if len(enc.indentPrefix) > 0 || len(enc.indentValue) > 0 {
		if enc.indentBuf == nil {
			enc.indentBuf = new(bytes.Buffer)
		}
		enc.indentBuf.Reset()
		err = Indent(enc.indentBuf, b, enc.indentPrefix, enc.indentValue)
		if err != nil {
			return err
		}
		b = enc.indentBuf.Bytes()
	}
	if _, err = enc.w.Write(b); err != nil {
		enc.err = err
	}
	encodeStatePool.Put(state)
	return err
}

// SetIndent instructs the encoder to format each subsequent encoded value as if indented by the package-level function Indent(dst, src, prefix, indent).
// Calling SetIndent("", "") disables indentation.
func (enc *Encoder) SetIndent(prefix, indent string) {
	enc.indentPrefix = prefix
	enc.indentValue = indent
}

// SetEscapeHTML specifies whether problematic HTML characters should be escaped inside JSON isBasic strings.
// The default behavior is to escape &, <, and > to \u0026, \u003c, and \u003e to avoid certain safety problems that can arise when embedding JSON in HTML.
//
// In non-HTML settings where the escaping interferes with the readability of the output, SetEscapeHTML(false) disables this behavior.
func (enc *Encoder) SetEscapeHTML(on bool) {
	enc.escapeHTML = on
}

// SetSortMapKeys specifies whether map keys will get sorted or not on encoding
func (enc *Encoder) SetSortMapKeys(on bool) {
	enc.sortMapKeys = on
}
