/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"bytes"
	"io"
)

// UseNumber causes the Decoder to unmarshal a number into an interface{} as a Number instead of as a float64.
func (d *Decoder) UseNumber() { d.state.useNumber = true }

// UseStrict causes the Decoder to return an error when the destination is a struct and the input contains object keys which do not match any non-ignored, exported fields in the destination.
func (d *Decoder) UseStrict() { d.state.useStrict = true }

// Decode reads the next JSON-encoded value from its input and stores it in the value pointed to by v.
// See the documentation for Unmarshal for details about the conversion of JSON into a Go value.
func (d *Decoder) Decode(target interface{}) error {
	if d.err != nil {
		return d.err
	}

	if err := d.tokenPrepareForDecode(); err != nil {
		return err
	}

	if !d.tokenValueAllowed() {
		return &SyntaxError{msg: "not at beginning of value", Offset: d.offset()}
	}

	// Read whole value into buffer.
	n, err := d.readValue()
	if err != nil {
		return err
	}
	d.state.init(d.buf[d.scanp : d.scanp+n])
	d.scanp += n

	// Don't save err from unmarshal into d.err: the connection is still usable since we read a complete JSON object from it before the error happened.
	err = d.state.unmarshal(target)

	// fixup token streaming state
	d.tokenValueEnd()

	return err
}

// Buffered returns a reader of the data remaining in the Decoder's buffer. The reader is valid until the next call to Decode.
func (d *Decoder) Buffered() io.Reader {
	return bytes.NewReader(d.buf[d.scanp:])
}

// readValue reads a JSON value into dec.buf. It returns the length of the encoding.
func (d *Decoder) readValue() (int, error) {
	d.scan.reset()

	scanp := d.scanp
	var err error
Input:
	for {
		// Look in the buffer for a new value.
		for i, c := range d.buf[scanp:] {
			d.scan.bytes++
			cur := d.scan.step(c)
			if cur == scanEnd {
				scanp += i
				break Input
			}
			// scanEnd is delayed one byte.
			// We might block trying to get that byte from src,
			// so instead invent a space byte.
			if (cur == scanEndObject || cur == scanEndArray) && d.scan.step(space) == scanEnd {
				scanp += i + 1
				break Input
			}
			if cur == scanError {
				d.err = d.scan.err
				return 0, d.scan.err
			}
		}
		scanp = len(d.buf)

		// Did the last read have an error?
		// Delayed until now to allow buffer scan.
		if err != nil {
			if err == io.EOF {
				if d.scan.step(space) == scanEnd {
					break Input
				}
				if nonSpace(d.buf) {
					err = io.ErrUnexpectedEOF
				}
			}
			d.err = err
			return 0, err
		}

		n := scanp - d.scanp
		err = d.refill()
		scanp = d.scanp + n
	}
	return scanp - d.scanp, nil
}

func (d *Decoder) refill() error {
	// Make room to read more into the buffer.
	// First slide down data already consumed.
	if d.scanp > 0 {
		d.scanned += int64(d.scanp)
		n := copy(d.buf, d.buf[d.scanp:])
		d.buf = d.buf[:n]
		d.scanp = 0
	}

	// Grow buffer if not large enough.
	const minRead = 512
	if cap(d.buf)-len(d.buf) < minRead {
		newBuf := make([]byte, len(d.buf), 2*cap(d.buf)+minRead)
		copy(newBuf, d.buf)
		d.buf = newBuf
	}

	// Read. Delay error for next iteration (after scan).
	n, err := d.reader.Read(d.buf[len(d.buf):cap(d.buf)])
	d.buf = d.buf[0 : len(d.buf)+n]

	return err
}

// advance tokenstate from a separator state to a value state
func (d *Decoder) tokenPrepareForDecode() error {
	// Note: Not calling peek before switch, to avoid putting peek into the standard Decode path. peek is only called when using the Token API.
	switch d.tokenState {
	case tokenArrayComma:
		c, err := d.peek()
		if err != nil {
			return err
		}
		if c != comma {
			return &SyntaxError{"expected comma after array element", d.offset()}
		}
		d.scanp++
		d.tokenState = tokenArrayValue
	case tokenObjectColon:
		c, err := d.peek()
		if err != nil {
			return err
		}
		if c != colon {
			return &SyntaxError{"expected colon after object key", d.offset()}
		}
		d.scanp++
		d.tokenState = tokenObjectValue
	}
	return nil
}

func (d *Decoder) tokenValueAllowed() bool {
	switch d.tokenState {
	case tokenTopValue, tokenArrayStart, tokenArrayValue, tokenObjectValue:
		return true
	}
	return false
}

func (d *Decoder) tokenValueEnd() {
	switch d.tokenState {
	case tokenArrayStart, tokenArrayValue:
		d.tokenState = tokenArrayComma
	case tokenObjectValue:
		d.tokenState = tokenObjectComma
	}
}

// Token returns the next JSON token in the input stream.
// At the end of the input stream, Token returns nil, io.EOF.
// Token guarantees that the delimiters [ ] { } it returns are properly nested and matched: if Token encounters an unexpected delimiter in the input, it will return an error.
// The input stream consists of basic JSON values—bool, string, number, and null—along with delimiters [ ] { } of type Delim to mark the start and end of arrays and objects.
// Commas and colons are elided.
func (d *Decoder) Token() (Token, error) {
	for {
		c, err := d.peek()
		if err != nil {
			return nil, err
		}
		switch c {
		case squareOpen:
			if !d.tokenValueAllowed() {
				return d.tokenError(c)
			}
			d.scanp++
			d.tokenStack = append(d.tokenStack, d.tokenState)
			d.tokenState = tokenArrayStart
			return Delim(squareOpen), nil

		case squareClose:
			if d.tokenState != tokenArrayStart && d.tokenState != tokenArrayComma {
				return d.tokenError(c)
			}
			d.scanp++
			d.tokenState = d.tokenStack[len(d.tokenStack)-1]
			d.tokenStack = d.tokenStack[:len(d.tokenStack)-1]
			d.tokenValueEnd()
			return Delim(squareClose), nil

		case curlOpen:
			if !d.tokenValueAllowed() {
				return d.tokenError(c)
			}
			d.scanp++
			d.tokenStack = append(d.tokenStack, d.tokenState)
			d.tokenState = tokenObjectStart
			return Delim(curlOpen), nil

		case curlClose:
			if d.tokenState != tokenObjectStart && d.tokenState != tokenObjectComma {
				return d.tokenError(c)
			}
			d.scanp++
			d.tokenState = d.tokenStack[len(d.tokenStack)-1]
			d.tokenStack = d.tokenStack[:len(d.tokenStack)-1]
			d.tokenValueEnd()
			return Delim(curlClose), nil

		case colon:
			if d.tokenState != tokenObjectColon {
				return d.tokenError(c)
			}
			d.scanp++
			d.tokenState = tokenObjectValue
			continue

		case comma:
			if d.tokenState == tokenArrayComma {
				d.scanp++
				d.tokenState = tokenArrayValue
				continue
			}
			if d.tokenState == tokenObjectComma {
				d.scanp++
				d.tokenState = tokenObjectKey
				continue
			}
			return d.tokenError(c)

		case quote:
			if d.tokenState == tokenObjectStart || d.tokenState == tokenObjectKey {
				var x string
				old := d.tokenState
				d.tokenState = tokenTopValue
				err := d.Decode(&x)
				d.tokenState = old
				if err != nil {
					return nil, err
				}
				d.tokenState = tokenObjectColon
				return x, nil
			}
			fallthrough

		default:
			if !d.tokenValueAllowed() {
				return d.tokenError(c)
			}
			var x interface{}
			if err := d.Decode(&x); err != nil {
				return nil, err
			}
			return x, nil
		}
	}
}

func (d *Decoder) tokenError(c byte) (Token, error) {
	var context string
	switch d.tokenState {
	case tokenTopValue:
		context = " looking for beginning of value"
	case tokenArrayStart, tokenArrayValue, tokenObjectValue:
		context = " looking for beginning of value"
	case tokenArrayComma:
		context = " after array element"
	case tokenObjectKey:
		context = " looking for beginning of object key string"
	case tokenObjectColon:
		context = " after object key"
	case tokenObjectComma:
		context = " after object key:value pair"
	}
	return nil, &SyntaxError{"invalid character " + quoteChar(c) + " " + context, d.offset()}
}

// More reports whether there is another element in the current array or object being parsed.
func (d *Decoder) More() bool {
	c, err := d.peek()
	return err == nil && c != squareClose && c != curlClose
}

func (d *Decoder) peek() (byte, error) {
	var err error
	for {
		for i := d.scanp; i < len(d.buf); i++ {
			c := d.buf[i]
			// below expression, to be read `if isWhiteSpaceByte(c)`
			if whitespace&(1<<uint(c)) != 0 {
				continue
			}

			d.scanp = i
			return c, nil
		}
		// buffer has been scanned, now report any error
		if err != nil {
			return 0, err
		}
		err = d.refill()
	}
}

func (d *Decoder) offset() int64 {
	return d.scanned + int64(d.scanp)
}
