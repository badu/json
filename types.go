/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"bytes"
	"errors"
	"io"
	"reflect"
	"sync"
	"sync/atomic"
	"unicode/utf8"
)

const (
	caseMask     = ^byte(0x20) // Mask to ignore case in ASCII.
	kelvin       = '\u212a'
	smallLongEss = '\u017f'

	jsonTagName     = "json"
	stringTagOption = "string"
	omitTagOption   = "omitempty"

	comma       byte = ','
	squareOpen  byte = '['
	squareClose byte = ']'
	curlOpen    byte = '{'
	curlClose   byte = '}'
	zero        byte = '0'
	one         byte = '1'
	five        byte = '5'
	nine        byte = '9'
	tab         byte = '\t'
	newLine     byte = '\n'
	retChar     byte = '\r'
	space       byte = ' '
	quote       byte = '"'
	sQuote      byte = '\''
	backSlash   byte = '\\'
	slash       byte = '/'
	bChr        byte = 'b'
	fChr        byte = 'f'
	nChr        byte = 'n'
	rChr        byte = 'r'
	tChr        byte = 't'
	uChr        byte = 'u'
	lChr        byte = 'l'
	eChr        byte = 'e'
	sChr        byte = 's'
	aChr        byte = 'a'
	kChr        byte = 'k'
	zChr        byte = 'z'
	iChr        byte = 'i'
	gChr        byte = 'g'
	bigFChr     byte = 'F'
	bigAChr     byte = 'A'
	bigZChr     byte = 'Z'
	bigEChr     byte = 'E'
	bigKChr     byte = 'K'
	bigSChr     byte = 'S'
	bigNChr     byte = 'N'
	bigIChr     byte = 'I'
	minus       byte = '-'
	plus        byte = '+'
	colon       byte = ':'
	period      byte = '.'

	inLiteral       = "in literal "
	beginningValue  = "looking for beginning of value"
	beginningObject = "looking for beginning of object key string"
	inHexaEscape    = "in \\u hexadecimal character escape"
	trueString      = "true"
	falseString     = "false"
	nullString      = "null"
	/**

	// GoWhitespace is the default value for the Scanner's Whitespace field.
	// Its value selects Go's white space characters.
	// which is essentially cont GoWhitespace = 1<<9 | 1<<10 | 1<<13 | 1<<32 meaning 100000000000000000010011000000000
	const GoWhitespace = 1<<'\t' | 1<<'\n' | 1<<'\r' | 1<<' '

	// on init
	s.Whitespace = GoWhitespace

	// skip white space
		for s.Whitespace&(1<<uint(ch)) != 0 {
			ch = s.next()
		}

	//' ' == 32 so shifting 1 by 32 (1 << 32) bits sets 32nd bit as 1 etc.
	//
	//so the code
	//
	//GoWhitespace & (1<<uint(character))
	//would produce a non 0 value if the character is one of those characters in GoWhitespace

	*/
	whitespace = 1<<tab | 1<<newLine | 1<<retChar | 1<<space
)

// These values are returned by the state transition functions
// assigned to scanner.state and the method scanner.eof.
// They give details about the current state of the scan that
// callers might be interested to know about.
// It is okay to ignore the return value of any particular
// call to scanner.state: if one call returns scanError,
// every subsequent call will return scanError too.
const (
	// Continue.
	scanContinue     = iota // uninteresting byte
	scanBeginLiteral        // end implied by next result != scanContinue
	scanBeginObject         // begin object
	scanObjectKey           // just finished object key (string)
	scanObjectValue         // just finished non-last object value
	scanEndObject           // end object (implies scanObjectValue if possible)
	scanBeginArray          // begin array
	scanArrayValue          // just finished array value
	scanEndArray            // end array (implies scanArrayValue if possible)
	scanSkipSpace           // space byte; can skip; known to be last "continue" result

	// Stop.
	scanEnd   // top-level value ended *before* this byte; known to be first "stop" result
	scanError // hit an error, scanner.err.
)

// These values are stored in the parseState stack.
// They give the current state of a composite value
// being scanned. If the parser is inside a nested value
// the parseState describes the nested state, outermost at entry 0.
const (
	parseObjectKey   = iota // parsing object key (before colon)
	parseObjectValue        // parsing object value (after colon)
	parseArrayValue         // parsing array value
)

const (
	tokenTopValue = iota
	tokenArrayStart
	tokenArrayValue
	tokenArrayComma
	tokenObjectStart
	tokenObjectKey
	tokenObjectColon
	tokenObjectValue
	tokenObjectComma
)

type (
	// Unmarshaler is the interface implemented by types that can unmarshal a JSON description of themselves.
	// The input can be assumed to be a valid encoding of a JSON value. UnmarshalJSON must copy the JSON data if it wishes to retain the data after returning.
	// By convention, to approximate the behavior of Unmarshal itself, Unmarshalers implement UnmarshalJSON([]byte("null")) as a no-op.
	Unmarshaler interface {
		UnmarshalJSON([]byte) error
	}

	// Marshaler is the interface implemented by types that can marshal themselves into valid JSON.
	Marshaler interface {
		MarshalJSON() ([]byte, error)
	}

	// An UnmarshalTypeError describes a JSON value that was not appropriate for a value of a specific Go type.
	UnmarshalTypeError struct {
		Value  string       // description of JSON value - "bool", "array", "number -5"
		Type   reflect.Type // type of Go value it could not be assigned to
		Offset int64        // error occurred after reading Offset bytes
		Struct string       // name of the struct type containing the field
		Field  string       // name of the field holding the Go value
	}

	// An InvalidUnmarshalError describes an invalid argument passed to Unmarshal.
	// (The argument to Unmarshal must be a non-nil pointer.)
	InvalidUnmarshalError struct {
		Type reflect.Type
	}

	// A Number represents a JSON number literal.
	Number string

	// An encodeState encodes JSON into a bytes.Buffer.
	encodeState struct {
		bytes.Buffer // accumulated output
		scratch      [64]byte
		opts         encOpts
	}

	// decodeState represents the state while decoding a JSON value.
	decodeState struct {
		data         []byte
		scan         scanner
		nextScan     scanner // for calls to nextValue
		errorContext struct {
			Struct string
			Field  string
		}
		offset     int // provides context for type errors , read offset in data
		useNumber  bool
		useStrict  bool // don't allow the input to contains object keys which do not match any non-ignored, exported fields in the destination.
		savedError error
	}

	// An UnsupportedTypeError is returned by Marshal when attempting
	// to encode an unsupported value type.
	UnsupportedTypeError struct {
		Type reflect.Type
	}

	UnsupportedValueError struct {
		Str string
	}

	MarshalerError struct {
		Type reflect.Type
		Err  error
	}

	encOpts struct {
		quoted          bool // isBasic causes primitive fields to be encoded inside JSON strings.
		escapeHTML      bool // escapeHTML causes '<', '>', and '&' to be escaped in JSON strings.
		willSortMapKeys bool // map keys sorting. Default false
	}

	// A field represents a single field found in a struct.
	MarshalField struct {
		name     string
		indexes  []int
		Type     reflect.Type
		tag      bool
		willOmit bool
		isBasic  bool
	}
	marshalByIndex []MarshalField // byIndex sorts field by index sequence.

	qualFn func(srcKey, destKey []byte) bool
	// A field represents a single field found in a struct.
	field struct {
		name      string
		nameBytes []byte // []byte(name)
		indexes   []int
		Type      reflect.Type
		// TODO : extract equalFold from here and make a sync.Map (@see getFieldNamed of decode_state)
		equalFold qualFn // bytes.EqualFold or equivalent
		tag       bool
		willOmit  bool
		isBasic   bool
	}

	byIndex []field // byIndex sorts field by index sequence.

	// A SyntaxError is a description of a JSON syntax error.
	SyntaxError struct {
		msg    string // description of error
		Offset int64  // error occurred after reading Offset bytes
	}

	// A scanner is a JSON scanning state machine.
	// Callers call scan.reset() and then pass bytes in one at a time
	// by calling scan.step(&scan, c) for each byte.
	// The return value, referred to as an opcode, tells the
	// caller about significant parsing events like beginning
	// and ending literals, objects, and arrays, so that the
	// caller can follow along if it wishes.
	// The return value scanEnd indicates that a single top-level
	// JSON value has been completed, *before* the byte that
	// just got passed in.  (The indication must be delayed in order
	// to recognize the end of numbers: is 123 a whole value or
	// the beginning of 12345e+6?).
	scanner struct {
		step       func(byte) int // The step is a func to be called to execute the next transition.Also tried using an integer constant and a single func with a switch, but using the func directly was 10% faster on a 64-bit Mac Mini, and it's nicer to read.
		redoState  func(byte) int
		parseState []int // Stack of what we're in the middle of - array values, object keys, object values.
		bytes      int64 // total bytes consumed, updated by decoder.Decode
		redoCode   int
		err        error // Error that happened, if any.
		endTop     bool  // Reached end of top-level value.
		redo       bool  // 1-byte redo (see undo method)
	}

	// A Decoder reads and decodes JSON values from an input stream.
	Decoder struct {
		reader     io.Reader
		buf        []byte
		state      decodeState
		scanp      int   // start of unread data in buf
		scanned    int64 // amount of data already scanned
		scan       scanner
		err        error
		tokenState int
		tokenStack []int
	}

	// An Encoder writes JSON values to an output stream.
	Encoder struct {
		w            io.Writer
		err          error
		escapeHTML   bool
		sortMapKeys  bool
		indentBuf    *bytes.Buffer
		indentPrefix string
		indentValue  string
	}

	// RawMessage is a raw encoded JSON value.
	// It implements Marshaler and Unmarshaler and can
	// be used to delay JSON decoding or precompute a JSON encoding.
	RawMessage []byte

	// A Token holds a value of one of these types:
	//
	// 	Delim, for the four JSON delimiters [ ] { }
	// 	bool, for JSON booleans
	// 	float64, for JSON numbers
	// 	Number, for JSON numbers
	// 	string, for JSON string literals
	// 	nil, for JSON null
	//
	Token interface{}

	// A Delim is a JSON array or object delimiter, one of [ ] { or }.
	Delim rune

	// tagOptions is the string following a comma in a struct field's "json"
	// tag, or the empty string. It does not include the leading comma.
	tagOptions string
)

var (
	marshalerFieldCache struct {
		value atomic.Value // map[reflect.Type][]MarshalField
		mu    sync.Mutex   // used only by writers
	}

	unmarshalerFieldCache struct {
		value atomic.Value // map[reflect.Type][]field
		mu    sync.Mutex   // used only by writers
	}
	// errPhase is used for errors that should not happen unless
	// there is a bug in the JSON decoder or something is editing
	// the data slice while the decoder executes.
	errPhase = errors.New("JSON decoder out of sync - data changing underfoot?")

	nullLiteral  = []byte("null")
	trueLiteral  = []byte("true")
	falseLiteral = []byte("false")

	numberType = reflect.TypeOf(Number(""))

	hex = "0123456789abcdef"

	encodeStatePool sync.Pool

	marshalerType               = reflect.TypeOf(new(Marshaler)).Elem()
	unmarshalerType             = reflect.TypeOf(new(Unmarshaler)).Elem()
	_               Marshaler   = (*RawMessage)(nil)
	_               Unmarshaler = (*RawMessage)(nil)

	// safeSet holds the value true if the ASCII character with the given array
	// position can be represented inside a JSON string without any further
	// escaping.
	//
	// All values are true except for the ASCII control characters (0-31), the
	// double quote ("), and the backslash character ("\").
	safeSet = [utf8.RuneSelf]bool{
		' ':      true,
		'!':      true,
		'"':      false,
		'#':      true,
		'$':      true,
		'%':      true,
		'&':      true,
		'\'':     true,
		'(':      true,
		')':      true,
		'*':      true,
		'+':      true,
		',':      true,
		'-':      true,
		'.':      true,
		'/':      true,
		'0':      true,
		'1':      true,
		'2':      true,
		'3':      true,
		'4':      true,
		'5':      true,
		'6':      true,
		'7':      true,
		'8':      true,
		'9':      true,
		':':      true,
		';':      true,
		'<':      true,
		'=':      true,
		'>':      true,
		'?':      true,
		'@':      true,
		'A':      true,
		'B':      true,
		'C':      true,
		'D':      true,
		'E':      true,
		'F':      true,
		'G':      true,
		'H':      true,
		'I':      true,
		'J':      true,
		'K':      true,
		'L':      true,
		'M':      true,
		'N':      true,
		'O':      true,
		'P':      true,
		'Q':      true,
		'R':      true,
		'S':      true,
		'T':      true,
		'U':      true,
		'V':      true,
		'W':      true,
		'X':      true,
		'Y':      true,
		'Z':      true,
		'[':      true,
		'\\':     false,
		']':      true,
		'^':      true,
		'_':      true,
		'`':      true,
		'a':      true,
		'b':      true,
		'c':      true,
		'd':      true,
		'e':      true,
		'f':      true,
		'g':      true,
		'h':      true,
		'i':      true,
		'j':      true,
		'k':      true,
		'l':      true,
		'm':      true,
		'n':      true,
		'o':      true,
		'p':      true,
		'q':      true,
		'r':      true,
		's':      true,
		't':      true,
		'u':      true,
		'v':      true,
		'w':      true,
		'x':      true,
		'y':      true,
		'z':      true,
		'{':      true,
		'|':      true,
		'}':      true,
		'~':      true,
		'\u007f': true,
	}

	// htmlSafeSet holds the value true if the ASCII character with the given
	// array position can be safely represented inside a JSON string, embedded
	// inside of HTML <script> tags, without any additional escaping.
	//
	// All values are true except for the ASCII control characters (0-31), the
	// double quote ("), the backslash character ("\"), HTML opening and closing
	// tags ("<" and ">"), and the ampersand ("&").
	htmlSafeSet = [utf8.RuneSelf]bool{
		' ':      true,
		'!':      true,
		'"':      false,
		'#':      true,
		'$':      true,
		'%':      true,
		'&':      false,
		'\'':     true,
		'(':      true,
		')':      true,
		'*':      true,
		'+':      true,
		',':      true,
		'-':      true,
		'.':      true,
		'/':      true,
		'0':      true,
		'1':      true,
		'2':      true,
		'3':      true,
		'4':      true,
		'5':      true,
		'6':      true,
		'7':      true,
		'8':      true,
		'9':      true,
		':':      true,
		';':      true,
		'<':      false,
		'=':      true,
		'>':      false,
		'?':      true,
		'@':      true,
		'A':      true,
		'B':      true,
		'C':      true,
		'D':      true,
		'E':      true,
		'F':      true,
		'G':      true,
		'H':      true,
		'I':      true,
		'J':      true,
		'K':      true,
		'L':      true,
		'M':      true,
		'N':      true,
		'O':      true,
		'P':      true,
		'Q':      true,
		'R':      true,
		'S':      true,
		'T':      true,
		'U':      true,
		'V':      true,
		'W':      true,
		'X':      true,
		'Y':      true,
		'Z':      true,
		'[':      true,
		'\\':     false,
		']':      true,
		'^':      true,
		'_':      true,
		'`':      true,
		'a':      true,
		'b':      true,
		'c':      true,
		'd':      true,
		'e':      true,
		'f':      true,
		'g':      true,
		'h':      true,
		'i':      true,
		'j':      true,
		'k':      true,
		'l':      true,
		'm':      true,
		'n':      true,
		'o':      true,
		'p':      true,
		'q':      true,
		'r':      true,
		's':      true,
		't':      true,
		'u':      true,
		'v':      true,
		'w':      true,
		'x':      true,
		'y':      true,
		'z':      true,
		'{':      true,
		'|':      true,
		'}':      true,
		'~':      true,
		'\u007f': true,
	}
)
