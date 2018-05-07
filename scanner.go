/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

// reset prepares the scanner for use.
// It must be called before calling s.step.
func (s *scanner) reset() {
	s.step = s.stateBeginValue
	s.parseState = s.parseState[0:0]
	s.err = nil
	s.redo = false
	s.endTop = false
}

// eof tells the scanner that the end of input has been reached.
// It returns a scan status just as s.step does.
func (s *scanner) eof() int {
	if s.err != nil {
		return scanError
	}
	if s.endTop {
		return scanEnd
	}
	s.step(space)
	if s.endTop {
		return scanEnd
	}
	if s.err == nil {
		s.err = &SyntaxError{"unexpected end of JSON input", s.bytes}
	}
	return scanError
}

// pushParseState pushes a new parse state p onto the parse stack.
func (s *scanner) pushParseState(p int) {
	s.parseState = append(s.parseState, p)
}

// popParseState pops a parse state (already obtained) off the stack
// and updates s.step accordingly.
func (s *scanner) popParseState() {
	n := len(s.parseState) - 1
	s.parseState = s.parseState[0:n]
	s.redo = false
	if n == 0 {
		s.step = s.stateEndTop
		s.endTop = true
	} else {
		s.step = s.stateEndValue
	}
}

// error records an error and switches to the error state.
func (s *scanner) error(provided byte, context string, expected ...byte) int {
	s.step = s.stateError
	expectation := ""
	for idx, ex := range expected {
		if idx > 0 {
			expectation += "or "
		}
		expectation += quoteChar(ex) + " "
	}
	if len(expectation) == 0 {
		s.err = &SyntaxError{"invalid character " + quoteChar(provided) + " " + context, s.bytes}
	} else {
		s.err = &SyntaxError{"invalid character " + quoteChar(provided) + " expected " + expectation + context, s.bytes}
	}
	return scanError
}

// undo causes the scanner to return scanCode from the next state transition.
// This gives callers a simple 1-byte undo mechanism.
func (s *scanner) undo(scanCode int) {
	if s.redo {
		panic("json: invalid use of scanner")
	}
	s.redoCode = scanCode
	s.redoState = s.step
	s.step = s.stateRedo
	s.redo = true
}

// stateBeginValueOrEmpty is the state after reading `[`.
func (s *scanner) stateBeginValueOrEmpty(c byte) int {
	//if c <= space && isSpace(c) {
	if c <= space && whitespace&(1<<uint(c)) != 0 {
		return scanSkipSpace
	}
	if c == squareClose {
		return s.stateEndValue(c)
	}
	return s.stateBeginValue(c)
}

// stateBeginValue is the state at the beginning of the input.
func (s *scanner) stateBeginValue(c byte) int {
	//if c <= space && isSpace(c) {
	if c <= space && whitespace&(1<<uint(c)) != 0 {
		return scanSkipSpace
	}
	switch c {
	case curlOpen:
		s.step = s.stateBeginStringOrEmpty
		s.pushParseState(parseObjectKey)
		return scanBeginObject
	case squareOpen:
		s.step = s.stateBeginValueOrEmpty
		s.pushParseState(parseArrayValue)
		return scanBeginArray
	case quote:
		s.step = s.stateInString
		return scanBeginLiteral
	case minus:
		s.step = s.stateNeg
		return scanBeginLiteral
	case zero: // beginning of 0.123
		s.step = s.state0
		return scanBeginLiteral
	case tChr: // beginning of true
		s.step = s.stateT
		return scanBeginLiteral
	case fChr: // beginning of false
		s.step = s.stateF
		return scanBeginLiteral
	case nChr: // beginning of null
		s.step = s.stateN
		return scanBeginLiteral
	}
	if one <= c && c <= nine { // beginning of 1234.5
		s.step = s.state1
		return scanBeginLiteral
	}
	return s.error(c, beginningValue, space, curlOpen, squareOpen, quote, minus, zero, tChr, fChr, nChr, one)
}

// stateBeginStringOrEmpty is the state after reading `{`.
func (s *scanner) stateBeginStringOrEmpty(c byte) int {
	//if c <= space && isSpace(c) {
	if c <= space && whitespace&(1<<uint(c)) != 0 {
		return scanSkipSpace
	}
	if c == curlClose {
		n := len(s.parseState)
		s.parseState[n-1] = parseObjectValue
		return s.stateEndValue(c)
	}
	return s.stateBeginString(c)
}

// stateBeginString is the state after reading `{"key": value,`.
func (s *scanner) stateBeginString(c byte) int {
	//if c <= space && isWhiteSpaceByte(c) {
	if c <= space && whitespace&(1<<uint(c)) != 0 {
		return scanSkipSpace
	}
	if c == quote {
		s.step = s.stateInString
		return scanBeginLiteral
	}
	return s.error(c, beginningObject, quote, space)
}

// stateEndValue is the state after completing a value,
// such as after reading `{}` or `true` or `["x"`.
func (s *scanner) stateEndValue(c byte) int {
	n := len(s.parseState)
	if n == 0 {
		// Completed top-level before the current byte.
		s.step = s.stateEndTop
		s.endTop = true
		return s.stateEndTop(c)
	}
	//if c <= space && isSpace(c) {
	if c <= space && whitespace&(1<<uint(c)) != 0 {
		s.step = s.stateEndValue
		return scanSkipSpace
	}
	ps := s.parseState[n-1]
	switch ps {
	case parseObjectKey:
		if c == colon {
			s.parseState[n-1] = parseObjectValue
			s.step = s.stateBeginValue
			return scanObjectKey
		}
		return s.error(c, "after object key", colon)
	case parseObjectValue:
		if c == comma {
			s.parseState[n-1] = parseObjectKey
			s.step = s.stateBeginString
			return scanObjectValue
		}
		if c == curlClose {
			s.popParseState()
			return scanEndObject
		}
		return s.error(c, "after object key:value pair", comma, curlClose)
	case parseArrayValue:
		if c == comma {
			s.step = s.stateBeginValue
			return scanArrayValue
		}
		if c == squareClose {
			s.popParseState()
			return scanEndArray
		}
		return s.error(c, "after array element", comma, squareClose)
	}
	return s.error(c, "stateEndValue unkown")
}

// stateEndTop is the state after finishing the top-level value,
// such as after reading `{}` or `[1,2,3]`.
// Only space characters should be seen now.
func (s *scanner) stateEndTop(c byte) int {
	if c != space && c != tab && c != retChar && c != newLine {
		// Complain about non-space byte on next call.
		s.error(c, "after top-level value", space, tab, newLine)
	}
	return scanEnd
}

// stateInString is the state after reading `"`.
func (s *scanner) stateInString(c byte) int {
	if c == quote {
		s.step = s.stateEndValue
		return scanContinue
	}
	if c == backSlash {
		s.step = s.stateInStringEsc
		return scanContinue
	}
	if c < 0x20 {
		return s.error(c, "in string literal")
	}
	return scanContinue
}

// stateInStringEsc is the state after reading `"\` during a isBasic string.
func (s *scanner) stateInStringEsc(c byte) int {
	switch c {
	case bChr, fChr, nChr, rChr, tChr, backSlash, slash, quote:
		s.step = s.stateInString
		return scanContinue
	case uChr:
		s.step = s.stateInStringEscU
		return scanContinue
	}
	return s.error(c, "in string escape code")
}

// stateInStringEscU is the state after reading `"\u` during a isBasic string.
func (s *scanner) stateInStringEscU(c byte) int {
	if zero <= c && c <= nine || aChr <= c && c <= fChr || bigAChr <= c && c <= fChr {
		s.step = s.stateInStringEscU1
		return scanContinue
	}
	// numbers
	return s.error(c, inHexaEscape)
}

// stateInStringEscU1 is the state after reading `"\u1` during a isBasic string.
func (s *scanner) stateInStringEscU1(c byte) int {
	if zero <= c && c <= nine || aChr <= c && c <= fChr || bigAChr <= c && c <= fChr {
		s.step = s.stateInStringEscU12
		return scanContinue
	}
	// numbers
	return s.error(c, inHexaEscape)
}

// stateInStringEscU12 is the state after reading `"\u12` during a isBasic string.
func (s *scanner) stateInStringEscU12(c byte) int {
	if zero <= c && c <= nine || aChr <= c && c <= fChr || bigAChr <= c && c <= fChr {
		s.step = s.stateInStringEscU123
		return scanContinue
	}
	// numbers
	return s.error(c, inHexaEscape)
}

// stateInStringEscU123 is the state after reading `"\u123` during a isBasic string.
func (s *scanner) stateInStringEscU123(c byte) int {
	if zero <= c && c <= nine || aChr <= c && c <= fChr || bigAChr <= c && c <= fChr {
		s.step = s.stateInString
		return scanContinue
	}
	// numbers
	return s.error(c, inHexaEscape)
}

// stateNeg is the state after reading `-` during a number.
func (s *scanner) stateNeg(c byte) int {
	if c == zero {
		s.step = s.state0
		return scanContinue
	}
	if one <= c && c <= nine {
		s.step = s.state1
		return scanContinue
	}
	return s.error(c, "in numeric literal")
}

// state1 is the state after reading a non-zero integer during a number,
// such as after reading `1` or `100` but not `0`.
func (s *scanner) state1(c byte) int {
	if zero <= c && c <= nine {
		s.step = s.state1
		return scanContinue
	}
	return s.state0(c)
}

// state0 is the state after reading `0` during a number.
func (s *scanner) state0(c byte) int {
	if c == period {
		s.step = s.stateDot
		return scanContinue
	}
	if c == eChr || c == bigEChr {
		s.step = s.stateE
		return scanContinue
	}
	return s.stateEndValue(c)
}

// stateDot is the state after reading the integer and decimal point in a number,
// such as after reading `1.`.
func (s *scanner) stateDot(c byte) int {
	if zero <= c && c <= nine {
		s.step = s.stateDot0
		return scanContinue
	}
	return s.error(c, "after decimal point in numeric literal", zero, nine)
}

// stateDot0 is the state after reading the integer, decimal point, and subsequent
// digits of a number, such as after reading `3.14`.
func (s *scanner) stateDot0(c byte) int {
	if zero <= c && c <= nine {
		return scanContinue
	}
	if c == eChr || c == bigEChr {
		s.step = s.stateE
		return scanContinue
	}
	return s.stateEndValue(c)
}

// stateE is the state after reading the mantissa and e in a number,
// such as after reading `314e` or `0.314e`.
func (s *scanner) stateE(c byte) int {
	if c == plus || c == minus {
		s.step = s.stateESign
		return scanContinue
	}
	return s.stateESign(c)
}

// stateESign is the state after reading the mantissa, e, and sign in a number,
// such as after reading `314e-` or `0.314e+`.
func (s *scanner) stateESign(c byte) int {
	if zero <= c && c <= nine {
		s.step = s.stateE0
		return scanContinue
	}
	return s.error(c, "in exponent of numeric literal")
}

// stateE0 is the state after reading the mantissa, e, optional sign,
// and at least one digit of the exponent in a number,
// such as after reading `314e-2` or `0.314e+1` or `3.14e0`.
func (s *scanner) stateE0(c byte) int {
	if zero <= c && c <= nine {
		return scanContinue
	}
	return s.stateEndValue(c)
}

// stateT is the state after reading `t`.
func (s *scanner) stateT(c byte) int {
	if c == rChr {
		s.step = s.stateTr
		return scanContinue
	}
	return s.error(c, inLiteral+trueString, rChr)
}

// stateTr is the state after reading `tr`.
func (s *scanner) stateTr(c byte) int {
	if c == uChr {
		s.step = s.stateTru
		return scanContinue
	}
	return s.error(c, inLiteral+trueString, uChr)
}

// stateTru is the state after reading `tru`.
func (s *scanner) stateTru(c byte) int {
	if c == eChr {
		s.step = s.stateEndValue
		return scanContinue
	}
	return s.error(c, inLiteral+trueString, eChr)
}

// stateF is the state after reading `f`.
func (s *scanner) stateF(c byte) int {
	if c == aChr {
		s.step = s.stateFa
		return scanContinue
	}
	return s.error(c, inLiteral+falseString, aChr)
}

// stateFa is the state after reading `fa`.
func (s *scanner) stateFa(c byte) int {
	if c == lChr {
		s.step = s.stateFal
		return scanContinue
	}
	return s.error(c, inLiteral+falseString, lChr)
}

// stateFal is the state after reading `fal`.
func (s *scanner) stateFal(c byte) int {
	if c == sChr {
		s.step = s.stateFals
		return scanContinue
	}
	return s.error(c, inLiteral+falseString, sChr)
}

// stateFals is the state after reading `fals`.
func (s *scanner) stateFals(c byte) int {
	if c == eChr {
		s.step = s.stateEndValue
		return scanContinue
	}
	return s.error(c, inLiteral+falseString, eChr)
}

// stateN is the state after reading `n`.
func (s *scanner) stateN(c byte) int {
	if c == uChr {
		s.step = s.stateNu
		return scanContinue
	}
	return s.error(c, inLiteral+nullString, uChr)
}

// stateNu is the state after reading `nu`.
func (s *scanner) stateNu(c byte) int {
	if c == lChr {
		s.step = s.stateNul
		return scanContinue
	}
	return s.error(c, inLiteral+nullString, lChr)
}

// stateNul is the state after reading `nul`.
func (s *scanner) stateNul(c byte) int {
	if c == lChr {
		s.step = s.stateEndValue
		return scanContinue
	}
	return s.error(c, inLiteral+nullString, lChr)
}

// stateError is the state after reaching a syntax error,
// such as after reading `[1}` or `5.1.2`.
func (s *scanner) stateError(c byte) int {
	return scanError
}

// stateRedo helps implement the scanner's 1-byte undo.
func (s *scanner) stateRedo(c byte) int {
	s.redo = false
	s.step = s.redoState
	return s.redoCode
}

// checkValid verifies that data is valid JSON-encoded data.
// scan is passed in for use by checkValid to avoid an allocation.
// TODO : "scan is passed in for use by checkValid to avoid an allocation"
func (s *scanner) checkValid(data []byte) error {
	s.reset()
	for _, c := range data {
		s.bytes++
		if s.step(c) == scanError {
			return s.err
		}
	}
	if s.eof() == scanError {
		return s.err
	}
	return nil
}

// nextValue splits data after the next whole JSON value,
// returning that value and the bytes that follow it as separate slices.
// scan is passed in for use by nextValue to avoid an allocation.
func (s *scanner) nextValue(data []byte) (value, rest []byte, err error) {
	s.reset()
	for i, c := range data {
		v := s.step(c)
		if v >= scanEndObject {
			switch v {
			// probe the scanner with a space to determine whether we will
			// get scanEnd on the next character. Otherwise, if the next character
			// is not a space, scanEndTop allocates a needless error.
			case scanEndObject, scanEndArray:
				if s.step(space) == scanEnd {
					return data[:i+1], data[i+1:], nil
				}
			case scanError:
				return nil, nil, s.err
			case scanEnd:
				return data[:i], data[i:], nil
			}
		}
	}
	if s.eof() == scanError {
		return nil, nil, s.err
	}
	return data, nil, nil
}
