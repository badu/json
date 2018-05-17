/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

// reset prepares the scanner for use.
// It must be called before calling s.step.
func scanReset(s *scanner) {
	s.step = stateBeginValue
	s.parseState = s.parseState[0:0]
	s.err = nil
	s.redo = false
	s.endTop = false
}

// eof tells the scanner that the end of input has been reached.
// It returns a scan status just as s.step does.
func scanEof(s *scanner) int {
	if s.err != nil {
		return scanError
	}
	if s.endTop {
		return scanEnd
	}
	s.step(s, space)
	if s.endTop {
		return scanEnd
	}
	if s.err == nil {
		s.err = &SyntaxError{"unexpected end of JSON input", s.bytes}
	}
	return scanError
}

// pushParseState pushes a new parse state p onto the parse stack.
func pushParseState(s *scanner, p int) {
	s.parseState = append(s.parseState, p)
}

// popParseState pops a parse state (already obtained) off the stack
// and updates s.step accordingly.
func popParseState(s *scanner) {
	n := len(s.parseState) - 1
	s.parseState = s.parseState[0:n]
	s.redo = false
	if n == 0 {
		s.step = stateEndTop
		s.endTop = true
	} else {
		s.step = stateEndValue
	}
}

// error records an error and switches to the error state.
func makeScanError(s *scanner, provided byte, context string, expected ...byte) int {
	s.step = stateError
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
func scanUndo(s *scanner, scanCode int) {
	if s.redo {
		panic("json: invalid use of scanner")
	}
	s.redoCode = scanCode
	s.redoState = s.step
	s.step = stateRedo
	s.redo = true
}

// stateBeginValueOrEmpty is the state after reading `[`.
func stateBeginValueOrEmpty(s *scanner, c byte) int {
	//if c <= space && isSpace(c) {
	if c <= space && whitespace&(1<<uint(c)) != 0 {
		return scanSkipSpace
	}
	if c == squareClose {
		return stateEndValue(s, c)
	}
	return stateBeginValue(s, c)
}

// stateBeginValue is the state at the beginning of the input.
func stateBeginValue(s *scanner, c byte) int {
	//if c <= space && isSpace(c) {
	if c <= space && whitespace&(1<<uint(c)) != 0 {
		return scanSkipSpace
	}
	switch c {
	case curlOpen:
		s.step = stateBeginStringOrEmpty
		pushParseState(s, parseObjectKey)
		return scanBeginObject
	case squareOpen:
		s.step = stateBeginValueOrEmpty
		pushParseState(s, parseArrayValue)
		return scanBeginArray
	case quote:
		s.step = stateInString
		return scanBeginLiteral
	case minus:
		s.step = stateNeg
		return scanBeginLiteral
	case zero: // beginning of 0.123
		s.step = state0
		return scanBeginLiteral
	case tChr: // beginning of true
		s.step = stateT
		return scanBeginLiteral
	case fChr: // beginning of false
		s.step = stateF
		return scanBeginLiteral
	case nChr: // beginning of null
		s.step = stateN
		return scanBeginLiteral
	}
	if one <= c && c <= nine { // beginning of 1234.5
		s.step = state1
		return scanBeginLiteral
	}
	return makeScanError(s, c, beginningValue, space, curlOpen, squareOpen, quote, minus, zero, tChr, fChr, nChr, one)
}

// stateBeginStringOrEmpty is the state after reading `{`.
func stateBeginStringOrEmpty(s *scanner, c byte) int {
	//if c <= space && isSpace(c) {
	if c <= space && whitespace&(1<<uint(c)) != 0 {
		return scanSkipSpace
	}
	if c == curlClose {
		n := len(s.parseState)
		s.parseState[n-1] = parseObjectValue
		return stateEndValue(s, c)
	}
	return stateBeginString(s, c)
}

// stateBeginString is the state after reading `{"key": value,`.
func stateBeginString(s *scanner, c byte) int {
	//if c <= space && isWhiteSpaceByte(c) {
	if c <= space && whitespace&(1<<uint(c)) != 0 {
		return scanSkipSpace
	}
	if c == quote {
		s.step = stateInString
		return scanBeginLiteral
	}
	return makeScanError(s, c, beginningObject, quote, space)
}

// stateEndValue is the state after completing a value,
// such as after reading `{}` or `true` or `["x"`.
func stateEndValue(s *scanner, c byte) int {
	n := len(s.parseState)
	if n == 0 {
		// Completed top-level before the current byte.
		s.step = stateEndTop
		s.endTop = true
		return stateEndTop(s, c)
	}
	//if c <= space && isSpace(c) {
	if c <= space && whitespace&(1<<uint(c)) != 0 {
		s.step = stateEndValue
		return scanSkipSpace
	}
	ps := s.parseState[n-1]
	switch ps {
	case parseObjectKey:
		if c == colon {
			s.parseState[n-1] = parseObjectValue
			s.step = stateBeginValue
			return scanObjectKey
		}
		return makeScanError(s, c, "after object key", colon)
	case parseObjectValue:
		if c == comma {
			s.parseState[n-1] = parseObjectKey
			s.step = stateBeginString
			return scanObjectValue
		}
		if c == curlClose {
			popParseState(s)
			return scanEndObject
		}
		return makeScanError(s, c, "after object key:value pair", comma, curlClose)
	case parseArrayValue:
		if c == comma {
			s.step = stateBeginValue
			return scanArrayValue
		}
		if c == squareClose {
			popParseState(s)
			return scanEndArray
		}
		return makeScanError(s, c, "after array element", comma, squareClose)
	}
	return makeScanError(s, c, "stateEndValue unkown")
}

// stateEndTop is the state after finishing the top-level value,
// such as after reading `{}` or `[1,2,3]`.
// Only space characters should be seen now.
func stateEndTop(s *scanner, c byte) int {
	if c != space && c != tab && c != retChar && c != newLine {
		// Complain about non-space byte on next call.
		makeScanError(s, c, "after top-level value", space, tab, newLine)
	}
	return scanEnd
}

// stateInString is the state after reading `"`.
func stateInString(s *scanner, c byte) int {
	if c == quote {
		s.step = stateEndValue
		return scanContinue
	}
	if c == backSlash {
		s.step = stateInStringEsc
		return scanContinue
	}
	if c < 0x20 {
		return makeScanError(s, c, "in string literal")
	}
	return scanContinue
}

// stateInStringEsc is the state after reading `"\` during a isBasic string.
func stateInStringEsc(s *scanner, c byte) int {
	switch c {
	case bChr, fChr, nChr, rChr, tChr, backSlash, slash, quote:
		s.step = stateInString
		return scanContinue
	case uChr:
		s.step = stateInStringEscU
		return scanContinue
	}
	return makeScanError(s, c, "in string escape code")
}

// stateInStringEscU is the state after reading `"\u` during a isBasic string.
func stateInStringEscU(s *scanner, c byte) int {
	if zero <= c && c <= nine || aChr <= c && c <= fChr || bigAChr <= c && c <= fChr {
		s.step = stateInStringEscU1
		return scanContinue
	}
	// numbers
	return makeScanError(s, c, inHexaEscape)
}

// stateInStringEscU1 is the state after reading `"\u1` during a isBasic string.
func stateInStringEscU1(s *scanner, c byte) int {
	if zero <= c && c <= nine || aChr <= c && c <= fChr || bigAChr <= c && c <= fChr {
		s.step = stateInStringEscU12
		return scanContinue
	}
	// numbers
	return makeScanError(s, c, inHexaEscape)
}

// stateInStringEscU12 is the state after reading `"\u12` during a isBasic string.
func stateInStringEscU12(s *scanner, c byte) int {
	if zero <= c && c <= nine || aChr <= c && c <= fChr || bigAChr <= c && c <= fChr {
		s.step = stateInStringEscU123
		return scanContinue
	}
	// numbers
	return makeScanError(s, c, inHexaEscape)
}

// stateInStringEscU123 is the state after reading `"\u123` during a isBasic string.
func stateInStringEscU123(s *scanner, c byte) int {
	if zero <= c && c <= nine || aChr <= c && c <= fChr || bigAChr <= c && c <= fChr {
		s.step = stateInString
		return scanContinue
	}
	// numbers
	return makeScanError(s, c, inHexaEscape)
}

// stateNeg is the state after reading `-` during a number.
func stateNeg(s *scanner, c byte) int {
	if c == zero {
		s.step = state0
		return scanContinue
	}
	if one <= c && c <= nine {
		s.step = state1
		return scanContinue
	}
	return makeScanError(s, c, "in numeric literal")
}

// state1 is the state after reading a non-zero integer during a number,
// such as after reading `1` or `100` but not `0`.
func state1(s *scanner, c byte) int {
	if zero <= c && c <= nine {
		s.step = state1
		return scanContinue
	}
	return state0(s, c)
}

// state0 is the state after reading `0` during a number.
func state0(s *scanner, c byte) int {
	if c == period {
		s.step = stateDot
		return scanContinue
	}
	if c == eChr || c == bigEChr {
		s.step = stateE
		return scanContinue
	}
	return stateEndValue(s, c)
}

// stateDot is the state after reading the integer and decimal point in a number,
// such as after reading `1.`.
func stateDot(s *scanner, c byte) int {
	if zero <= c && c <= nine {
		s.step = stateDot0
		return scanContinue
	}
	return makeScanError(s, c, "after decimal point in numeric literal", zero, nine)
}

// stateDot0 is the state after reading the integer, decimal point, and subsequent
// digits of a number, such as after reading `3.14`.
func stateDot0(s *scanner, c byte) int {
	if zero <= c && c <= nine {
		return scanContinue
	}
	if c == eChr || c == bigEChr {
		s.step = stateE
		return scanContinue
	}
	return stateEndValue(s, c)
}

// stateE is the state after reading the mantissa and e in a number,
// such as after reading `314e` or `0.314e`.
func stateE(s *scanner, c byte) int {
	if c == plus || c == minus {
		s.step = stateESign
		return scanContinue
	}
	return stateESign(s, c)
}

// stateESign is the state after reading the mantissa, e, and sign in a number,
// such as after reading `314e-` or `0.314e+`.
func stateESign(s *scanner, c byte) int {
	if zero <= c && c <= nine {
		s.step = stateE0
		return scanContinue
	}
	return makeScanError(s, c, "in exponent of numeric literal")
}

// stateE0 is the state after reading the mantissa, e, optional sign,
// and at least one digit of the exponent in a number,
// such as after reading `314e-2` or `0.314e+1` or `3.14e0`.
func stateE0(s *scanner, c byte) int {
	if zero <= c && c <= nine {
		return scanContinue
	}
	return stateEndValue(s, c)
}

// stateT is the state after reading `t`.
func stateT(s *scanner, c byte) int {
	if c == rChr {
		s.step = stateTr
		return scanContinue
	}
	return makeScanError(s, c, inLiteral+trueString, rChr)
}

// stateTr is the state after reading `tr`.
func stateTr(s *scanner, c byte) int {
	if c == uChr {
		s.step = stateTru
		return scanContinue
	}
	return makeScanError(s, c, inLiteral+trueString, uChr)
}

// stateTru is the state after reading `tru`.
func stateTru(s *scanner, c byte) int {
	if c == eChr {
		s.step = stateEndValue
		return scanContinue
	}
	return makeScanError(s, c, inLiteral+trueString, eChr)
}

// stateF is the state after reading `f`.
func stateF(s *scanner, c byte) int {
	if c == aChr {
		s.step = stateFa
		return scanContinue
	}
	return makeScanError(s, c, inLiteral+falseString, aChr)
}

// stateFa is the state after reading `fa`.
func stateFa(s *scanner, c byte) int {
	if c == lChr {
		s.step = stateFal
		return scanContinue
	}
	return makeScanError(s, c, inLiteral+falseString, lChr)
}

// stateFal is the state after reading `fal`.
func stateFal(s *scanner, c byte) int {
	if c == sChr {
		s.step = stateFals
		return scanContinue
	}
	return makeScanError(s, c, inLiteral+falseString, sChr)
}

// stateFals is the state after reading `fals`.
func stateFals(s *scanner, c byte) int {
	if c == eChr {
		s.step = stateEndValue
		return scanContinue
	}
	return makeScanError(s, c, inLiteral+falseString, eChr)
}

// stateN is the state after reading `n`.
func stateN(s *scanner, c byte) int {
	if c == uChr {
		s.step = stateNu
		return scanContinue
	}
	return makeScanError(s, c, inLiteral+nullString, uChr)
}

// stateNu is the state after reading `nu`.
func stateNu(s *scanner, c byte) int {
	if c == lChr {
		s.step = stateNul
		return scanContinue
	}
	return makeScanError(s, c, inLiteral+nullString, lChr)
}

// stateNul is the state after reading `nul`.
func stateNul(s *scanner, c byte) int {
	if c == lChr {
		s.step = stateEndValue
		return scanContinue
	}
	return makeScanError(s, c, inLiteral+nullString, lChr)
}

// stateError is the state after reaching a syntax error,
// such as after reading `[1}` or `5.1.2`.
func stateError(_ *scanner, _ byte) int {
	return scanError
}

// stateRedo helps implement the scanner's 1-byte undo.
func stateRedo(s *scanner, _ byte) int {
	s.redo = false
	s.step = s.redoState
	return s.redoCode
}

// checkValid verifies that data is valid JSON-encoded data.
// scan is passed in for use by checkValid to avoid an allocation.
// TODO : "scan is passed in for use by checkValid to avoid an allocation"
func checkValid(s *scanner, data []byte) error {
	scanReset(s)
	for _, c := range data {
		s.bytes++
		if s.step(s, c) == scanError {
			return s.err
		}
	}
	if scanEof(s) == scanError {
		return s.err
	}
	return nil
}

// nextValue splits data after the next whole JSON value,
// returning that value and the bytes that follow it as separate slices.
// scan is passed in for use by nextValue to avoid an allocation.
func nextValue(s *scanner, data []byte) (value, rest []byte, err error) {
	scanReset(s)
	for i, c := range data {
		v := s.step(s, c)
		if v >= scanEndObject {
			switch v {
			// probe the scanner with a space to determine whether we will
			// get scanEnd on the next character. Otherwise, if the next character
			// is not a space, scanEndTop allocates a needless error.
			case scanEndObject, scanEndArray:
				if s.step(s, space) == scanEnd {
					return data[:i+1], data[i+1:], nil
				}
			case scanError:
				return nil, nil, s.err
			case scanEnd:
				return data[:i], data[i:], nil
			}
		}
	}
	if scanEof(s) == scanError {
		return nil, nil, s.err
	}
	return data, nil, nil
}
