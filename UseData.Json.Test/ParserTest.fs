module UseData.Json.Test.Parser

open System
open System.Collections.Generic
open System.Text

open NUnit.Framework

open UseData.Json.Parser

module BasicString =
    [<Test>]
    let ``empty string`` ()  =
        let bytes = Encoding.UTF8.GetBytes "\"\""
        let span = ReadOnlySpan bytes
        let struct (raw, pos) = parseString span 0

        Assert.AreEqual(2, pos)
        Assert.AreEqual({ ContentStartPos = 1; ContentEndPos = 1; StringLength = 0 }, raw)
        Assert.AreEqual("", decodeUtf16 span raw)

        let bytes' = Array.copy bytes
        Assert.AreEqual(0, decodeUtf8InPlace (Span bytes') raw)
        CollectionAssert.AreEqual(bytes, bytes')

    [<Test>]
    let ``ordinary word`` ()  =
        let bytes = Encoding.UTF8.GetBytes "\"Hello world!\""
        let span = ReadOnlySpan bytes
        let struct (raw, pos) = parseString span 0

        Assert.AreEqual(14, pos)
        Assert.AreEqual({ ContentStartPos = 1; ContentEndPos = 13; StringLength = 12 }, raw)
        Assert.AreEqual("Hello world!", decodeUtf16 span raw)

        let bytes' = Array.copy bytes
        Assert.AreEqual(12, decodeUtf8InPlace (Span bytes') raw)
        CollectionAssert.AreEqual(bytes, bytes')

    [<Test>]
    let ``escape sequences`` ()  =
        let bytes = Encoding.UTF8.GetBytes "\"\\\\\\\"\\n\\r\\u0041\""
        let span = ReadOnlySpan bytes
        let struct (raw, pos) = parseString span 0

        Assert.AreEqual(16, pos)
        Assert.AreEqual({ ContentStartPos = 1; ContentEndPos = 15; StringLength = 5 }, raw)
        Assert.AreEqual("\\\"\n\rA", decodeUtf16 span raw)

        let bytes' = Array.copy bytes
        Assert.AreEqual(5, decodeUtf8InPlace (Span bytes') raw)
        CollectionAssert.AreEqual(
            [| '"'B
               '\\'B; '"'B; '\n'B; '\r'B; 'A'B  // UTF-8 bytes.
               'n'B; '\\'B; 'r'B; '\\'B; 'u'B; '0'B; '0'B; '4'B; '1'B  // Remaining part of JSON string.
               '"'B
            |],
            bytes')

    [<Test>]
    let ``initial pos greater than 0`` ()  =
        let bytes = Encoding.UTF8.GetBytes "\"bar\" \"foo\""
        let span = ReadOnlySpan bytes
        let struct (raw, pos) = parseString span 6

        Assert.AreEqual(11, pos)
        Assert.AreEqual({ ContentStartPos = 7; ContentEndPos = 10; StringLength = 3 }, raw)
        Assert.AreEqual("foo", decodeUtf16 span raw)

        let bytes' = Array.copy bytes
        Assert.AreEqual(3, decodeUtf8InPlace (Span bytes') raw)
        CollectionAssert.AreEqual(bytes, bytes')

    [<Test>]
    let ``string without quote at the end`` ()  =
        let bytes = Encoding.UTF8.GetBytes "\"bar"

        let ex = Assert.Throws<ParsingException>(fun () -> parseString (ReadOnlySpan bytes) 0 |> ignore)
        Assert.AreEqual(4, ex.pos)
        Assert.AreEqual(Error.UnexpectedEndOfSpan, ex.error)

module Utf8 =
    [<Test>]
    let ``any code point except control chars, surrogates, double quote and backslash can be present in strings`` () =
        let buf = Array.zeroCreate 6
        buf[0] <- '"'B
        for codePoint in 32 .. 1_114_111 do
            // Skip code points for surrogates, double quote and backslash.
            if (codePoint < 0xD800 || 0xDFFF < codePoint) && codePoint <> 34 && codePoint <> 92 then
                let rune = Rune(codePoint)
                let byteLength = rune.EncodeToUtf8(Span(buf, 1, 4))
                buf[byteLength + 1] <- '"'B
                let span = ReadOnlySpan buf
                let struct (raw, pos) = parseString span 0
                Assert.AreEqual(byteLength + 2, pos)
                Assert.AreEqual(
                    { ContentStartPos = 1; ContentEndPos = byteLength + 1; StringLength = rune.Utf16SequenceLength },
                    raw)
                Assert.AreEqual(string rune, decodeUtf16 span raw)

                let buf' = Array.copy buf
                Assert.AreEqual(byteLength, decodeUtf8InPlace (Span buf') raw)
                CollectionAssert.AreEqual(buf, buf')

    [<Test>]
    let ``control characters must not be present in strings`` () =
        let buf = [| '\"'B; 0uy; '\"'B |]
        for codePoint in 0 .. 31 do
            Assert.AreEqual(1, Rune(codePoint).EncodeToUtf8(Span(buf, 1, 1)))
            let ex = Assert.Throws<ParsingException>(fun () -> parseString (ReadOnlySpan buf) 0 |> ignore)
            Assert.AreEqual(1, ex.pos)
            Assert.AreEqual(Error.UnexpectedControlCharacter, ex.error)

    [<Test>]
    let ``code points reserved for surrogate pairs must not be present in strings`` () =
        let buf = [| '\"'B; 0uy; 0uy; 0uy; '\"'B |]
        for codePoint in 0xD800 .. 0xDFFF do
            // `Rune` correctly does not allow working with code points reserved for surrogate pairs.
            Assert.Throws<ArgumentOutOfRangeException>(fun () -> Rune(codePoint) |> ignore)
            |> ignore

            // We have to encode these code points manually. They'll be encoded in 3 bytes.
            buf[1] <- 0b1110_0000uy ||| byte (codePoint >>> 12)
            buf[2] <- 0b1000_0000uy ||| byte ((codePoint >>> 6) &&& 0b0011_1111)
            buf[3] <- 0b1000_0000uy ||| byte (codePoint &&& 0b0011_1111)

            let ex = Assert.Throws<ParsingException>(fun () -> parseString (ReadOnlySpan buf) 0 |> ignore)
            Assert.AreEqual(3, ex.pos)  // Last byte of code point.
            Assert.AreEqual(Error.InvalidUtf8, ex.error)

    [<Test>]
    let ``encoding of code points must be minimal`` () =
        // Encoded as 2 bytes even though 1 byte is needed.
        let buf = [| '\"'B; 0uy; 0uy; '\"'B |]
        for codePoint in 0x10 .. 0x7F do
            buf[1] <- 0b1100_0000uy ||| byte (codePoint >>> 6)
            buf[2] <- 0b1000_0000uy ||| byte (codePoint &&& 0b0011_1111)

            let ex = Assert.Throws<ParsingException>(fun () -> parseString (ReadOnlySpan buf) 0 |> ignore)
            Assert.AreEqual(2, ex.pos)  // Last byte of code point.
            Assert.AreEqual(Error.InvalidUtf8EncodingNotMinimal, ex.error)

        // Encoded as 3 bytes even though 1 or 2 bytes are needed.
        let buf = [| '\"'B; 0uy; 0uy; 0uy; '\"'B |]
        for codePoint in 0x10 .. 0x07FF do
            buf[1] <- 0b1110_0000uy ||| byte (codePoint >>> 12)
            buf[2] <- 0b1000_0000uy ||| byte ((codePoint >>> 6) &&& 0b0011_1111)
            buf[3] <- 0b1000_0000uy ||| byte (codePoint &&& 0b0011_1111)

            let ex = Assert.Throws<ParsingException>(fun () -> parseString (ReadOnlySpan buf) 0 |> ignore)
            Assert.AreEqual(3, ex.pos)  // Last byte of code point.
            Assert.AreEqual(Error.InvalidUtf8EncodingNotMinimal, ex.error)

        // Encoded as 4 bytes even though 1 or 2 or 3 bytes are needed.
        let buf = [| '\"'B; 0b1111_0000uy; 0uy; 0uy; 0uy; '\"'B |]
        for codePoint in 0x10 .. 0xFFFF do
            // First byte `buf[1]` will be constant.
            buf[2] <- 0b1000_0000uy ||| byte ((codePoint >>> 12) &&& 0b0011_1111)
            buf[3] <- 0b1000_0000uy ||| byte ((codePoint >>> 6) &&& 0b0011_1111)
            buf[4] <- 0b1000_0000uy ||| byte (codePoint &&& 0b0011_1111)

            let ex = Assert.Throws<ParsingException>(fun () -> parseString (ReadOnlySpan buf) 0 |> ignore)
            Assert.AreEqual(4, ex.pos)  // Last byte of code point.
            Assert.AreEqual(Error.InvalidUtf8EncodingNotMinimal, ex.error)

    [<Test>]
    let ``incomplete code points are rejected`` () =
        let buf = Array.zeroCreate 6
        buf[0] <- '"'B
        // Test every code point which has at least 2 bytes in UTF-8.
        for codePoint in 128 .. 1_114_111 do
            // Skip code points for surrogates.
            if codePoint < 0xD800 || 0xDFFF < codePoint then
                let rune = Rune(codePoint)
                let byteLength = rune.EncodeToUtf8(Span(buf, 1, 4))
                Assert.GreaterOrEqual(byteLength, 2)

                for keepBytes = byteLength - 1 downto 1 do
                    buf[keepBytes + 1] <- '"'B

                    // String ends in the middle of UTF-8 code point.
                    let ex = Assert.Throws<ParsingException>(fun () -> parseString (ReadOnlySpan buf) 0 |> ignore)
                    Assert.AreEqual(keepBytes + 1, ex.pos)  // Double quote.
                    Assert.AreEqual(Error.InvalidUtf8, ex.error)

                    // Span ends in the middle of UTF-8 code point.
                    let ex = Assert.Throws<ParsingException>(fun () ->
                        parseString (ReadOnlySpan(buf, 0, keepBytes + 1)) 0 |> ignore)
                    Assert.AreEqual(1, ex.pos)  // First byte of code point.
                    Assert.AreEqual(Error.UnexpectedEndOfSpan, ex.error)

module Utf16 =
    [<Test>]
    let ``hex digits in unicode escape sequences can be lowercase and uppercase`` () =
        let bytes = Encoding.UTF8.GetBytes "\"\\uABcd\\ueFAa\""
        let span = ReadOnlySpan bytes
        let struct (raw, pos) = parseString span 0

        Assert.AreEqual(14, pos)
        Assert.AreEqual(
            { ContentStartPos = 1; ContentEndPos = 13; StringLength = 2 }, raw)
        Assert.AreEqual("ꯍ", decodeUtf16 span raw)

        let bytes' = Array.copy bytes
        Assert.AreEqual(6, decodeUtf8InPlace (Span bytes') raw)
        CollectionAssert.AreEqual(
            [| '"'B
               0xEAuy; 0xAFuy; 0x8Duy;  // 3 bytes UTF-8 bytes for `\uABCD`.
               0xEEuy; 0xBEuy; 0xAAuy;  // 3 bytes UTF-8 bytes for `\uEFAA`.
               '\\'B; 'u'B; 'e'B; 'F'B; 'A'B; 'a'B  // Remaining part of JSON string.
               '"'B
            |],
            bytes')

    // Create unicode escape sequence `\uXXXX` for char `c`.
    let charToEscapeSequence (c : char) = sprintf "\\u%s" ((int c).ToString "X4")

    [<Test>]
    let ``all code points except those reserved for surrogates can be encoded by Unicode escape sequence`` () =
        let charBuf = [| 'a'; 'b' |]
        for codePoint in 0 .. 1_114_111 do
            // Skip code points for surrogates.
            if codePoint < 0xD800 || 0xDFFF < codePoint then
                let rune = Rune(codePoint)
                let charLength = rune.EncodeToUtf16(Span charBuf)
                let contentStr =
                    if charLength = 1
                    then charToEscapeSequence charBuf[0]
                    else charToEscapeSequence charBuf[0] + charToEscapeSequence charBuf[1]
                let buf = Encoding.UTF8.GetBytes(sprintf "\"%s\"" contentStr)
                let span = ReadOnlySpan buf
                let struct (raw, pos) = parseString span 0

                Assert.AreEqual(charLength * 6 + 2, pos)
                Assert.AreEqual(
                    { ContentStartPos = 1; ContentEndPos = charLength * 6 + 2 - 1; StringLength = charLength }, raw)
                Assert.AreEqual(string rune, decodeUtf16 span raw)

                let buf' = Array.copy buf
                let expectedLength, expectedBuf' =
                    let b = Array.copy buf
                    rune.EncodeToUtf8(Span(b).Slice(1)), b
                Assert.AreEqual(expectedLength, decodeUtf8InPlace (Span buf') raw)
                CollectionAssert.AreEqual(expectedBuf', buf')

    [<Test>]
    let ``incomplete surrogates are rejected`` () =
        for highSurrogate in 0xD800 .. 0xDBFF do
            let spanEndsAfterHighSurrogate = "\"" + charToEscapeSequence (char highSurrogate)
            let buf = Encoding.UTF8.GetBytes spanEndsAfterHighSurrogate
            let ex = Assert.Throws<ParsingException>(fun () -> parseString (ReadOnlySpan buf) 0 |> ignore)
            Assert.AreEqual(7, ex.pos)
            Assert.AreEqual(Error.UnexpectedEndOfSpan, ex.error)

            let stringEndsAfterHighSurrogate = "\"" + charToEscapeSequence (char highSurrogate) + "\""
            let buf = Encoding.UTF8.GetBytes stringEndsAfterHighSurrogate
            let ex = Assert.Throws<ParsingException>(fun () -> parseString (ReadOnlySpan buf) 0 |> ignore)
            Assert.AreEqual(7, ex.pos)
            // Throws `Error.UnexpectedEndOfSpan` instead of `Error.InvalidUtf16` because implementation checks
            // that span has enough bytes for another unicode escape sequence which is not the case.
            Assert.AreEqual(Error.UnexpectedEndOfSpan, ex.error)

            // As described in the above test. We have make the span long enough
            // otherwise the parser immediately know that there cannot be another unicode escape sequence.
            // To make the span long enough we append ` blah blah` after the end of JSON string.
            let stringEndsAfterHighSurrogate = "\"" + charToEscapeSequence (char highSurrogate) + "\" blah blah"
            let buf = Encoding.UTF8.GetBytes stringEndsAfterHighSurrogate
            let ex = Assert.Throws<ParsingException>(fun () -> parseString (ReadOnlySpan buf) 0 |> ignore)
            Assert.AreEqual(7, ex.pos)
            Assert.AreEqual(Error.InvalidUtf16, ex.error)

            // Same as above. The span would be to short without appending additional data
            // after the end of JSON string and if the span is too short to contain another
            // unicode escape sequence parser throws immediately and we're not testing new case
            // but instead repeating the first case from this test.
            let highSurrogateFollowedByNormalChar = "\"" + charToEscapeSequence (char highSurrogate) + "X\" blah blah"
            let buf = Encoding.UTF8.GetBytes highSurrogateFollowedByNormalChar
            let ex = Assert.Throws<ParsingException>(fun () -> parseString (ReadOnlySpan buf) 0 |> ignore)
            Assert.AreEqual(7, ex.pos)
            Assert.AreEqual(Error.InvalidUtf16, ex.error)

            let highSurrogateFollowedByEscapedNormalChar =
                "\"" + charToEscapeSequence (char highSurrogate) + "\\uABCD\""
            let buf = Encoding.UTF8.GetBytes highSurrogateFollowedByEscapedNormalChar
            let ex = Assert.Throws<ParsingException>(fun () -> parseString (ReadOnlySpan buf) 0 |> ignore)
            Assert.AreEqual(9, ex.pos)  // Position where hex digits `ABCD` start. Kind of ad hoc :-/
            Assert.AreEqual(Error.InvalidUtf16, ex.error)

            let highSurrogateFollowedByAnotherHighSurrogate =
                "\"" + charToEscapeSequence (char highSurrogate) + "\\uD800\""
            let buf = Encoding.UTF8.GetBytes highSurrogateFollowedByAnotherHighSurrogate
            let ex = Assert.Throws<ParsingException>(fun () -> parseString (ReadOnlySpan buf) 0 |> ignore)
            Assert.AreEqual(9, ex.pos)  // Position where hex digits `D800` start. Kind of ad hoc :-/
            Assert.AreEqual(Error.InvalidUtf16, ex.error)

    [<Test>]
    let ``low surrogate without preceding high surrogate is rejected`` () =
        for lowSurrogate in 0xDC00 .. 0xDFFF do
            let buf = Encoding.UTF8.GetBytes("\"" + charToEscapeSequence (char lowSurrogate) + "foo\"")
            let ex = Assert.Throws<ParsingException>(fun () -> parseString (ReadOnlySpan buf) 0 |> ignore)
            Assert.AreEqual(3, ex.pos)
            Assert.AreEqual(Error.InvalidUtf16, ex.error)

module BasicNumber =
    [<Test>]
    let ``zero`` () =
        let bytes = Encoding.UTF8.GetBytes "0"
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0

        Assert.AreEqual(1, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 1 }, raw)

    [<Test>]
    let ``zero with zero fractional part`` () =
        let bytes = Encoding.UTF8.GetBytes "0.00"
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0

        Assert.AreEqual(4, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 4 }, raw)

    [<Test>]
    let ``zero with zero exponent`` () =
        let bytes = Encoding.UTF8.GetBytes "0e+0000"
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0

        Assert.AreEqual(7, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 7 }, raw)

    [<Test>]
    let ``zero with zero fractional part and zero exponent`` () =
        let bytes = Encoding.UTF8.GetBytes "0.000e-000"
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0

        Assert.AreEqual(10, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 10 }, raw)

    [<Test>]
    let ``integer with multiple digits`` () =
        let bytes = Encoding.UTF8.GetBytes "123"
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0

        Assert.AreEqual(3, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 3 }, raw)


    [<Test>]
    let ``fractional number`` () =
        let bytes = Encoding.UTF8.GetBytes "3.14"
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0

        Assert.AreEqual(4, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 4 }, raw)

    [<Test>]
    let ``exponent can have optional plus or minus sign`` () =
        let bytes = Encoding.UTF8.GetBytes "12e1"
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0
        Assert.AreEqual(4, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 4 }, raw)

        let bytes = Encoding.UTF8.GetBytes "12e+1"
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0
        Assert.AreEqual(5, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 5 }, raw)

        let bytes = Encoding.UTF8.GetBytes "12e-1"
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0
        Assert.AreEqual(5, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 5 }, raw)

    [<Test>]
    let ``number in middle of span`` () =
        let bytes = Encoding.UTF8.GetBytes "  0.2   "
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 2
        Assert.AreEqual(5, pos)
        Assert.AreEqual({ ContentStartPos = 2; ContentEndPos = 5 }, raw)

    [<Test>]
    let ``character after number is left unparsed`` () =
        let bytes = Encoding.UTF8.GetBytes "1foo"
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0
        Assert.AreEqual(1, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 1 }, raw)

        let bytes = Encoding.UTF8.GetBytes "1.0.13"  // Suffix starting with the second dot is left unparsed.
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0
        Assert.AreEqual(3, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 3 }, raw)

        let bytes = Encoding.UTF8.GetBytes "1e32e4"  // Suffix starting with the second `e` is left unparsed.
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0
        Assert.AreEqual(4, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 4 }, raw)

        // Number in JSON can start only with one zero so remaining two zeros will be left unparsed.
        let bytes = Encoding.UTF8.GetBytes "000"
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0
        Assert.AreEqual(1, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 1 }, raw)

    [<Test>]
    let ``e can be lowercase or uppercase`` () =
        let bytes = Encoding.UTF8.GetBytes "99E1"
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0
        Assert.AreEqual(4, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 4 }, raw)

        let bytes = Encoding.UTF8.GetBytes "99e1"
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0
        Assert.AreEqual(4, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 4 }, raw)

    [<Test>]
    let ``no digits after initial minus is invalid`` () =
        let bytes = Encoding.UTF8.GetBytes "-.12"
        let ex = Assert.Throws<ParsingException>(fun () -> parseNumber (ReadOnlySpan bytes) 0 |> ignore)
        Assert.AreEqual(1, ex.pos)
        Assert.AreEqual(Error.InvalidNumber, ex.error)

        let bytes = Encoding.UTF8.GetBytes "-e1"
        let ex = Assert.Throws<ParsingException>(fun () -> parseNumber (ReadOnlySpan bytes) 0 |> ignore)
        Assert.AreEqual(1, ex.pos)
        Assert.AreEqual(Error.InvalidNumber, ex.error)

    [<Test>]
    let ``initial plus is invalid`` () =
        let bytes = Encoding.UTF8.GetBytes "+1"
        let ex = Assert.Throws<ParsingException>(fun () -> parseNumber (ReadOnlySpan bytes) 0 |> ignore)
        Assert.AreEqual(0, ex.pos)
        Assert.AreEqual(Error.InvalidNumber, ex.error)


    [<Test>]
    let ``only fractional is invalid`` () =
        let bytes = Encoding.UTF8.GetBytes ".4"
        let ex = Assert.Throws<ParsingException>(fun () -> parseNumber (ReadOnlySpan bytes) 0 |> ignore)
        Assert.AreEqual(0, ex.pos)
        Assert.AreEqual(Error.InvalidNumber, ex.error)

    [<Test>]
    let ``only exponent is invalid`` () =
        let bytes = Encoding.UTF8.GetBytes "e1"
        let ex = Assert.Throws<ParsingException>(fun () -> parseNumber (ReadOnlySpan bytes) 0 |> ignore)
        Assert.AreEqual(0, ex.pos)
        Assert.AreEqual(Error.InvalidNumber, ex.error)

    [<Test>]
    let ``long integer`` () =
        let sb = StringBuilder()
        for i in 1 .. 600_000 do
            let digit = char ((i % 10) + int '0')
            sb.Append(digit) |> ignore
        let bytes = Encoding.UTF8.GetBytes(sb.ToString())
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0
        Assert.AreEqual(600_000, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 600_000 }, raw)

    [<Test>]
    let ``huge exponent`` () =
        let bytes = Encoding.UTF8.GetBytes "632e2384932870437589483474385978943"
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0
        Assert.AreEqual(35, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 35 }, raw)

    [<Test>]
    let ``long fractional part`` () =
        let bytes = Encoding.UTF8.GetBytes "37.00343412334432439088978998789483474385978943"
        let struct (raw, pos) = parseNumber (ReadOnlySpan bytes) 0
        Assert.AreEqual(47, pos)
        Assert.AreEqual({ ContentStartPos = 0; ContentEndPos = 47 }, raw)

module BasicValue =
    // This should make tests work even if indentation changes or if newline changes.
    let stripMargin (s : string) =
        let lines = s.Split([| "\n"; "\r\n" |], StringSplitOptions.None)
        let spacesToStrip =
            lines
            |> Seq.choose (fun l ->
                let spaces = l |> Seq.takeWhile ((=) ' ') |> Seq.length
                if l.Length = spaces
                then None
                else Some spaces)
            |> Seq.min
        lines
        |> Array.map (fun l -> l[spacesToStrip..])
        |> String.concat "\n"

    [<Test>]
    let ``object with no fields, one field and several fields`` () =
        let json = stripMargin """
            {
                "number": 1,
                "array": [1, true, null, false, { "x": 3.3, "y": null }],
                "nested object": {
                    "value": "F#"
                },
                "empty object": {}
            }
        """
        let bytes = Encoding.UTF8.GetBytes json
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 4

        Assert.AreEqual(158, pos)
        Assert.AreEqual(
            Object
                { StartPos = 1
                  EndPos = 158
                  Fields = dict [
                      "number", RawNumber { ContentStartPos = 17; ContentEndPos = 18 }
                      "array", Array { StartPos = 33
                                       EndPos = 80
                                       Items =
                                           [| RawNumber { ContentStartPos = 34; ContentEndPos = 35 }
                                              True
                                              Null
                                              False
                                              Object
                                                  { StartPos = 56
                                                    EndPos = 79
                                                    Fields = dict [
                                                        "x", RawNumber { ContentStartPos = 63; ContentEndPos = 66 }
                                                        "y", Null
                                                    ]
                                                  }
                                           |]
                                     }
                      "nested object", Object { StartPos = 103
                                                EndPos = 132
                                                Fields = dict [
                                                    "value", RawString { ContentStartPos = 123
                                                                         ContentEndPos = 125
                                                                         StringLength = 2 }
                                                ]
                                              }
                      "empty object", Object { StartPos = 154; EndPos = 156; Fields = dict [] }
                  ]
                },
            raw)

    [<Test>]
    let ``top-level primitives`` () =
        let bytes = Encoding.UTF8.GetBytes "true"
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(4, pos)
        Assert.AreEqual(True, raw)

        let bytes = Encoding.UTF8.GetBytes "false"
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(5, pos)
        Assert.AreEqual(False, raw)

        let bytes = Encoding.UTF8.GetBytes "null"
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(4, pos)
        Assert.AreEqual(Null, raw)

        let bytes = Encoding.UTF8.GetBytes "17.2e44"
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(7, pos)
        Assert.AreEqual(RawNumber { ContentStartPos = 0; ContentEndPos = 7 }, raw)

        let bytes = Encoding.UTF8.GetBytes "\"hello!\""
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(8, pos)
        Assert.AreEqual(RawString { ContentStartPos = 1; ContentEndPos = 7; StringLength = 6 }, raw)

    [<Test>]
    let ``top-level array`` () =
        let bytes = Encoding.UTF8.GetBytes "[\"first\", \"last\"]"
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 2
        Assert.AreEqual(17, pos)
        Assert.AreEqual(
            Array { StartPos = 0
                    EndPos = 17
                    Items = [| RawString { ContentStartPos = 2; ContentEndPos = 7; StringLength = 5 }
                               RawString { ContentStartPos = 11; ContentEndPos = 15; StringLength = 4 } |] },
            raw)

    [<Test>]
    let ``nesting limit for objects`` () =
        // Following example has nesting 4 - because 4 nested values are parsed
        // (top level object, object at `a`, object at `b`, number at `c`).
        let bytes = Encoding.UTF8.GetBytes """{ "a": { "b": {"c": 1} }  }"""

        // Max nesting 0 fails before the first opening brace.
        let ex = Assert.Throws<ParsingException>(fun () ->
            parseRawValue (ReadOnlySpan bytes) 0 0 |> ignore)
        Assert.AreEqual(0, ex.pos)
        Assert.AreEqual(Error.TooDeepNesting, ex.error)

        // Max nesting 1 fails after the first colon.
        let ex = Assert.Throws<ParsingException>(fun () ->
            parseRawValue (ReadOnlySpan bytes) 0 1 |> ignore)
        Assert.AreEqual(6, ex.pos)
        Assert.AreEqual(Error.TooDeepNesting, ex.error)

        // Max nesting 2 fails after the second colon.
        let ex = Assert.Throws<ParsingException>(fun () ->
            parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(13, ex.pos)
        Assert.AreEqual(Error.TooDeepNesting, ex.error)

        // Max nesting 3 fails after the third colon.
        let ex = Assert.Throws<ParsingException>(fun () ->
            parseRawValue (ReadOnlySpan bytes) 0 3 |> ignore)
        Assert.AreEqual(19, ex.pos)
        Assert.AreEqual(Error.TooDeepNesting, ex.error)

        // Max nesting 4 succeeds.
        let struct (_, pos) = parseRawValue (ReadOnlySpan bytes) 0 4
        Assert.AreEqual(27, pos)

    [<Test>]
    let ``nesting limit for arrays`` () =
        // Following example has nesting 5 - because 5 nested values are parsed.
        let bytes = Encoding.UTF8.GetBytes """[1.2, [], [[[]]], [[[0]]], []]"""

        // Max nesting 0 fails before the first opening bracket.
        let ex = Assert.Throws<ParsingException>(fun () ->
            parseRawValue (ReadOnlySpan bytes) 0 0 |> ignore)
        Assert.AreEqual(0, ex.pos)
        Assert.AreEqual(Error.TooDeepNesting, ex.error)

        // Max nesting 1 fails after the first opening bracket.
        let ex = Assert.Throws<ParsingException>(fun () ->
            parseRawValue (ReadOnlySpan bytes) 0 1 |> ignore)
        Assert.AreEqual(1, ex.pos)
        Assert.AreEqual(Error.TooDeepNesting, ex.error)

        // Max nesting 2 fails after parsing `[1.2, [], [`
        // because there's a value `[[]]` inside the array.
        // It doesn't fail after parsing `[1.2, [` because the array is empty,
        // so there's no need to parse another nested value.
        let ex = Assert.Throws<ParsingException>(fun () ->
            parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(11, ex.pos)
        Assert.AreEqual(Error.TooDeepNesting, ex.error)

        // Max nesting 3 fails after parsing `[1.2, [], [[`
        // because there's a value `[]` inside the array.
        let ex = Assert.Throws<ParsingException>(fun () ->
            parseRawValue (ReadOnlySpan bytes) 0 3 |> ignore)
        Assert.AreEqual(12, ex.pos)
        Assert.AreEqual(Error.TooDeepNesting, ex.error)

        // Max nesting 4 fails after parsing `[1.2, [], [[[]]], [[[`
        // because there's a value `0` inside the array.
        // It doesn't fail after parsing `[1.2, [], [[[` because the array is empty,
        // so there's no need to parse another nested value.
        let ex = Assert.Throws<ParsingException>(fun () ->
            parseRawValue (ReadOnlySpan bytes) 0 4 |> ignore)
        Assert.AreEqual(21, ex.pos)
        Assert.AreEqual(Error.TooDeepNesting, ex.error)

        // Max nesting 5 succeeds.
        let struct (_, pos) = parseRawValue (ReadOnlySpan bytes) 0 5
        Assert.AreEqual(30, pos)

    [<Test>]
    let ``nesting limit for objects and arrays`` () =
        // Following example has nesting 3 - because 3 nested values are parsed.
        let bytes = Encoding.UTF8.GetBytes """[{}, {"x": 1}]"""

        // Max nesting 0 fails before the first opening bracket.
        let ex = Assert.Throws<ParsingException>(fun () ->
            parseRawValue (ReadOnlySpan bytes) 0 0 |> ignore)
        Assert.AreEqual(0, ex.pos)
        Assert.AreEqual(Error.TooDeepNesting, ex.error)

        // Max nesting 1 fails after the first opening bracket.
        let ex = Assert.Throws<ParsingException>(fun () ->
            parseRawValue (ReadOnlySpan bytes) 0 1 |> ignore)
        Assert.AreEqual(1, ex.pos)
        Assert.AreEqual(Error.TooDeepNesting, ex.error)

        // Max nesting 2 fails after the first colon, ie. after parsing `[{}, {"x":`.
        let ex = Assert.Throws<ParsingException>(fun () ->
            parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(10, ex.pos)
        Assert.AreEqual(Error.TooDeepNesting, ex.error)

        // Max nesting 3 succeeds.
        let struct (_, pos) = parseRawValue (ReadOnlySpan bytes) 0 3
        Assert.AreEqual(14, pos)

    [<Test>]
    let ``spacing in objects`` () =
        // Empty object.
        let bytes = Encoding.UTF8.GetBytes "{}"
        let expected = Object { StartPos = 0; EndPos = 2; Fields = dict [] }
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(2, pos)
        Assert.AreEqual(expected, raw)
        let bytes = Encoding.UTF8.GetBytes "  {  }  "
        let expected = Object { StartPos = 2; EndPos = 6; Fields = dict [] }
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(6, pos)
        Assert.AreEqual(expected, raw)

        // Object with one field.
        let bytes = Encoding.UTF8.GetBytes """{"a":1e2}"""
        let expected = Object { StartPos = 0
                                EndPos = 9
                                Fields = dict ["a", RawNumber { ContentStartPos = 5; ContentEndPos = 8 }] }
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 2
        Assert.AreEqual(9, pos)
        Assert.AreEqual(expected, raw)
        let bytes = Encoding.UTF8.GetBytes  """  {  "a"  :  1e2  }  """
        let expected = Object { StartPos = 2
                                EndPos = 19
                                Fields = dict ["a", RawNumber { ContentStartPos = 13; ContentEndPos = 16 }] }
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 2
        Assert.AreEqual(19, pos)
        Assert.AreEqual(expected, raw)

        // Object with two fields.
        let bytes = Encoding.UTF8.GetBytes """{"a":1,"b":""}"""
        let expected =
            Object { StartPos = 0
                     EndPos = 14
                     Fields = dict ["a", RawNumber { ContentStartPos = 5; ContentEndPos = 6 }
                                    "b", RawString { ContentStartPos = 12; ContentEndPos = 12; StringLength = 0 } ] }
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 2
        Assert.AreEqual(14, pos)
        Assert.AreEqual(expected, raw)
        let bytes = Encoding.UTF8.GetBytes """  {  "a"  :  1  ,  "b"  :  ""  }  """
        let expected =
            Object { StartPos = 2
                     EndPos = 32
                     Fields = dict ["a", RawNumber { ContentStartPos = 13; ContentEndPos = 14 }
                                    "b", RawString { ContentStartPos = 28; ContentEndPos = 28; StringLength = 0 } ] }
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 2
        Assert.AreEqual(32, pos)
        Assert.AreEqual(expected, raw)

    [<Test>]
    let ``spacing in arrays`` () =
        // Empty array.
        let bytes = Encoding.UTF8.GetBytes "[]"
        let expected = Array { StartPos = 0; EndPos = 2; Items = [||] }
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(2, pos)
        Assert.AreEqual(expected, raw)
        let bytes = Encoding.UTF8.GetBytes "  [  ]  "
        let expected = Array { StartPos = 2; EndPos = 6; Items = [||] }
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(6, pos)
        Assert.AreEqual(expected, raw)

        // Array with one item.
        let bytes = Encoding.UTF8.GetBytes "[-1]"
        let expected = Array { StartPos = 0
                               EndPos = 4
                               Items = [| RawNumber { ContentStartPos = 1; ContentEndPos = 3 } |] }
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 2
        Assert.AreEqual(4, pos)
        Assert.AreEqual(expected, raw)
        let bytes = Encoding.UTF8.GetBytes "  [  -1  ]  "
        let expected = Array { StartPos = 2
                               EndPos = 10
                               Items = [| RawNumber { ContentStartPos = 5; ContentEndPos = 7 } |] }
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 2
        Assert.AreEqual(10, pos)
        Assert.AreEqual(expected, raw)

        // Array with two items.
        let bytes = Encoding.UTF8.GetBytes "[-1,-3]"
        let expected = Array { StartPos = 0
                               EndPos = 7
                               Items = [| RawNumber { ContentStartPos = 1; ContentEndPos = 3 }
                                          RawNumber { ContentStartPos = 4; ContentEndPos = 6 } |] }
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 2
        Assert.AreEqual(7, pos)
        Assert.AreEqual(expected, raw)
        let bytes = Encoding.UTF8.GetBytes "  [  -1  ,  -3  ]  "
        let expected = Array { StartPos = 2
                               EndPos = 17
                               Items = [| RawNumber { ContentStartPos = 5; ContentEndPos = 7 }
                                          RawNumber { ContentStartPos = 12; ContentEndPos = 14 } |] }
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 2
        Assert.AreEqual(17, pos)
        Assert.AreEqual(expected, raw)

    [<Test>]
    let ``spacing around primitive values`` () =
        // Strings.
        let bytes = Encoding.UTF8.GetBytes "\"a\""
        let expected = RawString { ContentStartPos = 1; ContentEndPos = 2; StringLength = 1 }
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(3, pos)
        Assert.AreEqual(expected, raw)
        let bytes = Encoding.UTF8.GetBytes "  \"a\"  "
        let expected = RawString { ContentStartPos = 3; ContentEndPos = 4; StringLength = 1 }
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(5, pos)
        Assert.AreEqual(expected, raw)

        // Numbers.
        let bytes = Encoding.UTF8.GetBytes "1"
        let expected = RawNumber { ContentStartPos = 0; ContentEndPos = 1 }
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(1, pos)
        Assert.AreEqual(expected, raw)
        let bytes = Encoding.UTF8.GetBytes "  1  "
        let expected = RawNumber { ContentStartPos = 2; ContentEndPos = 3 }
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(3, pos)
        Assert.AreEqual(expected, raw)

        // True.
        let bytes = Encoding.UTF8.GetBytes "true"
        let expected = True
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(4, pos)
        Assert.AreEqual(expected, raw)
        let bytes = Encoding.UTF8.GetBytes "  true  "
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(6, pos)
        Assert.AreEqual(expected, raw)

        // False.
        let bytes = Encoding.UTF8.GetBytes "false"
        let expected = False
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(5, pos)
        Assert.AreEqual(expected, raw)
        let bytes = Encoding.UTF8.GetBytes "  false  "
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(7, pos)
        Assert.AreEqual(expected, raw)

        // True.
        let bytes = Encoding.UTF8.GetBytes "null"
        let expected = Null
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(4, pos)
        Assert.AreEqual(expected, raw)
        let bytes = Encoding.UTF8.GetBytes "  null  "
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(6, pos)
        Assert.AreEqual(expected, raw)

    [<Test>]
    let ``duplicate fields are not allowed`` () =
        // Duplicate field is rejected even if value is same.
        let bytes = Encoding.UTF8.GetBytes """{ "a": 1, "a": 1 }"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(16, ex.pos)  // Position after parsing associated value.
        Assert.AreEqual(Error.DuplicateField, ex.error)

        // Field is duplicate if code points after decoding are same.
        let bytes = Encoding.UTF8.GetBytes """{ "a": 1, "\u0061": 1 }"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(21, ex.pos)  // Position after parsing associated value.
        Assert.AreEqual(Error.DuplicateField, ex.error)

        // If we have same characters but code points differ then it's allowed.
        // The first field is `Latin Small Letter C with Caron`.
        // The second field is `Latin Small Letter C` followed by `Combining Caron`.
        let bytes = Encoding.UTF8.GetBytes """{ "č": 1, "č": 1 }"""
        let struct (_, pos) = parseRawValue (ReadOnlySpan bytes) 0 2
        // Position is 21 because the first field name has 2 bytes (C4 8D)
        // and the second field name has 3 bytes (63 CC 8C).
        Assert.AreEqual(21, pos)

    [<Test>]
    let ``valid value can be followed by another value`` () =
        let bytes = Encoding.UTF8.GetBytes """001e2{"x":1.2}nulltrue133.6e-5false"hello"  []{}"""
        let expected = [
            RawNumber { ContentStartPos = 0; ContentEndPos = 1 }  // 0
            RawNumber { ContentStartPos = 1; ContentEndPos = 2 }  // 0
            RawNumber { ContentStartPos = 2; ContentEndPos = 5 }  // 1e2
            Object { StartPos = 5
                     EndPos = 14
                     Fields = dict [ "x", RawNumber { ContentStartPos = 10; ContentEndPos = 13 } ] }  // {"x":1.2}
            Null
            True
            RawNumber { ContentStartPos = 22; ContentEndPos = 30 }  // 133.6e-5
            False
            RawString { ContentStartPos = 36; ContentEndPos = 41; StringLength = 5 }  // "hello"
            Array { StartPos = 44; EndPos = 46; Items = [||] }
            Object { StartPos = 46; EndPos = 48; Fields = dict [] }
        ]
        let actual = ResizeArray()

        let mutable pos = 0
        let span = ReadOnlySpan bytes
        while pos < span.Length do
            let struct (raw, endPos) = parseRawValue span pos 2
            actual.Add raw
            pos <- skipWhitespace span endPos  // Skip optional whitespace between values.

        CollectionAssert.AreEqual(expected, actual)

    [<Test>]
    let ``valid value can be followed by garbage`` () =
        let bytes = Encoding.UTF8.GetBytes "{}garbage"
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(2, pos)
        Assert.AreEqual(Object { StartPos = 0; EndPos = 2; Fields = dict [] }, raw)

        let bytes = Encoding.UTF8.GetBytes "[]garbage"
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(2, pos)
        Assert.AreEqual(Array { StartPos = 0; EndPos = 2; Items = [||] }, raw)

        let bytes = Encoding.UTF8.GetBytes "\"hi\"garbage"
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(4, pos)
        Assert.AreEqual(RawString { ContentStartPos = 1; ContentEndPos = 3; StringLength = 2 }, raw)

        let bytes = Encoding.UTF8.GetBytes "1garbage"
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(1, pos)
        Assert.AreEqual(RawNumber { ContentStartPos = 0; ContentEndPos = 1 }, raw)

        let bytes = Encoding.UTF8.GetBytes "1e2e3"
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(3, pos)
        Assert.AreEqual(RawNumber { ContentStartPos = 0; ContentEndPos = 3 }, raw)

        let bytes = Encoding.UTF8.GetBytes "1.2.4e2"
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(3, pos)
        Assert.AreEqual(RawNumber { ContentStartPos = 0; ContentEndPos = 3 }, raw)

        let bytes = Encoding.UTF8.GetBytes "truegarbage"
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(4, pos)
        Assert.AreEqual(True, raw)

        let bytes = Encoding.UTF8.GetBytes "falsegarbage"
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(5, pos)
        Assert.AreEqual(False, raw)

        let bytes = Encoding.UTF8.GetBytes "nullgarbage"
        let struct (raw, pos) = parseRawValue (ReadOnlySpan bytes) 0 1
        Assert.AreEqual(4, pos)
        Assert.AreEqual(Null, raw)

    [<Test>]
    let ``unfinished primitive`` () =
        let bytes = Encoding.UTF8.GetBytes """"Missing end quote"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 1 |> ignore)
        Assert.AreEqual(18, ex.pos)
        Assert.AreEqual(Error.UnexpectedEndOfSpan, ex.error)

        let bytes = Encoding.UTF8.GetBytes "1e"
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 1 |> ignore)
        Assert.AreEqual(2, ex.pos)
        Assert.AreEqual(Error.UnexpectedEndOfSpan, ex.error)

        let bytes = Encoding.UTF8.GetBytes "  0.  "
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 1 |> ignore)
        Assert.AreEqual(4, ex.pos)
        Assert.AreEqual(Error.InvalidNumber, ex.error)

        let bytes = Encoding.UTF8.GetBytes "77.9e"
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 1 |> ignore)
        Assert.AreEqual(5, ex.pos)
        Assert.AreEqual(Error.UnexpectedEndOfSpan, ex.error)

        let bytes = Encoding.UTF8.GetBytes "tru"
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 1 |> ignore)
        // Position is 0 because parser checks if there's enough bytes immediately after seeing initial `t`.
        Assert.AreEqual(0, ex.pos)
        Assert.AreEqual(Error.UnexpectedEndOfSpan, ex.error)

        // We need at least 3 spaces after `t` so parser sees there's enough bytes.
        let bytes = Encoding.UTF8.GetBytes "  t   "
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 1 |> ignore)
        Assert.AreEqual(2, ex.pos)
        Assert.AreEqual(Error.InvalidValue, ex.error)

        let bytes = Encoding.UTF8.GetBytes "f"
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 1 |> ignore)
        Assert.AreEqual(0, ex.pos)
        Assert.AreEqual(Error.UnexpectedEndOfSpan, ex.error)

        let bytes = Encoding.UTF8.GetBytes "  fals  "
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 1 |> ignore)
        Assert.AreEqual(2, ex.pos)
        Assert.AreEqual(Error.InvalidValue, ex.error)

        let bytes = Encoding.UTF8.GetBytes "n"
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 1 |> ignore)
        Assert.AreEqual(0, ex.pos)
        Assert.AreEqual(Error.UnexpectedEndOfSpan, ex.error)

        let bytes = Encoding.UTF8.GetBytes " nul  "
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 1 |> ignore)
        Assert.AreEqual(1, ex.pos)
        Assert.AreEqual(Error.InvalidValue, ex.error)

    [<Test>]
    let ``invalid object`` () =
        let bytes = Encoding.UTF8.GetBytes """{"a"}"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(4, ex.pos)
        Assert.AreEqual(Error.InvalidObject, ex.error)

        let bytes = Encoding.UTF8.GetBytes """{"a":}"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(5, ex.pos)
        // Because the value is expected and instead the closing brace is found.
        Assert.AreEqual(Error.InvalidValue, ex.error)

        let bytes = Encoding.UTF8.GetBytes """{"a":,}"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(5, ex.pos)
        // Because the value is expected and instead the comma is found.
        Assert.AreEqual(Error.InvalidValue, ex.error)

        let bytes = Encoding.UTF8.GetBytes """{"a":: 1}"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(5, ex.pos)
        // Because the value is expected and instead the colon is found.
        Assert.AreEqual(Error.InvalidValue, ex.error)

        let bytes = Encoding.UTF8.GetBytes """{"a" ,}"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(5, ex.pos)
        Assert.AreEqual(Error.InvalidObject, ex.error)

        let bytes = Encoding.UTF8.GetBytes """{:}"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(1, ex.pos)
        Assert.AreEqual(Error.InvalidObject, ex.error)

        let bytes = Encoding.UTF8.GetBytes """{,}"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(1, ex.pos)
        Assert.AreEqual(Error.InvalidObject, ex.error)

        let bytes = Encoding.UTF8.GetBytes """{true: false}"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(1, ex.pos)
        Assert.AreEqual(Error.InvalidObject, ex.error)

        let bytes = Encoding.UTF8.GetBytes """{1: 2}"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(1, ex.pos)
        Assert.AreEqual(Error.InvalidObject, ex.error)

        let bytes = Encoding.UTF8.GetBytes """{"a": """
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(6, ex.pos)
        Assert.AreEqual(Error.UnexpectedEndOfSpan, ex.error)

        let bytes = Encoding.UTF8.GetBytes """{"a": 1"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(7, ex.pos)
        Assert.AreEqual(Error.UnexpectedEndOfSpan, ex.error)

        let bytes = Encoding.UTF8.GetBytes """{"a": 1,"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(8, ex.pos)
        Assert.AreEqual(Error.UnexpectedEndOfSpan, ex.error)

        let bytes = Encoding.UTF8.GetBytes """{"a": 1 "b": 2}"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(8, ex.pos)
        Assert.AreEqual(Error.InvalidObject, ex.error)

        let bytes = Encoding.UTF8.GetBytes """{"a": 1,, "b": 2}"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(8, ex.pos)
        Assert.AreEqual(Error.InvalidObject, ex.error)

        let bytes = Encoding.UTF8.GetBytes """{"a": 1 ]"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(8, ex.pos)
        Assert.AreEqual(Error.InvalidObject, ex.error)

    [<Test>]
    let ``invalid array`` () =
        let bytes = Encoding.UTF8.GetBytes """[1, ]"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(4, ex.pos)
        Assert.AreEqual(Error.InvalidValue, ex.error)

        let bytes = Encoding.UTF8.GetBytes """[1 2]"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(3, ex.pos)
        Assert.AreEqual(Error.InvalidArray, ex.error)

        let bytes = Encoding.UTF8.GetBytes """[1, 2,,]"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(6, ex.pos)
        Assert.AreEqual(Error.InvalidValue, ex.error)

        let bytes = Encoding.UTF8.GetBytes """[1, """
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(4, ex.pos)
        Assert.AreEqual(Error.UnexpectedEndOfSpan, ex.error)

        let bytes = Encoding.UTF8.GetBytes """[1, 2}"""
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(5, ex.pos)
        Assert.AreEqual(Error.InvalidArray, ex.error)

    [<Test>]
    let ``invalid top-level byte`` () =
        let bytes = Encoding.UTF8.GetBytes "}"
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(0, ex.pos)
        Assert.AreEqual(Error.InvalidValue, ex.error)

        let bytes = Encoding.UTF8.GetBytes "]"
        let ex = Assert.Throws<ParsingException>(fun () -> parseRawValue (ReadOnlySpan bytes) 0 2 |> ignore)
        Assert.AreEqual(0, ex.pos)
        Assert.AreEqual(Error.InvalidValue, ex.error)
