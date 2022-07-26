module UseData.Json.Test.Parser

open System
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

    [<Test>]
    let ``ordinary word`` ()  =
        let bytes = Encoding.UTF8.GetBytes "\"Hello world!\""
        let span = ReadOnlySpan bytes
        let struct (raw, pos) = parseString span 0

        Assert.AreEqual(14, pos)
        Assert.AreEqual({ ContentStartPos = 1; ContentEndPos = 13; StringLength = 12 }, raw)
        Assert.AreEqual("Hello world!", decodeUtf16 span raw)

    [<Test>]
    let ``escape sequences`` ()  =
        let bytes = Encoding.UTF8.GetBytes "\"\\\\\\\"\\n\\r\\u0041\""
        let span = ReadOnlySpan bytes
        let struct (raw, pos) = parseString span 0

        Assert.AreEqual(16, pos)
        Assert.AreEqual({ ContentStartPos = 1; ContentEndPos = 15; StringLength = 5 }, raw)
        Assert.AreEqual("\\\"\n\rA", decodeUtf16 span raw)

    [<Test>]
    let ``initial pos greater than 0`` ()  =
        let bytes = Encoding.UTF8.GetBytes "\"bar\" \"foo\""
        let span = ReadOnlySpan bytes
        let struct (raw, pos) = parseString span 6

        Assert.AreEqual(11, pos)
        Assert.AreEqual({ ContentStartPos = 7; ContentEndPos = 10; StringLength = 3 }, raw)
        Assert.AreEqual("foo", decodeUtf16 span raw)

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
