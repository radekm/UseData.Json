module UseData.Json.Parser

open System

[<RequireQualifiedAccess>]
type Error =
    | UnexpectedEndOfSpan
    | InvalidUtf8
    | InvalidUtf8EncodingNotMinimal
    | InvalidUtf16
    | InvalidEscapeSequence
    | InvalidHexDigit
    | UnexpectedControlCharacter

exception ParsingException of pos:int * error:Error * info:string
    with
        override me.Message = $"Error %A{me.error} at %d{me.pos}: %s{me.info}"

let raiseParsingException pos code info = raise (ParsingException (pos, code, info))

[<Struct>]
type RawString = { ContentStartPos : int
                   ContentEndPos : int
                   /// Size when converted to .NET string.
                   StringLength : int }

let inline checkUtf8ContByte b pos =
    if b &&& 0b1100_0000uy <> 0b1000_0000uy then
        raiseParsingException pos Error.InvalidUtf8 "Continuation byte is not valid - it does not start with bits 10"

// FIXME In the code which follows we frequently use condition `pos + something >= span.Length`
//       to detect whether we have reached the end of span or there are still some bytes which can be read.
//       Unfortunately this condition doesn't work if `pos` is too big and `pos + something`
//       overflows. In such case `pos + something` will be negative thus be smaller than `span.Length`
//       and our code will fail.

/// Reads 4 hex digits and returns number between 0 and 2^16-1
let inline readUtf16CodeUnitInHex (span : ReadOnlySpan<byte>) (pos : int) =
    let inline digit b pos =
        match b with
        | '0'B -> 0
        | '1'B -> 1
        | '2'B -> 2
        | '3'B -> 3
        | '4'B -> 4
        | '5'B -> 5
        | '6'B -> 6
        | '7'B -> 7
        | '8'B -> 8
        | '9'B -> 9
        | 'A'B | 'a'B -> 10
        | 'B'B | 'b'B -> 11
        | 'C'B | 'c'B -> 12
        | 'D'B | 'd'B -> 13
        | 'E'B | 'e'B -> 14
        | 'F'B | 'f'B -> 15
        | _ -> raiseParsingException pos Error.InvalidHexDigit "Expecting hex digit"

    if pos + 3 >= span.Length then
        raiseParsingException pos Error.UnexpectedEndOfSpan "Expecting 4 hex digits which encode UTF-16 code unit"

    let mutable codeUnit = 0
    for i in 0 .. 3 do
        let pos = pos + i
        codeUnit <- codeUnit * 16 + digit span[pos] pos

    codeUnit

/// The first byte at `span[pos]` must be quote. But it's not checked.
let inline parseString (span : ReadOnlySpan<byte>) (pos : int) : struct (RawString * int) =
    let startPos = pos + 1  // Skip initial quote.
    let mutable stringLength = 0

    let mutable pos = startPos
    let mutable stop = false
    while pos < span.Length && not stop do
        match span[pos] with
        | '"'B ->
            pos <- pos + 1
            stop <- true
        | '\\'B ->
            if pos + 1 >= span.Length then
                raiseParsingException pos Error.UnexpectedEndOfSpan "Escape sequence is not finished"
            pos <- pos + 1  // Processed `\`.
            match span[pos] with
            | '"'B | '\\'B | '/'B | 'b'B | 'f'B | 'n'B | 'r'B | 't'B ->
                pos <- pos + 1  // Processed `\x` of single character `x`.
                stringLength <- stringLength + 1
            | 'u'B ->
                pos <- pos + 1  // Processed `\u`.
                let codeUnit = readUtf16CodeUnitInHex span pos
                // Surrogate pairs are in range from D800 to DFFF.
                // So if `codeUnit` doesn't start with bits 11011
                // then it's not a surrogate pair.
                if codeUnit &&& 0b1111_1000_0000_0000 <> 0b1101_1000_0000_0000 then
                    pos <- pos + 4  // Processed `\uXXXX`.
                    stringLength <- stringLength + 1
                // Surrogate pair.
                else
                    let highSurrogate = codeUnit
                    // High surrogate must start with bits 110110.
                    if highSurrogate &&& 0b1111_1100_0000_0000 <> 0b1101_1000_0000_0000 then
                        raiseParsingException pos Error.InvalidUtf16 "High surrogate is out of range"
                    pos <- pos + 4  // Processed `\uXXXX`.

                    if pos + 5 >= span.Length then
                        raiseParsingException pos Error.UnexpectedEndOfSpan "Expecting hex code of low surrogate"
                    if span[pos] <> '\\'B || span[pos + 1] <> 'u'B then
                        raiseParsingException pos Error.InvalidUtf16 "Low surrogate is malformed"
                    pos <- pos + 2  // Processed `\uXXXX\u`.
                    let lowSurrogate = readUtf16CodeUnitInHex span pos
                    // Low surrogate must start with bits 110111.
                    if lowSurrogate &&& 0b1111_1100_0000_0000 <> 0b1101_1100_0000_0000 then
                        raiseParsingException pos Error.InvalidUtf16 "Low surrogate is out of range"
                    pos <- pos + 4
                    stringLength <- stringLength + 2
            | _ -> raiseParsingException pos Error.InvalidEscapeSequence "Backslash followed by unexpected character"

        // Code point which consists of 1 byte.
        // It has 7 bits.
        | b when b &&& 0b1000_0000uy = 0uy ->
            if b &&& 0b1110_0000uy = 0uy then
                raiseParsingException pos Error.UnexpectedControlCharacter "Control characters must not be in strings"
            pos <- pos + 1
            stringLength <- stringLength + 1
        // Code point which consists of 2 bytes.
        // It has 5 + 6 = 11 bits.
        | b when b &&& 0b1110_0000uy = 0b1100_0000uy ->
            if pos + 1 >= span.Length then
                raiseParsingException pos Error.UnexpectedEndOfSpan "Unfinished code point of size 2"
            pos <- pos + 1
            checkUtf8ContByte span[pos] pos

            // Check that code point encoding is minimal.
            // Ie. code point cannot be encoded in 7 or less bits.
            // Ie. at least one of highest 4 bits must be set.
            if b &&& 0b0001_1110uy = 0uy then
                raiseParsingException pos Error.InvalidUtf8EncodingNotMinimal "Code point can be encoded in 1 byte"

            pos <- pos + 1
            stringLength <- stringLength + 1
        // Code point which consists of 3 bytes.
        // It has 4 + 6 + 6 = 16 bits
        | b when b &&& 0b1111_0000uy = 0b1110_0000uy ->
            if pos + 2 >= span.Length then
                raiseParsingException pos Error.UnexpectedEndOfSpan "Unfinished code point of size 3"
            pos <- pos + 1
            let b2 = span[pos]
            checkUtf8ContByte b2 pos
            pos <- pos + 1
            checkUtf8ContByte span[pos] pos

            // Check that code point encoding is minimal.
            // Ie. code point cannot be encoded in 11 or less bits.
            // Ie. at least one of highest 5 bits must be set.
            if b &&& 0b0000_1111uy = 0uy && b2 &&& 0b0010_0000uy = 0uy then
                raiseParsingException pos Error.InvalidUtf8EncodingNotMinimal
                    "Code point can be encoded in 1 or 2 bytes"

            // Code points from D800 to DFFF are reserved for encoding UTF-16 surrogates
            // and must no be used with UTF-8. Ie. code point must not start with bits 11011.
            if b = 0b1110_1101uy && b2 &&& 0b0010_0000uy = 0b0010_0000uy then
                raiseParsingException pos Error.InvalidUtf8 "Code point is reserved for encoding surrogate pairs"

            pos <- pos + 1
            stringLength <- stringLength + 1
        // Code point which consists of 4 bytes.
        // It has 3 + 6 + 6 + 6 = 21 bits
        | b when b &&& 0b1111_1000uy = 0b1111_0000uy ->
            if pos + 3 >= span.Length then
                raiseParsingException pos Error.UnexpectedEndOfSpan "Unfinished code point of size 3"
            pos <- pos + 1
            let b2 = span[pos]
            checkUtf8ContByte b2 pos
            pos <- pos + 1
            checkUtf8ContByte span[pos] pos
            pos <- pos + 1
            checkUtf8ContByte span[pos] pos

            // Check that code point encoding is minimal.
            // Ie. code point cannot be encoded in 16 or less bits.
            // Ie. at least one of highest 5 bits must be set.
            if b &&& 0b0000_0111uy = 0uy && b2 &&& 0b0011_0000uy = 0uy then
                raiseParsingException pos Error.InvalidUtf8EncodingNotMinimal
                    "Code point can be encoded in 1 or 2 or 3 bytes"

            pos <- pos + 1
            stringLength <- stringLength + 2
        | _ -> raiseParsingException pos Error.InvalidUtf8 "Unexpected start of code point"

    if not stop then
        raiseParsingException pos Error.UnexpectedEndOfSpan "Closing double quote is missing"

    struct ({ ContentStartPos = startPos; ContentEndPos = pos - 1; StringLength = stringLength }, pos)
