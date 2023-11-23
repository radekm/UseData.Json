module UseData.Json.Parser

open System
open System.Collections.Generic
open System.Text

[<RequireQualifiedAccess>]
type Error =
    | UnexpectedEndOfSpan
    | InvalidUtf8
    | InvalidUtf8EncodingNotMinimal
    | InvalidUtf16
    | InvalidEscapeSequence
    | InvalidHexDigit
    | UnexpectedControlCharacter
    | InvalidNumber
    | TooDeepNesting
    | DuplicateField
    | InvalidObject
    | InvalidArray
    | InvalidValue

exception ParsingException of pos:int * error:Error * info:string
    with
        override me.Message = $"Error %A{me.error} at %d{me.pos}: %s{me.info}"

let raiseParsingException pos code info = raise (ParsingException (pos, code, info))

[<Struct; NoComparison>]
type RawString = { /// Start position without opening quote.
                   ContentStartPos : int
                   /// End position without closing quote.
                   ContentEndPos : int
                   /// Length of JSON string after conversion to .NET string (from UTF-8 to UTF-16).
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
        // It has 4 + 6 + 6 = 16 bits.
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
        // It has 3 + 6 + 6 + 6 = 21 bits.
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

/// Assumes that JSON string is valid.
let decodeUtf16 (span : ReadOnlySpan<byte>) (rawString : RawString) : string =
    if rawString.StringLength = 0
    then String.Empty
    else
        let sb = StringBuilder(rawString.StringLength, rawString.StringLength)
        let mutable pos = rawString.ContentStartPos
        while pos < rawString.ContentEndPos do
            match span[pos] with
            | '\\'B ->
                pos <- pos + 1  // Processed `\`.
                match span[pos] with
                | '"'B | '\\'B | '/'B ->
                    sb.Append(char span[pos]) |> ignore
                    pos <- pos + 1  // Processed `\"` or `\\` or `\/`.
                | 'b'B ->
                    sb.Append('\b') |> ignore
                    pos <- pos + 1  // Processed `\b`.
                | 'f'B ->
                    sb.Append('\f') |> ignore
                    pos <- pos + 1  // Processed `\f`.
                | 'n'B ->
                    sb.Append('\n') |> ignore
                    pos <- pos + 1  // Processed `\n`.
                | 'r'B ->
                    sb.Append('\r') |> ignore
                    pos <- pos + 1  // Processed `\r`.
                | 't'B ->
                    sb.Append('\t') |> ignore
                    pos <- pos + 1  // Processed `\t`.
                | 'u'B ->
                    pos <- pos + 1  // Processed `\u`.
                    let codeUnit = readUtf16CodeUnitInHex span pos
                    sb.Append(char codeUnit) |> ignore
                    pos <- pos + 4
                | _ -> failwith "Not valid JSON string: Backslash followed by unexpected character"

            // Code point which consists of 1 byte.
            // It has 7 bits.
            | b when b &&& 0b1000_0000uy = 0uy ->
                sb.Append(char b) |> ignore
                pos <- pos + 1
            // Code point which consists of 2 bytes.
            // It has 5 + 6 = 11 bits.
            | b when b &&& 0b1110_0000uy = 0b1100_0000uy ->
                let b2 = span[pos + 1]
                let mutable codePoint = int b &&& 0b0001_1111
                codePoint <- (codePoint <<< 6) ||| (int b2 &&& 0b0011_1111)
                sb.Append(char codePoint) |> ignore
                pos <- pos + 2
            // Code point which consists of 3 bytes.
            // It has 4 + 6 + 6 = 16 bits.
            | b when b &&& 0b1111_0000uy = 0b1110_0000uy ->
                let b2 = span[pos + 1]
                let b3 = span[pos + 2]
                let mutable codePoint = int b &&& 0b0000_1111
                codePoint <- (codePoint <<< 6) ||| (int b2 &&& 0b0011_1111)
                codePoint <- (codePoint <<< 6) ||| (int b3 &&& 0b0011_1111)
                sb.Append(char codePoint) |> ignore
                pos <- pos + 3
            // Code point which consists of 4 bytes.
            // It has 3 + 6 + 6 + 6 = 21 bits.
            | b when b &&& 0b1111_1000uy = 0b1111_0000uy ->
                let b2 = span[pos + 1]
                let b3 = span[pos + 2]
                let b4 = span[pos + 3]
                let mutable codePoint = int b &&& 0b0000_1111
                codePoint <- (codePoint <<< 6) ||| (int b2 &&& 0b0011_1111)
                codePoint <- (codePoint <<< 6) ||| (int b3 &&& 0b0011_1111)
                codePoint <- (codePoint <<< 6) ||| (int b4 &&& 0b0011_1111)
                // We know that `codePoint` cannot be encoded in 16 bits so it must be at least `0x10000`.
                let x = codePoint - 0x10000
                0xD800 + (x >>> 10) |> char |> sb.Append |> ignore  // Higher 10 bits of `x`.
                0xDC00 + (x &&& 0x3FF) |> char |> sb.Append |> ignore  // Lower 10 bits of `x`.
                pos <- pos + 4
            | _ -> failwith "Not valid JSON string: Unexpected start of code point"
        sb.ToString()

/// Assumes that JSON string is valid.
let decodeUtf8InPlace (span : Span<byte>) (rawString : RawString) : int =
    let mutable readPos = rawString.ContentStartPos
    let mutable writePos = rawString.ContentStartPos
    while readPos < rawString.ContentEndPos do
        match span[readPos] with
        | '\\'B ->
            readPos <- readPos + 1  // Processed `\`.
            match span[readPos] with
            | '"'B | '\\'B | '/'B ->
                span[writePos] <- span[readPos]
                readPos <- readPos + 1  // Processed `\"` or `\\` or `\/`.
                writePos <- writePos + 1
            | 'b'B ->
                span[writePos] <- '\b'B
                readPos <- readPos + 1  // Processed `\b`.
                writePos <- writePos + 1
            | 'f'B ->
                span[writePos] <- '\f'B
                readPos <- readPos + 1  // Processed `\f`.
                writePos <- writePos + 1
            | 'n'B ->
                span[writePos] <- '\n'B
                readPos <- readPos + 1  // Processed `\n`.
                writePos <- writePos + 1
            | 'r'B ->
                span[writePos] <- '\r'B
                readPos <- readPos + 1  // Processed `\r`.
                writePos <- writePos + 1
            | 't'B ->
                span[writePos] <- '\t'B
                readPos <- readPos + 1  // Processed `\t`.
                writePos <- writePos + 1
            | 'u'B ->
                readPos <- readPos + 1  // Processed `\u`.
                let codeUnit = readUtf16CodeUnitInHex (Span.op_Implicit span) readPos
                readPos <- readPos + 4  // Processed `\uXXXX`.

                // Surrogate pairs are in range from D800 to DFFF.
                // So if `codeUnit` doesn't start with bits 11011
                // then it's not a surrogate pair and it will be ransformed into 1 or 2 or 3 UTF-8 bytes.
                if codeUnit &&& 0b1111_1000_0000_0000 <> 0b1101_1000_0000_0000 then
                    let codePoint = codeUnit
                    // Code point needs 0-7 bits.
                    if codePoint <= 0x007F then
                        span[writePos] <- byte codePoint
                        writePos <- writePos + 1
                    // Code point needs 8-11 bits.
                    elif codePoint <= 0x07FF then
                        span[writePos] <- byte (0b1100_0000 ||| (codePoint >>> 6))
                        span[writePos + 1] <- byte (0b1000_0000 ||| (codePoint &&& 0b0011_1111))
                        writePos <- writePos + 2
                    // Code point needs 12-16 bits.
                    else
                        span[writePos] <- byte (0b1110_0000 ||| (codePoint >>> 12))
                        span[writePos + 1] <- byte (0b1000_0000 ||| ((codePoint >>> 6) &&& 0b0011_1111))
                        span[writePos + 2] <- byte (0b1000_0000 ||| (codePoint &&& 0b0011_1111))
                        writePos <- writePos + 3
                // Surrogate pair will be transformed into 4 UTF-8 bytes.
                else
                    let highSurrogate = codeUnit
                    // We know that another `\uXXXX` follows.
                    readPos <- readPos + 2  // Skip `\u`.
                    let lowSurrogate = readUtf16CodeUnitInHex (Span.op_Implicit span) readPos
                    readPos <- readPos + 4

                    // Concatenate lower 10 bits from each surrogate and add 0x10000.
                    // Resulting code point needs 17-21 bits.
                    let codePoint = (((highSurrogate &&& 0x3FF) <<< 10) ||| (lowSurrogate &&& 0x3FF)) + 0x10000

                    span[writePos] <- byte (0b1111_0000 ||| (codePoint >>> 18))
                    span[writePos + 1] <- byte (0b1000_0000 ||| ((codePoint >>> 12) &&& 0b0011_1111))
                    span[writePos + 2] <- byte (0b1000_0000 ||| ((codePoint >>> 6) &&& 0b0011_1111))
                    span[writePos + 3] <- byte (0b1000_0000 ||| (codePoint &&& 0b0011_1111))
                    writePos <- writePos + 4
            | _ -> failwith "Not valid JSON string: Backslash followed by unexpected character"
        // Copy UTF-8 byte to output.
        | b ->
            span[writePos] <- b
            readPos <- readPos + 1
            writePos <- writePos + 1
    writePos - rawString.ContentStartPos

[<Struct; NoComparison>]
type RawNumber = { ContentStartPos : int
                   ContentEndPos : int }

let inline skipDigits (span : ReadOnlySpan<byte>) (pos : int) : int =
    let mutable pos = pos

    let mutable stop = false
    while pos < span.Length && not stop do
        match span[pos] with
        | '0'B | '1'B | '2'B | '3'B | '4'B | '5'B | '6'B | '7'B | '8'B | '9'B -> pos <- pos + 1  // Skip digit.
        | _ -> stop <- true

    pos

let inline skipAtLeastOneDigit (span : ReadOnlySpan<byte>) (startPos : int) =
    let pos = skipDigits span startPos
    if pos = startPos
    then
        if pos >= span.Length
        then raiseParsingException pos Error.UnexpectedEndOfSpan "Digit expected"
        else raiseParsingException pos Error.InvalidNumber "Digit expected"
    else pos

let inline skipExponent (span : ReadOnlySpan<byte>) (pos : int) =
    if pos < span.Length then
        match span[pos] with
        | 'E'B | 'e'B ->
            let mutable pos = pos + 1
            if pos >= span.Length then
                raiseParsingException pos Error.UnexpectedEndOfSpan "Exponent must have at least one digit"
            match span[pos] with
            | '-'B | '+'B -> pos <- pos + 1
            | _ -> ()
            skipAtLeastOneDigit span pos
        | _ -> pos
    else pos

let inline skipFraction (span : ReadOnlySpan<byte>) (pos : int) =
    if pos < span.Length then
        match span[pos] with
        | '.'B -> skipAtLeastOneDigit span (pos + 1)
        | _ -> pos
    else pos

/// The first byte at `span[pos]` must be either `-` or digit.
let inline parseNumber (span : ReadOnlySpan<byte>) (pos : int) : struct (RawNumber * int) =
    let startPos = pos

    // Skip optional `-`.
    let mutable pos = pos
    match span[pos] with
    | '-'B ->
        pos <- pos + 1
        if pos >= span.Length then
            raiseParsingException pos Error.UnexpectedEndOfSpan "Number must have at least one digit after sign"
    | _ -> ()

    match span[pos] with
    | '0'B -> pos <- pos + 1  // After initial `zero` there can be only fractional part or exponent.
    | '1'B | '2'B | '3'B | '4'B | '5'B | '6'B | '7'B | '8'B | '9'B -> pos <- skipDigits span pos
    | _ -> raiseParsingException pos Error.InvalidNumber "Number must start with minus sign or digit"

    pos <- skipFraction span pos
    pos <- skipExponent span pos

    struct ({ ContentStartPos = startPos; ContentEndPos = pos }, pos)

type FieldName = string

[<NoComparison>]
type RawValue =
    | Object of RawObject
    | Array of RawArray
    | RawString of RawString
    | RawNumber of RawNumber
    | True
    | False
    | Null

and [<Struct; CustomEquality; NoComparison>] RawObject =
    { StartPos : int
      EndPos : int
      Fields : IDictionary<FieldName, RawValue> }

    override me.Equals(o : obj) =
        match o with
        | :? RawObject as o ->
            me.StartPos = o.StartPos &&
            me.EndPos = o.EndPos &&
            me.Fields.Count = o.Fields.Count &&
            me.Fields
            |> Seq.forall (fun kv ->
                let present, value = o.Fields.TryGetValue kv.Key
                present && kv.Value = value)
        | _ -> false

    override me.GetHashCode() = me.StartPos

and [<Struct; CustomEquality; NoComparison>] RawArray =
    { StartPos : int
      EndPos : int
      Items : RawValue[] }

    override me.Equals(arr : obj) =
        match arr with
        | :? RawArray as arr ->
            me.StartPos = arr.StartPos &&
            me.EndPos = arr.EndPos &&
            me.Items = arr.Items
        | _ -> false

    override me.GetHashCode() = me.StartPos

/// Returns position of the first first non-whitespace byte or after the last byte of span.
let inline skipWhitespace (span : ReadOnlySpan<byte>) (pos : int) : int =
    let mutable pos = pos

    let mutable stop = false
    while pos < span.Length && not stop do
        match span[pos] with
        | '\t'B | '\n'B | '\r'B | ' 'B -> pos <- pos + 1  // Skip whitespace.
        | _ -> stop <- true

    pos

/// Same as `skipWhitespace` but additionally ensures that returned position is still inside span.
let inline skipWhitespaceAndEnsureNotEndOfSpan (span : ReadOnlySpan<byte>) (pos : int) (msg : string) =
    let pos = skipWhitespace span pos
    if pos >= span.Length then
        raiseParsingException pos Error.UnexpectedEndOfSpan msg
    pos

/// The first byte at `span[pos]` must be quote. But it's not checked.
let rec inline parseField
    (span : ReadOnlySpan<byte>)
    (pos : int)
    (maxNesting : int) : struct (FieldName * RawValue * int) =

    let struct (rawField, pos) = parseString span pos
    let field = decodeUtf16 span rawField
    let pos = skipWhitespaceAndEnsureNotEndOfSpan span pos "Expecting colon"
    if span[pos] <> ':'B then
        raiseParsingException pos Error.InvalidObject "Expecting colon"
    let struct (rawValue, pos) = parseRawValue span (pos + 1) maxNesting
    struct (field, rawValue, pos)

/// The first byte at `span[pos]` must be opening brace. But it's not checked.
and inline parseObject
    (span : ReadOnlySpan<byte>)
    (pos : int)
    (maxNesting : int) : struct (RawValue * int) =

    let startPos = pos
    let fields = Dictionary()

    let pos = skipWhitespaceAndEnsureNotEndOfSpan span (pos + 1) "Expecting first field or object end"
    match span[pos] with
    | '}'B -> struct (Object { StartPos = startPos; EndPos = pos + 1; Fields = fields }, pos + 1)
    | '"'B ->
        let struct (field, rawValue, pos) = parseField span pos maxNesting
        fields[field] <- rawValue
        let mutable nextFieldPos = skipWhitespaceAndEnsureNotEndOfSpan span pos "Expecting comma or object end"

        while span[nextFieldPos] = ','B do
            let pos = skipWhitespaceAndEnsureNotEndOfSpan span (nextFieldPos + 1) "Expecting field"

            if span[pos] <> '"'B then
                raiseParsingException pos Error.InvalidObject "Expecting field"
            let struct (field, rawValue, pos) = parseField span pos maxNesting

            let n = fields.Count
            fields[field] <- rawValue
            if fields.Count = n then
                raiseParsingException pos Error.DuplicateField "Field names consist of same code points"

            nextFieldPos <- skipWhitespaceAndEnsureNotEndOfSpan span pos "Expecting comma or object end"

        if span[nextFieldPos] <> '}'B then
            raiseParsingException nextFieldPos Error.InvalidObject "Expecting object end"

        struct (Object { StartPos = startPos; EndPos = nextFieldPos + 1; Fields  = fields }, nextFieldPos + 1)
    | _ -> raiseParsingException pos Error.InvalidObject "Expecting first field or object end"

/// The first byte at `span[pos]` must be opening bracket. But it's not checked.
and inline parseArray
    (span : ReadOnlySpan<byte>)
    (pos : int)
    (maxNesting : int) : struct (RawValue * int) =

    let startPos = pos

    let pos = skipWhitespaceAndEnsureNotEndOfSpan span (pos + 1) "Expecting value or array end"
    match span[pos] with
    | ']'B -> struct (Array { StartPos = startPos; EndPos = pos + 1; Items = [||] }, pos + 1)
    | _ ->
        let items = ResizeArray()

        let struct (rawValue, pos) = parseRawValue span pos maxNesting
        items.Add(rawValue)
        let mutable nextItemPos = skipWhitespaceAndEnsureNotEndOfSpan span pos "Expecting comma or array end"

        while span[nextItemPos] = ','B do
            let pos = skipWhitespaceAndEnsureNotEndOfSpan span (nextItemPos + 1) "Expecting value"

            let struct (rawValue, pos) = parseRawValue span pos maxNesting
            items.Add(rawValue)

            nextItemPos <- skipWhitespaceAndEnsureNotEndOfSpan span pos "Expecting comma or array end"

        if span[nextItemPos] <> ']'B then
            raiseParsingException nextItemPos Error.InvalidArray "Expecting array end"

        struct (Array { StartPos = startPos; EndPos = nextItemPos + 1; Items = items.ToArray() }, nextItemPos + 1)

and parseRawValue
    (span : ReadOnlySpan<byte>)
    (pos : int)
    (maxNesting : int) : struct (RawValue * int) =

    if maxNesting <= 0 then
        raiseParsingException pos Error.TooDeepNesting "Too many nested objects or arrays"

    let pos = skipWhitespaceAndEnsureNotEndOfSpan span pos "Expected JSON value"

    match span[pos] with
    | '{'B -> parseObject span pos (maxNesting - 1)
    | '['B -> parseArray span pos (maxNesting - 1)
    | '"'B ->
        let struct (raw, pos) = parseString span pos
        struct (RawString raw, pos)
    | '0'B | '1'B | '2'B | '3'B | '4'B | '5'B | '6'B | '7'B | '8'B | '9'B | '-'B ->
        let struct (raw, pos) = parseNumber span pos
        struct (RawNumber raw, pos)
    | 't'B ->
        if pos + 3 >= span.Length then
            raiseParsingException pos Error.UnexpectedEndOfSpan "Expecting true"
        if span[pos + 1] <> 'r'B || span[pos + 2] <> 'u'B || span[pos + 3] <> 'e'B then
            raiseParsingException pos Error.InvalidValue "Expecting true but got something different"
        struct (True, pos + 4)
    | 'f'B ->
        if pos + 4 >= span.Length then
            raiseParsingException pos Error.UnexpectedEndOfSpan "Expecting false"
        if span[pos + 1] <> 'a'B || span[pos + 2] <> 'l'B || span[pos + 3] <> 's'B || span[pos + 4] <> 'e'B then
            raiseParsingException pos Error.InvalidValue "Expecting false but got something different"
        struct (False, pos + 5)
    | 'n'B ->
        if pos + 3 >= span.Length then
            raiseParsingException pos Error.UnexpectedEndOfSpan "Expecting null"
        if span[pos + 1] <> 'u'B || span[pos + 2] <> 'l'B || span[pos + 3] <> 'l'B then
            raiseParsingException pos Error.InvalidValue "Expecting null but got something different"
        struct (Null, pos + 4)
    | _ -> raiseParsingException pos Error.InvalidValue "Unexpected first byte of value"
