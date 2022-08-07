namespace UseData.Json

open System
open System.Collections.Generic

open System.Globalization
open UseData.Json.Parser

type FieldName = string

type WhichValue =
    | Root
    | ObjectField of parent:WhichValue * field:FieldName
    | ArrayItem of parent:WhichValue * indexInArray:int

type ITracer =
    abstract OnUnusedFields : which:WhichValue * fields:FieldName[] -> unit

[<RequireQualifiedAccess>]
type Error =
    | UnexpectedKind
    | FieldNotPresent
    | FieldAlreadyUsed
    | ValueAlreadyUsed
    | ParsingFailed

type JsonParsingException(which : WhichValue, error : Error, info : string, innerException : exn) =
    inherit Exception($"Error %A{error} when parsing %A{which}: %s{info}", innerException)

    member _.Which = which
    member _.Error = error
    member _.Info = info

[<Sealed>]
type JsonValue internal (which : WhichValue, tracer : ITracer, buffer : Memory<byte>, raw : RawValue) =
    let mutable usedFields = Unchecked.defaultof<HashSet<FieldName>>
    let mutable used = false
    let mutable disposed = false

    member internal _.Buffer = buffer
    member internal _.Which = which
    member internal _.Tracer = tracer
    member _.Raw = raw

    member internal _.UsedFields =
        if isNull usedFields then
            usedFields <- HashSet()
        usedFields

    member internal _.Used
        with get () = used
        and set value = used <- value

    interface IDisposable with
        override _.Dispose() =
            if not disposed then
                disposed <- true
                match raw with
                | Object fields ->
                    if fields.Count > 0 then
                        // All remaining fields are unused because `field` and `fieldOpt` remove used fields.
                        tracer.OnUnusedFields(which, fields.Keys |> Seq.toArray)
                | _ -> ()

module JsonValue =
    /// Warning: `raw` and `utf8String` may be modified during parsing.
    /// They could contain garbage after parsing.
    let parseRawValue
        (tracer : ITracer)
        (utf8String : Memory<byte>)
        (raw : RawValue)
        (f : JsonValue -> 'T) : 'T =

        use v = new JsonValue(Root, tracer, utf8String, raw)
        f v

    /// Warning: `utf8String` may be modified during parsing.
    /// It could contain garbage after parsing.
    let parseUtf8String
        (tracer : ITracer)
        (utf8String : Memory<byte>)
        (maxNesting : int)
        (f : JsonValue -> 'T) : struct ('T * int) =

        let struct (raw, pos) = Parser.parseRawValue (Span.op_Implicit utf8String.Span) 0 maxNesting
        let result = parseRawValue tracer utf8String raw f
        struct (result, pos)

module Json =
    let private raiseJsonParsingExceptionWithCause (v : JsonValue) error info cause =
        raise (JsonParsingException (v.Which, error, info, cause))

    let private raiseJsonParsingException v error info =
        raiseJsonParsingExceptionWithCause v error info Unchecked.defaultof<_>

    let raw (v : JsonValue) : RawValue = v.Raw

    // Fails if field is not present.
    let field (name : FieldName) (f : JsonValue -> 'T) (v : JsonValue) : 'T =
        match v.Raw with
        | Object fields ->
            if not (v.UsedFields.Add name) then
                raiseJsonParsingException v Error.FieldAlreadyUsed $"Field %s{name}"
            // Removing used fields has two benefits:
            // - It speeds up finding unused fields - simply all remaining fields are unused.
            // - It conserves memory.
            // Unfortunately it could be unexpected that underlying `RawValue` is modified.
            let present, raw = fields.Remove name
            if not present
            then raiseJsonParsingException v Error.FieldNotPresent $"Field %s{name}"
            else
                use v = new JsonValue(ObjectField (v.Which, name), v.Tracer, v.Buffer, raw)
                f v
        | _ -> raiseJsonParsingException v Error.UnexpectedKind $"Cannot get field %s{name} when kind is not object"

    /// Returns `None` iff field is not present or null.
    let fieldOpt (name : FieldName) (f : JsonValue -> 'T) (v : JsonValue) : 'T voption =
        match v.Raw with
        | Object fields ->
            if not (v.UsedFields.Add name) then
                raiseJsonParsingException v Error.FieldAlreadyUsed $"Field %s{name}"
            let present, raw = fields.Remove name
            if not present || raw = Null
            then ValueNone
            else
                use v = new JsonValue(ObjectField (v.Which, name), v.Tracer, v.Buffer, raw)
                ValueSome (f v)
        | _ -> raiseJsonParsingException v Error.UnexpectedKind $"Cannot get field %s{name} when kind is not object"

    let map (f : JsonValue -> 'T) (v : JsonValue) : 'T[] =
        match v.Raw with
        | Array items ->
            if v.Used then
                raiseJsonParsingException v Error.ValueAlreadyUsed "Array already used"
            v.Used <- true
            // TODO Should we set array items to null to conserve memory?
            items
            |> Array.mapi (fun i raw ->
                use v = new JsonValue(ArrayItem (v.Which, i), v.Tracer, v.Buffer, raw)
                f v)
        | _ -> raiseJsonParsingException v Error.UnexpectedKind "Expected array"

    let iter (f : JsonValue -> unit) (v : JsonValue) : unit =
        match v.Raw with
        | Array items ->
            if v.Used then
                raiseJsonParsingException v Error.ValueAlreadyUsed "Array already used"
            v.Used <- true
            // TODO Should we set array items to null to conserve memory?
            items
            |> Array.iteri (fun i raw ->
                use v = new JsonValue(ArrayItem (v.Which, i), v.Tracer, v.Buffer, raw)
                f v)
        | _ -> raiseJsonParsingException v Error.UnexpectedKind "Expected array"

    let string (v : JsonValue) : string =
        match v.Raw with
        | RawString raw ->
            if v.Used then
                raiseJsonParsingException v Error.ValueAlreadyUsed "String already used"
            v.Used <- true
            decodeUtf16 (Span.op_Implicit v.Buffer.Span) raw
        | _ -> raiseJsonParsingException v Error.UnexpectedKind "Expected string"

    let utf8StringDecodedInPlace (v : JsonValue) : Memory<byte> =
        match v.Raw with
        | RawString raw ->
            if v.Used then
                raiseJsonParsingException v Error.ValueAlreadyUsed "String already used"
            v.Used <- true
            let nBytes = decodeUtf8InPlace (v.Buffer.Span) raw
            v.Buffer.Slice(raw.ContentStartPos, nBytes)
        | _ -> raiseJsonParsingException v Error.UnexpectedKind "Expected string"

    // TODO Make functions below (dateTimeOffset, int, decimal, ...) use either
    //      in place decoded UTF-8 strings or original bytes directly.

    // `dateTimeOffset` must be defined before overriding `int.`
    /// Accepts either format without fraction seconds or format with up to 7 digits of fractional seconds:
    /// - Without fractional seconds `YYYY-mm-ddTHH:mm:ssZ` or `YYYY-mm-dd HH:mm:ssZ` (string length 20).
    /// - With fractional seconds `YYYY-mm-ddTHH:mm:ss.fffZ` or `YYYY-mm-dd HH:mm:ss.fffZ` (string length 22-28).
    let dateTimeOffset (v : JsonValue) : DateTimeOffset =
        let s = string v
        let error cause =
            raiseJsonParsingExceptionWithCause v Error.ParsingFailed $"Expected date time offset but got: %s{s}" cause
        let inline readDigit i =
            let b = s[i]
            let d = int b - int '0'
            if d < 0 || d > 9
            then failwithf "Not a digit %c" b  // This will be caught and wrapped inside `JsonParsingException`.
            else d

        if s.Length < 20 || s[4] <> '-' || s[7] <> '-' || (s[10] <> 'T' && s[10] <> ' ') || s[13] <> ':' || s[16] <> ':'
        then error (Exception "Unexpected form")
        else
            try
                let year = ((readDigit 0 * 10 + readDigit 1) * 10 + readDigit 2) * 10 + readDigit 3
                let month = readDigit 5 * 10 + readDigit 6
                let day = readDigit 8 * 10 + readDigit 9
                let hour = readDigit 11 * 10 + readDigit 12
                let minute = readDigit 14 * 10 + readDigit 15
                let second = readDigit 17 * 10 + readDigit 18

                let withoutFractionalSeconds = DateTimeOffset(year, month, day, hour, minute, second, TimeSpan.Zero)

                match s.Length with
                | 20 when s[19] = 'Z' -> withoutFractionalSeconds
                | len when len >= 22 && len <= 28 && s[19] = '.' && s[len - 1] = 'Z' ->
                    let mutable ticks = 0
                    // Read fractional digits.
                    for i = 20 to len - 2 do
                        ticks <- ticks * 10 + readDigit i
                    // Padding.
                    for i = 1 to 28 - len do
                        ticks <- ticks * 10
                    withoutFractionalSeconds.AddTicks ticks
                | _ -> error (Exception "Unexpected form")
            with e -> error e

    let inline private numberToString (v : JsonValue) =
        match v.Raw with
        | RawNumber raw ->
            if v.Used then
                raiseJsonParsingException v Error.ValueAlreadyUsed "Number already used"
            v.Used <- true
            decodeUtf16 (Span.op_Implicit v.Buffer.Span)
                { ContentStartPos = raw.ContentStartPos
                  ContentEndPos = raw.ContentEndPos
                  StringLength =  raw.ContentEndPos - raw.ContentStartPos }
        | _ -> raiseJsonParsingException v Error.UnexpectedKind "Expected number"

    let int (v : JsonValue) : int =
        let s = numberToString v
        try Int32.Parse(s, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture)
        with e -> raiseJsonParsingExceptionWithCause v Error.ParsingFailed "Expected int" e

    let uint (v : JsonValue) : uint =
        let s = numberToString v
        try UInt32.Parse(s, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture)
        with e -> raiseJsonParsingExceptionWithCause v Error.ParsingFailed "Expected uint" e

    let int64 (v : JsonValue) : int64 =
        let s = numberToString v
        try Int64.Parse(s, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture)
        with e -> raiseJsonParsingExceptionWithCause v Error.ParsingFailed "Expected int64" e

    let uint64 (v : JsonValue) : uint64 =
        let s = numberToString v
        try UInt64.Parse(s, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture)
        with e -> raiseJsonParsingExceptionWithCause v Error.ParsingFailed "Expected uint64" e

    let unixTimeMilliseconds (v : JsonValue) : DateTimeOffset =
        let n = int64 v
        try DateTimeOffset.FromUnixTimeMilliseconds(n)
        with e -> raiseJsonParsingExceptionWithCause v Error.ParsingFailed "Milliseconds since start of UNIX epoch" e

    let decimal (v : JsonValue) : decimal =
        let s = numberToString v
        try
            Decimal.Parse(
                s,
                NumberStyles.AllowLeadingSign ||| NumberStyles.AllowDecimalPoint ||| NumberStyles.AllowExponent,
                CultureInfo.InvariantCulture)
        with e -> raiseJsonParsingExceptionWithCause v Error.ParsingFailed "Expected decimal" e

    let bool (v : JsonValue) : bool =
        match v.Raw with
        | True ->
            if v.Used then
                raiseJsonParsingException v Error.ValueAlreadyUsed "Bool already used"
            v.Used <- true
            true
        | False ->
            if v.Used then
                raiseJsonParsingException v Error.ValueAlreadyUsed "Bool already used"
            v.Used <- true
            false
        | _ -> raiseJsonParsingException v Error.UnexpectedKind "Expected bool"

    let inline nullable ([<InlineIfLambda>] f : JsonValue -> 'T) (v : JsonValue) : 'T ValueOption =
        match v.Raw with
        | Null -> ValueNone
        | _ -> ValueSome (f v)
