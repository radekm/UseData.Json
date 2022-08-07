module UseData.Json.Test.JsonValue

open System
open System.Collections.Generic
open System.Text

open NUnit.Framework

open UseData.Json

let maxNesting = 5

let failTestOnUnusedFieldTracer =
    { new ITracer with
        override _.OnUnusedFields(which, fields) = Assert.Fail($"Unused fields in %A{which}: %A{fields}") }

let ignoreUnusedFieldTracer =
    { new ITracer with
        override _.OnUnusedFields(which, fields) = () }

[<Test>]
let ``parse simple object`` () =
    let json = Encoding.UTF8.GetBytes """
        {
            "user": "Peter",
            "active": true,
            "registered": "2022-04-23T12:30:21Z"
        }
    """
    let struct (res, len) =
        JsonValue.parseUtf8String failTestOnUnusedFieldTracer (Memory json) maxNesting (fun v ->
            {| User = v |> Json.field "user" Json.string
               Active = v |> Json.field "active" Json.bool
               Registered = v |> Json.field "registered" Json.dateTimeOffset
            |})
    Assert.AreEqual(Array.LastIndexOf(json, '}'B) + 1, len)
    Assert.AreEqual(
        {| User = "Peter"; Active = true; Registered = DateTimeOffset(2022, 4, 23, 12, 30, 21, TimeSpan.Zero) |},
        res)

[<Test>]
let ``parse array of decimals`` () =
    let json = Encoding.UTF8.GetBytes """
        [
            1,
            0,
            0.2,
            1e4,
            123.4e4
        ]
    """
    let struct (res, len) =
        JsonValue.parseUtf8String failTestOnUnusedFieldTracer (Memory json) maxNesting (Json.map Json.decimal)
    Assert.AreEqual(Array.LastIndexOf(json, ']'B) + 1, len)
    CollectionAssert.AreEqual(
        [| 1M; 0M; 0.2M; 1e4M; 123.4e4M |],
        res)

[<Test>]
let ``decode UTF-8 in place`` () =
    let json = Encoding.UTF8.GetBytes " \"\\uABCD\\n😈\" "
    let struct (res, pos) =
        JsonValue.parseUtf8String failTestOnUnusedFieldTracer (Memory json) maxNesting Json.utf8StringDecodedInPlace

    Assert.AreEqual(15, pos)
    CollectionAssert.AreEqual(
        [| 0xEAuy; 0xAFuy; 0x8Duy; '\n'B; 0xF0uy; 0x9Fuy; 0x98uy; 0x88uy |],
        res.ToArray())

[<Test>]
let ``unused fields are reported`` () =
    let unusedFields = HashSet()
    let tracer =
        { new ITracer with
            override _.OnUnusedFields(which, fields) = fields |> Array.iter (unusedFields.Add >> ignore) }

    let json = Encoding.UTF8.GetBytes """
        {
            "x": 20.1,
            "y": 12.3
        }
    """
    let struct (res, len) = JsonValue.parseUtf8String tracer (Memory json) maxNesting (fun v ->
        v |> Json.field "x" Json.decimal)
    Assert.AreEqual(Array.LastIndexOf(json, '}'B) + 1, len)
    Assert.AreEqual(
        20.1M,
        res)
    CollectionAssert.AreEquivalent(["y"], unusedFields)

[<Test>]
let ``fails when required field is missing`` () =
    let json = Encoding.UTF8.GetBytes """
        {
            "bar": true
        }
    """
    let ex = Assert.Throws<JsonParsingException>(fun () ->
        JsonValue.parseUtf8String ignoreUnusedFieldTracer (Memory json) maxNesting (fun v ->
            v |> Json.field "foo" Json.bool)
        |> ignore)
    Assert.AreEqual(Root, ex.Which)
    Assert.AreEqual(Error.FieldNotPresent, ex.Error)

[<Test>]
let ``fails when one field is read twice`` () =
    let json = Encoding.UTF8.GetBytes """
        {
            "buy": 10.2,
            "sell": 4
        }
    """
    let ex = Assert.Throws<JsonParsingException>(fun () ->
        JsonValue.parseUtf8String ignoreUnusedFieldTracer (Memory json) maxNesting (fun v ->
            // We read `buy` twice. And this mistake should be caught by exception.
            Json.field "buy" Json.decimal v + Json.field "buy" Json.decimal v)
        |> ignore)
    Assert.AreEqual(Root, ex.Which)
    Assert.AreEqual(Error.FieldAlreadyUsed, ex.Error)

[<Test>]
let ``fails when array is read twice`` () =
    let json = Encoding.UTF8.GetBytes """
        {
            "numbers": [1, 2, 3]
        }
    """
    let ex = Assert.Throws<JsonParsingException>(fun () ->
        JsonValue.parseUtf8String failTestOnUnusedFieldTracer (Memory json) maxNesting (fun v ->
            v
            |> Json.field "numbers" (fun v ->
                let positive = v |> Json.map Json.int |> Array.filter (fun x -> x > 0)
                let negative = v |> Json.map Json.int |> Array.filter (fun x -> x < 0)
                positive, negative))
        |> ignore)
    Assert.AreEqual(ObjectField (Root, "numbers"), ex.Which)
    Assert.AreEqual(Error.ValueAlreadyUsed, ex.Error)

[<Test>]
let ``fails when string is read twice`` () =
    let json = Encoding.UTF8.GetBytes """ "foo" """
    let ex = Assert.Throws<JsonParsingException>(fun () ->
        JsonValue.parseUtf8String failTestOnUnusedFieldTracer (Memory json) maxNesting (fun v ->
            Json.string v + Json.string v)
        |> ignore)
    Assert.AreEqual(Root, ex.Which)
    Assert.AreEqual(Error.ValueAlreadyUsed, ex.Error)

[<Test>]
let ``fails when number is read twice`` () =
    let json = Encoding.UTF8.GetBytes """ 23 """
    let ex = Assert.Throws<JsonParsingException>(fun () ->
        JsonValue.parseUtf8String failTestOnUnusedFieldTracer (Memory json) maxNesting (fun v ->
            Json.int v + Json.int v)
        |> ignore)
    Assert.AreEqual(Root, ex.Which)
    Assert.AreEqual(Error.ValueAlreadyUsed, ex.Error)
