module UseData.Json.Test.JsonValue

open System
open System.Collections.Generic
open System.Text

open NUnit.Framework

open UseData.Json

let intern (fields : IDictionary<string, FieldId>) fieldName =
    if fields.ContainsKey fieldName
    then fields[fieldName]
    else
        let id = fields.Count
        fields[fieldName] <- id
        id

let maxNesting = 5

let failTestOnUnusedFieldTracer =
    { new ITracer with
        override _.OnUnusedFields(which, fields) = Assert.Fail($"Unused fields in %A{which}: %A{fields}") }

let ignoreUnusedFieldTracer =
    { new ITracer with
        override _.OnUnusedFields(which, fields) = () }

[<Test>]
let ``parse simple object`` () =
    let fields = Dictionary()
    let intern = intern fields

    let fieldUser = intern "user"
    let fieldRegistered = intern "registered"
    let fieldActive = intern "active"

    let json = Encoding.UTF8.GetBytes """
        {
            "user": "Peter",
            "active": true,
            "registered": "2022-04-23T12:30:21Z"
        }
    """
    let struct (res, len) =
        JsonValue.parseUtf8String intern failTestOnUnusedFieldTracer (Memory json) maxNesting (fun v ->
            {| User = v |> Json.field fieldUser Json.string
               Active = v |> Json.field fieldActive Json.bool
               Registered = v |> Json.field fieldRegistered Json.dateTimeOffset
            |})
    Assert.AreEqual(Array.LastIndexOf(json, '}'B) + 1, len)
    Assert.AreEqual(
        {| User = "Peter"; Active = true; Registered = DateTimeOffset(2022, 4, 23, 12, 30, 21, TimeSpan.Zero) |},
        res)

[<Test>]
let ``parse array of decimals`` () =
    let fields = Dictionary()
    let intern = intern fields

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
        JsonValue.parseUtf8String intern failTestOnUnusedFieldTracer (Memory json) maxNesting (fun v ->
            v |> Json.map Json.decimal)
    Assert.AreEqual(Array.LastIndexOf(json, ']'B) + 1, len)
    CollectionAssert.AreEqual(
        [| 1M; 0M; 0.2M; 1e4M; 123.4e4M |],
        res)

[<Test>]
let ``unused fields are reported`` () =
    let fields = Dictionary()
    let intern = intern fields

    let fieldX = intern "x"
    let fieldY = intern "y"

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
    let struct (res, len) = JsonValue.parseUtf8String intern tracer (Memory json) maxNesting (fun v ->
        v |> Json.field fieldX Json.decimal)
    Assert.AreEqual(Array.LastIndexOf(json, '}'B) + 1, len)
    Assert.AreEqual(
        20.1M,
        res)
    CollectionAssert.AreEquivalent([fieldY], unusedFields)

[<Test>]
let ``fails when required field is missing`` () =
    let fields = Dictionary()
    let intern = intern fields

    let fieldFoo = intern "foo"

    let json = Encoding.UTF8.GetBytes """
        {
            "bar": true
        }
    """
    let ex = Assert.Throws<JsonParsingException>(fun () ->
        JsonValue.parseUtf8String intern ignoreUnusedFieldTracer (Memory json) maxNesting (fun v ->
            v |> Json.field fieldFoo Json.bool)
        |> ignore)
    Assert.AreEqual(Root, ex.Which)
    Assert.AreEqual(Error.FieldNotPresent, ex.Error)

[<Test>]
let ``fails when one field is read twice`` () =
    let fields = Dictionary()
    let intern = intern fields

    let fieldBuy = intern "buy"
    //let fieldSell = intern "sell"

    let json = Encoding.UTF8.GetBytes """
        {
            "buy": 10.2,
            "sell": 4
        }
    """
    let ex = Assert.Throws<JsonParsingException>(fun () ->
        JsonValue.parseUtf8String intern ignoreUnusedFieldTracer (Memory json) maxNesting (fun v ->
            // We read `fieldBuy` twice. And this mistake should be caught by exception.
            Json.field fieldBuy Json.decimal v + Json.field fieldBuy Json.decimal v)
        |> ignore)
    Assert.AreEqual(Root, ex.Which)
    Assert.AreEqual(Error.FieldAlreadyUsed, ex.Error)

[<Test>]
let ``fails when array is read twice`` () =
    let fields = Dictionary()
    let intern = intern fields
    let fieldNumbers = intern "numbers"

    let json = Encoding.UTF8.GetBytes """
        {
            "numbers": [1, 2, 3]
        }
    """
    let ex = Assert.Throws<JsonParsingException>(fun () ->
        JsonValue.parseUtf8String intern failTestOnUnusedFieldTracer (Memory json) maxNesting (fun v ->
            v
            |> Json.field fieldNumbers (fun v ->
                let positive = v |> Json.map Json.int |> Array.filter (fun x -> x > 0)
                let negative = v |> Json.map Json.int |> Array.filter (fun x -> x < 0)
                positive, negative))
        |> ignore)
    Assert.AreEqual(ObjectField (Root, fieldNumbers), ex.Which)
    Assert.AreEqual(Error.ValueAlreadyUsed, ex.Error)

[<Test>]
let ``fails when string is read twice`` () =
    let fields = Dictionary()
    let intern = intern fields

    let json = Encoding.UTF8.GetBytes """ "foo" """
    let ex = Assert.Throws<JsonParsingException>(fun () ->
        JsonValue.parseUtf8String intern failTestOnUnusedFieldTracer (Memory json) maxNesting (fun v ->
            Json.string v + Json.string v)
        |> ignore)
    Assert.AreEqual(Root, ex.Which)
    Assert.AreEqual(Error.ValueAlreadyUsed, ex.Error)

[<Test>]
let ``fails when number is read twice`` () =
    let fields = Dictionary()
    let intern = intern fields

    let json = Encoding.UTF8.GetBytes """ 23 """
    let ex = Assert.Throws<JsonParsingException>(fun () ->
        JsonValue.parseUtf8String intern failTestOnUnusedFieldTracer (Memory json) maxNesting (fun v ->
            Json.int v + Json.int v)
        |> ignore)
    Assert.AreEqual(Root, ex.Which)
    Assert.AreEqual(Error.ValueAlreadyUsed, ex.Error)