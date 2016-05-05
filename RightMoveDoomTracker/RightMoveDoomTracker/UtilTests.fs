module UtilTests

open NUnit.Framework
open Utils

[<Test>]
let ``caseInsensitiveContains finds capitalised string in lower case string``() = 
    let res = caseInsensitiveContains "TEST" "this is a test"
    Assert.AreEqual(true, res)

[<Test>]
let ``caseInsensitiveContains finds lower case string in lower case string``() = 
    let res = caseInsensitiveContains "test" "this is a test"
    Assert.AreEqual(true, res)

[<Test>]
let ``caseInsensitiveContains does not find lower case string in lower case string``() = 
    let res = caseInsensitiveContains "notpresent" "this is a test"
    Assert.AreEqual(false, res)

[<Test>]
let ``containsAny finds item that exists``() = 
    let res = containsAny [|"notpresent"; "present"|] "this is a present test"
    Assert.AreEqual(true, res)

[<Test>]
let ``containsAny does not find item that does not exist``() = 
    let res = containsAny [|"notpresent"; "alsonotpresent"|] "this is a present test"
    Assert.AreEqual(false, res)

[<Test>]
let ``startsWithAny finds when string starts with``() = 
    let res = startsWithAny [|"start"; "end"|] "start this is a present test end"
    Assert.AreEqual(true, res)

[<Test>]
let ``startsWithAny does not find when string doesn't start with``() = 
    let res = startsWithAny [|"end"|] "start this is a present test end"
    Assert.AreEqual(false, res)

[<Test>]
let ``toPercentage returns correct value``() = 
    let res = toPercentage 50 100
    Assert.AreEqual(50, res)

[<Test>]
let ``convertToDateTime returns correct value``() = 
    let res = convertToDateTime 1462399692000L
    Assert.AreEqual(new System.DateTime(2016, 05, 04, 22, 08, 12), res)

[<Test>]
let ``splitIntoSentances returns correct value``() =
    let res = splitIntoSentances "Sentence 1. Sentence 2! Sentence 3."
    Assert.AreEqual(3, Seq.length res)

[<Test>]
let ``isBetween returns correct value true``() =
    let start = System.DateTime(2016, 1, 1) 
    let endDate = System.DateTime(2016, 2, 2)
    let date = System.DateTime(2016, 1, 15) |> Some
    let res = isBetween start endDate date
    Assert.AreEqual(true, res)

[<Test>]
let ``isBetween returns correct value false``() =
    let start = System.DateTime(2016, 1, 1) 
    let endDate = System.DateTime(2016, 2, 2)
    let date = System.DateTime(2016, 3, 15) |> Some
    let res = isBetween start endDate date
    Assert.AreEqual(false, res)