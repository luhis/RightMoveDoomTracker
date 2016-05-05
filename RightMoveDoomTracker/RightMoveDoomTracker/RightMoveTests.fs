module RightMoveTests

open NUnit.Framework
open RightMove

[<Test>]
let ``caseInsensitiveContains finds capitalised string in lower case string``() = 
    let res = generateUrl "Brighton" 3 425000 1
    let expected = "http://labs.rightmove.co.uk/api/_search?" + 
                    "locationIdentifier=Brighton" +  
                    "&minBedrooms=3" + 
                    "&maxPrice=425000" + 
                    "&index=0" + 
                    "&numberOfPropertiesPerPage=48" + 
                    "&radius=0.0&sortType=10&viewType=LIST&channel=BUY"
    Assert.AreEqual(expected, res)

[<Test>]
let ``generateTypeAheadLookupUrl produces correct output``() = 
    let res= generateTypeAheadLookupUrl "brighton"
    Assert.AreEqual("http://www.rightmove.co.uk/typeAhead/uknostreet/BR/IG/HT/ON", res)

[<Test>]
let ``findVacantSentance finds string``() = 
    let res = findVacantSentance "This house is a dump!  Chain free as the previous owner snuffed it!"
    Assert.AreEqual("Chain free as the previous owner snuffed it", res)

[<Test>]
let ``findIsLetSentance finds string``() = 
    let res = findIsLetSentance "This shithole is currently let for a million quid a year! Be your own slum lord"
    Assert.AreEqual("This shithole is currently let for a million quid a year", res)