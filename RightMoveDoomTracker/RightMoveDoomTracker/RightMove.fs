module RightMove
    open FSharp.Data
    open Utils

    type RightMove = JsonProvider<"RightMoveSample.json">
    type TypeAhead = JsonProvider<"http://www.rightmove.co.uk/typeAhead/uknostreet/BR/IG/HT/ON">
    type ItemType = 
    | Vacant
    | OutOfTownAgent
    | Reduced
    | CurrentRental
    | NoIssue

    type PropertyAndType = (RightMove.Property * ItemType)

    let generateUrl location minBedrooms maxPrice page = 
        let perPage = 48
        "http://labs.rightmove.co.uk/api/_search?" + 
        "locationIdentifier=" + location.ToString() + 
        "&minBedrooms=" + minBedrooms.ToString() + 
        "&maxPrice=" + maxPrice.ToString() + 
        "&index=" + ((page-1) * perPage).ToString() +
        "&numberOfPropertiesPerPage=" + perPage.ToString() +
        "&radius=0.0&sortType=10&viewType=LIST&channel=BUY"

    let generateTypeAheadLookupUrl (term:string) =
        let uppered = term.ToUpper()
        let searchPairs = uppered |> Seq.chunkBySize 2 |> Seq.map (fun a -> new string [|for c in a -> c|])
        "http://www.rightmove.co.uk/typeAhead/uknostreet/" + String.concat "/" searchPairs

    let getLocation searchTerm =
        let lookupurl = generateTypeAheadLookupUrl searchTerm
        let taItems = TypeAhead.Load(lookupurl)

        taItems.TypeAheadLocations |> 
        Seq.filter (fun a -> Utils.caseInsensitiveContains searchTerm a.NormalisedSearchTerm) |> 
        Seq.head

    let getProperties locationString minBeds price = 
        let location = getLocation locationString
        let pageLessUrl = generateUrl location.LocationIdentifier minBeds price
        let firstUrl = pageLessUrl 1
        let loadedData = RightMove.Load(firstUrl)
        let page1Properties = loadedData.Properties
        let total = loadedData.Pagination.Total
        let pagesToFetch = {2 .. total}
        if total = 1 then
            page1Properties
        else
            Seq.fold (fun acc i -> 
                            let url = pageLessUrl i
                            let properties = RightMove.Load(url).Properties
                            Array.append properties acc
                ) page1Properties pagesToFetch

    
    let vacantPhraises = ["no onward chain"; "no ongoing chain"; "vacant possession"; "chain free"]
    let letPhraises = ["is let"; "currently let"; "currently rented"; "PCM"; "rented out";  "holiday let"]

    let matcher (property: RightMove.Property) =
        match property with
        //| p when not <| startsWithAny areaCodesForRegion p.Customer.ContactTelephone -> (p, OutOfTownAgent)
        | p when p.AddedOrReduced.IsSome && p.AddedOrReduced.Value.StartsWith("Reduced") -> (p, Reduced)
        | p when containsAny vacantPhraises p.Summary -> (p, Vacant)
        | p when containsAny letPhraises p.Summary -> (p, CurrentRental)
        | p -> (p, NoIssue)

    let findVacantSentance s =
        splitIntoSentances s |> Seq.filter (fun a -> containsAny vacantPhraises a) |> Seq.head

    let findIsLetSentance s =
        splitIntoSentances s |> Seq.filter (fun a -> containsAny letPhraises a) |> Seq.head