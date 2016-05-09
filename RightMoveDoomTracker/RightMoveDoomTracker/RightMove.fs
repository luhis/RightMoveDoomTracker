module RightMove
    open FSharp.Data
    open Utils

    type RightMove = JsonProvider<"RightMoveSample.json">
    type TypeAhead = JsonProvider<"http://www.rightmove.co.uk/typeAhead/uknostreet/BR/IG/HT/ON">
    type ItemType = 
    | Vacant
    | Reduced
    | CurrentRental
    | NoIssue

    type PropertyAndType = RightMove.Property * ItemType

    let generateUrl location minBedrooms maxPrice page = 
        let perPage = 48
        sprintf 
            "http://labs.rightmove.co.uk/api/_search\
            ?locationIdentifier=%s\
            &minBedrooms=%d\
            &maxPrice=%d\
            &index=%d\
            &numberOfPropertiesPerPage=%d\
            &radius=0.0&sortType=10&viewType=LIST&channel=BUY" 
            location
            minBedrooms
            maxPrice
            ((page-1) * perPage)
            perPage

    let generateTypeAheadLookupUrl (term:string) =
        let uppered = term.ToUpper()
        let searchPairs = uppered |> Seq.chunkBySize 2 |> Seq.map (fun a -> new string [|for c in a -> c|])
        let pairsWithSlashes = String.concat "/" searchPairs
        sprintf "http://www.rightmove.co.uk/typeAhead/uknostreet/%s" pairsWithSlashes

    let getLocation searchTerm =
        let lookupurl = generateTypeAheadLookupUrl searchTerm
        let taItems = TypeAhead.Load lookupurl

        taItems.TypeAheadLocations |> 
        Seq.filter (fun a -> Utils.caseInsensitiveContains searchTerm a.NormalisedSearchTerm) |> 
        Seq.head

    let getProperties locationString minBeds price = 
        let location = getLocation locationString
        let pageLessUrl = generateUrl location.LocationIdentifier minBeds price
        let firstUrl = pageLessUrl 1
        let loadedData = RightMove.Load firstUrl
        let page1Properties = loadedData.Properties
        let total = loadedData.Pagination.Total
        if total = 1 then
            page1Properties
        else
            let pagesToFetch = {2 .. total}
            Seq.fold (fun acc i -> 
                            let url = pageLessUrl i
                            let data = RightMove.Load url
                            Array.append data.Properties acc
                ) page1Properties pagesToFetch

    
    let vacantPhraises = ["no onward chain"; "no ongoing chain"; "vacant possession"; "chain free"]
    let letPhraises = ["is let"; "currently let"; "currently rented"; "PCM"; "rented out";  "holiday let"]

    let isReducedProperty (p:RightMove.Property) = 
        match p.AddedOrReduced with
        | Some added -> added.StartsWith "Reduced"
        | _ -> false

    let isVacantProperty (p:RightMove.Property) =
        containsAny vacantPhraises p.Summary

    let isLetProperty (p:RightMove.Property) =
        containsAny letPhraises p.Summary

    let categorise (property:RightMove.Property) =
        match property with
        | p when isReducedProperty p -> (p, Reduced)
        | p when isVacantProperty p -> (p, Vacant)
        | p when isLetProperty p -> (p, CurrentRental)
        | p -> (p, NoIssue)

    let findSentence test =
        splitIntoSentances >> Seq.filter test >> Seq.head

    let findVacantSentance =
        containsAny vacantPhraises |> findSentence

    let findIsLetSentance =
        containsAny letPhraises |> findSentence