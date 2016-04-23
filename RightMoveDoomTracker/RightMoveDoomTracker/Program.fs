open Utils
open System

open RightMove

[<EntryPoint>]
let main argv = 
    let findings = getProperties "Brighton and Hove" 3 425000 |> Seq.map matcher

    let printResult (r:PropertyAndType) =
        let s = match r with
                | p, Vacant -> Some <| "Vacant - " + findVacantSentance p.Summary
                | p, OutOfTownAgent -> Some <| "Out of town agent - " + p.Customer.ContactTelephone
                | p, Reduced -> Some <| "Reduced - " + p.AddedOrReduced.Value
                | p, CurrentRental -> Some <|"Current Rental - " + findIsLetSentance p.Summary
                | _, NoIssue -> None
        match s with
        | Some s -> printfn "%s" s
        | _ -> ()
        
    findings |> Seq.iter printResult |> ignore

    let printSummary results =
        let grouped = results |> Seq.groupBy (fun (_, t) -> t)
        let getCountOfCat groupedData cat = 
            groupedData |> 
            Seq.tryFind (fun (c, _) -> c = cat) |>
            function
                | Some (_, s) -> s |> Seq.length
                | _ -> 0
        let totalVacant = getCountOfCat grouped  Vacant
        let totalOutOfTown = getCountOfCat grouped  OutOfTownAgent
        let totalReduced = getCountOfCat grouped  Reduced
        let totalNoIssue = getCountOfCat grouped  NoIssue
        let totalCurrentRental = getCountOfCat grouped  CurrentRental
        let total = totalNoIssue + totalOutOfTown + totalReduced + totalVacant

        printfn "Total Items: %d" total
        printfn "Vacant: %d (%f%%)" totalVacant <| Utils.toPercentage totalVacant total
        printfn "Out Of Town Agent: %d (%f%%)" totalOutOfTown <| Utils.toPercentage totalOutOfTown total
        printfn "Reduced: %d (%f%%)" totalReduced <| Utils.toPercentage totalReduced total
        printfn "Current Rental: %d (%f%%)" totalCurrentRental <| Utils.toPercentage totalCurrentRental total
        printfn "No Issue: %d (%f%%)" totalNoIssue <| Utils.toPercentage totalNoIssue total
    printSummary findings
        
    let filterByDate startDate endDate (data:seq<PropertyAndType>) =
        let builtIsBetween = isBetween startDate endDate
        data |> Seq.filter (fun (p, _) ->  toDateTime p.AutoEmailDate |> builtIsBetween)

    let lastWeekDate = DateTime.Now.Date.AddDays -7.0
    let lastWeek = findings |> filterByDate lastWeekDate DateTime.Now.Date
    printfn "\nResults last week"
    printSummary lastWeek

    let weekBeforeLastDate = DateTime.Now.Date.AddDays -14.0
    let weekBeforeLast = findings |> filterByDate weekBeforeLastDate lastWeekDate
    printfn "\nResults week before last"
    printSummary weekBeforeLast

    let weekBeforeBeforeLastDate = DateTime.Now.Date.AddDays -21.0
    let weekBeforeBeforeLast = findings |> filterByDate weekBeforeBeforeLastDate weekBeforeLastDate
    printfn "\nResults week before before last"
    printSummary weekBeforeBeforeLast

    Console.ReadLine () |> ignore
    0
