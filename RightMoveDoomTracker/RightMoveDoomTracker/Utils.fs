module Utils

    let caseInsensitiveContains needle (s:string) = 
        s.IndexOf(needle, System.StringComparison.InvariantCultureIgnoreCase) >= 0

    let containsAny needles (s:string) = 
        needles |> Seq.exists (fun needle -> caseInsensitiveContains needle s)

    let startsWithAny needles (s:string) =
        needles |> Seq.exists s.StartsWith

    let toPercentage a b = ((float a)/(float b)) * float 100

    let convertToDateTime millis =
        let epoch = System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
        epoch.AddSeconds(float (millis/1000L))

    let split c (s:string) =
        s.Split [|c|]

    let splitIntoSentances = split '.'

    let toDateTime millis =
        match millis with
        | Some d -> Some <| convertToDateTime d
        | _ -> None

    let isBetween startDate endDate date =
        match date with
            | Some d -> d > startDate && d < endDate  
            | _ -> false