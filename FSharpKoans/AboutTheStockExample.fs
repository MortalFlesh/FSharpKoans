namespace FSharpKoans
open FSharpKoans.Core
open System.Globalization

[<RequireQualifiedAccessAttribute>]
module Solution1 =
    let parseLines data =
        let parseLine (line: string) =
            line.Split(',')
        
        data
        |> List.skip 1
        |> List.map parseLine

    let strToFloat (str: string) =
        System.Double.Parse(str, CultureInfo.InvariantCulture)

    let selectOpenAndClosePerDate lines =
        lines
        |> List.map(fun line ->
            match line with
            | [| date; open'; high; low; close; volume; adjClose|] ->
                (
                    date,
                    (open' |> strToFloat, close |> strToFloat)
                )
            | _ -> failwithf "Line %A has a different shape" line
        )
        |> Map.ofList

    let computeAbsoluteDifference lines =
        lines
        |> Map.map (fun _ (a: float, b: float) ->
            (a - b) |> abs
        )

    let selectDateWithMaxDifference differencesPerDate =
        differencesPerDate
        |> Map.toList
        |> List.maxBy snd
        |> fst

    let solution stockData = 
        stockData
            |> parseLines
            |> selectOpenAndClosePerDate
            |> computeAbsoluteDifference
            |> selectDateWithMaxDifference

[<RequireQualifiedAccessAttribute>]
module Solution2 =
    let parseLines data =
        let parseLine (line: string) =
            line.Split(',')

        data
        |> List.skip 1
        |> List.map parseLine

    let strToFloat (str: string) =
        System.Double.Parse(str, CultureInfo.InvariantCulture)

    let selectDifferencePerDate lines =
        lines
        |> List.map(fun line ->
            match line with
            | [| date; open'; high; low; close; volume; adjClose|] ->
                let difference = ((open' |> strToFloat) - (close |> strToFloat)) |> abs
                (date, difference)
            | _ -> failwithf "Line %A has a different shape" line
        )
        |> Map.ofList

    let selectDateWithMaxDifference differencesPerDate =
        differencesPerDate
        |> Map.toList
        |> List.maxBy snd
        |> fst

    let solution stockData = 
        stockData
            |> parseLines
            |> selectDifferencePerDate
            |> selectDateWithMaxDifference

[<RequireQualifiedAccessAttribute>]
module Solution3 =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseLine line =
        match line with
        | Regex @"(\d{4}-\d{2}-\d{2}),(\d{2}.\d{2}),.*?,.*?,(\d{2}.\d{2}),.*?" [ date; open'; close ] ->
            let diff =
                [open'; close]
                |> List.map (fun n -> System.Double.Parse(n, CultureInfo.InvariantCulture))
                |> List.fold (fun diff n -> (diff - n) |> abs) 0.0
            Some (date, diff)
        | _ -> None

    let solution stockData = 
        stockData
        |> List.map parseLine
        |> List.choose id
        |> List.maxBy snd
        |> fst

//---------------------------------------------------------------
// Apply Your Knowledge!
//
// Below is a list containing comma separated data about 
// Microsoft's stock prices during March of 2012. Without
// modifying the list, programatically find the day with the
// greatest difference between the opening and closing prices.
//
// The following functions may be of use:
// 
// abs - takes the absolute value of an argument
// 
// System.Double.Parse - converts a string argument into a 
//                       numerical value.
//
// Hint: Use CultureInfo.InvariantCulture to always parse '.' as 
// the decimal point.
//
// The following function will convert a comma separated string
// into an array of the column values.
//                       
// let splitCommas (x:string) =
//     x.Split([|','|])
//---------------------------------------------------------------
[<Koan(Sort = 15)>]
module ``about the stock example`` =
    
    let stockData =
        [ "Date,Open,High,Low,Close,Volume,Adj Close";
          "2012-03-30,32.40,32.41,32.04,32.26,31749400,32.26";
          "2012-03-29,32.06,32.19,31.81,32.12,37038500,32.12";
          "2012-03-28,32.52,32.70,32.04,32.19,41344800,32.19";
          "2012-03-27,32.65,32.70,32.40,32.52,36274900,32.52";
          "2012-03-26,32.19,32.61,32.15,32.59,36758300,32.59";
          "2012-03-23,32.10,32.11,31.72,32.01,35912200,32.01";
          "2012-03-22,31.81,32.09,31.79,32.00,31749500,32.00";
          "2012-03-21,31.96,32.15,31.82,31.91,37928600,31.91";
          "2012-03-20,32.10,32.15,31.74,31.99,41566800,31.99";
          "2012-03-19,32.54,32.61,32.15,32.20,44789200,32.20";
          "2012-03-16,32.91,32.95,32.50,32.60,65626400,32.60";
          "2012-03-15,32.79,32.94,32.58,32.85,49068300,32.85";
          "2012-03-14,32.53,32.88,32.49,32.77,41986900,32.77";
          "2012-03-13,32.24,32.69,32.15,32.67,48951700,32.67";
          "2012-03-12,31.97,32.20,31.82,32.04,34073600,32.04";
          "2012-03-09,32.10,32.16,31.92,31.99,34628400,31.99";
          "2012-03-08,32.04,32.21,31.90,32.01,36747400,32.01";
          "2012-03-07,31.67,31.92,31.53,31.84,34340400,31.84";
          "2012-03-06,31.54,31.98,31.49,31.56,51932900,31.56";
          "2012-03-05,32.01,32.05,31.62,31.80,45240000,31.80";
          "2012-03-02,32.31,32.44,32.00,32.08,47314200,32.08";
          "2012-03-01,31.93,32.39,31.85,32.29,77344100,32.29";
          "2012-02-29,31.89,32.00,31.61,31.74,59323600,31.74"; ]
    
    // Feel free to add extra [<Koan>] members here to write
    // tests for yourself along the way. You can also try 
    // using the F# Interactive window to check your progress.

    let parseLines data =
        let parseLine (line: string) =
            line.Split(',')
        
        data
        |> List.skip 1
        |> List.map parseLine

    let lines =
        stockData
        |> parseLines

    [<Koan>]
    let ShouldParseStockDataToLines() =
        let firstLine =
            lines
            |> List.head

        AssertEquality [| "2012-03-30"; "32.40"; "32.41"; "32.04"; "32.26"; "31749400"; "32.26" |] firstLine

    let strToFloat (str: string) =
        System.Double.Parse(str, CultureInfo.InvariantCulture)

    let selectOpenAndClosePerDate lines =
        lines
        |> List.map(fun line ->
            match line with
            | [| date; open'; high; low; close; volume; adjClose|] ->
                (
                    date,
                    (open' |> strToFloat, close |> strToFloat)
                )
            | _ -> failwithf "Line %A has a different shape" line
        )
        |> Map.ofList

    let selectedPerDate =
        lines
        |> selectOpenAndClosePerDate

    [<Koan>]
    let ShouldSelectOpenAndClosePerDate() =
        let firstLine =
            selectedPerDate
            |> Map.find "2012-03-30"

        AssertEquality (32.40, 32.26) firstLine

    let computeAbsoluteDifference lines =
        lines
        |> Map.map (fun _ (a: float, b: float) ->
            (a - b) |> abs
        )

    let differenesPerDate =
        selectedPerDate
        |> computeAbsoluteDifference

    [<Koan>]
    let ShouldComputeAbsoluteDifference() =
        let firstLine =
            differenesPerDate
            |> Map.find "2012-03-30"

        AssertEquality "0.14" (firstLine |> string |> (fun s -> s.Substring(0, 4)))

    let selectDateWithMaxDifference differencesPerDate =
        differencesPerDate
        |> Map.toList
        |> List.maxBy snd
        |> fst

    [<Koan>]
    let YouGotTheAnswerCorrect() =
        let result = 
            stockData
            |> parseLines
            |> selectOpenAndClosePerDate
            |> computeAbsoluteDifference
            |> selectDateWithMaxDifference

        AssertEquality "2012-03-13" result
    
    [<Koan>]
    let YouGotTheAnswerCorrectWithSolution1() =
        let result = 
            stockData
            |> Solution1.solution

        AssertEquality "2012-03-13" result
    
    [<Koan>]
    let YouGotTheAnswerCorrectWithSolution2() =
        let result = 
            stockData
            |> Solution2.solution

        AssertEquality "2012-03-13" result
    
    [<Koan>]
    let YouGotTheAnswerCorrectWithSolution3() =
        let result = 
            stockData
            |> Solution3.solution

        AssertEquality "2012-03-13" result
