module GinRummy

open Cards

type Sets = { rank: Rank; hasSet: bool }
type Runs = { suit: Suit; hasRun: bool }

// Add helper functions to help compute Deadwood function
let CardValue (card:Card) = 
    match card.rank with
    | Ace -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | _ -> 10

let totalSuit (hand:Hand) suit =
    Seq.filter (fun (x:Card) -> x.suit = suit) hand
    |> Seq.length

let totalRank (hand:Hand) rank =
    Seq.filter (fun (x:Card) -> x.rank = rank) hand
    |> Seq.length

let totalValue (hand:Hand) =
    Seq.map (fun x -> CardValue x) hand
    |> Seq.sum

let rankHasSet (hand:Hand) (card:Card) =
    ((totalRank hand card.rank) > 2)

let suitHasRun (hand:Hand) (card:Card) =
    ((totalSuit hand card.suit) > 2)

//adds a Set to a sequence if it has a set
let cardHasSet (hand:Hand) set card =
    match (rankHasSet hand card) with
    | true -> { rank = card.rank; hasSet = true } :: set
    | false -> set

let cardHasRun (hand:Hand) run card =
    match (suitHasRun hand card) with
    | true -> { suit = card.suit; hasRun = true } :: run
    | false -> run

//returns a sequence of ranks with sets        
let checkHandForSets (hand:Hand) =
    let set = List.empty<Sets>
    Seq.map (fun x -> cardHasSet hand set x) hand
    |> Seq.distinct        

let checkHandPotentialForRuns (hand:Hand) =
    let run = List.empty<Runs>
    Seq.map (fun x -> cardHasRun hand run x) hand
    |> Seq.distinct



let Deadwood (hand:Hand) = 
    totalValue hand
    // Fixme change so that it computes the actual deadwood score

let Score (firstOut:Hand) (secondOut:Hand) =
    0
    // Fixme change so that it computes how many points should be scored by the firstOut hand
    // (score should be negative if the secondOut hand is the winner)

// Add other functions related to Gin Rummy here ...