module GinRummy

open Cards


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

let nextRank (card:Card) =
    match card.rank with
    | Ace ->  {rank=Two; suit= card.suit}
    | Two -> {rank=Three; suit= card.suit}
    | Three -> {rank=Four; suit= card.suit}
    | Four -> {rank=Five; suit= card.suit}
    | Five -> {rank=Six; suit= card.suit}
    | Six -> {rank=Seven; suit= card.suit}
    | Seven -> {rank=Eight; suit= card.suit}
    | Eight -> {rank=Nine; suit= card.suit}
    | Nine -> {rank=Ten; suit= card.suit}
    | Ten -> {rank=Jack; suit= card.suit}
    | Jack -> {rank=Queen; suit= card.suit}
    | Queen -> {rank=King; suit= card.suit}
    | King -> card

let previousRank (card:Card) =
    match card.rank with
    | Ace -> card
    | Two -> {rank=Ace; suit= card.suit}
    | Three -> {rank=Two; suit= card.suit}
    | Four -> {rank=Three; suit= card.suit}
    | Five -> {rank=Four; suit= card.suit}
    | Six -> {rank=Five; suit= card.suit}
    | Seven -> {rank=Six; suit= card.suit}
    | Eight -> {rank=Seven; suit= card.suit}
    | Nine -> {rank=Eight; suit= card.suit}
    | Ten -> {rank=Nine; suit= card.suit}
    | Jack -> {rank=Ten; suit= card.suit}
    | Queen -> {rank=Jack; suit= card.suit}
    | King -> {rank=Queen; suit= card.suit}

//total value of a hand
let totalValue (hand:Hand) =
    Seq.map (fun x -> CardValue x) hand
    |> Seq.sum

//checks if card rank or suit has potential for run or set
let totalSuit (hand:Hand) suit =
    Seq.filter (fun (x:Card) -> x.suit = suit) hand
    |> Seq.length

let totalRank (hand:Hand) rank =
    Seq.filter (fun (x:Card) -> x.rank = rank) hand
    |> Seq.length

let rankHasSet (hand:Hand) (card:Card) =
    ((totalRank hand card.rank) > 2)

let suitHasRun (hand:Hand) (card:Card) =
    ((totalSuit hand card.suit) > 2)

//returns a sequence of cards that are in sets
let cardHasSet (hand:Hand) set card =
    match (rankHasSet hand card) with
    | true -> card :: set
    | false -> set

let handSets (hand:Hand):Hand =
    let cards = List.empty<Card>
    Seq.map(fun x -> cardHasSet hand cards x) hand
    |> Seq.filter (fun x -> x.Length > 0)
    |> Seq.concat 
    |> Seq.sortBy (fun x -> x.rank)

//returns a sequence of cards that are in runs
let cardExists (hand:Hand) (card:Card) =
    Seq.exists ((=) card) hand

let previousRankExists (hand:Hand) card =
    match ((previousRank card) = card) with
    | true -> false
    | false -> cardExists hand (previousRank card)

let nextRankExists (hand:Hand) card =
    match ((nextRank card) = card) with
    | true -> false
    | false -> cardExists hand (nextRank card)

//returns a sequence of cards that are in a run in the hand
let nextCards (hand:Hand) =
    Seq.filter(fun x -> (nextRankExists hand x && previousRankExists hand x)) hand
    |> Seq.map(fun x -> nextRank x)

let previousCards (hand:Hand) =
    Seq.filter(fun x -> (nextRankExists hand x && previousRankExists hand x)) hand
    |> Seq.map(fun x -> previousRank x)

let centerCard (hand:Hand) =
     Seq.filter(fun x -> (nextRankExists hand x && previousRankExists hand x)) hand

let handRuns (hand:Hand):Hand =
    Seq.append (centerCard hand) (nextCards hand)
    |> Seq.append (previousCards hand)
    |> Seq.sortBy(fun x -> x.suit, x.rank)
    |> Seq.distinct

//Remove cards from hand
let removeCards(hand:Hand) (cards:Hand) =
    Seq.filter(fun x -> if (cardExists cards x) then false else true) hand

let runReduce (hand:Hand):Hand =
    handRuns hand
    |> removeCards hand
    |> Seq.sortBy(fun x -> x.suit, x.rank)

let setReduce (hand:Hand):Hand =
    handSets hand
    |> removeCards hand
    |> Seq.sortBy(fun x -> x.suit, x.rank)

//Remove runs first or sets first and return value
let removeSetsFirst (hand:Hand) =
    setReduce hand
    |> runReduce
    |> totalValue

let removeRunsFirst (hand:Hand) =
    runReduce hand
    |> setReduce
    |> totalValue

//Compare what gives best result
let simpleResult (hand:Hand) =
    match (removeSetsFirst hand > removeRunsFirst hand) with
    | true -> removeRunsFirst hand
    | false -> removeSetsFirst hand

//Break runs to suits
let sequenceOfSuits (hand:Hand) (suit:Suit) =
    Seq.filter(fun x -> x.suit = suit) hand
    |> handRuns

//Values of sets and Runs
let setValue (hand:Hand) =
    totalValue (handSets hand)

let runValue (hand:Hand) =
    totalValue (handRuns hand)

//Deadwood score
let Deadwood (hand:Hand) = 
    match Seq.length hand with
    | 10 -> simpleResult hand
    | _ -> totalValue hand
    // Fixme change so that it computes the actual deadwood score

let Score (firstOut:Hand) (secondOut:Hand) =
    Deadwood firstOut - Deadwood secondOut
    // Fixme change so that it computes how many points should be scored by the firstOut hand
    // (score should be negative if the secondOut hand is the winner)

// Add other functions related to Gin Rummy here ...