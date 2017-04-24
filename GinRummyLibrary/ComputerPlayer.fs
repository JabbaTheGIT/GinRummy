module ComputerPlayer

open Cards
open GinRummy

type Move = Gin | Knock | Continue

//Value in hand when adding another card
let handValue (hand:Hand) (card:Card) =
    Seq.append hand [card]
    |> Deadwood

let possibleDeckTotalVal compHand possibleDeck =
    Seq.map(fun x -> Seq.append compHand [x]) possibleDeck
    |> Seq.map(fun x -> Deadwood x)
    |> Seq.sum

let ComputerPickupDiscard computerHand topDiscard possibleDeck =
    let possDeckVal = (possibleDeckTotalVal computerHand possibleDeck) / (Seq.length computerHand + 1)
    let possTopDiscardVal = handValue computerHand topDiscard
    
    (possDeckVal < possTopDiscardVal)

    // Fixme: change function so that it computes if Computer should pickup from Discard pile 
    //        or draw a fresh card from the deck

let returnCards (newHand:Hand) = 
    let discardCard = Seq.minBy (fun x -> Deadwood (removeCard (Seq.toList newHand) x)) newHand
    let hand = removeCard (Seq.toList newHand) discardCard
    //(Continue, Some card)
    match (Deadwood hand) with
    | i when i < 11 && i > 0 -> (Knock, Some discardCard)
    | i when i > 10 -> (Continue, Some discardCard)
    | i when i = 0 -> (Gin, Some discardCard)

let ComputerMove (newHand:Hand) =
    let dw = Deadwood newHand
    match dw with
    | 0 -> (Gin, None)
    | _ -> returnCards newHand
    // Fixme: change function so that it computes which action the Computer should take: Continue, Knock or Gin 
    //        and which card would be best to discard
