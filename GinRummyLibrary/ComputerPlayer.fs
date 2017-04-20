module ComputerPlayer

open Cards
open GinRummy

type Move = Gin | Knock | Continue

//Value in hand when adding another card
let handValue (hand:Hand) (card:Card) =
    Seq.append hand [card]
    |> Deadwood

let ComputerPickupDiscard computerHand topDiscard possibleDeck =
    true
    // Fixme: change function so that it computes if Computer should pickup from Discard pile 
    //        or draw a fresh card from the deck

let ComputerMove newHand =
    let card = Seq.head newHand
    //(Continue, Some card)
    match (Deadwood newHand) with
    | i when i < 11 && i > 0 -> (Knock, Some card)
    | i when i > 10 -> (Continue, Some card)
    | i when i = 0 -> (Gin, None)
    // Fixme: change function so that it computes which action the Computer should take: Continue, Knock or Gin 
    //        and which card would be best to discard
