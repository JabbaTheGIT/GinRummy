module Cards

type Suit = Spades | Clubs | Hearts | Diamonds
type Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type Card = { suit: Suit; rank: Rank}

type Hand = Card seq
type Deck = Card seq

let AllSuits = [ Spades; Clubs; Hearts; Diamonds ]
let AllRanks = [ Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King ]

let allCards = 
    seq { 
        for s in AllSuits do
            for r in AllRanks do
                yield {suit=s; rank=r}
    }
    
let FullDeck = 
    allCards

//Random Function
let rnd = System.Random()

//Given Check duplicates function
let CheckDuplicates cards = 
    let duplicates = cards |> Seq.groupBy id |> Seq.map snd |> Seq.exists (fun s -> (Seq.length s) > 1)
    if (duplicates) then
        raise(new System.Exception "duplicates found!")

//Get random card from a list
//Length issue
let rndCard (cardList:List<Card>) =     
    List.nth cardList (rnd.Next(List.length cardList))

let addCard list card = 
    List.append list [card]

let rec removeCard list card = 
    match list with
    | h :: tail when h = card -> tail
    | h :: tail -> h :: (removeCard tail card)
    | [] -> [] 

let rec shuff list =
    let card = rndCard list
    seq {yield card;     
         yield! shuff (removeCard list card)}


//Shuffle the deck
let Shuffle (deck:Deck) =  
    shuff (Seq.toList deck)
    |> Seq.take 52

// Add other functions here related to Card Games ...
    
let printSqn mySeq = 
    printfn "%A" (Seq.toList mySeq)