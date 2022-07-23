let show desc x = printfn "%s %s" desc x
let rand = System.Random()
let percentChange pc = rand.Next(0, 100) < pc

type Character = 
    {
        Name : string
        Health : int 
        State : CharacterState

    }
and CharacterState = 
| Normal
| Asleep of endTurn : int

type AttackType =
    | Fire of damage : int
    | Water of damage : int
    | Sleep

type RoundStage = 
    | PlayerOneAttack
    | PlayerTwoAttack
    | RoundEnd

with 
    override this.ToString() =
        match this with
        | PlayerOneAttack -> "Player one is attacking"
        | PlayerTwoAttack -> "Player two is attacking"
        | RoundEnd -> "Round over"

type GameState = 
    {
        PlayerOne : Character
        PlayerTwo : Character
        RoundNumber : int 
        Stage : RoundStage
    }

let rec progressStage attackType (state:GameState) = 
    let showBattleState text (state:GameState) = 
        if text <> "" then printfn "---%A---" text
        printfn "--- Round %A ---" state.RoundNumber
        printfn "--- Player one : %A ---" state.PlayerOne
        printfn "--- Player two : %A ---" state.PlayerTwo
        
    let randomAttack () = 
        match rand.Next(0, 2) with
        | 0 -> Fire (rand.Next(0, 50))
        | 1 -> Water (rand.Next(25, 40))
        | 2 -> Sleep
        | r -> failwithf "Random generator producter unexpected result %A" r

    let attack attackType state victim =
        match attackType with
        | Sleep -> { victim with State = if percentChange 50 then Asleep (state.RoundNumber + 2) else victim.State}
        | Fire damage
        | Water damage -> { victim with Health = victim.Health - damage}

    showBattleState "" state

    if (state.PlayerOne.Health <= 0) then
        printfn "Player Two won"
    elif (state.PlayerTwo.Health <= 0) then
        printfn "Player One won"
    else
        match state.Stage with
        | PlayerOneAttack ->  progressStage (randomAttack ()) { state with PlayerTwo = attack attackType state state.PlayerTwo; Stage = PlayerTwoAttack }
        | PlayerTwoAttack ->  progressStage (randomAttack ()) { state with PlayerOne = attack attackType state state.PlayerOne; Stage = RoundEnd }
        | RoundEnd -> progressStage (randomAttack ()) { state with RoundNumber = state.RoundNumber + 1; Stage = PlayerOneAttack }
        
{
    PlayerOne = { Name = "Character 1"; Health = 100; State = Normal }
    PlayerTwo ={ Name = "Character 2"; Health = 100; State = Normal }
    RoundNumber = 0
    Stage = PlayerOneAttack 
}
|> progressStage (Sleep)