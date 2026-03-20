namespace RpgGame

open System

module TypeDefinitions =
    type EntityId = EntityId of int
    type DamageValue = DamageValue of int
    type Element =
        | Physical = 0
        | Fire = 1
        | Ice = 2
    type CombatStats = {MaxHp: int; Armor: int; MagicResist: int}
    type DeathDetails = {VictimId: EntityId; TurnNumber: int; Reason: string}
    type ActionType =
        | Attack of damage: DamageValue * element: Element
        | Heal of int
        | Flee
        
    [<Struct>]
    type Vector3D(x: float, y: float, z: float) =
        member this.X = x
        member this.Y = y
        member this.Z = z
    
module CombatLogic =
    open TypeDefinitions
    
    let generateSoulKey (EntityId id) secret =
        ((id <<< 4) ^^^ (secret >>> 2)) &&& 0xFF
    let calcMana baseInt level =
        let rec loop currentLevel acc =
            if currentLevel <= 0 then
                acc
            else
                loop (currentLevel - 1) (acc + (baseInt * 2))
        loop level 0
    let reduceByArmor armor (DamageValue damage) = System.Math.Max(0, damage - armor)
    let addBonusDamage bonus damage = bonus + damage
    let createDamageCalculator bonus armor =
        reduceByArmor armor >> addBonusDamage bonus
    
module Entities =
    open TypeDefinitions
    open CombatLogic
    
    type ICombatant =
        abstract member ReceiveAction: ActionType -> int -> unit
    
    [<AbstractClass>]
    type Actor(id: EntityId, name: string, stats: CombatStats) =
        let mutable currentHp = stats.MaxHp
        let mutable isDefending = false
        let deathEvent = new Event<DeathDetails>()
        
        member this.Id = id
        member this.Name = name
        member this.Stats = stats
        member this.CurrentHp = currentHp
        
        [<CLIEvent>]
        member this.OnDeath = deathEvent.Publish
        
        member internal this.TakeDamage(amount: int, turn: int) =
            if amount > 0 && currentHp > 0 then
                currentHp <- currentHp - amount
                if currentHp <= 0 then
                    currentHp <- 0
                    deathEvent.Trigger({VictimId = id; TurnNumber = turn; Reason = "Фатальный урон"})
        
        abstract member PerformTurn: ICombatant -> ActionType
        
        member this.Defend() = isDefending <- true
        override this.Equals(obj) =
            match obj with
            | :? Actor as other -> this.Id = other.Id
            | _ -> false
        override this.GetHashCode() = hash this.Id
        
        interface ICombatant with
            member this.ReceiveAction action round =
                match action with
                | Attack (damageValue, element) ->
                    let calcFunc = createDamageCalculator 0 stats.Armor
                    let finalDmg = calcFunc damageValue
                    this.TakeDamage(finalDmg, round)
                    printfn "%s получил %d урона от стихии %A" this.Name finalDmg element
                | Heal amount ->
                    currentHp <- System.Math.Min(stats.MaxHp, currentHp + amount)
                    printfn "%s исцелен на %d HP" this.Name amount
                | Flee -> printfn "[%s] пытается сбежать, но это невозможно!" this.Name
    
    type Hero(id: EntityId, name: string) =
        inherit Actor(id, name, {MaxHp = 100; Armor = 5; MagicResist = 10})
        override this.PerformTurn(_) =
            printfn "Герой %s атакует мечом!" this.Name
            Attack(DamageValue(35), Element.Physical)
            
    type Boss(id: EntityId, name: string) =
        inherit Actor(id, name, {MaxHp = 300; Armor = 15; MagicResist = 50})
        override this.PerformTurn(target) =
            printfn "БОСС %s кастует заклинание!" this.Name
            Attack(DamageValue(50), Element.Fire)


open Entities

module Program =
    open TypeDefinitions
    open Entities
    
    [<EntryPoint>]
    let main argv =
        let paladin = new Hero(EntityId 1, "Артур")
        let dragon = new Boss(EntityId 2, "Дракон")
        
        paladin.OnDeath.Add(fun details -> printfn "Герой пал на %d ходу. Причина: %s" details.TurnNumber details.Reason)
        dragon.OnDeath.Add(fun details -> printfn "Дракон повержен! ID: %A" details.VictimId)
        
        let fakePaladin = new Hero(EntityId 1, "Клон")
        printfn "Паладин и Клон одна сущность? %b" (paladin = fakePaladin)
        
        let combatantList: list<ICombatant> = [paladin :> ICombatant; dragon :> ICombatant]
        let roundCounter = Seq.initInfinite(fun i -> i+1)
        
        use enumerator = roundCounter.GetEnumerator()
        while paladin.CurrentHp > 0 && dragon.CurrentHp > 0 do
            enumerator.MoveNext() |> ignore
            
            let currentRound = enumerator.Current
            printfn "Раунд %d" currentRound
            
            let heroAction = paladin.PerformTurn(dragon)
            let dragonTarget = dragon :> ICombatant
            dragonTarget.ReceiveAction heroAction currentRound
            
            if dragon.CurrentHp > 0 then
                let dragonAction = dragon.PerformTurn(paladin)
                let heroTarget = paladin :> ICombatant
                heroTarget.ReceiveAction dragonAction currentRound
            
        for i = 1 to 3 do
            let mana = CombatLogic.calcMana 10 i
            printfn "Мана паладина на %d уровне: %d" i mana
            
        let survivors =
            combatantList |> List.filter(fun combatant ->
                let actor = combatant :?> Actor
                actor.CurrentHp > 0)
        
        
        for survivor in survivors do
            let actor = survivor :?> Actor
            printfn "Выжил %s с HP: %d" actor.Name actor.CurrentHp
            
        let secretCode = CombatLogic.generateSoulKey paladin.Id 4096
        printfn "Секретный код души победителя: %d" secretCode
        
        0