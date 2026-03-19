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
        abstract member ReceiveAction: ActionType -> unit
    
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
            member this.ReceiveAction(action) =
                match action with
                | Attack (damageValue, element) ->
                    let calcFunc = createDamageCalculator 0 stats.Armor
                    let finalDmg = calcFunc damageValue
                    this.TakeDamage(finalDmg, 1)
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
            