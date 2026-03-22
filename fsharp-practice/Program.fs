namespace RpgGame

open System
open System.IO
open System.Linq

module TypeDefinitions =
    type EntityId = EntityId of int
    type DamageValue = DamageValue of int
    type Element =
        | Physical = 0
        | Fire = 1
        | Ice = 2
    type CombatStats = {MaxHp: int; Armor: int; MagicResist: int; Immunities: Set<Element>}
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
    
    type Inventory(size: int) =
        let items = Array.create size ""
        
        member this.Item
            with get(index) = items.[index]
            and set index value = items.[index] <- value
    
    exception MagicFizzleException of heroName: string * reason: string
    
module CombatLogic =
    open TypeDefinitions
    let (|Critical|Normal|Weak|) (dmg, armor) =
        if dmg > armor * 3 then Critical
        elif dmg <= armor then Weak
        else Normal
    let analyzeHistory (history: list<ActionType>): string =
        match history with
        | Attack _ :: Attack _ :: Attack _ :: _ ->
            "Ультует! (3 атаки подряд)"
        | (Heal _ | Flee) :: (Heal _ | Flee) :: _ ->
            "Пытается выжить"
        | _ ->
            "Действует тактически"
    let parseCommand (input: string) : Result<ActionType, string> =
        let parts = input.Split(' ')
        match parts with
        | [|"HEAL"; amountStr|] ->
            try
                let amount = int amountStr
                Ok(Heal amount)
            with
            | :? FormatException -> Error("Ошибка парсинга!")
        | [|"FLEE"|] -> Ok(Flee)
        | _ -> Error("Ошибка парсинга")
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
        let mutable level = 1
        let deathEvent = new Event<DeathDetails>()
        static let mutable globalActorCount = 0
        
        do
            globalActorCount <- globalActorCount + 1
            
        member this.Id = id
        member this.Name = name
        member this.Stats = stats
        member this.CurrentHp = currentHp
        member this.Level
            with get() = level
            and set(value) =
                if value < 1 then
                    raise (ArgumentOutOfRangeException("Уровень не может быть меньше 1!"))
                else
                    level <- value
        
        [<CLIEvent>]
        member this.OnDeath = deathEvent.Publish
        
        member internal this.TakeDamage(amount: int, turn: int) =
            if amount > 0 && currentHp > 0 then
                currentHp <- currentHp - amount
                if currentHp <= 0 then
                    currentHp <- 0
                    deathEvent.Trigger({VictimId = id; TurnNumber = turn; Reason = "Фатальный урон"})
        static member GetTotalActors() = globalActorCount
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
                | Attack(_, element) when stats.Immunities.Contains(element) ->
                    printfn "Иммунитет! %s игнорирует урон от стихии %A" this.Name element
                | Attack (DamageValue damageValue, element) ->
                    match (damageValue, stats.Armor) with
                    | Critical ->
                        let finalDmg = damageValue - stats.Armor
                        this.TakeDamage(finalDmg, round)
                        printfn "КРИТ! %s получил %d урона от стихии %A" this.Name finalDmg element
                    | Weak ->
                        printfn "Царапина! Броня %s поглотила весь урон" this.Name
                    | Normal ->
                        let finalDmg = damageValue - stats.Armor
                        this.TakeDamage(finalDmg, round)
                        printfn "%s получил %d урона от стихии %A" this.Name finalDmg element
                | Heal amount ->
                    currentHp <- System.Math.Min(stats.MaxHp, currentHp + amount)
                    printfn "%s исцелен на %d HP" this.Name amount
                | Flee -> printfn "[%s] пытается сбежать, но это невозможно!" this.Name
    
    type Hero(id: EntityId, name: string) =
        inherit Actor(id, name, {MaxHp = 100; Armor = 5; MagicResist = 10; Immunities = Set.empty})
        new(name: string) = new Hero(EntityId 99, name)
        
        override this.PerformTurn(_) =
            printfn "Герой %s атакует мечом!" this.Name
            Attack(DamageValue(35), Element.Physical)
            
    type Boss(id: EntityId, name: string) =
        inherit Actor(id, name, {MaxHp = 300; Armor = 15; MagicResist = 50; Immunities = Set.singleton Element.Fire})
        override this.PerformTurn(target) =
            printfn "БОСС %s кастует заклинание!" this.Name
            Attack(DamageValue(50), Element.Fire)


open Entities

module Program =
    open TypeDefinitions
    open Entities
    
    [<EntryPoint>]
    let main argv = 
        let paladin = new Hero("Артур")
        let dragon = new Boss(EntityId 2, "Дракон")
        
        let bag = new Inventory(5)
        bag.[0] <- "Зелье регенерации"
        bag.[1] <- "Зелье силы"
        
        let slot = bag.[0]
        printfn "%s" slot
        
        printfn "Сумка героя: %s, %s" bag.[0] bag.[1]
        
        let mutable battleHistory: Map<EntityId, list<ActionType>> = Map.empty
        
        battleHistory <- battleHistory.Add(paladin.Id, [])
        battleHistory <- battleHistory.Add(dragon.Id, [])
        
        try
            raise (MagicFizzleException(paladin.Name, "Закончилась мана!"))
        with
        | MagicFizzleException (name, reason) ->
            printfn "Уведомление: Маг %s не смог кастануть заклинание по причине: %s" name reason
        
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
            battleHistory <- battleHistory.Change(paladin.Id, (fun item ->
                    match item with
                    | Some list -> Some(heroAction :: list)
                    | None -> None))
            
            let commandResult = CombatLogic.parseCommand("HEAL 20")
            match commandResult with
            | Ok action ->
                let target = paladin :> ICombatant
                target.ReceiveAction action currentRound
                battleHistory <- battleHistory.Change(paladin.Id, (fun item ->
                    match item with
                    | Some list -> Some(action :: list)
                    | None -> None))
            | Error message ->
                printfn "Произошла ошибка: %s" message
                     
            if dragon.CurrentHp > 0 then
                let dragonAction = dragon.PerformTurn(paladin)
                let heroTarget = paladin :> ICombatant
                heroTarget.ReceiveAction dragonAction currentRound
                battleHistory <- battleHistory.Change(dragon.Id, (fun item ->
                    match item with
                    | Some list -> Some(dragonAction :: list)
                    | None -> None))
            
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
        
        let heroState = CombatLogic.analyzeHistory(battleHistory.[paladin.Id])
        printfn "Состояние героя: %s" heroState
        
        try
            try
                use writer = StreamWriter("battle_log.txt")
                for pair in battleHistory do
                    let (EntityId id) = pair.Key
                    writer.WriteLine(sprintf "Сущность с ID %d совершила действий: %d" id pair.Value.Length)
                raise(UnauthorizedAccessException("Ошибка записи!"))
            with
            | :? UnauthorizedAccessException as ex ->
                printfn "Ошибка записи: Нет прав на запись!"
            | :? IOException as ex ->
                printfn "Ошибка файловой системы: %s" ex.Message
        finally
            Console.ForegroundColor <- ConsoleColor.Cyan
            printfn "Попытка сохранения логов завершена. Ресурсы памяти уже освобождены"
            Console.ResetColor()
        0