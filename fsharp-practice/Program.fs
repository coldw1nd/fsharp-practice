namespace RpgGame

module GameLogic =
    let bitwiseKey id secret = (id <<< 2) &&& secret
    let rec calcXp level =
        if level <= 1 then 100
        else calcXp (level - 1) + (level * 50)
        
    let addHealth amount hp = hp + amount
    let multiplyHealth factor hp = factor * hp
    let applyBuffs currentHp = currentHp |> addHealth 50 |> multiplyHealth 2
    let buffCombo = addHealth 20 >> multiplyHealth 3