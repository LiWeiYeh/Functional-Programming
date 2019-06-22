module Tanks

type Gun =
  {
    Name              : string
    Penetration       : float
    Damage            : float
  }
  with
    static member Create(name: string, penetration : float, damage : float) =
      { Name = name; Penetration = penetration; Damage = damage }

type Tank =
  {
    Name    : string
    Weapon  : Gun
    Armor   : float
    Health  : float
  }
  with
    static member Create(name :string, weapon : Gun, armor : float, health : float) =
      {
        Name = name
        Weapon = weapon
        Armor = armor
        Health = health
      }
    
    member this.Shoot(tank : Tank) =
          if this.Weapon.Penetration > tank.Armor then
            printfn "%s shoots %s with %s causing %f damage --> HEALTH: %f" 
              this.Name 
              tank.Name 
              this.Weapon.Name 
              this.Weapon.Damage 
              tank.Health 
            { tank with Health = tank.Health - this.Weapon.Damage }
          else
            printfn "%s shoots %s with %s reducing armour by %f --> ARMOUR: %f" 
              this.Name 
              tank.Name 
              this.Weapon.Name 
              this.Weapon.Penetration
              tank.Armor
            { tank with Armor = tank.Armor - this.Weapon.Penetration }

let kwk36 = Gun.Create("88mm KwK 36", 150.0, 90.0)
let f32 = Gun.Create("76mm F-32", 70.0, 60.0)
let kwk40short = Gun.Create("75mm kwk 37", 35.5, 55.5)
let kwk40Long = Gun.Create("75mm KwK 40", 99.5, 55.5)
let m1a1 = Gun.Create("76mm M1A1", 99.0, 60.0)
let tiger = Tank.Create("Pz.Kpfw. VI Tiger Ausf. E", kwk36, 340.0, 800.0)
let t34 = Tank.Create("T-34/76", f32, 200.0, 400.0)
let p4f = Tank.Create("Pz.Kpfw. IV", kwk40short, 130.0, 350.0)
let p4g = Tank.Create("Pz.Kpfw. IV", kwk40Long, 130.0, 350.0)
let shermanE8 = Tank.Create("M4A3 Sherman E8", m1a1, 220.0, 450.0)



let rec fight (t1 : Tank) (t2 : Tank) =
  if t1.Health <= 0.0 then
    printfn "%s: KABOOOM!!! %s wins" t1.Name t2.Name
    t1,t2
  elif t2.Health <= 0.0 then
    printfn "%s: KABOOOM!!! %s wins" t2.Name t1.Name
    t1,t2
  else
    let t2 = t1.Shoot t2
    let t1 = t2.Shoot t1
    fight t1 t2


let testTanks() =
    fight(shermanE8)(t34)