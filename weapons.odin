package main

import "core:time"

WeaponType :: enum {
    SLIME_BOLT,
}

Weapon :: struct {
    fire_rate: time.Duration,
    type:      WeaponType,
}

make_weapon :: proc(fire_rate: time.Duration, type: WeaponType) -> Weapon {
    return Weapon{fire_rate, type}
}
