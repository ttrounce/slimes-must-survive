package main

import "core:time"

WeaponType :: enum {
	SLIME_BOLT,
}

Weapon :: struct {
	fire_rate:   time.Duration,
	projectiles: int,
    spread:      f32,
	type:        WeaponType,
}

make_weapon :: proc(fire_rate: time.Duration, projectiles: int, spread: f32, type: WeaponType) -> Weapon {
	return Weapon{fire_rate, projectiles, spread, type}
}
