package main

import "core:math"
import "core:math/rand"
import "core:time"

import b2 "vendor:box2d"

spawn_spear :: proc(game: ^Game) {
    distance := f32(125)
    angle := rand.float32_range(0, math.TAU)

    new_spear_pos :=
        b2.Body_GetPosition(game.special_entities.player.body) +
        ({math.cos(angle), math.sin(angle)} * distance)

    spear := new_entity(game, setup_entity_spear, new_spear_pos)
    spear_v := &spear.variant.(Variant_EnemySpear)
    spear_v.launch_time = time.time_add(time.now(), time.Millisecond * 1500)

    play_sound(game.sounds["spear_charging"], rand.float32_range(1.0, 1.3), 1.0)
}

spawn_skull :: proc(game: ^Game) {
    distance := rand.float32_range(200, 300)
    angle := rand.float32_range(0, math.TAU)

    new_skull_pos :=
        b2.Body_GetPosition(game.special_entities.player.body) +
        ({math.cos(angle), math.sin(angle)} * distance)
    skull := new_entity(game, setup_entity_skull, new_skull_pos)
}

spawner :: proc(game: ^Game) {
    if !is_player_alive(game) {
        return
    }

    switch {
    // case time.since(game.play_start_time) < 60 * time.Second:
    // 	spawner_stage_0(game)
    case: 
        spawner_stage_x(game)
    }
}

spawner_stage_0 :: proc(game: ^Game) {
    switch {
    case time.since(game.spawn_timers.next_skull) > 0:
        spawn_skull(game)
        game.spawn_timers.next_skull = time.time_add(time.now(), time.Second * 1)
    }
}

spawner_stage_x :: proc(game: ^Game) {
    switch {
    case time.since(game.spawn_timers.next_skull) > 0:
        spawn_skull(game)
        game.spawn_timers.next_skull = time.time_add(time.now(), time.Second * 1)
    case time.since(game.spawn_timers.next_spear) > 0:
        spawn_spear(game)
        game.spawn_timers.next_spear = time.time_add(time.now(), time.Second * 3)
    }
}
