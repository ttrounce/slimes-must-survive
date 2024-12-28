package main

import b2 "vendor:box2d"
import rl "vendor:raylib"
import rlgl "vendor:raylib/rlgl"

import "base:runtime"
import "core:fmt"
import "core:math"
import "core:math/linalg"
import "core:math/rand"
import "core:mem"
import "core:time"
import "util"

START_WINDOW_SIZE :: [2]i32{800, 600}
START_WINDOW_TITLE :: "Slimes Must Survive!"

MAX_ENTITIES :: 1024

main :: proc() {
    rl.SetConfigFlags({.VSYNC_HINT})
    rl.InitWindow(START_WINDOW_SIZE.x, START_WINDOW_SIZE.y, START_WINDOW_TITLE)
    defer rl.CloseWindow()

    rl.InitAudioDevice()
    defer rl.CloseAudioDevice()

    game := Game{}
    setup(&game)

    for !rl.WindowShouldClose() {
        rl.SetWindowTitle(fmt.ctprintf("%s %.1f", START_WINDOW_TITLE, 1.0 / rl.GetFrameTime()))
        rl.ClearBackground(rl.WHITE)

        update(&game)
        draw(&game)
    }
}

restart :: proc(game: ^Game) {
    clean(game)
    mem.set(game, 0, size_of(Game))
    setup(game)
}

play_sound :: proc(sound: rl.Sound, pitch: f32, volume: f32) {
    rl.SetSoundPitch(sound, pitch)
    rl.SetSoundVolume(sound, volume)
    rl.PlaySound(sound)
}

Game :: struct {
    camera:           rl.Camera2D,
    world:            b2.WorldId,
    textures:         map[string]rl.Texture2D,
    shaders:          map[string]rl.Shader,
    sounds:           map[string]rl.Sound,
    state:            GameState,
    entities:         [MAX_ENTITIES]Entity,
    handle_counter:   u32,
    debug:            bool,
    play_start_time:  time.Time,
    special_entities: struct {
        player: ^Entity,
    },
    spawn_timers:     struct {
        next_skull: time.Time,
        next_spear: time.Time,
    },
}


GameState :: union {
    GameState_Menu,
    GameState_Play,
}

GameState_Play :: struct {}
GameState_Menu :: struct {}

ContextWrapper :: struct {
    ctx: runtime.Context,
}

setup :: proc(game: ^Game) {
    b2.SetLengthUnitsPerMeter(16)

    world_def := b2.DefaultWorldDef()
    world_def.gravity = 0
    game.world = b2.CreateWorld(world_def)

    // wrapper := ContextWrapper{context}
    // b2.World_SetPreSolveCallback(game.world, presolve, &wrapper)

    game.camera = rl.Camera2D {
        target = {0, 0},
        offset = {f32(rl.GetScreenWidth() / 2), f32(rl.GetScreenHeight() / 2)},
        zoom   = 2.0,
    }

    game.textures = make(map[string]rl.Texture2D)
    game.textures["slime"] = rl.LoadTexture("assets/slime.png")
    game.textures["floor"] = rl.LoadTexture("assets/floor.png")
    game.textures["spear"] = rl.LoadTexture("assets/spear.png")
    game.textures["skull"] = rl.LoadTexture("assets/skull.png")
    game.textures["slime_bolt"] = rl.LoadTexture("assets/slime_bolt.png")

    game.shaders = make(map[string]rl.Shader)
    game.shaders["pixel_glow"] = rl.LoadShader(nil, "shaders/pixel_glow.fs")
    game.shaders["mix"] = rl.LoadShader(nil, "shaders/mix.fs")

    game.sounds = make(map[string]rl.Sound)
    game.sounds["hit"] = rl.LoadSound("assets/hit.ogg")
    game.sounds["skull_hit"] = rl.LoadSound("assets/skull_hit.ogg")
    game.sounds["slime_bolt"] = rl.LoadSound("assets/slime_bolt.ogg")
    game.sounds["spear_charging"] = rl.LoadSound("assets/spear_charging.ogg")

    game.special_entities.player = new_entity(game, setup_entity_player)

    game.play_start_time = time.now()
}

clean :: proc(game: ^Game) {
    b2.DestroyWorld(game.world)

    for _, texture in game.textures {
        rl.UnloadTexture(texture)
    }

    for _, shader in game.shaders {
        rl.UnloadShader(shader)
    }

    for _, sound in game.sounds {
        rl.UnloadSound(sound)
    }

    delete(game.textures)
    delete(game.shaders)
}

update :: proc(game: ^Game) {
    b2.World_Step(game.world, 1.0 / 60.0, 4)

    update_events(game)

    if rl.IsKeyPressed(.F2) {game.debug = !game.debug}
    if !is_player_alive(game) && rl.IsKeyPressed(.SPACE) {
        restart(game)
        return
    }

    spawner(game)

    for &e in game.entities {
        if .ALLOCATED not_in e.flags {continue}

        update_entity(game, &e)
    }
}

update_events :: proc(game: ^Game) {
    sensor_events := b2.World_GetSensorEvents(game.world)
    for ev in sensor_events.beginEvents[:sensor_events.beginCount] {

        sensor := cast(^Entity)b2.Shape_GetUserData(ev.sensorShapeId)
        visitor := cast(^Entity)b2.Shape_GetUserData(ev.visitorShapeId)

        if spear, is_spear := sensor.variant.(Variant_EnemySpear); is_spear {
            if health, has_health := &visitor.health.?; (has_health && health.points > 0) {
                play_sound(game.sounds["hit"], rand.float32_range(1.4, 1.6), 0.2)
                health.points -= 1
                visitor.hit_time = time.now()
            }
            b2.Body_SetLinearDamping(sensor.body, 3.5)
        }
    }

    is_contact_pair :: proc(
        e0: ^Entity,
        e1: ^Entity,
        $T0: typeid,
        $T1: typeid,
    ) -> (
        ^Entity,
        ^Entity,
        bool,
    ) {
        _, e0_t0 := e0.variant.(T0)
        _, e1_t1 := e1.variant.(T1)
        _, e0_t1 := e0.variant.(T1)
        _, e1_t0 := e1.variant.(T0)
        if e0_t0 && e1_t1 {return e0, e1, true}
        if e0_t1 && e1_t0 {return e1, e0, true}
        return nil, nil, false
    }

    contact_events := b2.World_GetContactEvents(game.world)
    for ev in contact_events.beginEvents[:contact_events.beginCount] {
        e0 := cast(^Entity)b2.Shape_GetUserData(ev.shapeIdA)
        e1 := cast(^Entity)b2.Shape_GetUserData(ev.shapeIdB)

        if skull, player, ok := is_contact_pair(e0, e1, Variant_EnemySkull, Variant_Player); ok {
            if health, has_health := &e0.health.?; (has_health && health.points > 0) {
                play_sound(game.sounds["hit"], rand.float32_range(1.4, 1.6), 0.2)
                health.points -= 1
                e0.hit_time = time.now()
            }
        }

        if skull, projectile, ok := is_contact_pair(
            e0,
            e1,
            Variant_EnemySkull,
            Variant_Projectile,
        ); ok {
            if health, has_health := &skull.health.?; (has_health && health.points > 0) {
                play_sound(game.sounds["skull_hit"], rand.float32_range(0.9, 1.1), 1.0)
                health.points -= 1
                skull.hit_time = time.now()
            }
            kill_entity(projectile)
        }
    }
}

update_entity :: proc(game: ^Game, e: ^Entity) {
    pos := b2.Body_GetPosition(e.body)

    hit_length :: time.Millisecond * 250
    if time.since(e.hit_time) < hit_length {
        end := time.diff(e.hit_time, time.time_add(e.hit_time, hit_length))
        now := time.diff(e.hit_time, time.now())
        step := math.smoothstep(0.0, time.duration_seconds(end), time.duration_seconds(now))
        e.shader = make_shader_mix(
            rl.WHITE,
            f32(math.min(math.cos(math.lerp(0.0, math.PI / 2, step)), 1)),
        )
    }

    if util.is_some(e.health) && (!util.is_some(e.expiry) && e.health.?.points <= 0) {
        if _, is_player := e.variant.(Variant_Player); !is_player {
            e.expiry = time.time_add(time.now(), time.Millisecond*200)
        }
    }

    if util.is_some(e.expiry) && time.since(e.expiry.?) > 0 {
        kill_entity(e)
        return
    }
   
    #partial switch &v in e.variant {
    case Variant_Player:
        if !is_player_alive(game) {
            break
        }

        game.camera.offset = {f32(rl.GetScreenWidth()) / 2, f32(rl.GetScreenHeight()) / 2}
        game.camera.target = linalg.lerp(game.camera.target, pos, 0.5)
        game.camera.zoom = 2

        aim_and_shoot(game)

        mv := rl.Vector2{}
        if rl.IsKeyDown(.A) {mv.x -= 1}
        if rl.IsKeyDown(.D) {mv.x += 1}
        if rl.IsKeyDown(.W) {mv.y -= 1}
        if rl.IsKeyDown(.S) {mv.y += 1}

        if b2.LengthSquared(mv) > 0 {
            b2.Body_ApplyForceToCenter(e.body, b2.Normalize(mv) * v.force, true)
        }
    case Variant_EnemySpear:
        if !is_player_alive(game) {
            break
        }

        player_pos := b2.Body_GetPosition(game.special_entities.player.body)
        vec_to_player := b2.Normalize(player_pos - pos)

        if !v.launched {
            if time.since(v.launch_time) > 0 {
                v.launched = true
                b2.Body_SetAngularVelocity(e.body, 0)
                b2.Body_ApplyLinearImpulseToCenter(e.body, vec_to_player * v.force, true)
            } else {
                impulse := b2.ComputeAngularVelocity(
                    b2.Body_GetRotation(e.body),
                    b2.MakeRot(math.atan2(vec_to_player.y, vec_to_player.x)),
                    10,
                )
                b2.Body_SetAngularVelocity(e.body, impulse)
            }
        }
    case Variant_EnemySkull:
        player_pos := b2.Body_GetPosition(game.special_entities.player.body)
        vec_to_player := b2.Normalize(player_pos - pos)

        impulse := b2.ComputeAngularVelocity(
            b2.Body_GetRotation(e.body),
            b2.MakeRot(math.atan2(vec_to_player.y, vec_to_player.x)),
            10,
        )

        if is_player_alive(game) {
            b2.Body_SetAngularVelocity(e.body, impulse)
            b2.Body_ApplyForceToCenter(e.body, vec_to_player * v.force, true)
        } else {
            b2.Body_SetAngularVelocity(e.body, -impulse)
            b2.Body_ApplyForceToCenter(e.body, -vec_to_player * v.force, true)
        }
    }
}

presolve :: proc "c" (
    first: b2.ShapeId,
    second: b2.ShapeId,
    manifold: ^b2.Manifold,
    ctx: rawptr,
) -> bool {
    wrapper := cast(^ContextWrapper)ctx
    context = wrapper.ctx

    entity_first := cast(^Entity)b2.Shape_GetUserData(first)
    entity_second := cast(^Entity)b2.Shape_GetUserData(second)
    if spear, is_spear := entity_first.variant.(Variant_EnemySpear); is_spear {
        if player, is_player := entity_second.variant.(Variant_Player); is_player {
            b2.Body_Disable(entity_first.body)
            return true
        }
    }
    return true
}

draw :: proc(game: ^Game) {
    rl.BeginMode2D(game.camera)
    for x: i32 = -32; x < 32; x += 1 {
        for y: i32 = -32; y < 32; y += 1 {
            rl.DrawTexture(game.textures["floor"], x * 32, y * 32, rl.WHITE)
        }
    }

    for &e in game.entities {
        if .ALLOCATED in e.flags {
            draw_entity(game, &e)
        }
    }
    rl.EndMode2D()

    draw_ui(game)
    rl.EndDrawing()
}

draw_entity :: proc(game: ^Game, e: ^Entity) {
    pos := b2.Body_GetPosition(e.body)
    rot := b2.Body_GetRotation(e.body)

    #partial switch v in e.variant {
    case Variant_Player:
        if game.debug {
            for i := f32(50.0); i <= 200; i += 50 {
                rl.DrawCircleLinesV(pos, i, rl.ColorAlpha(rl.WHITE, 0.25))
                text := fmt.ctprint(i)
                p := rl.GetWorldToScreen2D(rl.Vector2{pos.x - i + 2, pos.y}, game.camera)
                rl.EndMode2D()
                rl.DrawText(text, i32(p.x), i32(p.y), 12, rl.WHITE)
                rl.BeginMode2D(game.camera)
            }
        }
    }

    if texture, texture_found := game.textures[e.sprite.id]; texture_found {
        entity_rect := rl.Rectangle{pos.x, pos.y, e.sprite.uv.width, e.sprite.uv.height}

        shader, shader_info, using_shader := get_entity_shader(game, e)
        if using_shader {
            rl.BeginShaderMode(shader^)
            shader_info.load_uniforms(shader^, shader_info^)
        }

        tint := rl.WHITE
        uv := e.sprite.uv

        if e.sprite.keep_upright {
            angle := b2.Rot_GetAngle(b2.Body_GetRotation(e.body))
            if angle > math.PI / 2.0 || angle < -math.PI / 2.0 {
                uv.y = uv.y + uv.height
                uv.height = -uv.height
            }
        }

        if util.is_some(e.fade_duration) {
            fade_duration := e.fade_duration.?

            if time.since(e.birth) < fade_duration {
                fade_in_step := math.smoothstep(0.0, time.duration_seconds(fade_duration), time.duration_seconds(time.since(e.birth)))
                tint = rl.ColorAlpha(tint, f32(math.sin(math.lerp(0.0, math.PI/2, fade_in_step))))
            }
            else if util.is_some(e.expiry) {
                d := time.duration_seconds(time.since(time.time_add(e.expiry.?, -fade_duration)))
                fade_out_step := math.smoothstep(0.0, time.duration_seconds(fade_duration), d)
                tint = rl.ColorAlpha(tint, f32(math.cos(math.lerp(0.0, math.PI/2.0, fade_out_step))))
            }
        }

        #partial switch v in e.variant {
        case Variant_EnemySpear:
            until_launch_ms := time.since(v.launch_time) / time.Millisecond
            switch {
            case until_launch_ms > -50:
                uv.x = 96
            case until_launch_ms > -100 && until_launch_ms < 0:
                uv.x = 64
            case until_launch_ms > -150 && until_launch_ms < 0:
                uv.x = 32
            }
        case Variant_Player:
            if health, has_health := e.health.?; has_health {
                if health.points <= 0 {
                    uv.x = 64
                }
            }
        }

        rl.DrawTexturePro(
            texture,
            uv,
            entity_rect,
            e.sprite.uv_origin,
            math.to_degrees(e.sprite.uv_rot + b2.Rot_GetAngle(rot)),
            tint,
        )

        if using_shader {
            rl.EndShaderMode()
        }
    }

    if game.debug {
        #partial switch b2.Shape_GetType(e.shape) {
        case .circleShape:
            circle := b2.Shape_GetCircle(e.shape)
            rl.DrawCircleLinesV(pos + circle.center, circle.radius, rl.WHITE)
        case .polygonShape:
            polygon := b2.Shape_GetPolygon(e.shape)
            angle := b2.Rot_GetAngle(b2.Body_GetRotation(e.body))
            hull := b2.ComputeHull(polygon.vertices[:polygon.count])
            for &v in hull.points {v = rl.Vector2Rotate(v, angle) + pos}
            rl.DrawLineStrip(raw_data(hull.points[:]), hull.count, rl.WHITE)
            rl.DrawLineV(hull.points[0], hull.points[hull.count - 1], rl.WHITE)
        }
    }
}

draw_ui :: proc(game: ^Game) {
    player := game.special_entities.player
    if health, has_health := player.health.?; has_health {
        for hp := 0; hp <= health.points; hp += 1 {
            rl.DrawTexturePro(
                game.textures[player.sprite.id],
                player.sprite.uv,
                rl.Rectangle{f32(rl.GetScreenWidth() - i32(hp * 32)), 0, 32, 32},
                {},
                0,
                rl.ColorAlpha(rl.WHITE, 0.5),
            )
        }


        time_buf := [time.MIN_HMS_LEN]u8{}
        time := time.duration_to_string_hms(time.since(game.play_start_time), time_buf[:])
        rl.DrawText(fmt.ctprint(time), 0, 0, 18, rl.WHITE)

        if health.points <= 0 {
            rl.DrawRectangle(
                0,
                0,
                rl.GetScreenWidth(),
                rl.GetScreenHeight(),
                rl.ColorAlpha(rl.BLACK, 0.2),
            )
            death_text :: "You've reverted to a puddle..."
            death_text_size :: 40
            rl.DrawText(
                death_text,
                (rl.GetScreenWidth() - rl.MeasureText(death_text, death_text_size)) / 2.0,
                rl.GetScreenHeight() / 4.0,
                death_text_size,
                rl.WHITE,
            )

            death_restart_text :: "Press SPACE to restart."
            death_restart_text_size :: 20
            rl.DrawText(
                death_restart_text,
                (rl.GetScreenWidth() -
                    rl.MeasureText(death_restart_text, death_restart_text_size)) /
                2.0,
                death_text_size + rl.GetScreenHeight() / 4.0,
                death_restart_text_size,
                rl.WHITE,
            )
        }
    }
}

EntitySetupProc :: proc(e: ^Entity, world: b2.WorldId, pos := b2.Vec2{}, angle := b2.Rot_identity)

new_entity :: proc(
    game: ^Game,
    setup_proc: EntitySetupProc,
    pos := b2.Vec2{},
    angle := b2.Rot_identity,
) -> ^Entity {
    new_entity: ^Entity
    for &e in game.entities {
        if .ALLOCATED not_in e.flags {
            new_entity = &e
        }
    }

    if new_entity == nil {
        return nil
    }
    new_entity.flags = {.ALLOCATED}
    new_entity.birth = time.now()
    setup_proc(new_entity, game.world, pos, angle)
    return new_entity
}

kill_entity :: proc(e: ^Entity) {
    b2.DestroyBody(e.body)
    mem.set(e, 0, size_of(Entity))
}

aim_and_shoot :: proc(game: ^Game) {
    player := game.special_entities.player
    variant := &player.variant.(Variant_Player)

    vec_to_mouse := b2.Normalize(
        rl.GetScreenToWorld2D(rl.GetMousePosition(), game.camera) -
        b2.Body_GetPosition(player.body),
    )

    if time.since(variant.last_shot) < variant.weapon.fire_rate {
        return
    }

    switch variant.weapon.type {
    case .SLIME_BOLT:
        if (rl.IsMouseButtonDown(.LEFT)) {
            bullet := new_entity(
                game,
                setup_entity_slime_bolt,
                b2.Body_GetPosition(player.body),
                b2.MakeRot(math.atan2(vec_to_mouse.y, vec_to_mouse.x)),
            )
            b2.Body_ApplyLinearImpulseToCenter(
                bullet.body,
                vec_to_mouse * bullet.variant.(Variant_Projectile).force,
                true,
            )
            rl.SetSoundPitch(game.sounds["slime_bolt"], rand.float32_range(0.9, 1.1))
            rl.PlaySound(game.sounds["slime_bolt"])

            variant.last_shot = time.now()
        }
    }
}

is_player_alive :: proc(game: ^Game) -> bool {
    return game.special_entities.player.health.?.points > 0
}