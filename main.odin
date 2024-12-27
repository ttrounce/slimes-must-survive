package main

import b2 "vendor:box2d"
import rl "vendor:raylib"
import rlgl "vendor:raylib/rlgl"

import "core:fmt"
import "core:math"
import "core:math/linalg"
import "core:math/rand"
import "base:runtime"
import "core:mem"
import "core:time"

START_WINDOW_SIZE :: [2]i32{800, 600}
START_WINDOW_TITLE :: "Slimes Must Survive!"

MAX_ENTITIES :: 1024

SPAWN_RANGE_MAX :: 150
SPAWN_RANGE_MIN :: 100

main :: proc() {
	rl.SetConfigFlags({.VSYNC_HINT})
	rl.InitWindow(START_WINDOW_SIZE.x, START_WINDOW_SIZE.y, START_WINDOW_TITLE)
	defer rl.CloseWindow()

	game := Game{}
	setup(&game)

	for !rl.WindowShouldClose() {
		rl.ClearBackground(rl.WHITE)

		update(&game)
		draw(&game)
	}
}

Game :: struct {
	camera:           rl.Camera2D,
	world:            b2.WorldId,
	textures:         map[string]rl.Texture2D,
	shaders:          map[string]rl.Shader,
	state:            GameState,
	entities:         [MAX_ENTITIES]Entity,
	handle_counter:   u32,
	debug:            bool,
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

Entity :: struct {
	sprite:    string,
	shader:    Maybe(ShaderInfo),
	uv:        rl.Rectangle,
	uv_origin: rl.Vector2,
	uv_rot:    f32,
	variant:   EntityVariant,
	flags:     bit_set[EntityFlags],
	body:      b2.BodyId,
	shape:     b2.ShapeId,
	expiry:    Maybe(time.Time),
	birth:     time.Time,
	health:    Maybe(Health),
}

EntityFlags :: enum {
	ALLOCATED,
}

EntityVariant :: union {
	Variant_Player,
	Variant_EnemySkull,
	Variant_EnemySpear,
}

Variant_Player :: struct {
	force: f32,
}

Variant_EnemySpear :: struct {
	force:       f32,
	launch_time: time.Time,
	launched:    bool,
}

Variant_EnemySkull :: struct {
	force: f32,
}

Health :: struct {
	points: i32,
	max:    i32,
}

ContextWrapper :: struct {
    ctx: runtime.Context
}

setup :: proc(game: ^Game) {
	b2.SetLengthUnitsPerMeter(16)

	world_def := b2.DefaultWorldDef()
	world_def.gravity = 0
	game.world = b2.CreateWorld(world_def)

    wrapper := ContextWrapper {context}
    b2.World_SetPreSolveCallback(game.world, presolve, &wrapper)

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

	game.shaders = make(map[string]rl.Shader)
	game.shaders["pixel_glow"] = rl.LoadShader(nil, "shaders/pixel_glow.fs")

	game.special_entities.player = new_entity(game, setup_entity_player)
}

update :: proc(game: ^Game) {
	b2.World_Step(game.world, 1.0 / 60.0, 4)

	if rl.IsKeyPressed(.F2) {game.debug = !game.debug}

	spawner(game)

	for &e in game.entities {
		if .ALLOCATED not_in e.flags {continue}

		update_entity(game, &e)
	}
}

update_entity :: proc(game: ^Game, e: ^Entity) {
	pos := b2.Body_GetPosition(e.body)

	if expiry, has_expiry := e.expiry.?; has_expiry && time.since(expiry) > 0 {
		kill_entity(e)
		return
	}

	if health, has_health := e.health.?; has_health && health.points < 0 {
		kill_entity(e)
		return
	}

	#partial switch &v in e.variant {
	case Variant_Player:
		game.camera.offset = {f32(rl.GetScreenWidth()) / 2, f32(rl.GetScreenHeight()) / 2}
		game.camera.target = linalg.lerp(game.camera.target, pos, 0.5)
		game.camera.zoom = 2

		mv := rl.Vector2{}
		if rl.IsKeyDown(.A) {mv.x -= 1}
		if rl.IsKeyDown(.D) {mv.x += 1}
		if rl.IsKeyDown(.W) {mv.y -= 1}
		if rl.IsKeyDown(.S) {mv.y += 1}

		if b2.LengthSquared(mv) > 0 {
			b2.Body_ApplyForceToCenter(e.body, b2.Normalize(mv) * v.force, true)
		}
	case Variant_EnemySpear:
		player_pos := b2.Body_GetPosition(game.special_entities.player.body)
		vec_to_player := b2.Normalize(player_pos - pos)

		if !v.launched {
			if time.since(v.launch_time) > 0 {
				v.launched = true
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

		b2.Body_ApplyForceToCenter(e.body, vec_to_player * v.force, true)
	}
}

presolve :: proc "c" (first: b2.ShapeId, second: b2.ShapeId, manifold: ^b2.Manifold, ctx: rawptr) -> bool {
    wrapper := cast(^ContextWrapper) ctx
    context = wrapper.ctx

    entity_first := cast(^Entity) b2.Shape_GetUserData(first)
    entity_second := cast(^Entity) b2.Shape_GetUserData(second)
    if spear, is_spear := entity_first.variant.(Variant_EnemySpear); is_spear {
        if player, is_player := entity_second.variant.(Variant_Player); is_player {
            return false
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
	rl.EndDrawing()
}

draw_entity :: proc(game: ^Game, e: ^Entity) {
	pos := b2.Body_GetPosition(e.body)
	rot := b2.Body_GetRotation(e.body)

	#partial switch v in e.variant {
	case Variant_Player:
		if game.debug {
			rl.DrawCircleLinesV(pos, SPAWN_RANGE_MIN, rl.ColorAlpha(rl.WHITE, 0.25))
			rl.DrawText(
				fmt.ctprint(SPAWN_RANGE_MIN),
				i32(pos.x) + SPAWN_RANGE_MIN,
				i32(pos.y),
				1,
				rl.WHITE,
			)
			rl.DrawCircleLinesV(pos, SPAWN_RANGE_MAX, rl.ColorAlpha(rl.WHITE, 0.25))
		}
	}

	if texture, texture_found := game.textures[e.sprite]; texture_found {
		entity_rect := rl.Rectangle{pos.x, pos.y, e.uv.width, e.uv.height}

		shader, shader_info, using_shader := get_entity_shader(game, e)
		if using_shader {
			rl.BeginShaderMode(shader^)
			shader_info.load_uniforms(shader^, shader_info^)
		}

		tint := rl.WHITE

		#partial switch v in e.variant {
		case Variant_EnemySpear:
			step := math.smoothstep(
				0.0,
				time.duration_seconds(time.diff(e.birth, e.expiry.?)),
				time.duration_seconds(time.diff(e.birth, time.now())),
			)
			tint = rl.ColorAlpha(tint, f32(math.min(5*math.sin(math.lerp(0.0, math.PI, step)), 1)))
		}

		rl.DrawTexturePro(
			texture,
			e.uv,
			entity_rect,
			e.uv_origin,
			math.to_degrees(e.uv_rot + b2.Rot_GetAngle(rot)),
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

EntitySetupProc :: proc(e: ^Entity, world: b2.WorldId, pos := b2.Vec2{})

new_entity :: proc(game: ^Game, setup_proc: EntitySetupProc, pos := b2.Vec2{}) -> ^Entity {
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
	setup_proc(new_entity, game.world, pos)
	return new_entity
}

kill_entity :: proc(e: ^Entity) {
	b2.DestroyBody(e.body)
	mem.set(e, 0, size_of(Entity))
}

get_default_defs :: proc() -> (b2.BodyDef, b2.ShapeDef) {
	body_def := b2.DefaultBodyDef()
	body_def.type = .dynamicBody
	body_def.fixedRotation = true

	return body_def, b2.DefaultShapeDef()
}

ShapeFilterFlags :: enum {
	Player = 1,
	Skull  = 2,
	Spear  = 4,
}

setup_entity_player :: proc(e: ^Entity, world: b2.WorldId, pos := b2.Vec2{}) {
	e.sprite = "slime"
	e.uv = {16, 0, 16, 16}
	e.uv_origin = {8, 8}
	e.variant = Variant_Player {
		force = 4000,
	}
	e.health = Health {
		points = 3,
		max    = 3,
	}

	body_def, shape_def := get_default_defs()
	body_def.position = pos
	body_def.automaticMass = false
	body_def.linearDamping = 10
	e.body = b2.CreateBody(world, body_def)
	b2.Body_SetMassData(e.body, b2.MassData{8, {}, 0})

	shape_def.userData = e
	shape_def.filter.categoryBits = u32(ShapeFilterFlags.Player)
	shape_def.filter.maskBits = u32(ShapeFilterFlags.Skull | .Spear)
	e.shape = b2.CreateCircleShape(e.body, shape_def, b2.Circle{b2.Vec2{0, 0}, 7})
}

setup_entity_spear :: proc(e: ^Entity, world: b2.WorldId, pos := b2.Vec2{}) {
	e.sprite = "spear"
	e.shader = make_shader_pixel_glow(rl.WHITE, size = 0.6, threshold = 0.1, intensity = 0.5)
	e.uv = {0, 0, 32, 32}
	e.uv_origin = {16, 16}
	e.uv_rot = math.to_radians_f32(-45.0)
	e.variant = Variant_EnemySpear {
		force = 500,
	}
	e.expiry = time.time_add(time.now(), time.Second * 5)

	body_def, shape_def := get_default_defs()
	body_def.position = pos
	body_def.fixedRotation = false
	body_def.automaticMass = false
	body_def.linearDamping = 0
	body_def.isBullet = true
	e.body = b2.CreateBody(world, body_def)
	b2.Body_SetMassData(e.body, b2.MassData{1, {}, 0})

	shape_def.userData = e
    shape_def.enablePreSolveEvents = true
    shape_def.restitution = 0.3
	shape_def.filter.categoryBits = u32(ShapeFilterFlags.Spear)
	shape_def.filter.maskBits = u32(ShapeFilterFlags.Player)

	pixel_hull := []rl.Vector2{{-13, -12}, {-12, -13}, {13, 12}, {12, 13}}
	for &v in pixel_hull {
		v = rl.Vector2Rotate(v, e.uv_rot)
	}
	e.shape = b2.CreatePolygonShape(
		e.body,
		shape_def,
		b2.MakePolygon(b2.ComputeHull(pixel_hull), 0),
	)
}

setup_entity_skull :: proc(e: ^Entity, world: b2.WorldId, pos := b2.Vec2{}) {
	e.sprite = "skull"
	e.uv = {0, 0, 16, 16}
	e.uv_origin = {8, 8}
	e.uv_rot = 0
	e.variant = Variant_EnemySkull {
		force = 10,
	}

	body_def, shape_def := get_default_defs()
	body_def.position = pos
	body_def.automaticMass = false
	body_def.linearDamping = 1
	e.body = b2.CreateBody(world, body_def)
	b2.Body_SetMassData(e.body, b2.MassData{1, {}, 0})

	shape_def.userData = e
	shape_def.filter.categoryBits = u32(ShapeFilterFlags.Skull)
	shape_def.filter.maskBits = u32(ShapeFilterFlags.Player | .Skull)
	e.shape = b2.CreateCircleShape(e.body, shape_def, b2.Circle{{}, 6})
}

spawner :: proc(game: ^Game) {
	if time.since(game.spawn_timers.next_spear) > 0 {
		distance := rand.float32_range(SPAWN_RANGE_MIN+30, SPAWN_RANGE_MAX)
		angle := rand.float32_range(0, math.TAU)

		new_spear_pos :=
			b2.Body_GetPosition(game.special_entities.player.body) +
			({math.cos(angle), math.sin(angle)} * distance)

		spear := new_entity(game, setup_entity_spear, new_spear_pos)
		spear_v := &spear.variant.(Variant_EnemySpear)
		spear_v.launch_time = time.time_add(time.now(), time.Second * 2)

		game.spawn_timers.next_spear = time.time_add(time.now(), time.Second * 2)
	}

	if time.since(game.spawn_timers.next_skull) > 0 {
		distance := rand.float32_range(SPAWN_RANGE_MIN, SPAWN_RANGE_MAX)
		angle := rand.float32_range(0, math.TAU)

		new_skull_pos :=
			b2.Body_GetPosition(game.special_entities.player.body) +
			({math.cos(angle), math.sin(angle)} * distance)
		skull := new_entity(game, setup_entity_skull, new_skull_pos)

		game.spawn_timers.next_skull = time.time_add(time.now(), time.Second * 3)
	}
}
