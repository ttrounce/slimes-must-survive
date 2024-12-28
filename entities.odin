package main

import "core:math"
import "core:time"
import b2 "vendor:box2d"
import rl "vendor:raylib"

Entity :: struct {
    shader:        Maybe(ShaderInfo),
    sprite:        Sprite,
    variant:       EntityVariant,
    flags:         bit_set[EntityFlags],
    body:          b2.BodyId,
    shape:         b2.ShapeId,
    expiry:        Maybe(time.Time),
    birth:         time.Time,
    health:        Maybe(Health),
    hit_time:      time.Time,
    fade_duration: Maybe(time.Duration),
}

Sprite :: struct {
    id:           string,
    uv:           rl.Rectangle,
    uv_origin:    rl.Vector2,
    uv_rot:       f32,
    keep_upright: bool,
}

EntityFlags :: enum {
    ALLOCATED,
}

EntityVariant :: union {
    Variant_Player,
    Variant_EnemySkull,
    Variant_EnemySpear,
    Variant_Projectile,
}

Variant_Player :: struct {
    force:     f32,
    weapon:    Weapon,
    last_shot: time.Time,
}

Variant_EnemySpear :: struct {
    force:       f32,
    launch_time: time.Time,
    launched:    bool,
}

Variant_EnemySkull :: struct {
    force: f32,
}

Variant_Projectile :: struct {
    force: f32,
}

Health :: struct {
    points: int,
    max:    int,
}

EntityFilterFlags :: enum {
    Player = 1,
    Skull  = 2,
    Spear  = 4,
    Bullet = 8,
}

get_default_defs :: proc() -> (b2.BodyDef, b2.ShapeDef) {
    body_def := b2.DefaultBodyDef()
    body_def.type = .dynamicBody
    body_def.fixedRotation = false

    return body_def, b2.DefaultShapeDef()
}

setup_entity_player :: proc(
    e: ^Entity,
    world: b2.WorldId,
    pos := b2.Vec2{},
    rot := b2.Rot_identity,
) {
    e.sprite = Sprite {
        id = "slime",
        uv = {0, 0, 16, 16},
        uv_origin = {8, 8},
    } 
    e.variant = Variant_Player {
        force  = 4000,
        weapon = make_weapon(time.Millisecond * 500, 3, 0.1, .SLIME_BOLT),
    }
    e.health = Health {
        points = 3,
        max    = 3,
    }

    body_def, shape_def := get_default_defs()
    body_def.position = pos
    body_def.rotation = rot
    body_def.automaticMass = false
    body_def.linearDamping = 10
    e.body = b2.CreateBody(world, body_def)
    b2.Body_SetMassData(e.body, b2.MassData{8, {}, 0})

    shape_def.userData = e
    shape_def.filter.categoryBits = u32(EntityFilterFlags.Player)
    shape_def.filter.maskBits = u32(EntityFilterFlags.Skull | .Spear)
    e.shape = b2.CreateCircleShape(e.body, shape_def, b2.Circle{b2.Vec2{0, 0}, 7})
}

setup_entity_spear :: proc(
    e: ^Entity,
    world: b2.WorldId,
    pos := b2.Vec2{},
    rot := b2.Rot_identity,
) {
    e.shader = make_shader_pixel_glow(rl.WHITE, size = 0.6, threshold = 0.1, intensity = 0.5)
    e.sprite = Sprite {
        id = "spear",
        uv = {0, 0, 32, 32},
        uv_origin = {16, 16},
        uv_rot = math.to_radians_f32(-45.0),
    }
    e.variant = Variant_EnemySpear {
        force = 500,
    }
    e.expiry = time.time_add(time.now(), time.Second * 5)
    e.fade_duration = time.Millisecond * 500

    body_def, shape_def := get_default_defs()
    body_def.position = pos
    body_def.automaticMass = false
    body_def.linearDamping = 0
    body_def.isBullet = true
    e.body = b2.CreateBody(world, body_def)
    b2.Body_SetMassData(e.body, b2.MassData{1, {}, 0})

    shape_def.userData = e
    shape_def.enablePreSolveEvents = true
    shape_def.restitution = 0.3
    shape_def.isSensor = true
    shape_def.filter.categoryBits = u32(EntityFilterFlags.Spear)
    shape_def.filter.maskBits = u32(EntityFilterFlags.Player)

    pixel_hull := []rl.Vector2{{-13, -12}, {-12, -13}, {13, 12}, {12, 13}}
    for &v in pixel_hull {
        v = rl.Vector2Rotate(v, e.sprite.uv_rot)
    }
    e.shape = b2.CreatePolygonShape(
        e.body,
        shape_def,
        b2.MakePolygon(b2.ComputeHull(pixel_hull), 0),
    )
}

setup_entity_skull :: proc(
    e: ^Entity,
    world: b2.WorldId,
    pos := b2.Vec2{},
    rot := b2.Rot_identity,
) {
    e.sprite = Sprite {
        id = "skull",
        uv = {0, 0, 16, 16},
        uv_origin = {8, 8},
        uv_rot = 0,
        keep_upright = true,
    }
    e.health = Health {
        points = 2,
        max    = 2,
    }
    e.variant = Variant_EnemySkull {
        force = 50,
    }
    e.fade_duration = time.Millisecond * 200

    body_def, shape_def := get_default_defs()
    body_def.position = pos
    body_def.rotation = rot
    body_def.automaticMass = false
    body_def.linearDamping = 1
    e.body = b2.CreateBody(world, body_def)
    b2.Body_SetMassData(e.body, b2.MassData{1, {}, 0})

    shape_def.userData = e
    shape_def.filter.categoryBits = u32(EntityFilterFlags.Skull)
    shape_def.filter.maskBits = u32(EntityFilterFlags.Player | .Skull | .Bullet)
    e.shape = b2.CreateCircleShape(e.body, shape_def, b2.Circle{{}, 6})
}

setup_entity_slime_bolt :: proc(
    e: ^Entity,
    world: b2.WorldId,
    pos := b2.Vec2{},
    rot := b2.Rot_identity,
) {
    e.sprite = Sprite {
        id = "slime_bolt",
        uv = {0, 0, 16, 16},
        uv_origin = {8, 8},
        uv_rot = math.to_radians_f32(45),
    }
    e.variant = Variant_Projectile {
        force = 75,
    }
    e.expiry = time.time_add(time.now(), time.Second*5)

    body_def, shape_def := get_default_defs()
    body_def.position = pos
    body_def.rotation = rot
    body_def.automaticMass = false
    body_def.linearDamping = 0
    body_def.isBullet = false
    e.body = b2.CreateBody(world, body_def)
    b2.Body_SetMassData(e.body, b2.MassData{0.25, {}, 0})

    shape_def.userData = e
    shape_def.filter.categoryBits = u32(EntityFilterFlags.Bullet)
    shape_def.filter.maskBits = u32(EntityFilterFlags.Skull)
    e.shape = b2.CreateCircleShape(e.body, shape_def, b2.Circle{{}, 6})
}
