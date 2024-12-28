package util

is_some :: proc(m: Maybe($T)) -> bool {
    if _, ok := m.?; ok {
        return true
    }
    return false
}