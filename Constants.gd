extends Node

# Gameplay

var block_size = 5

# enum TILES {TILE_EMPTY, TILE_WALL, TILE_EMPTY_RED, TILE_EMPTY_BLUE}
var TILE_EMPTY = 0
var TILE_WALL = 1
var TILE_EMPTY_RED = 2
var TILE_EMPTY_BLUE = 3

# Drawing
var background_fade = Color(0.5, 0.5, 0.5, 0.5)
var player_highlight = Color(0.7, 0.1, 0.1)

# enum TINT_COLORS {TINT_RED, TINT_BLUE}
var TINT_RED = 0
var TINT_BLUE = 1

var transparent_tint_blue = Color(0,0,1,0.2)
var transparent_tint_red  = Color(1,0,0,0.2)

func tint_for(code):
	if code == TINT_RED:
		return transparent_tint_red
	elif code == TINT_BLUE:
		return transparent_tint_blue
	return Color(1,1,1,0.2)

# Animation

var camera_lerp = 1.5
var camera_zoom_lerp = 2.0
var move_duration = 0.4
