################################################################################
## Imports

extends Node2D

const util_script = preload("res://Util.gd")
var Util = util_script.new()

################################################################################
## Vars

var player_chunk = null

# As a world position
var camera_target = Vector2(0, 0)
var camera_pos = Vector2(0, 0)
var camera_zoom = 1
var camera_zoom_target = 1

func _ready():
	VisualServer.set_default_clear_color(Constants.background_fade)
	self.set_fixed_process(true)

func setup():
	self.find_target()
	self.camera_pos = self.camera_target

################################################################################
## Camera

# func move_camera(move_vect):
# 	var block_size = Constants.block_size * self.player_block.tilemap.get_cell_size()
# 	self.camera_pos -= move_vect * block_size

# func zoom_camera():
# 	var previous_block_size = self.player_block.tilemap.get_cell_size()
# 	var block_size = Constants.block_size * self.player_block.tilemap.get_cell_size()
# 	var block_center = Vector2(0.5, 0.5) * block_size

# 	# Currently this works by assuming self.player_block has already been replaced
# 	var shift_pos = (self.player_block.visual_position_on_parent() + Vector2(0.5, 0.5)) * previous_block_size

# 	self.camera_pos = (self.camera_pos - block_center) / Constants.block_size + shift_pos
# 	self.camera_zoom *= Constants.block_size

# func find_target():
# 	var block_size = Constants.block_size * self.player_block.tilemap.get_cell_size()
# 	var block_grid_position = Vector2(0, 0)
# 	var block_center = (block_grid_position + Vector2(0.5, 0.5)) * block_size

# 	self.camera_target = block_center

# func adjust_camera(delta):
# 	var diff = self.camera_target - self.camera_pos
# 	self.camera_pos += diff * Constants.camera_lerp * delta / self.camera_zoom

# 	var zoom_diff = self.camera_zoom_target - self.camera_zoom
# 	self.camera_zoom += zoom_diff * Constants.camera_zoom_lerp * delta

# func set_camera():
# 	var screen_center = self.get_viewport().get_rect().size / 2
# 	var screen_transform = Matrix32(0, screen_center)

# 	var camera_transform = Matrix32(0, -self.camera_pos).scaled(Vector2(camera_zoom, camera_zoom))
# 	self.get_viewport().set_canvas_transform(screen_transform*camera_transform)


################################################################################
## Drawing

func _draw():
	self.draw_with_parents(self.player_block.parent_block, Vector2(0, 0), 0, 4, 0)

func _fixed_process(delta):
	adjust_camera(delta)
	self.update()
