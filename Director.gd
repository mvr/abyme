extends Node2D

var player_block = null

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

func int_exp(i,e):
	if e < 0:
		return 1.0 / int_exp(i,-e)
	if e == 0:
		return 1
	else:
		return i * int_exp(i, e-1)

func log_base(x, b):
	return log(x) / log(b)

################################################################################
## Drawing

func draw_rect(rect, colour, width):
	var corner1 = rect.pos
	var corner2 = rect.pos + Vector2(0, rect.size.y)
	var corner3 = rect.end
	var corner4 = rect.pos + Vector2(rect.size.x, 0)

	self.draw_line(corner1, corner2, colour, width)
	self.draw_line(corner2, corner3, colour, width)
	self.draw_line(corner3, corner4, colour, width)
	self.draw_line(corner4, corner1, colour, width)

func draw_tilemap_manually(block, pos, scale, depth, start_depth):
	var tileset = block.tilemap.get_tileset()
	var block_screen_size = block.tilemap.get_cell_size() * int_exp(Constants.block_size, scale)
	var cell_screen_size = block.tilemap.get_cell_size() * int_exp(Constants.block_size, scale - 1)

	var fade_colour = null
	if depth == start_depth :
		var fade_amount = log_base(self.camera_zoom, Constants.block_size)
		fade_colour = Color(1,1,1).linear_interpolate(Constants.background_fade, fade_amount)
	elif depth < start_depth:
		fade_colour = Constants.background_fade
	else:
		fade_colour = Color(1,1,1)

	for i in range(Constants.block_size):
		for j in range(Constants.block_size):
			var tile = block.tilemap.get_cell(i, j)
			var texture = tileset.tile_get_texture(tile)
			var corner = pos + cell_screen_size * Vector2(i, j)
			var drawrect = Rect2(corner, cell_screen_size)
			self.draw_texture_rect(texture, drawrect, false, fade_colour)

func draw_block_manually(block, pos, scale, depth, start_depth, max_depth):
	if depth > max_depth:
		# Just use existing texture
		var block_screen_size = block.tilemap.get_cell_size() * int_exp(Constants.block_size, scale)
		var drawrect = Rect2(pos, block_screen_size)
		var texture = block.viewport.get_render_target_texture()
		self.draw_texture_rect(texture, drawrect, false)
		return

	var block_screen_size = block.tilemap.get_cell_size() * int_exp(Constants.block_size, scale)
	var cell_screen_size  = block.tilemap.get_cell_size() * int_exp(Constants.block_size, scale - 1)

	if depth == 0:
		self.draw_tilemap_manually(block, pos, scale, depth, start_depth)

	# This ordering is required to stop things drawing over each other
	for b in block.child_blocks:
		var child_pos = pos + (b.visual_position_on_parent() * cell_screen_size)
		self.draw_tilemap_manually(b, child_pos, scale - 1, depth + 1, start_depth)

	for b in block.child_blocks:
		var child_pos = pos + (b.visual_position_on_parent() * cell_screen_size)
		self.draw_block_manually(b, child_pos, scale - 1, depth + 1, start_depth, max_depth)

	# for b in block.child_blocks:
	# 	var child_pos = pos + (b.visual_position_on_parent() * cell_screen_size)

	# 	var corner1 = child_pos
	# 	var corner2 = child_pos + Vector2(0, cell_screen_size.y)
	# 	var corner3 = child_pos + Vector2(cell_screen_size.x, cell_screen_size.y)
	# 	var corner4 = child_pos + Vector2(cell_screen_size.x, 0)

	# 	var grey = Color(0.5, 0.5, 0.5)
	# 	self.draw_line(corner1, corner2, grey, 1)
	# 	self.draw_line(corner2, corner3, grey, 1)
	# 	self.draw_line(corner3, corner4, grey, 1)
	# 	self.draw_line(corner4, corner1, grey, 1)

func draw_with_parents(block, pos, depth, up_depth, down_depth):
	if depth == up_depth:
		self.draw_block_manually(block, pos, depth+1, 0, up_depth, up_depth + down_depth)
		return

	var background_offset = -block.visual_position_on_parent() * block.tilemap.get_cell_size() * int_exp(Constants.block_size, depth + 1)

	self.draw_with_parents(block.parent_block, pos + background_offset, depth + 1, up_depth, down_depth)

func _draw():
	self.draw_with_parents(self.player_block.parent_block, Vector2(0, 0), 0, 4, 0)

	var parent_rect = Rect2(Vector2(0,0), self.player_block.tilemap.get_cell_size() * Constants.block_size)
	# self.draw_rect(parent_rect, Constants.player_highlight, 1)

	var self_rect = Rect2(self.player_block.visual_position_on_parent() * self.player_block.tilemap.get_cell_size(), self.player_block.tilemap.get_cell_size())
	self.draw_rect(self_rect, Constants.player_highlight, 1)

func _fixed_process(delta):
	find_target()
	adjust_camera(delta)
	set_camera()
	self.update()

################################################################################
## Camera

func move_camera(move_vect):
	var block_size = Constants.block_size * self.player_block.tilemap.get_cell_size()
	self.camera_pos -= move_vect * block_size

func zoom_camera():
	var previous_block_size = self.player_block.tilemap.get_cell_size()
	var block_size = Constants.block_size * self.player_block.tilemap.get_cell_size()
	var block_center = Vector2(0.5, 0.5) * block_size

	# Currently this works by assuming self.player_block has already been replaced
	var shift_pos = (self.player_block.visual_position_on_parent() + Vector2(0.5, 0.5)) * previous_block_size

	self.camera_pos = (self.camera_pos - block_center) / Constants.block_size + shift_pos
	self.camera_zoom *= Constants.block_size

func find_target():
	var block_size = Constants.block_size * self.player_block.tilemap.get_cell_size()
	var block_grid_position = Vector2(0, 0)
	var block_center = (block_grid_position + Vector2(0.5, 0.5)) * block_size

	self.camera_target = block_center

func adjust_camera(delta):
	var diff = self.camera_target - self.camera_pos
	self.camera_pos += diff * Constants.camera_lerp * delta / self.camera_zoom

	var zoom_diff = self.camera_zoom_target - self.camera_zoom
	self.camera_zoom += zoom_diff * Constants.camera_zoom_lerp * delta

func set_camera():
	var screen_center = self.get_viewport().get_rect().size / 2
	var screen_transform = Matrix32(0, screen_center)

	var camera_transform = Matrix32(0, -self.camera_pos).scaled(Vector2(camera_zoom, camera_zoom))
	self.get_viewport().set_canvas_transform(screen_transform*camera_transform)
