extends Node2D

var player_block = null
var background_island = {}
var origin_island = {}

# As a world position
var distant_home = Vector2(-1000000, -1000000)
var camera_target = Vector2(0, 0)
var camera_pos = Vector2(0, 0)


func _ready():
	self.set_pos(self.distant_home)
	set_fixed_process(true)

func find_island(origin):
	var found = {origin : Vector2(0, 0)}
	var to_search = found

	while not to_search.empty():
		var next_search = {}
		for b in to_search:
			var as = b.adjacent_blocks_with_displacement()
			for a in as.keys():
				if not found.has(a):
					var apos = found[b] + as[a]
					next_search[a] = apos
					found[a] = apos
		to_search = next_search

	return found

func arrange():
	self.origin_island = self.find_island(self.player_block.parent_block)
	self.background_island = self.find_island(self.player_block.parent_block.parent_block)

func arrange_initial():
	arrange()
	find_target()
	self.camera_pos = self.camera_target

func draw_island(island, origin, scale):
	for b in island:
		var grid_pos = island[b]

		var block_size = Constants.block_size * b.tilemap.get_cell_size() * scale

		var pos = origin + grid_pos * block_size
		var drawrect = Rect2(pos, block_size)

		var texture = b.viewport.get_render_target_texture()
		self.draw_texture_rect(texture, drawrect, false)

func _draw():
	# var cell_size = self.player_block.tilemap.get_cell_size()
	var background_offset = self.player_block.parent_block.world_position_on_parent() * Constants.block_size
	draw_island(self.background_island, -background_offset, Constants.block_size)

	# Foreground
	draw_island(self.origin_island, Vector2(0, 0), 1)

func _fixed_process(delta):
	find_target()
	adjust_camera(delta)
	set_camera()

func find_target():
	var block_size = Constants.block_size * self.player_block.tilemap.get_cell_size()
	var block_grid_position = self.origin_island[self.player_block.parent_block]
	var block_center = (block_grid_position + Vector2(0.5, 0.5)) * block_size

	self.camera_target = self.distant_home + block_center

func adjust_camera(delta):
	var diff = self.camera_target - self.camera_pos
	self.camera_pos += diff * Constants.camera_lerp * delta

func set_camera():
	var screen_center = self.get_viewport().get_rect().size / 2

	var transform = Matrix32(0, screen_center - self.camera_pos)
	self.get_viewport().set_canvas_transform(transform)
