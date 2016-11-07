extends Node2D

var player_block = null
var origin_island = {}

# As a grid position
var camera_target = Vector2(0, 0)

func _ready():
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
	self.origin_island = find_island(self.player_block.parent_block)

func draw_island(island):
	for b in island:
		var grid_pos = origin_island[b]

		var block_size = b.size * b.tilemap.get_cell_size()

		var pos = grid_pos * block_size
		var drawrect = Rect2(pos, block_size)

		var texture = b.viewport.get_render_target_texture()
		self.draw_texture_rect(texture, drawrect, false)

func _draw():
	adjust_camera()
	draw_island(self.origin_island)

func _fixed_process(delta):
	adjust_camera()

func adjust_camera():
	var screen_center = self.get_viewport().get_rect().size / 2

	var block_size = self.player_block.size * self.player_block.tilemap.get_cell_size()
	var block_grid_position = self.origin_island[self.player_block.parent_block]
	var block_center = (block_grid_position + Vector2(0.5, 0.5)) * block_size


	var transform = Matrix32(0, screen_center - block_center)
	self.get_viewport().set_canvas_transform(transform)
