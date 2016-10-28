extends Node2D

var origin_block = null
var blocks = {}

func _ready():
	pass

func find_island():
	var found = {self.origin_block : Vector2(0, 0)}
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

	self.blocks = found

func arrange():
	find_island()

func _draw():
	for b in blocks:
		var grid_pos = blocks[b]

		var block_size = b.size * b.tilemap.get_cell_size()

		var pos = grid_pos * block_size
		var drawrect = Rect2(pos, block_size)

		var texture = b.viewport.get_render_target_texture()
		self.draw_texture_rect(texture, drawrect, false)
