extends Node2D

onready var viewport  = self.get_node("SelfViewport")
onready var tilemap   = self.get_node("TileMap")
onready var self_rect = self.get_self_rect()

export(NodePath) var parent_block_path = null
export(Vector2)  var position_on_parent = Vector2(0, 0)
export(bool)     var is_player = false

var parent_block = null
var child_blocks = []

# TODO: calculate?
var size = 5

func get_self_rect():
	var pos = tilemap.get_global_pos()
	# TODO: scale?
	var width = self.size * tilemap.get_cell_size().x
	return Rect2(pos.x, pos.y, width, width)

func find_children():
	var all_blocks = get_tree().get_nodes_in_group("blocks")
	var child_blocks = []
	for b in all_blocks:
		if b.parent_block == self:
			child_blocks.append(b)
	return child_blocks

func set_child_blocks():
	self.child_blocks = self.find_children()
	
func find_parent_block():
	self.parent_block = self.get_node(self.parent_block_path)

func _ready():
	# Add to block group
	self.add_to_group("blocks")
	
	# Setup viewport

	viewport.set_world_2d(self.get_world_2d())
	viewport.set_rect(self.self_rect)
	#viewport.set_size_override(true, self.self_rect.size)
	var transform = Matrix32(0, -self.self_rect.pos)
	viewport.set_canvas_transform(transform)

func _draw():
	# Draw child blocks
	var tile_size = tilemap.get_cell_size()

	for b in self.child_blocks:
		var texture = b.viewport.get_render_target_texture()
		var worldcoords = tilemap.map_to_world(b.position_on_parent)
		var drawrect = Rect2(worldcoords, tile_size)
		# False for gl tiling
		self.draw_texture_rect(texture, drawrect, false) 
	self.update() # TODO: remove