extends Node2D

################################################################################
### Vars

# Gameplay
onready var tilemap   = self.get_node("TileMap")
enum TILES {TILE_EMPTY, TILE_WALL}

export(NodePath) var parent_block_path = null
export(Vector2)  var position_on_parent = Vector2(0, 0)
export(bool)     var is_player = false

var parent_block = null
var child_blocks = []

# Movement animation
var is_moving = false
var move_vector = Vector2(0.0, 0.0)
var previous_parent = null
var previous_position_on_parent = self.position_on_parent

var current_move_time = 0

# Drawing
onready var viewport  = self.get_node("SelfViewport")
onready var self_rect = self.get_self_rect()

################################################################################
### Setup

func _ready():
	# Add to block group
	self.add_to_group("blocks")

	# Setup viewport
	viewport.set_world_2d(self.get_world_2d())
	viewport.set_rect(self.self_rect)
	#viewport.set_size_override(true, self.self_rect.size)
	#viewport.set_size_override_stretch(true)
	var transform = Matrix32(0, -self.self_rect.pos)
	viewport.set_canvas_transform(transform)

	self.set_fixed_process(true)


################################################################################
### Parents and Siblings

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
	self.previous_parent = self.parent_block

################################################################################
### Block position
# Represents a square in the world

class BlockPosition:
	var block = null
	var position = Vector2(0, 0)

	func _init(b, p):
		self.block    = b
		self.position = p

	func get_tile_id():
		return self.block.tilemap.get_cell(self.position.x, self.position.y)

	func adjacent(move_vect):
		return adjacent_recurse(move_vect, self.block)

	func adjacent_recurse(move_vect, original):
		var newpos = self.position + move_vect

		if newpos.x < 0:
			newpos.x = Constants.block_size - 1
		elif newpos.x >= Constants.block_size:
			newpos.x = 0
		elif newpos.y < 0:
			newpos.y = Constants.block_size - 1
		elif newpos.y >= Constants.block_size:
			newpos.y = 0
		else:
			return get_script().new(self.block, newpos)

		# We left the block

		if self.block.parent_block == original:
			return null

		var newsquare = self.block.own_position().adjacent_recurse(move_vect, original)

		if newsquare == null:
			return null

		var newblock = newsquare.block_at()

		if newblock == null:
			return null

		return get_script().new(newblock, newpos)

	func is_underlying_walkable():
		return self.get_tile_id() == TILE_EMPTY

	func block_at():
		for b in self.block.child_blocks:
			if b.position_on_parent == self.position:
				return b
		return null

	func is_empty():
		if not self.is_underlying_walkable():
			return false

		if not self.block_at() == null:
			return false

		return true

func own_position():
	return BlockPosition.new(parent_block, position_on_parent)

func adjacent_blocks():
	return adjacent_blocks_with_displacement().keys()

func adjacent_blocks_with_displacement():
	var adjacents = {}
	var a = null

	a = self.own_position().adjacent("left")
	if not a == null:
		var b = a.block_at()
		if not b == null:
			adjacents[b] = Vector2(-1, 0)

	a = self.own_position().adjacent("right")
	if not a == null:
		var b = a.block_at()
		if not b == null:
			adjacents[b] = Vector2(1, 0)

	a = self.own_position().adjacent("up")
	if not a == null:
		var b = a.block_at()
		if not b == null:
			adjacents[b] = Vector2(0, -1)

	a = self.own_position().adjacent("down")
	if not a == null:
		var b = a.block_at()
		if not b == null:
			adjacents[b] = Vector2(0, 1)

	return adjacents

################################################################################
### Drawing

func get_self_rect():
	var pos = tilemap.get_global_pos()
	# TODO: scale?
	var s = Constants.block_size * tilemap.get_cell_size()
	return Rect2(pos, s)

func visual_position_on_parent():
	if self.is_moving:
		var t = easing_function(self.current_move_time / Constants.move_duration)

		return interpolate(t, self.position_on_parent - self.move_vector, self.position_on_parent)
	else:
		return self.position_on_parent

func draw_self_texture_on(block, position):
	var texture = self.viewport.get_render_target_texture()

	var to_parent_transform = block.get_global_transform() * self.get_global_transform().affine_inverse()

	var cell_size = self.tilemap.get_cell_size() # TODO: self?
	var drawrect = Rect2(position, cell_size)

	self.draw_set_transform_matrix(to_parent_transform)
	self.draw_texture_rect(texture, drawrect, false)

func _draw(): # TODO: think about clearing the render target every frame
	# Border
	# var s = Constants.block_size * tilemap.get_cell_size().x
	# var grey = Color(0.5, 0.5, 0.5)
	# self.draw_line(Vector2(0, 0), Vector2(0, s), grey, 1)
	# self.draw_line(Vector2(0, s), Vector2(s, s), grey, 1)
	# self.draw_line(Vector2(s, s), Vector2(s, 0), grey, 1)
	# self.draw_line(Vector2(s, 0), Vector2(0, 0), grey, 1)

	if self.is_moving:
		var t = easing_function(self.current_move_time / Constants.move_duration)

		var newpos = self.parent_block.tilemap.map_to_world(self.position_on_parent)
		var oldpos = self.parent_block.tilemap.map_to_world(self.position_on_parent - self.move_vector)

		var pos = interpolate(t, oldpos, newpos)
		self.draw_self_texture_on(self.parent_block, pos)

		if not self.previous_parent == self.parent_block:
			var newpos = self.previous_parent.tilemap.map_to_world(self.previous_position_on_parent + self.move_vector)
			var oldpos = self.previous_parent.tilemap.map_to_world(self.previous_position_on_parent)

			var pos = interpolate(t, oldpos, newpos)
			self.draw_self_texture_on(self.previous_parent, pos)
	else:
		var worldcoords = self.parent_block.tilemap.map_to_world(self.position_on_parent)
		self.draw_self_texture_on(self.parent_block, worldcoords)

func interpolate(t, x1, x2):
	return (1-t)*x1 + t*x2

func easing_function(t):
	var u0 = 0
	var u1 = 0.2
	var u2 = 0.95
	var u3 = 1
	return u0 * (1-t*t*t) + 3*u1*(1-t*t)*t + 3*u2*(1-t)*t*t + u3*t*t*t*t

func _fixed_process(delta):
	if self.is_moving:
		self.current_move_time += delta
		if self.current_move_time > Constants.move_duration:
			self.is_moving = false
			self.current_move_time = 0

		self.update()

################################################################################
### Movement

func try_move(move_vect):
	if self.is_moving:
		return

	var a = self.own_position().adjacent(move_vect)
	if a == null:
		return

	if a.is_empty():
		do_move(move_vect, a)

func do_move(move_vect, new_square):
	self.is_moving = true
	self.move_vector = move_vect

	self.previous_parent = self.parent_block
	self.previous_position_on_parent = self.position_on_parent

	self.parent_block = new_square.block
	self.position_on_parent = new_square.position

	if not self.parent_block == self.previous_parent:
		self.previous_parent.child_blocks.erase(self)
		self.parent_block.child_blocks.append(self)
