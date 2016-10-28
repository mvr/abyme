extends Node2D

################################################################################
### Vars

# Gameplay
onready var tilemap   = self.get_node("TileMap")
enum TILES {TILE_EMPTY, TILE_WALL}

export(NodePath) var parent_block_path = null
export(Vector2)  var grid_position_on_parent = Vector2(0, 0)
onready var original_grid_position_on_parent = self.grid_position_on_parent
export(bool)     var is_player = false

var parent_block = null
var child_blocks = []

# TODO: calculate?
var size = 5

# Movement
var is_moving = false
var previous_grid_position_on_parent = grid_position_on_parent
export var move_duration = 0.5
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
	var transform = Matrix32(0, -self.self_rect.pos)
	viewport.set_canvas_transform(transform)

	self.set_fixed_process(true)
	self.set_process_input(true) # TODO: do input once in Level

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

	func adjacent(direction):
		return adjacent_recurse(direction, self.block)

	func direction_to_vect(direction):
		if direction == "left":
			return Vector2(-1, 0)
		elif direction == "right":
			return Vector2(1, 0)
		elif direction == "up":
			return Vector2(0, -1)
		elif direction == "down":
			return Vector2(0, 1)

	func adjacent_recurse(direction, original):
		var newpos = self.position + self.direction_to_vect(direction)

		if newpos.x < 0:
			newpos.x = self.block.size - 1
		elif newpos.x >= self.block.size:
			newpos.x = 0
		elif newpos.y < 0:
			newpos.y = self.block.size - 1
		elif newpos.y >= self.block.size:
			newpos.y = 0
		else:
			return get_script().new(self.block, newpos)

		# We left the block

		if self.block.parent_block == original:
			return null

		var newsquare = self.block.own_position().adjacent_recurse(direction, original)

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
			if b.grid_position_on_parent == self.position:
				return b
		return null

	func is_empty():
		if not self.is_underlying_walkable():
			return false

		if not self.block_at() == null:
			return false

		return true

func own_position():
	return BlockPosition.new(parent_block, grid_position_on_parent)

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
	var s = self.size * tilemap.get_cell_size()
	return Rect2(pos, s)

func _draw():
	# Draw child blocks
	var tile_size = tilemap.get_cell_size()

	for b in self.child_blocks:
		var texture = b.viewport.get_render_target_texture()
		var worldcoords = null
		if b.is_moving:
			var fromcoords = tilemap.map_to_world(b.previous_grid_position_on_parent)
			var tocoords = tilemap.map_to_world(b.grid_position_on_parent)
			var proportion = easing_function(b.current_move_time/b.move_duration)

			worldcoords = (1-proportion)*fromcoords + proportion*tocoords
		else:
			worldcoords = tilemap.map_to_world(b.grid_position_on_parent)

		var drawrect = Rect2(worldcoords, tile_size)
		# False for gl tiling
		self.draw_texture_rect(texture, drawrect, false)


func easing_function(t):
	var u0 = 0
	var u1 = 0.2
	var u2 = 0.95
	var u3 = 1
	return u0 * (1-t*t*t) + 3*u1*(1-t*t)*t + 3*u2*(1-t)*t*t + u3*t*t*t*t

func _fixed_process(delta):
	if self.is_moving:
		self.current_move_time += delta
		if self.current_move_time > self.move_duration:
			self.is_moving = false
			self.current_move_time = 0
			self.previous_grid_position_on_parent = self.grid_position_on_parent

		self.parent_block.update()

################################################################################
### Movement

func try_move(direction):
	if self.is_moving:
		return

	var a = self.own_position().adjacent(direction)
	if a == null:
		return

	if a.is_empty():
		do_move(a)

func do_move(new_square):
	self.is_moving = true

	self.parent_block = new_square.block
	self.grid_position_on_parent = new_square.position

func _input(event):
	if not self.is_player:
		return

	if event.is_action_pressed("move_left"):
		self.try_move("left")

	if event.is_action_pressed("move_right"):
		self.try_move("right")

	if event.is_action_pressed("move_up"):
		self.try_move("up")

	if event.is_action_pressed("move_down"):
		self.try_move("down")
