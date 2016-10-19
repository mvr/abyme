extends Node2D

### Gameplay
onready var tilemap   = self.get_node("TileMap")
export(NodePath) var parent_block_path = null
export(Vector2)  var grid_position_on_parent = Vector2(0, 0)
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

### Drawing
onready var viewport  = self.get_node("SelfViewport")
onready var self_rect = self.get_self_rect()

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

func is_walkable(grid_pos):
	return true
	
func can_move(direction):
	return true

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
	self.set_process_input(true) # TODO: do this once in Level

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
		parent_block.update()

func try_move(direction):
	if self.is_moving:
		return
		
	if not self.can_move(direction):
		return
	
	do_move(direction)

func do_move(direction):	
	self.is_moving = true
	
	if direction == "left":
		self.grid_position_on_parent.x -= 1
		
	if direction == "right":
		self.grid_position_on_parent.x += 1
	
	if direction == "up":
		self.grid_position_on_parent.y -= 1
		
	if direction == "down":
		self.grid_position_on_parent.y += 1

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