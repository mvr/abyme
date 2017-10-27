tool
################################################################################
### Imports

extends Node2D
const Util = preload("Util.gd")

################################################################################
### Vars

# Gameplay
onready var tilemap = self.get_node("TileMap")
onready var poly = self.poly_from_tilemap(tilemap)

var zoom_scale = 2

var parent_positions = {}
var child_shapes = []

# Drawing
export(Color, RGB) var outline_color = null
export(Color, RGB) var fill_color = null
# onready var viewport  = self.get_node("SelfViewport")
# onready var self_rect = self.get_self_rect()

# Movement animation
var is_moving = false
var moved_from = Vector2(0.0, 0.0)
var current_move_time = 0

################################################################################
### Setup

# Convenience
export(NodePath) var starting_parent_path = null
export(Vector2)  var starting_position_parent = Vector2(0, 0)
export(bool)     var starting_player = false

func _ready():
	# Add to shape group
	# self.add_to_group("shapes")

	# Setup viewport
	# viewport.set_world_2d(self.get_world_2d())
	# viewport.set_rect(self.self_rect)
	#viewport.set_size_override(true, self.self_rect.size)
	#viewport.set_size_override_stretch(true)
	# var transform = Matrix32(0, -self.self_rect.pos)
	# viewport.set_canvas_transform(transform)

	self.set_process(true)

func poly_from_tilemap(tilemap):
	return tilemap.get_used_cells() # TODO: Is this actually doing the right thing?

func find_children(all_shapes):
	var child_shapes = []
	for b in all_shapes:
		if b.parent_positions.has(self):
			child_shapes.append(b)
	return child_shapes

func set_child_shapes(all_shapes):
	self.child_shapes = self.find_children(all_shapes)

func set_starting_parent():
	self.parent_positions[get_node(self.starting_parent_path)] = self.starting_parent_position

################################################################################
### Relationships

func child_shapes():
	return self.child_shapes

func parent_shapes():
	return self.parent_positions.keys()

func child_shapes_with_position():
	var r = {}
	for c in self.child_shapes:
		r[c] = c.parent_positions[self]
	return r

func has_position(position):
	return self.poly.find(position) != -1

################################################################################
### Addressing

class Square:
	var shape = null
	var position = null

	func _init(b, p):
		self.shape    = b
		self.position = p

	# func get_tile_id():
	# 	return self.shape.tilemap.get_cell(self.position.x, self.position.y)

	func _location_on(parent):
		var pos_on  = self.parent_positions[parent]
		var new_pos = Util.pos_div(parent.zoom_scale, self.position + pos_on)
		var sub_pos = Util.pos_mod(parent.zoom_scale, self.position + pos_on)

		if parent.has_posotion(new_pos):
			return Location.new(Square.new(parent, new_pos), sub_pos) # TODO: this probably won't work
		else:
			return null

	func location():
		var l = null
		var count = 0
		for p in self.shape.parents:
			var r = _location_on(p)
			if r != null:
				l = r
				count += 1
		if count == 1:
			return l
		else:
			breakpoint # A square is not sitting on exactly one parent

class Location:
	var square = null
	var subpos = null

	func _init(s, subp):
		self.square = s
		self.subpos = subp

	# Relative to shape origin
	func as_position():
		return (self.square.shape.zoom_scale * self.square.position) + self.subpos

	func _check_child(child, cpos):
		var lpos = self.as_position()
		var ipos = lpos - cpos
		if child.has_position(ipos):
			Square.new(child, ipos)
		else:
			null

	func inhabitant():
		var l = null
		var count = 0
		for p in self.square.shape.child_shapes_with_position():
			var r = _check_child(p[0], p[1])
			if r != null:
				l = r
				count += 1
		if count == 0:
			return null
		elif count == 1:
			return l
		else:
			breakpoint # A location has multiple inhabitants

	func is_inhabited():
		return self.inhabitant() != null

################################################################################
### Drawing

func editor_draw():
	print("in editor draw")
	var squareSize = self.tilemap.get_cell_size()
	for p in poly:
		var rect = Rect2(p * squareSize, squareSize)
		self.draw_rect(rect, self.fill_color)

		var ul = p * squareSize
		var ur = ul + Vector2(squareSize.x, 0)
		var bl = ul + Vector2(0, squareSize.y)
		var br = ul + squareSize
		self.draw_line(ul, ur, self.outline_color)
		self.draw_line(ur, br, self.outline_color)
		self.draw_line(br, bl, self.outline_color)
		self.draw_line(bl, ul, self.outline_color)
	# TODO: outline

func _draw():
	if self.get_tree().is_editor_hint():
		self.editor_draw()

# func get_self_rect():
# 	var pos = tilemap.get_global_pos()
# 	# TODO: scale?
# 	var s = Constants.shape_size * tilemap.get_cell_size()
# 	return Rect2(pos, s)

# func visual_position_on_parent():
# 	if self.is_moving:
# 		var t = easing_function(self.current_move_time / Constants.move_duration)

# 		return interpolate(t, self.position_on_parent - self.move_vector, self.position_on_parent)
# 	else:
# 		return self.position_on_parent

# func draw_self_texture_on(shape, position):
# 	var texture = self.viewport.get_render_target_texture()

# 	var to_parent_transform = shape.get_global_transform() * self.get_global_transform().affine_inverse()

# 	var cell_size = self.tilemap.get_cell_size() # TODO: self?
# 	var drawrect = Rect2(position, cell_size)

# 	self.draw_set_transform_matrix(to_parent_transform)
# 	self.draw_texture_rect(texture, drawrect, false)

# func _draw(): # TODO: think about clearing the render target every frame
# 	if self.is_moving:
# 		var t = easing_function(self.current_move_time / Constants.move_duration)

# 		var newpos = self.parent_shape.tilemap.map_to_world(self.position_on_parent)
# 		var oldpos = self.parent_shape.tilemap.map_to_world(self.position_on_parent - self.move_vector)

# 		var pos = interpolate(t, oldpos, newpos)
# 		self.draw_self_texture_on(self.parent_shape, pos)

# 		if not self.previous_parent == self.parent_shape:
# 			var newpos = self.previous_parent.tilemap.map_to_world(self.previous_position_on_parent + self.move_vector)
# 			var oldpos = self.previous_parent.tilemap.map_to_world(self.previous_position_on_parent)

# 			var pos = interpolate(t, oldpos, newpos)
# 			self.draw_self_texture_on(self.previous_parent, pos)
# 	else:
# 		var worldcoords = self.parent_shape.tilemap.map_to_world(self.position_on_parent)
# 		self.draw_self_texture_on(self.parent_shape, worldcoords)

# func interpolate(t, x1, x2):
# 	return (1-t)*x1 + t*x2

# func easing_function(t):
# 	var u0 = 0
# 	var u1 = 0.2
# 	var u2 = 0.95
# 	var u3 = 1
# 	return u0 * (1-t*t*t) + 3*u1*(1-t*t)*t + 3*u2*(1-t)*t*t + u3*t*t*t*t

# func _process(delta):
# 	if self.is_moving:
# 		self.current_move_time += delta
# 		if self.current_move_time > Constants.move_duration:
# 			self.is_moving = false
# 			self.current_move_time = 0

# 		self.update()

################################################################################
### Movement

# func try_move(move_vect):
# 	if self.is_moving:
# 		return

# 	var a = self.own_position().adjacent(move_vect)
# 	if a == null:
# 		return

# 	if a.is_empty():
# 		do_move(move_vect, a)

# func do_move(move_vect, new_square):
# 	self.is_moving = true
# 	self.move_vector = move_vect

# 	self.previous_parent = self.parent_shape
# 	self.previous_position_on_parent = self.position_on_parent

# 	self.parent_shape = new_square.shape
# 	self.position_on_parent = new_square.position

# 	if not self.parent_shape == self.previous_parent:
# 		self.previous_parent.child_shapes.erase(self)
# 		self.parent_shape.child_shapes.append(self)
