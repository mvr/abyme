extends Node2D

################################################################################
## Imports


################################################################################
## Vars

onready var design = self.get_node("Design")
onready var director = self.get_node("Director")

################################################################################
## Setup

func _ready():
	self.set_process_input(true)
	self.set_process(true)

	self.design.hide() # Unnecessary?

	self.director.design = self.design()
	self.director.setup()

################################################################################
## Zooming

func zoom():
	pass
	# var new_player = self.player_chunk.parent_chunk()
	# self.player_chunk = new_player
	# self.director.player_chunk = new_player

	# self.director.add_zoom()

################################################################################
## Input

func direction_to_vect(direction):
	if direction == "left":
		return Vector2(-1, 0)
	elif direction == "right":
		return Vector2(1, 0)
	elif direction == "up":
		return Vector2(0, -1)
	elif direction == "down":
		return Vector2(0, 1)

func move_player(move_vect):
	var prev_parent = self.player_shape.parent_shape
	self.player_shape.try_move(move_vect)

	if not self.player_shape.parent_shape == prev_parent:
		self.director.move_camera(move_vect)

func _process(delta):
	return null
#	poll_directions()

func poll_directions():
	var move_vect = null
	if Input.is_action_pressed("move_left"):
		move_vect = Vector2(-1, 0)

	if Input.is_action_pressed("move_right"):
		move_vect = Vector2(1, 0)

	if Input.is_action_pressed("move_up"):
		move_vect = Vector2(0, -1)

	if Input.is_action_pressed("move_down"):
		move_vect = Vector2(0, 1)

	if not move_vect == null:
		self.move_player(move_vect)

func _input(event):
	if event.is_action_pressed("zoom"):
		self.zoom()
		return
