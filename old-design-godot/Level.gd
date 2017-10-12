extends Node2D

onready var director   = self.get_node("Director")

var blocks = []
var player_block = null

func _ready():
	self.set_process_input(true)
	self.set_process(true)

	var blocks = get_tree().get_nodes_in_group("blocks")

	for b in blocks:
		b.find_parent_block()

	for b in blocks:
		b.set_child_blocks()

	for b in blocks:
		if b.is_player:
			self.player_block = b
			break

	self.director.player_block = self.player_block
	self.director.setup()

################################################################################
## Zooming

func zoom():
	var new_player = self.player_block.parent_block
	new_player.is_player = true
	self.player_block.is_player = false
	self.player_block = new_player
	self.director.player_block = new_player

	self.director.zoom_camera()

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
	var prev_parent = self.player_block.parent_block
	self.player_block.try_move(move_vect)

	if not self.player_block.parent_block == prev_parent:
		self.director.move_camera(move_vect)

func _process(delta):
	poll_directions()

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
