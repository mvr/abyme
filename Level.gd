extends Node2D

onready var director   = self.get_node("Director")

var blocks = []
var player_block = null

func _ready():
	self.set_process_input(true)

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
	self.director.arrange_initial()

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

func _input(event):
	if event.is_action_pressed("move_left"):
		self.player_block.try_move("left")

	if event.is_action_pressed("move_right"):
		self.player_block.try_move("right")

	if event.is_action_pressed("move_up"):
		self.player_block.try_move("up")

	if event.is_action_pressed("move_down"):
		self.player_block.try_move("down")

	if event.is_action_pressed("zoom"):
		self.zoom()
