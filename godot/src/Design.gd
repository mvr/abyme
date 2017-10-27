extends Node2D

################################################################################
## Setup

onready var shapes = self.get_node("Shapes").get_children()
var player_chunk = null # Is this needed

################################################################################
## Setup

func _ready():
	for b in shapes:
		b.set_given_parent()

	for b in shapes:
		b.set_child_shapes(shapes)

	for b in shapes:
		if b.is_player:
			self.player_chunk = Chunk.new(b)
			break

################################################################################
## ???
