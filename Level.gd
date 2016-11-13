extends Node2D

var blocks = []
var player_block = null

func _ready():
	var blocks = get_tree().get_nodes_in_group("blocks")

	for b in blocks:
		b.find_parent_block()

	for b in blocks:
		b.set_child_blocks()

	for b in blocks:
		if b.is_player:
			self.player_block = b
			break

	var director = get_node("Director")
	director.player_block = self.player_block
	director.arrange_initial()
