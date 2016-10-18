extends Node2D

var blocks = []

func _ready():
	var blocks = get_children()
	for b in blocks:
		b.find_parent_block()
	for b in blocks:
		b.set_child_blocks()