func filter_nonnull(arr):
	return null
    # TODO

func pos_div(scale, p):
	return null
	# TODO

func pos_mod(scale, p):
	return null
	# TODO

func int_exp(i,e):
	if e < 0:
		return 1.0 / int_exp(i,-e)
	if e == 0:
		return 1
	else:
		return i * int_exp(i, e-1)

func log_base(x, b):
	return log(x) / log(b)
