#!/usr/bin/env -S awk -f

{
	# copy values for destructive operations
	for (i = 1; i <= NF; i++) {
		buffer[0][i] = buffer[1][i] = $i
	}
	for (limit = NF; limit > 1; limit--) {
		for (i = 1; i < limit; i++) {
			buffer[0][i] = buffer[0][i+1] - buffer[0][i]
			buffer[1][NF-i+1] -= buffer[1][NF-i]
		}
	}
	for (i = 1; i <= NF; i++) {
		sum[0] += buffer[0][i]
		buffer[1][NF-i] -= buffer[1][NF-i+1]
	}
	sum[1] += buffer[1][1]
}

END {
	print sum[0]
	print sum[1]
}
