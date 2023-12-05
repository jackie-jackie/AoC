#!/usr/bin/env -S awk -f

/seeds: / {
	for (i = 2; i <= NF; i++) {
		output[0][i-2] = $i
		output[1][i-2] = $i
	}
}

/map:/ {
	for (i = 0; i < length(output[0]); i++) {
		input[0][i] = output[0][i]
	}
	for (i = 0; i < length(output[1]); i++) {
		input[1][i] = output[1][i]
	}
}


/^[0-9]+ [0-9]+ [0-9]+$/ {
	dst = $1
	src = $2
	len = $3
	/* part 1 */
	for (i = 0; i < length(input[0]); i++) {
		diff = input[0][i] - src
		if (diff >= 0 && diff < len) {
			output[0][i] = dst + diff
		}
	}
	/* part 2 */
	for (i = 0; i < length(input[1]); i+=2) {
		start = input[1][i]
		end = start + input[1][i+1]
		if (start < src && end > src) { /* split of prefix */
			output[1][length(output[1])] = input[1][length(input[1])] = start
			output[1][length(output[1])] = input[1][length(input[1])] = src - start
			start = input[1][i] = src
			input[1][i+1] = end - src
		}
		if (start < src + len && end > src + len) { /* split of suffix */
			output[1][length(output[1])] = input[1][length(input[1])] = src + len
			output[1][length(output[1])] = input[1][length(input[1])] = end - (src + len)
			end = src + len
			input[1][i+1] = end - start
		}
		if (start >= src && end <= src + len) { /* range is guaranteed fully in mapping */
			output[1][i] = dst + (start - src)
			output[1][i+1] = input[1][i+1]
		}
	}
}

END {
	min[0] = output[0][0]
	min[1] = output[1][0]
	for (i = 1; i < length(output[0]); i++) {
		if (output[0][i] < min[0]) {
			min[0] = output[0][i]
		}
	}
	for (i = 2; i < length(output[1]); i += 2) {
		if (output[1][i] < min[1]) {
			min[1] = output[1][i]
		}
	}
	print min[0]
	print min[1]
}
