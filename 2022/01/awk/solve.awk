#!/usr/bin/env -S awk -f
/./ {
	sum += $0
}

/^$/ {
	if (sum > max[0]) {
		max[2] = max[1]
		max[1] = max[0]
		max[0] = sum
	} else if (sum > max[1]) {
		max[2] = max[1]
		max[1] = sum
	} else if (sum > max[2]) {
		max[2] = sum
	}
	sum = 0
}

END {
	print max[0]
	print max[0]+max[1]+max[2]
}
