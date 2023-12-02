#!/usr/bin/env -S awk -f

BEGIN {
	FS="[:;, ]"
}

!/(1[3-9]|[2-9][0-9]) red|(1[4-9]|[2-9][0-9]) green|(1[5-9]|[2-9][0-9]) blue/ {
	sum[0]+=$2
}

{
	red=green=blue=0
	for (i=4; i<=NF; i+=3) {
		switch ($(i+1)) {
		case "red":
			red = $i > red ? $i : red
			break
		case "green":
			green = $i > green ? $i : green
			break
		case "blue":
			blue = $i > blue ? $i : blue
			break
		}
	}
	sum[1]+=red * green * blue
}

END {
	print sum[0]
	print sum[1]
}
