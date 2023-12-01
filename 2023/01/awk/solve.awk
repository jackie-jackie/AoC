#!/usr/bin/env -S awk -f

function english_to_int(en) {
	switch (en) {
	case "one":
		return 1
	case "two":
		return 2
	case "three":
		return 3
	case "four":
		return 4
	case "five":
		return 5
	case "six":
		return 6
	case "seven":
		return 7
	case "eight":
		return 8
	case "nine":
		return 9
	default:
		return en
	}
}

{
	# part 1
	match($0, /[0-9]/, first)
	match($0, /.*([0-9])/, last) # star is greedy, so prefixing .* gets last match
	sum[0] += first[0] last[1]
	# part 2
	match($0, /[0-9]|one|two|three|four|five|six|seven|eight|nine/, first)
	match($0, /.*([0-9]|one|two|three|four|five|six|seven|eight|nine)/, last)
	sum[1] += english_to_int(first[0]) english_to_int(last[1])
}

END {
	print sum[0]
	print sum[1]
}
