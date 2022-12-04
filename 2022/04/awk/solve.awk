#!/usr/bin/env -S awk -f
match($0, /([0-9]*)-([0-9]*),([0-9]*)-([0-9]*)/, a) {
	if ((a[2] >= a[4] && a[1] <= a[3]) || (a[1] >= a[3] && a[2] <= a[4]))
		 count[0]++
	if ((a[2] >= a[3] && a[1] <= a[4]) || (a[4] >= a[1] && a[3] <= a[2]))
		count[1]++
}
END { print count[0]; print count[1] }
