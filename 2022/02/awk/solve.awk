#!/usr/bin/env -S awk -f
/A X/ { sum[0] += 4; sum[1] += 3}
/A Y/ { sum[0] += 8; sum[1] += 4}
/A Z/ { sum[0] += 3; sum[1] += 8}
/B X/ { sum[0] += 1; sum[1] += 1}
/B Y/ { sum[0] += 5; sum[1] += 5}
/B Z/ { sum[0] += 9; sum[1] += 9}
/C X/ { sum[0] += 7; sum[1] += 2}
/C Y/ { sum[0] += 2; sum[1] += 6}
/C Z/ { sum[0] += 6; sum[1] += 7}
END { print sum[0]; print sum[1] }
