head -c 10 enwik5 | od -An -t u1 -v -w1 | tr -d ' ' | paste -sd, -
