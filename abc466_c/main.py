n = int(input())

# 最終的に見つかった区間で(v*(v-1) /2)のnC2なんだよな
ans = 0
r = 2
for l in range(1,n+1):
    if r < l +1:
        r = l +1
    while r <= n:
        print(f"? {l} {r}", flush=True)
        if input() == "Yes":
            r += 1
        else:
            break
    ans += (r-1) -l

print(f"! {ans}")
