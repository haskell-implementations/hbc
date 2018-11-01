/*
**	random:		generate a random number given a seed.
**
*/
/* some constants we need */
#define A 16807
#define M 2147483647			/* Mersenne prime 2^31 -1 */
#define Q 127773			/* M div A (M / A) */
#define R 2836				/* M mod A (M % A) */
/* 10000 numbers with seed 1 should give 1043618065*/
#ifdef TEST
(let t = 1043618065 in
let rec f 0 s = s
||      f n s = f (n-1) (random s)
in	if t = f 10000 1 then "ok\n" else "wrong\n")
where
#else

module
export random;
#endif

random seed = 
	let hi = seed / Q
	and lo = seed % Q
	in let seed = A * lo - R * hi
	in let seed = if seed < 0 then seed + M else seed
	in seed
#ifndef TEST
end
#endif
