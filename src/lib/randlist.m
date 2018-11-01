/*
**	randlist:	gives an infinite list of pseudo-random numbers,
**			each number lies within a specified range;
**			a seed (an odd number) has to be provided.
**
**		randlist seed lo hi
*/
#define M 2147483647			/* Mersenne prime 2^31 -1 */
#define K 10000
module
-- WARNING: not self contained
export randlist;
rec

randlist seed low high =
	let r1 = random seed in
	let r2 = random r1 in
	let r = (r1/(M/K))*K + (r2/(M/K)) in
	r % (high-low+1) + low . randlist r2 low high
end
