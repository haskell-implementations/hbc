module
export Addrmode, Tag, Operator, Conditioncode, Mcode;
rec type Addrmode =	
	Vp +		-- Value stack pointer register (dump)
	Vind Int +	-- Vp indexing
	Vrel Int +	-- Pointer value Vp+offset
	pushV +		-- Push on V stack
	popV +		-- Pop from V stack
	
	Sp +		-- Pointer stack pointer register
	Sind Int +	-- Sp indexing
	Srel Int +	-- Pointer value Sp+index
	pushS +		-- Push on S stack
	popS +		-- Pop on S stack

	reg Int +	-- General register
	regind Int Int+ -- register # offset
	regrel Int Int+ -- the pointer value reg + offset

	hp +		-- the hp register
	hpind Int +	-- indirect hp register
	hprel Int +	-- the value hp+offset
	tohp +		--
	
	glob String +	-- Global identifier
	idlit String +	-- Global pointer constant
	const Int +	-- Basic value constant
	fconst Double +	-- Basic value constant
	retaddr String	-- as idlit on most machines

and type Tag =	oeval + ounwind + ojfun + ogettag + onumtag Int
and type Operator =	add + sub + mul + div + mod + neg + btand + btor + btxor + btcompl + btlsh + btrsh + btrsha + dfadd + dfsub + dfmul + dfdiv + dfneg + dftoi + itodf + sfadd + sfsub + sfmul + sfdiv + sfneg + sftoi + itosf + sftodf + dftosf
and type Conditioncode =
		eq + ne +
		lt + gt + le + ge + 	-- signed relational
		ltstack + ltheap +	-- comparing stack ans heap addresses
		gtstack + geheap +
		sfeq + sfne +		-- single floating compare
		sflt + sfgt + sfle + sfge +
		dfeq + dfne +		-- double floating compare
		dflt + dfgt + dfle + dfge 
and type Mcode =
	Mmove Addrmode Addrmode +			-- move data
	Mmovesf Addrmode Addrmode +			-- move data
	Mmovedf Addrmode Addrmode +			-- move data
	Madda Addrmode Addrmode +			-- add integer to address
	Mcall String +					-- ordinary call
	Mjumpf String +					-- jump to function
	Mreturn +					-- ordinary return
	Mcalltag Tag Int +				-- call; mode and register number
	Mjumptag Tag Int +				-- jump; mode and register number
	Mjump String +					-- ordinary jump
	Mjumpind Addrmode +				-- indirect jump
	Mcallind Addrmode +				-- indirect call
	Mjcond Conditioncode String +
	Mlabel String +
	Mcompare Addrmode Addrmode +
	Mcomparesf Addrmode Addrmode +
	Mcomparedf Addrmode Addrmode +
	Mop2 Operator Addrmode Addrmode +
	Mop3 Operator Addrmode Addrmode Addrmode +
	Mcase Addrmode Int Int Int (List String) Int +	-- Multiway branch:
					    -- case value
					    -- low limit, high limit,
					    -- max possible value,
					    -- labels
					    -- unique number
	Mboolcc Conditioncode Addrmode +		-- convert cc to bool

        Mnoop   +					-- insert a nop instruction (if applicable)
	Mdata	+					-- switch to data segment
	Mtext	+					-- switch to code segment
	Mtext1	+					-- switch to alternate code segment
	Mword Addrmode +				-- a word with given contents
	Msfloat String +				-- a single float
	Mdfloat String +				-- a double float
	Mstring String +				-- A null terminated ascii string
	Mexport String +				-- Export a symbol
	Mfunbegin String Int +				-- emitted at function start
	Mfunend	+					-- emitted at function end

	-- Directives
	Malign +					-- Align to word boundary
	Mcom String +					-- Comment in the assembler code
	Mpragma String +				-- Semi-comment in the assembler code

	-- Special
	Masm String (List Addrmode)			-- Only used in runtime system
end





