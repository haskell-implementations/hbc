module -- flags
export PrMtrans, realname, Sparc8, I486, I586, NoMOpt, FarJump, Solaris, Power, PowerPC, PIC, Linux;

rec PrMtrans = mem "-fmtrans" argv
and Sparc8   = mem "-msparc8" argv
and I486     = mem "-mi486"   argv
and I586     = mem "-mi586"   argv
and NoMOpt   = mem "-fno-m-opt" argv
and FarJump  = ~ (mem "-mno-far-jump" argv)
and Solaris  = mem "-msolaris" argv
and Linux    = mem "-mlinux" argv
and Power    = mem "-mPOWER" argv
and PowerPC  = mem "-mPowerPC" argv
and PIC      = mem "-fPIC" argv
and realname = "xxx"
end
