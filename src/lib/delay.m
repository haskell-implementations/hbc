/*
**	delay:		delay output
**			delay <delay> delays output with <delay> milliseconds.
*/

module
export delay;
    delay d = [CDELAY; chr d]
end

