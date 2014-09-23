def loop(): Boolean = loop

def and(x: Boolean, y: => Boolean) = if(x) y else false;

and(true, true) == true
and(false, true) == false
and(false, false) == false
and(false, loop) == false


def or(x: Boolean, y: => Boolean) = if(x) true else y;

or(true, true) == true
or(false, true) == true
or(true, false) == true
or(false, false) == false
or(true, loop) == true
