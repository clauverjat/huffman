with Liste;
package Byte is
    type T_Byte is mod 2**8;
    package Liste_Byte is
	    new Liste(T_Element => T_Byte);
end Byte;
